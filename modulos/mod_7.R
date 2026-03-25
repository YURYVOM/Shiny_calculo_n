# =========================================================
# MÓDULO 7: Resultados finales, asignación por área y exportación
# =========================================================

mod_resultados_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 7. Resultados finales"),
      uiOutput(ns("ui_selector_m")),
      radioButtons(
        ns("usa_area"),
        "¿Desea hacer asignación por área urbano/rural mediante IPFP?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_area")),
        radioButtons(
          ns("tipo_ingreso_ipfp"),
          "Forma de ingreso para IPFP:",
          choices = c("Proporciones" = "proporciones", "Totales" = "totales"),
          selected = "proporciones",
          inline = TRUE
        ),
        p("Ingrese valores de urbano y rural para cada DAM (o Nacional)."),
        uiOutput(ns("ipfp_ui"))
      ),
      h4("Resumen final"),
      tableOutput(ns("tabla_resultados")),
      p(strong(textOutput(ns("descripcion_escenario"), inline = TRUE))),
      h4(textOutput(ns("titulo_tabla_final"))),
      tableOutput(ns("tabla_salida_final")),
      br(),
      h4("Código R reproducible"),
      tags$pre(style = "max-height: 260px; overflow-y: auto;", textOutput(ns("codigo_r"))),
      downloadButton(ns("descargar_codigo"), "Descargar código R completo", class = "btn btn-success")
    )
  )
}

mod_resultados_server <- function(id, entrada_base) {
  moduleServer(id, function(input, output, session) {

    output$ui_selector_m <- renderUI({
      d <- entrada_base(); req(d)

      if (isTRUE(d$usa_dominios)) {
        tagList(
          p(strong("Se utilizarán los valores de m seleccionados por DAM en el módulo 6.")),
          tableOutput(session$ns("tabla_m_dam_resumen"))
        )
      } else {
        tags$p(strong(paste("Valor nacional de m seleccionado en el módulo 6:", d$m_seleccionado_nacional)))
      }
    })

    output$tabla_m_dam_resumen <- renderTable({
      d <- entrada_base(); req(d)
      if (!isTRUE(d$usa_dominios)) return(NULL)
      data.frame(
        DAM = seq_along(d$m_seleccionado_dam),
        m_seleccionado = d$m_seleccionado_dam,
        row.names = NULL
      )
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)

    nombres_dominio <- reactive({
      d <- entrada_base(); req(d)
      if (isTRUE(d$usa_dominios)) {
        paste0("DAM ", seq_len(d$n_dominios))
      } else {
        "Nacional"
      }
    })

    ipfp_df <- reactive({
      d <- entrada_base(); req(d)
      if (!identical(input$usa_area, "si")) return(NULL)

      dominios <- nombres_dominio()
      datos <- lapply(seq_along(dominios), function(i) {
        urbano <- input[[paste0("ipfp_urbano_", i)]]
        rural <- input[[paste0("ipfp_rural_", i)]]
        data.frame(
          DAM = dominios[i],
          urbano = as.numeric(urbano),
          rural = as.numeric(rural),
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, datos)
    })

    output$ipfp_ui <- renderUI({
      d <- entrada_base(); req(d)
      req(identical(input$usa_area, "si"))

      dominios <- nombres_dominio()
      ayuda <- if (identical(input$tipo_ingreso_ipfp, "proporciones")) {
        "Cada fila debe sumar 1 (urbano + rural)."
      } else {
        "Puede ingresar totales; la app los transformará en proporciones dentro de cada fila."
      }

      filas <- lapply(seq_along(dominios), function(i) {
        fluidRow(
          column(3, tags$div(style = "padding-top: 30px; font-weight: 700;", dominios[i])),
          column(4, numericInput(session$ns(paste0("ipfp_urbano_", i)), "Urbano", value = if (identical(input$tipo_ingreso_ipfp, "proporciones")) NA else 0, min = 0, step = 0.001)),
          column(4, numericInput(session$ns(paste0("ipfp_rural_", i)), "Rural", value = if (identical(input$tipo_ingreso_ipfp, "proporciones")) NA else 0, min = 0, step = 0.001))
        )
      })

      tagList(
        p(ayuda),
        do.call(tagList, filas)
      )
    })

    validacion <- reactive({
      d <- entrada_base(); req(d)

      if (!isTRUE(d$usa_dominios)) {
        if (is.null(d$m_seleccionado_nacional) || is.na(d$m_seleccionado_nacional) || !(d$m_seleccionado_nacional %in% d$m_vector)) {
          return("No hay un valor nacional válido de m seleccionado desde el módulo 6.")
        }
      } else {
        if (length(d$m_seleccionado_dam) != d$n_dominios || any(is.na(d$m_seleccionado_dam)) || any(!(d$m_seleccionado_dam %in% d$m_vector))) {
          return("Revise la selección de m por DAM realizada en el módulo 6.")
        }
      }

      if (identical(input$usa_area, "si")) {
        datos <- ipfp_df()
        if (is.null(datos) || any(is.na(datos$urbano)) || any(is.na(datos$rural))) {
          return("Complete los valores de urbano y rural para IPFP.")
        }

        if (any(datos$urbano < 0) || any(datos$rural < 0)) {
          return("Los valores de IPFP no pueden ser negativos.")
        }

        if (identical(input$tipo_ingreso_ipfp, "proporciones")) {
          suma_filas <- datos$urbano + datos$rural
          if (any(abs(suma_filas - 1) > 1e-6)) {
            return("Cada fila debe sumar 1 entre urbano y rural cuando se usan proporciones.")
          }
        } else {
          if (any((datos$urbano + datos$rural) <= 0)) {
            return("En ingreso por totales, cada fila debe tener suma mayor a 0.")
          }
        }
      }

      NULL
    })

    tabla_base_final <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)

      if (isTRUE(d$usa_dominios)) {
        tabla_detalle <- d$tabla_m_seleccionado_dam
        req(nrow(tabla_detalle) > 0)
        data.frame(
          DAM = paste0("DAM ", tabla_detalle$DAM),
          HouseholdsPerPSU = tabla_detalle$HouseholdsPerPSU,
          DEFF = tabla_detalle$DEFF,
          PSUinSample = tabla_detalle$PSUinSample,
          HouseholdsInSample = tabla_detalle$HouseholdsInSample,
          stringsAsFactors = FALSE
        )
      } else {
        base_res <- if (d$tipo_param == "Media") {
          if (identical(d$unidad, "Personas")) {
            ss4HHSm(
              N = d$N,
              M = d$M,
              rho = d$rho,
              r = d$r,
              b = d$b,
              mu = d$xbarra,
              sigma = d$s,
              delta = d$delta,
              conf = d$conf,
              m = d$m_seleccionado_nacional
            )
          } else {
            ss4HHSm(
              N = d$N,
              M = d$M,
              rho = d$rho,
              mu = d$xbarra,
              sigma = d$s,
              delta = d$delta,
              conf = d$conf,
              m = d$m_seleccionado_nacional
            )
          }
        } else {
          if (identical(d$unidad, "Personas")) {
            ss4HHSp(
              N = d$N,
              M = d$M,
              rho = d$rho,
              r = d$r,
              b = d$b,
              P = d$p,
              delta = d$delta,
              conf = d$conf,
              m = d$m_seleccionado_nacional
            )
          } else {
            ss4HHSp(
              N = d$N,
              M = d$M,
              r = 1,
              b = 1,
              rho = d$rho,
              P = d$p,
              delta = d$delta,
              conf = d$conf,
              m = d$m_seleccionado_nacional
            )
          }
        }

        data.frame(
          Nivel = "Nacional",
          HouseholdsPerPSU = base_res$HouseholdsPerPSU[1],
          DEFF = base_res$DEFF[1],
          PSUinSample = base_res$PSUinSample[1],
          HouseholdsInSample = base_res$HouseholdsInSample[1],
          stringsAsFactors = FALSE
        )
      }
    })

    tabla_area_final <- reactive({
      validate(need(is.null(validacion()), validacion()))
      req(identical(input$usa_area, "si"))

      tabla_base <- tabla_base_final()
      datos_ipfp <- ipfp_df()

      if (!isTRUE(entrada_base()$usa_dominios)) {
        fila <- datos_ipfp[1, c("urbano", "rural")]
        prop_fila <- as.numeric(fila)
        if (identical(input$tipo_ingreso_ipfp, "totales")) {
          prop_fila <- prop_fila / sum(prop_fila)
        }
        asignacion <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$HouseholdsInSample[1])

        return(data.frame(
          Nivel = "Nacional",
          HouseholdsInSample = tabla_base$HouseholdsInSample[1],
          distribucion_urbano = as.numeric(asignacion[1, 1]),
          distribucion_rural = as.numeric(asignacion[1, 2]),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(seq_len(nrow(tabla_base)), function(i) {
        fila <- datos_ipfp[i, c("urbano", "rural")]
        prop_fila <- as.numeric(fila)
        if (identical(input$tipo_ingreso_ipfp, "totales")) {
          prop_fila <- prop_fila / sum(prop_fila)
        }
        asignacion <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$HouseholdsInSample[i])
        data.frame(
          DAM = tabla_base$DAM[i],
          HouseholdsInSample = tabla_base$HouseholdsInSample[i],
          distribucion_urbano = as.numeric(asignacion[1, 1]),
          distribucion_rural = as.numeric(asignacion[1, 2]),
          stringsAsFactors = FALSE
        )
      }))
    })

    alcance_resultado <- reactive({
      d <- entrada_base(); req(d)
      if (!isTRUE(d$usa_dominios) && !identical(input$usa_area, "si")) return("Solo nivel nacional")
      if (isTRUE(d$usa_dominios) && !identical(input$usa_area, "si")) return("Nivel DAM")
      if (!isTRUE(d$usa_dominios) && identical(input$usa_area, "si")) return("Nacional + área urbano-rural")
      "DAM + área urbano-rural"
    })

    tabla_salida_final <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)

      if (!identical(input$usa_area, "si")) {
        base <- tabla_base_final()
        return(base)
      }

      tabla_area_final()
    })

    output$titulo_tabla_final <- renderText({
      d <- entrada_base(); req(d)
      if (!identical(input$usa_area, "si")) {
        if (isTRUE(d$usa_dominios)) {
          "Asignación final por DAM"
        } else {
          "Asignación nacional final"
        }
      } else {
        if (isTRUE(d$usa_dominios)) {
          "Asignación final por DAM y área"
        } else {
          "Asignación final nacional y por área"
        }
      }
    })

    output$descripcion_escenario <- renderText({
      d <- entrada_base(); req(d)
      if (!isTRUE(d$usa_dominios) && !identical(input$usa_area, "si")) {
        return("Caso 1: selección solo nacional, sin asignación por área.")
      }
      if (isTRUE(d$usa_dominios) && !identical(input$usa_area, "si")) {
        return("Caso 2: selección con representatividad DAM, sin asignación por área.")
      }
      if (!isTRUE(d$usa_dominios) && identical(input$usa_area, "si")) {
        return("Caso 3: selección nacional con asignación urbano/rural.")
      }
      "Caso 4: selección con representatividad DAM y asignación urbano/rural dentro de cada DAM."
    })

    calc <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)

      tabla_base <- tabla_base_final()
      tabla_final <- tabla_salida_final()

      n_hogares_total <- sum(tabla_base$HouseholdsInSample)
      upm_total <- sum(tabla_base$PSUinSample)
      n_encuestas <- if (isTRUE(d$usa_dominios)) {
        ceiling(sum(tabla_base$HouseholdsInSample * d$r_dam * d$b_dam))
      } else {
        ceiling(n_hogares_total * d$r * d$b)
      }

      list(
        resumen = data.frame(
          Indicador = c("Valor(es) de m seleccionado(s)", "Tamaño de muestra total", "Número estimado de encuestas", "Número total de UPM", "Alcance final"),
          Valor = c(
            if (isTRUE(d$usa_dominios)) paste(d$m_seleccionado_dam, collapse = ", ") else d$m_seleccionado_nacional,
            n_hogares_total,
            n_encuestas,
            upm_total,
            alcance_resultado()
          ),
          stringsAsFactors = FALSE
        ),
        tabla_final = tabla_final
      )
    })

    output$tabla_resultados <- renderTable(calc()$resumen, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$tabla_salida_final <- renderTable(calc()$tabla_final, striped = TRUE, bordered = TRUE, rownames = FALSE)

    codigo_r <- reactive({
      d <- entrada_base(); req(d)
      usa_area <- identical(input$usa_area, "si")
      usa_dominios <- isTRUE(d$usa_dominios)
      tipo_ingreso_ipfp <- if (!is.null(input$tipo_ingreso_ipfp)) input$tipo_ingreso_ipfp else "proporciones"

      base_script <- paste0(
"# Script reproducible - módulo 7\n",
"source('modulos/mod_utils.R')\n\n",
"tipo_param <- '", d$tipo_param, "'\n",
"unidad <- '", d$unidad, "'\n",
"N <- ", d$N, "; M <- ", d$M, "; rho <- ", d$rho, "\n",
"delta <- ", d$delta, "; conf <- ", d$conf, "\n",
"xbarra <- ", d$xbarra, "; s <- ", d$s, "; p <- ", d$p, "\n",
"r <- ", d$r, "; b <- ", d$b, "\n\n"
      )

      if (!usa_dominios && !usa_area) {
        return(paste0(
          base_script,
          "# Caso 1: Solo nivel nacional\n",
          "m_nacional <- ", d$m_seleccionado_nacional, "\n",
          "resultado_nacional <- if (tipo_param == 'Media') {\n",
          "  if (unidad == 'Personas') ss4HHSm(N=N,M=M,rho=rho,r=r,b=b,mu=xbarra,sigma=s,delta=delta,conf=conf,m=m_nacional)\n",
          "  else ss4HHSm(N=N,M=M,rho=rho,mu=xbarra,sigma=s,delta=delta,conf=conf,m=m_nacional)\n",
          "} else {\n",
          "  if (unidad == 'Personas') ss4HHSp(N=N,M=M,rho=rho,r=r,b=b,P=p,delta=delta,conf=conf,m=m_nacional)\n",
          "  else ss4HHSp(N=N,M=M,rho=rho,r=1,b=1,P=p,delta=delta,conf=conf,m=m_nacional)\n",
          "}\n",
          "tabla_final <- data.frame(\n",
          "  Nivel = 'Nacional',\n",
          "  HouseholdsPerPSU = resultado_nacional$HouseholdsPerPSU[1],\n",
          "  DEFF = resultado_nacional$DEFF[1],\n",
          "  PSUinSample = resultado_nacional$PSUinSample[1],\n",
          "  HouseholdsInSample = resultado_nacional$HouseholdsInSample[1]\n",
          ")\n\nprint(tabla_final)\n"
        ))
      }

      if (usa_dominios && !usa_area) {
        tabla_dam_txt <- paste(capture.output(dput(tabla_base_final())), collapse = "\n")
        return(paste0(
          base_script,
          "# Caso 2: Nivel DAM\n",
          "tabla_final <- ", tabla_dam_txt, "\n\n",
          "print(tabla_final)\n"
        ))
      }

      if (!usa_dominios && usa_area) {
        tabla_ipfp_txt <- paste(capture.output(dput(ipfp_df())), collapse = "\n")
        return(paste0(
          base_script,
          "# Caso 3: Nacional + área urbano-rural\n",
          "resultado_nacional <- ", paste(capture.output(dput(tabla_base_final())), collapse = "\n"), "\n",
          "ipfp <- ", tabla_ipfp_txt, "\n",
          "prop <- c(ipfp$urbano[1], ipfp$rural[1])\n",
          if (identical(tipo_ingreso_ipfp, "totales")) "prop <- prop / sum(prop)\n" else "",
          "asig <- ipfp_aproximada(matrix(prop, nrow=1), resultado_nacional$HouseholdsInSample[1])\n",
          "tabla_final <- data.frame(\n",
          "  Nivel='Nacional',\n",
          "  HouseholdsInSample=resultado_nacional$HouseholdsInSample[1],\n",
          "  distribucion_urbano=as.numeric(asig[1,1]),\n",
          "  distribucion_rural=as.numeric(asig[1,2])\n",
          ")\n\nprint(tabla_final)\n"
        ))
      }

      tabla_ipfp_txt <- paste(capture.output(dput(ipfp_df())), collapse = "\n")
      tabla_dam_txt <- paste(capture.output(dput(tabla_base_final())), collapse = "\n")
      paste0(
        base_script,
        "# Caso 4: DAM + área urbano-rural\n",
        "tabla_dam <- ", tabla_dam_txt, "\n",
        "ipfp <- ", tabla_ipfp_txt, "\n",
        "tabla_final <- do.call(rbind, lapply(seq_len(nrow(tabla_dam)), function(i){\n",
        "  prop <- c(ipfp$urbano[i], ipfp$rural[i])\n",
        if (identical(tipo_ingreso_ipfp, "totales")) "  prop <- prop / sum(prop)\n" else "",
        "  asig <- ipfp_aproximada(matrix(prop, nrow=1), tabla_dam$HouseholdsInSample[i])\n",
        "  data.frame(\n",
        "    DAM = tabla_dam$DAM[i],\n",
        "    HouseholdsInSample = tabla_dam$HouseholdsInSample[i],\n",
        "    distribucion_urbano = as.numeric(asig[1,1]),\n",
        "    distribucion_rural = as.numeric(asig[1,2])\n",
        "  )\n",
        "}))\n\nprint(tabla_final)\n"
      )
    })

    output$codigo_r <- renderText(codigo_r())

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("calculo_muestra_completo_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_r(), file)
    )



    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          usa_area = identical(input$usa_area, "si"),
          tipo_ingreso_ipfp = input$tipo_ingreso_ipfp,
          tabla_ipfp = if (identical(input$usa_area, "si")) ipfp_df() else NULL,
          tabla_consolidada = tabla_salida_final()
        )
      })
    )
  })
}
