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
      fluidRow(
        column(6, downloadButton(ns("descargar_codigo"), "Descargar código R completo", class = "btn btn-success")),
        column(6, uiOutput(ns("ui_descarga_dam")))
      )
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
          m_seleccionado = tabla_detalle$HouseholdsPerPSU,
          tamano_muestra = tabla_detalle$HouseholdsInSample,
          UPM = tabla_detalle$PSUinSample,
          stringsAsFactors = FALSE
        )
      } else {
        n_base <- if (d$tipo_param == "Media") {
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
              rho = d$rho,
              P = d$p,
              delta = d$delta,
              conf = d$conf,
              m = d$m_seleccionado_nacional
            )
          }
        }

        data.frame(
          DAM = "Nacional",
          m_seleccionado = d$m_seleccionado_nacional,
          tamano_muestra = as.numeric(n_base),
          UPM = ceiling(as.numeric(n_base) / d$m_seleccionado_nacional),
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
        asignacion <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$tamano_muestra[1])

        return(data.frame(
          Nivel = "Nacional",
          tamano_muestra = tabla_base$tamano_muestra[1],
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
        asignacion <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$tamano_muestra[i])
        data.frame(
          DAM = tabla_base$DAM[i],
          tamano_muestra = tabla_base$tamano_muestra[i],
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
      if (!isTRUE(d$usa_dominios) && identical(input$usa_area, "si")) return("Nivel nacional + área urbano/rural")
      "Nivel DAM + área urbano/rural"
    })

    tabla_salida_final <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)

      if (!identical(input$usa_area, "si")) {
        base <- tabla_base_final()
        if (!isTRUE(d$usa_dominios)) {
          return(data.frame(
            Nivel = "Nacional",
            m_seleccionado = base$m_seleccionado,
            tamano_muestra = base$tamano_muestra,
            UPM = base$UPM,
            stringsAsFactors = FALSE
          ))
        }
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

      n_hogares_total <- sum(tabla_base$tamano_muestra)
      upm_total <- sum(tabla_base$UPM)
      n_encuestas <- if (isTRUE(d$usa_dominios)) {
        ceiling(sum(tabla_base$tamano_muestra * d$r_dam * d$b_dam))
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

      tabla_ipfp_txt <- if (usa_area) paste(capture.output(dput(ipfp_df())), collapse = "\n") else "NULL"
      tabla_dam_txt <- if (usa_dominios) paste(capture.output(dput(d$tabla_m_seleccionado_dam)), collapse = "\n") else "NULL"

      paste0(
"# =========================================================\n",
"# Script reproducible exportado desde la app\n",
"# Módulo 7 - cuatro escenarios de asignación final\n",
"# =========================================================\n\n",
"source('modulos/mod_utils.R')\n\n",
"# -------------------- Entradas guardadas -----------------\n",
"tipo_param <- '", d$tipo_param, "'\n",
"unidad <- '", d$unidad, "'\n",
"N <- ", d$N, "\n",
"M <- ", d$M, "\n",
"rho <- ", d$rho, "\n",
"delta <- ", d$delta, "\n",
"conf <- ", d$conf, "\n",
"xbarra <- ", d$xbarra, "\n",
"s <- ", d$s, "\n",
"p <- ", d$p, "\n",
"r <- ", d$r, "\n",
"b <- ", d$b, "\n",
"usa_dominios <- ", tolower(as.character(usa_dominios)), "\n",
"usa_area <- ", tolower(as.character(usa_area)), "\n",
"tipo_ingreso_ipfp <- '", tipo_ingreso_ipfp, "'\n",
"m_nacional <- ", d$m_seleccionado_nacional, "\n",
"tabla_dam_seleccionada <- ", tabla_dam_txt, "\n",
"tabla_ipfp_ingresada <- ", tabla_ipfp_txt, "\n\n",
"# -------------------- Funciones auxiliares ----------------\n",
"calcular_base_nacional <- function() {\n",
"  if (tipo_param == 'Media') {\n",
"    n_base <- if (unidad == 'Personas') {\n",
"      ss4HHSm(N = N, M = M, rho = rho, r = r, b = b, mu = xbarra, sigma = s, delta = delta, conf = conf, m = m_nacional)\n",
"    } else {\n",
"      ss4HHSm(N = N, M = M, rho = rho, mu = xbarra, sigma = s, delta = delta, conf = conf, m = m_nacional)\n",
"    }\n",
"  } else {\n",
"    n_base <- if (unidad == 'Personas') {\n",
"      ss4HHSp(N = N, M = M, rho = rho, r = r, b = b, P = p, delta = delta, conf = conf, m = m_nacional)\n",
"    } else {\n",
"      ss4HHSp(N = N, M = M, rho = rho, P = p, delta = delta, conf = conf, m = m_nacional)\n",
"    }\n",
"  }\n",
"  data.frame(\n",
"    DAM = 'Nacional',\n",
"    m_seleccionado = m_nacional,\n",
"    tamano_muestra = as.numeric(n_base),\n",
"    UPM = ceiling(as.numeric(n_base) / m_nacional)\n",
"  )\n",
"}\n\n",
"normalizar_fila <- function(urbano, rural, tipo_ingreso) {\n",
"  fila <- c(urbano, rural)\n",
"  if (tipo_ingreso == 'totales') fila <- fila / sum(fila)\n",
"  fila\n",
"}\n\n",
"# -------------------- Tabla base --------------------------\n",
"tabla_base <- if (isTRUE(usa_dominios)) {\n",
"  data.frame(\n",
"    DAM = paste0('DAM ', tabla_dam_seleccionada$DAM),\n",
"    m_seleccionado = tabla_dam_seleccionada$HouseholdsPerPSU,\n",
"    tamano_muestra = tabla_dam_seleccionada$HouseholdsInSample,\n",
"    UPM = tabla_dam_seleccionada$PSUinSample\n",
"  )\n",
"} else {\n",
"  calcular_base_nacional()\n",
"}\n\n",
"# -------------------- Cuatro escenarios -------------------\n",
"if (!isTRUE(usa_dominios) && !isTRUE(usa_area)) {\n",
"  # Caso 1: nacional sin área\n",
"  tabla_final <- data.frame(\n",
"    Nivel = 'Nacional',\n",
"    m_seleccionado = tabla_base$m_seleccionado,\n",
"    tamano_muestra = tabla_base$tamano_muestra,\n",
"    UPM = tabla_base$UPM\n",
"  )\n",
"} else if (isTRUE(usa_dominios) && !isTRUE(usa_area)) {\n",
"  # Caso 2: DAM sin área\n",
"  tabla_final <- tabla_base\n",
"} else if (!isTRUE(usa_dominios) && isTRUE(usa_area)) {\n",
"  # Caso 3: nacional + área\n",
"  prop_fila <- normalizar_fila(\n",
"    tabla_ipfp_ingresada$urbano[1],\n",
"    tabla_ipfp_ingresada$rural[1],\n",
"    tipo_ingreso_ipfp\n",
"  )\n",
"  asig <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$tamano_muestra[1])\n",
"  tabla_final <- data.frame(\n",
"    Nivel = 'Nacional',\n",
"    tamano_muestra = tabla_base$tamano_muestra[1],\n",
"    distribucion_urbano = as.numeric(asig[1, 1]),\n",
"    distribucion_rural = as.numeric(asig[1, 2])\n",
"  )\n",
"} else {\n",
"  # Caso 4: DAM + área\n",
"  tabla_final <- do.call(rbind, lapply(seq_len(nrow(tabla_base)), function(i) {\n",
"    prop_fila <- normalizar_fila(\n",
"      tabla_ipfp_ingresada$urbano[i],\n",
"      tabla_ipfp_ingresada$rural[i],\n",
"      tipo_ingreso_ipfp\n",
"    )\n",
"    asig <- ipfp_aproximada(matrix(prop_fila, nrow = 1), tabla_base$tamano_muestra[i])\n",
"    data.frame(\n",
"      DAM = tabla_base$DAM[i],\n",
"      tamano_muestra = tabla_base$tamano_muestra[i],\n",
"      distribucion_urbano = as.numeric(asig[1, 1]),\n",
"      distribucion_rural = as.numeric(asig[1, 2])\n",
"    )\n",
"  }))\n",
"}\n\n",
"print(tabla_final)\n"
      )
    })

    output$codigo_r <- renderText(codigo_r())

    output$ui_descarga_dam <- renderUI({
      d <- entrada_base(); req(d)
      if (!isTRUE(d$usa_dominios)) return(NULL)
      downloadButton(session$ns("descargar_codigo_dam"), "Descargar script cálculo DAM", class = "btn btn-primary")
    })

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("calculo_muestra_completo_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_r(), file)
    )

    output$descargar_codigo_dam <- downloadHandler(
      filename = function() paste0("calculo_muestra_dam_", Sys.Date(), ".R"),
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
