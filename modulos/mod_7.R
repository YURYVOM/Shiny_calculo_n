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
        p("Ingrese los valores para cada DAM y área. Se generan automáticamente dos columnas: urbano y rural."),
        uiOutput(ns("ipfp_ui"))
      ),
      h4("Resumen final"),
      tableOutput(ns("tabla_resultados")),
      h4("Tabla consolidada final"),
      tableOutput(ns("tabla_consolidada")),
      h4("Asignación urbano/rural (IPFP)"),
      tableOutput(ns("tabla_area")),
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
        "Cada fila debe sumar 1 cuando se ingresan proporciones."
      } else {
        "Ingrese totales por DAM y área; la app los convertirá en proporciones internas antes de aplicar IPFP."
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

    tabla_ipfp_base <- reactive({
      datos <- ipfp_df(); req(datos)

      if (any(is.na(datos$urbano)) || any(is.na(datos$rural))) {
        return(NULL)
      }

      matriz <- as.matrix(datos[, c("urbano", "rural")])
      if (identical(input$tipo_ingreso_ipfp, "totales")) {
        total_general <- sum(matriz)
        if (total_general <= 0) return(NULL)
        matriz <- matriz / total_general
      }
      matriz
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
        mat <- tabla_ipfp_base()
        if (is.null(mat)) return("Complete los valores de urbano y rural para IPFP.")
        if (any(mat < 0)) return("Los valores de IPFP no pueden ser negativos.")

        if (identical(input$tipo_ingreso_ipfp, "proporciones")) {
          suma_filas <- rowSums(mat)
          if (any(abs(suma_filas - 1) > 1e-6)) return("Cada DAM debe sumar 1 entre urbano y rural cuando se usan proporciones.")
          if (abs(sum(mat) - d$n_dominios) > 1e-6) {
            return("La suma total de las proporciones debe coincidir con el número de dominios informados.")
          }
        }
      }
      NULL
    })

    tabla_consolidada <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)

      if (isTRUE(d$usa_dominios)) {
        tabla_detalle <- d$tabla_m_seleccionado_dam
        req(nrow(tabla_detalle) > 0)
        tabla_base <- data.frame(
          DAM = paste0("DAM ", tabla_detalle$DAM),
          m_seleccionado = tabla_detalle$HouseholdsPerPSU,
          tamano_muestra = tabla_detalle$HouseholdsInSample,
          UPM = tabla_detalle$PSUinSample,
          stringsAsFactors = FALSE
        )
      } else {
        n_base <- if (d$tipo_param == "Media") {
          ss4HHSm(d$N, d$M, d$rho, d$xbarra, d$s, d$delta, d$conf, d$m_seleccionado_nacional)
        } else {
          ss4HHSp(d$N, d$M, d$rho, d$p, d$delta, d$conf, d$m_seleccionado_nacional)
        }
        tabla_base <- data.frame(
          DAM = "Nacional",
          m_seleccionado = d$m_seleccionado_nacional,
          tamano_muestra = as.numeric(n_base),
          UPM = ceiling(as.numeric(n_base) / d$m_seleccionado_nacional),
          stringsAsFactors = FALSE
        )
      }

      if (identical(input$usa_area, "si")) {
        matriz_ipfp <- tabla_ipfp_base()
        if (identical(input$tipo_ingreso_ipfp, "proporciones")) {
          matriz_ipfp <- matriz_ipfp / sum(matriz_ipfp)
        }
        distribucion <- ipfp_aproximada(matriz_ipfp, sum(tabla_base$tamano_muestra))
        tabla_area_local <- as.data.frame(distribucion)
        colnames(tabla_area_local) <- c("urbano", "rural")
        tabla_base$distribucion_urbano <- tabla_area_local$urbano
        tabla_base$distribucion_rural <- tabla_area_local$rural
      } else {
        tabla_base$distribucion_urbano <- NA_real_
        tabla_base$distribucion_rural <- NA_real_
      }

      tabla_base
    })

    calc <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)
      tabla_final <- tabla_consolidada()

      n_hogares_total <- sum(tabla_final$tamano_muestra)
      upm_total <- sum(tabla_final$UPM)
      n_encuestas <- if (isTRUE(d$usa_dominios)) {
        ceiling(sum(tabla_final$tamano_muestra * d$r_dam * d$b_dam))
      } else {
        ceiling(n_hogares_total * d$r * d$b)
      }

      tabla_area <- if (identical(input$usa_area, "si")) {
        tabla_final[, c("DAM", "distribucion_urbano", "distribucion_rural")]
      } else {
        data.frame(DAM = tabla_final$DAM, distribucion_urbano = NA_real_, distribucion_rural = NA_real_)
      }

      list(
        resumen = data.frame(
          Indicador = c("Valor(es) de m seleccionado(s)", "Tamaño de muestra total", "Número estimado de encuestas", "Número total de UPM"),
          Valor = c(
            if (isTRUE(d$usa_dominios)) paste(d$m_seleccionado_dam, collapse = ", ") else d$m_seleccionado_nacional,
            n_hogares_total,
            n_encuestas,
            upm_total
          ),
          stringsAsFactors = FALSE
        ),
        tabla_consolidada = tabla_final,
        area = tabla_area
      )
    })

    output$tabla_resultados <- renderTable(calc()$resumen, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$tabla_consolidada <- renderTable(calc()$tabla_consolidada, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$tabla_area <- renderTable(calc()$area, striped = TRUE, bordered = TRUE, rownames = FALSE)

    codigo_r <- reactive({
      d <- entrada_base(); req(d)
      usa_area <- identical(input$usa_area, "si")
      usa_dominios <- isTRUE(d$usa_dominios)
      tipo_ingreso_ipfp <- if (!is.null(input$tipo_ingreso_ipfp)) input$tipo_ingreso_ipfp else "proporciones"

      tabla_ipfp_txt <- if (usa_area) paste(capture.output(dput(tabla_ipfp_base())), collapse = "\n") else "NULL"
      m_dam_txt <- if (length(d$m_seleccionado_dam)) paste(d$m_seleccionado_dam, collapse = ", ") else ""
      parametro_dam_txt <- if (length(d$parametro_objetivo_dam)) paste(d$parametro_objetivo_dam, collapse = ", ") else ""
      desviacion_dam_txt <- if (length(d$desviacion_estandar_dam)) paste(d$desviacion_estandar_dam, collapse = ", ") else ""
      poblacion_dam_txt <- if (length(d$poblacion_dam)) paste(d$poblacion_dam, collapse = ", ") else ""
      upm_dam_txt <- if (length(d$upm_marco_dam)) paste(d$upm_marco_dam, collapse = ", ") else ""
      amplitud_dam_txt <- if (length(d$amplitud_dam)) paste(d$amplitud_dam, collapse = ", ") else ""
      rho_dam_txt <- if (length(d$rho_dam)) paste(d$rho_dam, collapse = ", ") else ""
      r_dam_txt <- if (length(d$r_dam)) paste(d$r_dam, collapse = ", ") else ""
      b_dam_txt <- if (length(d$b_dam)) paste(d$b_dam, collapse = ", ") else ""

      paste0(
"# =========================================================\n",
"# Script reproducible exportado desde la app\n",
"# =========================================================\n\n",
"source('modulos/mod_utils.R')\n\n",
"# =========================================================\n",
"# Parámetros generales\n",
"# =========================================================\n",
"tipo_parametro <- '", d$tipo_param, "'\n",
"nivel_confianza <- ", d$conf, "\n",
"rho_nacional <- ", d$rho, "\n",
"poblacion_nacional <- ", d$N, "\n",
"upm_marco_nacional <- ", d$M, "\n",
"amplitud_nacional <- ", d$amplitud, "\n",
"delta_nacional <- ", d$delta, "\n",
if (d$tipo_param == "Media") paste0("media_nacional <- ", d$xbarra, "\n", "desviacion_estandar_nacional <- ", d$s, "\n") else paste0("proporcion_nacional <- ", d$p, "\n"),
"r_nacional <- ", d$r, "\n",
"b_nacional <- ", d$b, "\n",
"m_vector <- c(", paste(d$m_vector, collapse = ", "), ")\n",
"m_nacional_seleccionado <- ", d$m_seleccionado_nacional, "\n\n",
"# =========================================================\n",
"# Cálculo a nivel nacional\n",
"# =========================================================\n",
"if (tipo_parametro == 'Media') {\n",
"  tamano_muestra_nacional <- ss4HHSm(\n",
"    N = poblacion_nacional,\n",
"    M = upm_marco_nacional,\n",
"    rho = rho_nacional,\n",
"    mu = media_nacional,\n",
"    sigma = desviacion_estandar_nacional,\n",
"    delta = delta_nacional,\n",
"    conf = nivel_confianza,\n",
"    m = m_nacional_seleccionado\n",
"  )\n",
"} else {\n",
"  tamano_muestra_nacional <- ss4HHSp(\n",
"    N = poblacion_nacional,\n",
"    M = upm_marco_nacional,\n",
"    rho = rho_nacional,\n",
"    p = proporcion_nacional,\n",
"    delta = delta_nacional,\n",
"    conf = nivel_confianza,\n",
"    m = m_nacional_seleccionado\n",
"  )\n",
"}\n",
"upm_nacionales_requeridas <- ceiling(tamano_muestra_nacional / m_nacional_seleccionado)\n",
"resultado_nacional <- data.frame(\n",
"  DAM = 'Nacional',\n",
"  m_seleccionado = m_nacional_seleccionado,\n",
"  tamano_muestra = tamano_muestra_nacional,\n",
"  UPM = upm_nacionales_requeridas\n",
")\n\n",
"# =========================================================\n",
"# Cálculo por DAM\n",
"# =========================================================\n",
"usa_dominios <- ", tolower(as.character(usa_dominios)), "\n",
"n_dominios <- ", d$n_dominios, "\n",
"unidad_dam <- '", d$unidad_dam, "'\n",
"m_dam_seleccionado <- c(", m_dam_txt, ")\n",
"r_dam <- c(", r_dam_txt, ")\n",
"b_dam <- c(", b_dam_txt, ")\n",
if (d$tipo_param == "Media") paste0("media_dam <- c(", parametro_dam_txt, ")\n", "desviacion_estandar_dam <- c(", desviacion_dam_txt, ")\n") else paste0("proporcion_dam <- c(", parametro_dam_txt, ")\n"),
"poblacion_dam <- c(", poblacion_dam_txt, ")\n",
"upm_marco_dam <- c(", upm_dam_txt, ")\n",
"amplitud_dam <- c(", amplitud_dam_txt, ")\n",
"delta_dam <- amplitud_dam / 2\n",
"rho_dam <- c(", rho_dam_txt, ")\n\n",
"if (isTRUE(usa_dominios)) {\n",
"  resultado_dam <- do.call(rbind, lapply(seq_len(n_dominios), function(i) {\n",
"    tamano_muestra_dam <- if (tipo_parametro == 'Media') {\n",
"      ss4HHSm(\n",
"        N = poblacion_dam[i],\n",
"        M = upm_marco_dam[i],\n",
"        rho = rho_dam[i],\n",
"        mu = media_dam[i],\n",
"        sigma = desviacion_estandar_dam[i],\n",
"        delta = delta_dam[i],\n",
"        conf = nivel_confianza,\n",
"        m = m_dam_seleccionado[i]\n",
"      )\n",
"    } else {\n",
"      ss4HHSp(\n",
"        N = poblacion_dam[i],\n",
"        M = upm_marco_dam[i],\n",
"        rho = rho_dam[i],\n",
"        p = proporcion_dam[i],\n",
"        delta = delta_dam[i],\n",
"        conf = nivel_confianza,\n",
"        m = m_dam_seleccionado[i]\n",
"      )\n",
"    }\n",
"\n",
"    data.frame(\n",
"      DAM = paste0('DAM ', i),\n",
"      m_seleccionado = m_dam_seleccionado[i],\n",
"      tamano_muestra = tamano_muestra_dam,\n",
"      UPM = ceiling(tamano_muestra_dam / m_dam_seleccionado[i])\n",
"    )\n",
"  }))\n",
"} else {\n",
"  resultado_dam <- resultado_nacional\n",
"}\n\n",
"# =========================================================\n",
"# Implementación del método IPFP\n",
"# =========================================================\n",
"usa_area <- ", tolower(as.character(usa_area)), "\n",
"tipo_ingreso_ipfp <- '", tipo_ingreso_ipfp, "'\n",
"matriz_ipfp <- ", tabla_ipfp_txt, "\n\n",
"tabla_final <- if (isTRUE(usa_dominios)) resultado_dam else resultado_nacional\n",
"if (isTRUE(usa_area)) {\n",
"  if (tipo_ingreso_ipfp == 'proporciones') {\n",
"    matriz_ipfp <- matriz_ipfp / sum(matriz_ipfp)\n",
"  } else {\n",
"    matriz_ipfp <- matriz_ipfp / sum(matriz_ipfp)\n",
"  }\n",
"\n",
"  asignacion_ipfp <- as.data.frame(ipfp_aproximada(matriz_ipfp, sum(tabla_final$tamano_muestra)))\n",
"  colnames(asignacion_ipfp) <- c('distribucion_urbano', 'distribucion_rural')\n",
"  tabla_final$distribucion_urbano <- asignacion_ipfp$distribucion_urbano\n",
"  tabla_final$distribucion_rural <- asignacion_ipfp$distribucion_rural\n",
"} else {\n",
"  tabla_final$distribucion_urbano <- NA_real_\n",
"  tabla_final$distribucion_rural <- NA_real_\n",
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
          tabla_ipfp = if (identical(input$usa_area, "si")) tabla_ipfp_base() else NULL,
          tabla_consolidada = tabla_consolidada()
        )
      })
    )
  })
}
