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
        "¿Desea hacer asignación por área?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_area")),
        numericInput(ns("n_areas"), "Número de áreas:", value = 2, min = 2),
        textAreaInput(ns("tabla_prop"), "Tabla cruzada de proporciones censales (CSV). Debe sumar 1.", rows = 6, placeholder = "0.10,0.15\n0.20,0.25\n0.10,0.20")
      ),
      h4("Resumen final"),
      tableOutput(ns("tabla_resultados")),
      h4("Resultados por DAM (m seleccionado)"),
      tableOutput(ns("tabla_dominios")),
      h4("Asignación por área"),
      tableOutput(ns("tabla_area")),
      br(),
      h4("Código R reproducible"),
      tags$pre(style = "max-height: 260px; overflow-y: auto;", textOutput(ns("codigo_r"))),
      downloadButton(ns("descargar_codigo"), "Descargar código R", class = "btn btn-success")
    )
  )
}

mod_resultados_server <- function(id, entrada_base) {
  moduleServer(id, function(input, output, session) {

    output$ui_selector_m <- renderUI({
      d <- entrada_base(); req(d)

      if (isTRUE(d$usa_dominios)) {
        selectInput(session$ns("m_sel"), "Seleccione su mejor m:", choices = as.character(unique(d$m_vector)))
      } else {
        tags$p(strong(paste("m nacional seleccionado en el módulo anterior:", d$m_sel_nacional)))
      }
    })

    observe({
      d <- entrada_base(); req(d)
      if (isTRUE(d$usa_dominios)) {
        choices <- as.character(unique(d$m_vector))
        selected <- if (!is.null(input$m_sel) && input$m_sel %in% choices) input$m_sel else choices[1]
        updateSelectInput(session, "m_sel", choices = choices, selected = selected)
      }
    })

    tabla_matrix <- reactive({
      req(input$tabla_prop)
      filas <- strsplit(trimws(input$tabla_prop), "\\n")[[1]]
      do.call(rbind, lapply(filas, function(f) as.numeric(strsplit(f, ",")[[1]])))
    })

    validacion <- reactive({
      d <- entrada_base(); req(d)
      if (isTRUE(d$usa_dominios)) {
        if (is.null(input$m_sel) || !(input$m_sel %in% as.character(unique(d$m_vector)))) return("Seleccione un m válido.")
      } else if (is.null(d$m_sel_nacional) || is.na(d$m_sel_nacional) || !(as.character(d$m_sel_nacional) %in% as.character(unique(d$m_vector)))) {
        return("No hay un m nacional válido seleccionado desde el módulo 6.")
      }

      if (identical(input$usa_area, "si")) {
        if (is.na(input$n_areas) || input$n_areas < 2) return("Número de áreas debe ser >= 2.")
        mat <- tryCatch(tabla_matrix(), error = function(e) NULL)
        if (is.null(mat)) return("La tabla cruzada no tiene formato CSV válido.")
        if (nrow(mat) != d$n_dominios) return(sprintf("La tabla debe tener %s filas (DAM).", d$n_dominios))
        if (ncol(mat) != input$n_areas) return(sprintf("La tabla debe tener %s columnas (áreas).", input$n_areas))
        if (any(is.na(mat)) || any(mat < 0)) return("La tabla no debe tener negativos ni faltantes.")
        if (abs(sum(mat) - 1) > 1e-6) return("La tabla de proporciones debe sumar 1.")
      }
      NULL
    })

    calc <- reactive({
      validate(need(is.null(validacion()), validacion()))
      d <- entrada_base(); req(d)
      m_sel <- if (isTRUE(d$usa_dominios)) as.numeric(input$m_sel) else as.numeric(d$m_sel_nacional)

      if (isTRUE(d$usa_dominios)) {
        delta_dom <- if (!is.null(d$delta_dom) && length(d$delta_dom) == length(d$param_dom)) d$delta_dom else rep(d$delta, length(d$param_dom))
        if (d$tipo_param == "Media") {
          n_dom <- mapply(function(mu_i, sd_i, N_i, M_i, delta_i) ss4HHSm(N_i, M_i, d$rho, mu_i, sd_i, delta_i, d$conf, m_sel), d$param_dom, d$sd_dom, d$N_dom, d$M_dom, delta_dom)
        } else {
          n_dom <- mapply(function(p_i, N_i, M_i, delta_i) ss4HHSp(N_i, M_i, d$rho, p_i, delta_i, d$conf, m_sel), d$param_dom, d$N_dom, d$M_dom, delta_dom)
        }
      } else {
        n_base <- if (d$tipo_param == "Media") {
          ss4HHSm(d$N, d$M, d$rho, d$xbarra, d$s, d$delta, d$conf, m_sel)
        } else {
          ss4HHSp(d$N, d$M, d$rho, d$p, d$delta, d$conf, m_sel)
        }
        n_dom <- n_base
      }

      tabla_dom <- data.frame(dam = seq_len(length(n_dom)), n_hogares = as.numeric(n_dom))
      n_hogares_total <- sum(n_dom)
      factor_personas <- if (isTRUE(d$usa_dominios)) d$r_dam * d$b_dam else d$r * d$b
      n_encuestas <- ceiling(n_hogares_total * factor_personas)
      upm <- ceiling(n_hogares_total / m_sel)

      tabla_area <- if (identical(input$usa_area, "si")) ipfp_aproximada(tabla_matrix(), n_hogares_total) else matrix(n_hogares_total, nrow = max(1, d$n_dominios), ncol = 1)

      list(
        resumen = data.frame(Indicador = c("m seleccionado", "n_hogares total", "n_encuestas", "UPM"), Valor = c(m_sel, n_hogares_total, n_encuestas, upm)),
        dom = tabla_dom,
        area = as.data.frame(tabla_area),
        m_sel = m_sel
      )
    })

    output$tabla_resultados <- renderTable(calc()$resumen, striped = TRUE, bordered = TRUE)
    output$tabla_dominios <- renderTable(calc()$dom, striped = TRUE, bordered = TRUE)
    output$tabla_area <- renderTable(calc()$area, striped = TRUE, bordered = TRUE)

    codigo_r <- reactive({
      d <- entrada_base(); req(d)
      m_sel <- calc()$m_sel
      usa_area <- identical(input$usa_area, "si")
      tabla_prop_txt <- if (usa_area) paste(capture.output(dput(tabla_matrix())), collapse = "\n") else "NULL"

      param_dom_txt <- if (length(d$param_dom)) paste(d$param_dom, collapse = ", ") else ""
      sd_dom_txt <- if (length(d$sd_dom)) paste(d$sd_dom, collapse = ", ") else ""
      N_dom_txt <- if (length(d$N_dom)) paste(d$N_dom, collapse = ", ") else ""
      M_dom_txt <- if (length(d$M_dom)) paste(d$M_dom, collapse = ", ") else ""
      amplitud_dom_txt <- if (length(d$amplitud_dom)) paste(d$amplitud_dom, collapse = ", ") else ""

      script <- paste0(
"# =========================================================\n",
"# Script reproducible exportado desde la app\n",
"# Incluye cálculo nacional, DAM (si aplica) e IPFP por área (si aplica)\n",
"# =========================================================\n\n",
"source('modulos/mod_utils.R')\n\n",
"# ---- Entradas generales ----\n",
"tipo_param <- '", d$tipo_param, "'\n",
"conf <- ", d$conf, "\n",
"rho <- ", d$rho, "\n",
"m_sel <- ", m_sel, "\n",
"N <- ", d$N, "\n",
"M <- ", d$M, "\n",
"amplitud <- ", d$amplitud, "\n",
"delta <- ", d$delta, "\n",
if (d$tipo_param == "Media") paste0("xbarra <- ", d$xbarra, "\n", "s <- ", d$s, "\n") else paste0("p <- ", d$p, "\n"),
"r <- ", d$r, "\n",
"b <- ", d$b, "\n\n",
"# ---- Entradas DAM ----\n",
"usa_dominios <- ", tolower(as.character(d$usa_dominios)), "\n",
"n_dominios <- ", d$n_dominios, "\n",
"unidad_dam <- '", d$unidad_dam, "'\n",
"r_dam <- ", d$r_dam, "\n",
"b_dam <- ", d$b_dam, "\n",
if (d$tipo_param == "Media") paste0("mu_dom <- c(", param_dom_txt, ")\n", "sigma_dom <- c(", sd_dom_txt, ")\n") else paste0("p_dom <- c(", param_dom_txt, ")\n"),
"N_dom <- c(", N_dom_txt, ")\n",
"M_dom <- c(", M_dom_txt, ")\n",
"amplitud_dom <- c(", amplitud_dom_txt, ")\n",
"delta_dom <- amplitud_dom / 2\n\n",
"# ---- Asignación por área ----\n",
"usa_area <- ", tolower(as.character(usa_area)), "\n",
"tabla_prop <- ", tabla_prop_txt, "\n\n",
"# ---- Cálculo principal ----\n",
"if (isTRUE(usa_dominios)) {\n",
"  # Si DAM es a nivel personas, r_dam y b_dam deben venir informados (>0 y [0,1])\n",
"  if (unidad_dam == 'Personas') {\n",
"    stopifnot(!is.na(r_dam), r_dam > 0, !is.na(b_dam), b_dam >= 0, b_dam <= 1)\n",
"  } else {\n",
"    r_dam <- 1; b_dam <- 1\n",
"  }\n",
"\n",
"  if (tipo_param == 'Media') {\n",
"    n_dom <- mapply(function(mu_i, sd_i, N_i, M_i, delta_i) {\n",
"      ss4HHSm(N = N_i, M = M_i, rho = rho, mu = mu_i, sigma = sd_i, delta = delta_i, conf = conf, m = m_sel)\n",
"    }, mu_dom, sigma_dom, N_dom, M_dom, delta_dom)\n",
"  } else {\n",
"    n_dom <- mapply(function(p_i, N_i, M_i, delta_i) {\n",
"      ss4HHSp(N = N_i, M = M_i, rho = rho, p = p_i, delta = delta_i, conf = conf, m = m_sel)\n",
"    }, p_dom, N_dom, M_dom, delta_dom)\n",
"  }\n",
"\n",
"  tabla_dom <- data.frame(dam = seq_along(n_dom), n_hogares = as.numeric(n_dom))\n",
"  n_hogares_total <- sum(n_dom)\n",
"  n_encuestas <- ceiling(n_hogares_total * r_dam * b_dam)\n",
"} else {\n",
"  # Cálculo nacional\n",
"  if (tipo_param == 'Media') {\n",
"    n_base <- ss4HHSm(N = N, M = M, rho = rho, mu = xbarra, sigma = s, delta = delta, conf = conf, m = m_sel)\n",
"  } else {\n",
"    n_base <- ss4HHSp(N = N, M = M, rho = rho, p = p, delta = delta, conf = conf, m = m_sel)\n",
"  }\n",
"  tabla_dom <- data.frame(dam = 1, n_hogares = as.numeric(n_base))\n",
"  n_hogares_total <- as.numeric(n_base)\n",
"  n_encuestas <- ceiling(n_hogares_total * r * b)\n",
"}\n\n",
"upm <- ceiling(n_hogares_total / m_sel)\n",
"resumen <- data.frame(Indicador = c('m seleccionado', 'n_hogares total', 'n_encuestas', 'UPM'),\n",
"                      Valor = c(m_sel, n_hogares_total, n_encuestas, upm))\n",
"print(resumen)\n",
"print(tabla_dom)\n\n",
"if (isTRUE(usa_area)) {\n",
"  tabla_area <- as.data.frame(ipfp_aproximada(tabla_prop, n_hogares_total))\n",
"  print(tabla_area)\n",
"}\n")

      script
    })

    output$codigo_r <- renderText(codigo_r())
    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("calculo_muestra_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_r(), file)
    )

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          m_sel = calc()$m_sel,
          usa_area = identical(input$usa_area, "si"),
          n_areas = if (identical(input$usa_area, "si")) input$n_areas else 1,
          tabla_prop = if (identical(input$usa_area, "si")) tabla_matrix() else NULL
        )
      })
    )
  })
}
