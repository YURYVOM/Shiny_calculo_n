# =========================================================
# MÓDULO 7: Resultados finales y exportación de código R
# =========================================================

mod_resultados_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 7. Tablas de muestreo y exportación"),
      h4("Resumen final (con m seleccionado)"),
      tableOutput(ns("tabla_resultados")),
      h4("Tamaño de muestra por dominio DAM"),
      tableOutput(ns("tabla_dominios")),
      h4("Tabla de asignación por área"),
      tableOutput(ns("tabla_area")),
      br(),
      h4("Código R reproducible"),
      tags$pre(style = "max-height: 260px; overflow-y: auto;", textOutput(ns("codigo_r"))),
      downloadButton(ns("descargar_codigo"), "Descargar código R", class = "btn btn-success")
    )
  )
}

mod_resultados_server <- function(id, entrada) {
  moduleServer(id, function(input, output, session) {

    calc <- reactive({
      d <- entrada(); req(d)

      n_base <- if (d$tipo_param == "Media") {
        ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = d$xbarra, sigma = d$s, delta = d$delta, conf = d$conf, m = d$m_sel)
      } else {
        ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = d$p, delta = d$delta, conf = d$conf, m = d$m_sel)
      }

      if (isTRUE(d$usa_dominios)) {
        tabla_dom <- data.frame(dominio = seq_len(d$n_dominios), n_hogares = rep(n_base, d$n_dominios))
        n_hogares_total <- n_base * d$n_dominios
      } else {
        tabla_dom <- data.frame(dominio = 1, n_hogares = n_base)
        n_hogares_total <- n_base
      }

      n_encuestas <- ceiling(n_hogares_total * d$r * d$b)
      upm <- ceiling(n_hogares_total / d$m_sel)
      tabla_area <- if (isTRUE(d$usa_area)) ipfp_aproximada(d$tabla_prop, n_hogares_total) else matrix(n_hogares_total, nrow = 1, ncol = 1)

      list(
        resumen = data.frame(
          Indicador = c("m seleccionado", "Delta", "n_hogares total", "n_encuestas", "UPM"),
          Valor = c(d$m_sel, round(d$delta, 4), n_hogares_total, n_encuestas, upm)
        ),
        dom = tabla_dom,
        area = as.data.frame(tabla_area)
      )
    })

    output$tabla_resultados <- renderTable(calc()$resumen, striped = TRUE, bordered = TRUE)
    output$tabla_dominios <- renderTable(calc()$dom, striped = TRUE, bordered = TRUE)
    output$tabla_area <- renderTable(calc()$area, striped = TRUE, bordered = TRUE)

    codigo_r <- reactive({
      d <- entrada(); req(d)
      paste0(
        "# Script exportado\n",
        "source('modulos/mod_utils.R')\n\n",
        "tipo_param <- '", d$tipo_param, "'\n",
        "N <- ", d$N, "\nM <- ", d$M, "\nrho <- ", d$rho, "\nm <- ", d$m_sel, "\n",
        "delta <- ", d$delta, "\nconf <- ", d$conf, "\n",
        "mu <- ", ifelse(is.null(d$xbarra), "NA", d$xbarra), "\n",
        "sigma <- ", ifelse(is.null(d$s), "NA", d$s), "\n",
        "p <- ", ifelse(is.null(d$p), "NA", d$p), "\n",
        "r <- ", d$r, "\nb <- ", d$b, "\n",
        "usa_dominios <- ", ifelse(d$usa_dominios, "TRUE", "FALSE"), "\n",
        "n_dominios <- ", d$n_dominios, "\n\n",
        "n_base <- if (tipo_param == 'Media') ss4HHSm(N, M, rho, mu, sigma, delta, conf, m) else ss4HHSp(N, M, rho, p, delta, conf, m)\n",
        "n_hogares_total <- if (usa_dominios) n_base * n_dominios else n_base\n",
        "n_encuestas <- ceiling(n_hogares_total * r * b)\n",
        "upm <- ceiling(n_hogares_total / m)\n",
        "print(list(n_base=n_base, n_hogares_total=n_hogares_total, n_encuestas=n_encuestas, upm=upm))\n"
      )
    })

    output$codigo_r <- renderText(codigo_r())
    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("calculo_muestra_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_r(), file)
    )
  })
}
