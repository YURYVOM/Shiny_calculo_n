# =========================================================
# MÓDULO 8: Resultados finales y exportación de código R
# =========================================================

mod_resultados_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("8. Tablas de muestreo y exportación"),
      tableOutput(ns("tabla_resultados")),
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
      d <- entrada()
      req(d)

      dg <- DGJunction(
        tipo_param = d$tipo_param,
        s = d$s,
        p = d$p,
        amplitud = d$amplitud,
        conf = d$conf,
        N = d$N,
        m = d$m,
        rho = d$rho,
        r = d$r,
        b = d$b,
        c_h = d$c_h,
        c_upm = d$c_upm,
        usa_dominios = d$usa_dominios,
        n_dominios = d$n_dominios,
        sim = d$sim
      )

      tabla_area <- if (isTRUE(d$usa_area)) {
        ipfp_aproximada(d$tabla_prop, dg$n_total)
      } else {
        matrix(dg$n_total, nrow = 1, ncol = 1)
      }

      list(
        indicadores = data.frame(
          Indicador = c("DEFF", "n_encuestas (SS4HHS*)", "n_total ajustado", "n_hogares", "UPM", "Costo total"),
          Valor = c(round(dg$deff, 4), dg$n_encuestas, dg$n_total, dg$n_hogares, dg$upm, round(dg$costo_total, 2))
        ),
        tabla_area = as.data.frame(tabla_area)
      )
    })

    output$tabla_resultados <- renderTable(calc()$indicadores, striped = TRUE, bordered = TRUE)
    output$tabla_area <- renderTable(calc()$tabla_area, striped = TRUE, bordered = TRUE)

    codigo_r <- reactive({
      d <- entrada(); req(d)
      paste0(
"# Script exportado\n",
"source('modulos/mod_utils.R')\n\n",
"tipo_param <- '", d$tipo_param, "'\n",
"s <- ", ifelse(is.null(d$s), "NA", d$s), "\n",
"p <- ", ifelse(is.null(d$p), "NA", d$p), "\n",
"amplitud <- ", d$amplitud, "\n",
"conf <- ", d$conf, "\n",
"N <- ", d$N, "\n",
"m <- ", d$m, "\n",
"rho <- ", d$rho, "\n",
"r <- ", d$r, "\n",
"b <- ", d$b, "\n",
"c_h <- ", d$c_h, "\n",
"c_upm <- ", d$c_upm, "\n",
"usa_dominios <- ", ifelse(d$usa_dominios, "TRUE", "FALSE"), "\n",
"n_dominios <- ", d$n_dominios, "\n",
"sim <- ", d$sim, "\n\n",
"res <- DGJunction(tipo_param, s, p, amplitud, conf, N, m, rho, r, b, c_h, c_upm, usa_dominios, n_dominios, sim)\n",
"print(res)\n"
      )
    })

    output$codigo_r <- renderText(codigo_r())
    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("calculo_muestra_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_r(), file)
    )
  })
}
