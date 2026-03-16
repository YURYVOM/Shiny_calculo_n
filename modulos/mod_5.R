# =========================================================
# MÓDULO 5: Resultados y exportación de código R
# =========================================================

mod_resultados_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("5. Resultados finales y exportación"),
      p("Revise el tamaño de muestra estimado y exporte el script de R con los datos ingresados."),
      tableOutput(ns("tabla_resultados")),
      br(),
      h4("Código R reproducible"),
      tags$pre(style = "max-height: 280px; overflow-y: auto;", textOutput(ns("codigo_r"))),
      downloadButton(ns("descargar_codigo"), "Descargar código R", class = "btn btn-success")
    )
  )
}

mod_resultados_server <- function(id, entrada) {
  moduleServer(id, function(input, output, session) {

    calculo <- reactive({
      d <- entrada()
      req(d)

      z <- qnorm(1 - (1 - d$conf) / 2)
      me <- d$amplitud / 2

      n0 <- if (d$tipo_param == "Media") {
        (z * d$s / me)^2
      } else {
        (z^2 * d$p * (1 - d$p)) / (me^2)
      }

      deff <- 1 + (d$m - 1) * d$rho
      n_diseno <- n0 * deff
      n_ajustado <- n_diseno / (1 + (n_diseno - 1) / d$N)
      n_encuestas <- ceiling(n_ajustado)
      n_hogares <- ceiling(n_encuestas / (d$r * d$b))
      upm <- ceiling(n_hogares / d$m)
      costo_total <- n_hogares * d$c_h + upm * d$c_upm

      data.frame(
        Indicador = c("z", "Error máximo (ME)", "n base", "DEFF", "n ajustado por diseño y N", "Hogares requeridos", "UPM requeridas", "Costo total"),
        Valor = c(round(z, 4), round(me, 4), ceiling(n0), round(deff, 4), n_encuestas, n_hogares, upm, round(costo_total, 2))
      )
    })

    codigo_r <- reactive({
      d <- entrada()
      req(d)

      paste0(
"# Script exportado desde la app de cálculo de muestra\n",
"tipo_param <- '", d$tipo_param, "'\n",
"xbarra <- ", ifelse(is.null(d$xbarra), "NA", d$xbarra), "\n",
"s <- ", ifelse(is.null(d$s), "NA", d$s), "\n",
"p <- ", ifelse(is.null(d$p), "NA", d$p), "\n",
"unidad <- '", d$unidad, "'\n",
"r <- ", d$r, "\n",
"b <- ", d$b, "\n",
"amplitud <- ", d$amplitud, "\n",
"conf <- ", d$conf, "\n",
"N <- ", d$N, "\n",
"M <- ", d$M, "\n",
"m <- ", d$m, "\n",
"rho <- ", d$rho, "\n",
"c_h <- ", d$c_h, "\n",
"c_upm <- ", d$c_upm, "\n\n",
"z <- qnorm(1 - (1 - conf)/2)\n",
"ME <- amplitud/2\n",
"n0 <- if (tipo_param == 'Media') (z * s / ME)^2 else (z^2 * p * (1-p))/ME^2\n",
"DEFF <- 1 + (m - 1) * rho\n",
"n_diseno <- n0 * DEFF\n",
"n_ajustado <- n_diseno / (1 + (n_diseno - 1)/N)\n",
"n_encuestas <- ceiling(n_ajustado)\n",
"n_hogares <- ceiling(n_encuestas / (r * b))\n",
"upm <- ceiling(n_hogares / m)\n",
"costo_total <- n_hogares * c_h + upm * c_upm\n\n",
"resultado <- data.frame(\n",
"  Indicador = c('z', 'ME', 'n0', 'DEFF', 'n_encuestas', 'n_hogares', 'upm', 'costo_total'),\n",
"  Valor = c(z, ME, n0, DEFF, n_encuestas, n_hogares, upm, costo_total)\n",
")\n",
"print(resultado)\n")
    })

    output$tabla_resultados <- renderTable({
      calculo()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$codigo_r <- renderText(codigo_r())

    output$descargar_codigo <- downloadHandler(
      filename = function() {
        paste0("calculo_muestra_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(codigo_r(), file)
      }
    )

    list(calculo = calculo, codigo_r = codigo_r)
  })
}
