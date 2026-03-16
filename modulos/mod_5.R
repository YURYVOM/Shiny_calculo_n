# =========================================================
# PASO 5: Cálculo de tamaño de muestra
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Paso 5. Tamaño de muestra"),
      p("En este paso no se selecciona m: se calcula y muestra la tabla para todos los valores de m."),
      h4("Tabla de muestreo para vector de m"),
      tableOutput(ns("tabla_muestreo")),
      h4("Base resultante completa (salida de la función)"),
      verbatimTextOutput(ns("base_resultante"))
    )
  )
}

mod_presupuesto_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    tabla_muestreo <- reactive({
      p <- parametro(); pr <- precision(); u <- unidad(); d <- diseno()
      req(p, pr, u, d)

      do.call(rbind, lapply(d$m_vector, function(m_i) {
        if (p$tipo == "Media") {
          llamada <- sprintf(
            "ss4HHSm(N = %s, M = %s, rho = %s, mu = %s, sigma = %s, delta = %s, conf = %s, m = %s)",
            d$N, d$M, d$rho, p$xbarra, p$s, pr$delta, pr$conf, m_i
          )
          n_hogares <- ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
          parametro <- p$xbarra
          dispersion <- p$s
        } else {
          llamada <- sprintf(
            "ss4HHSp(N = %s, M = %s, rho = %s, p = %s, delta = %s, conf = %s, m = %s)",
            d$N, d$M, d$rho, p$p, pr$delta, pr$conf, m_i
          )
          n_hogares <- ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
          parametro <- p$p
          dispersion <- NA_real_
        }

        upm <- ceiling(n_hogares / m_i)
        n_enc <- ceiling(n_hogares * u$r * u$b)

        data.frame(
          tipo = p$tipo,
          funcion = llamada,
          N = d$N,
          M = d$M,
          rho = d$rho,
          parametro = parametro,
          dispersion = dispersion,
          delta = pr$delta,
          conf = pr$conf,
          m = m_i,
          n_hogares = n_hogares,
          n_encuestas = n_enc,
          upm = upm,
          stringsAsFactors = FALSE
        )
      }))
    })

    validacion <- reactive({
      tb <- tabla_muestreo()
      if (is.null(tb) || nrow(tb) == 0) return("No hay resultados para mostrar.")
      NULL
    })

    output$tabla_muestreo <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tabla_muestreo()
    }, striped = TRUE, bordered = TRUE)

    output$base_resultante <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      print(tabla_muestreo(), row.names = FALSE)
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          tabla_muestreo = tabla_muestreo()
        )
      })
    )
  })
}
