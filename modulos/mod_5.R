# =========================================================
# Módulo 5: Cálculo de tamaño de muestra
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 5. Tamaño de muestra"),
      p("En este paso no se selecciona m: se calcula y muestra la tabla para todos los valores de m."),
      h4("Salida de la función (variables originales)"),
      dataTableOutput(ns("tabla_muestreo"))
    )
  )
}

mod_presupuesto_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    tabla_funcion <- reactive({
      p <- parametro(); pr <- precision(); d <- diseno()
      req(p, pr, d)

      do.call(rbind, lapply(d$m_vector, function(m_i) {
        n_hogares <- if (p$tipo == "Media") {
          ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
        } else {
          ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
        }

        data.frame(
          HouseholdsPerPSU = m_i,
          DEFF = round(1 + (m_i - 1) * d$rho, 2),
          PSUinSample = ceiling(n_hogares / m_i),
          HouseholdsInSample = n_hogares,
          stringsAsFactors = FALSE
        )
      }

      out <- as.data.frame(out)

      if ("HouseholdsPerPSU" %in% names(out)) {
        names(out)[names(out) == "HouseholdsPerPSU"] <- "m"
      }

      out
    })

    tabla_muestreo <- reactive({
      p <- parametro(); pr <- precision(); u <- unidad(); d <- diseno(); req(p, pr, u, d)
      tb <- tabla_funcion()
      transform(
        tb,
        n_encuestas = ceiling(HouseholdsInSample * u$r * u$b)
      )
    })

    validacion <- reactive({
      tb <- tabla_funcion()
      if (is.null(tb) || nrow(tb) == 0) return("No hay resultados para mostrar.")
      NULL
    })

    output$tabla_muestreo <- renderDataTable({
      validate(need(is.null(validacion()), validacion()))
      tabla_funcion()
    }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))


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
