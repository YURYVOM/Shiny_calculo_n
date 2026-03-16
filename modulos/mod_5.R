# =========================================================
# Módulo 5: Cálculo de tamaño de muestra
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 5. Tamaño de muestra"),
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
      p  <- parametro()
      pr <- precision()
      d  <- diseno()

      req(p, pr, d)
      req(!is.null(p$tipo), !is.na(p$tipo))

      out <- if (p$tipo == "Media") {
        ss4HHSm(
          N = d$N,
          M = d$M,
          rho = d$rho,
          mu = p$xbarra,
          sigma = p$s,
          delta = pr$delta,
          conf = pr$conf,
          m = d$m_vector
        )
      } else {
        ss4HHSp(
          N = d$N,
          M = d$M,
          rho = d$rho,
          p = p$p,
          delta = pr$delta,
          conf = pr$conf,
          m = d$m_vector
        )
      }

      out <- as.data.frame(out)

      if ("HouseholdsPerPSU" %in% names(out)) {
        names(out)[names(out) == "HouseholdsPerPSU"] <- "m"
      }

      out
    })

    validacion <- reactive({
      tb <- tabla_muestreo()

      if (is.null(tb)) return("No hay resultados para mostrar.")
      if (NROW(tb) == 0) return("No hay resultados para mostrar.")

      NULL
    })

    output$tabla_muestreo <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tabla_muestreo()
    }, striped = TRUE, bordered = TRUE, spacing = "m")

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
