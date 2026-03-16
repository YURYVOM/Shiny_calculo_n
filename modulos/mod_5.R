# =========================================================
# PASO 5: Parámetros de presupuesto + cálculo de tamaño de muestra
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Paso 5. Presupuesto y tamaño de muestra"),
      p("Ingrese presupuesto, revise la tabla para todos los m y seleccione el m preferido."),
      fluidRow(
        column(6,
               numericInput(ns("c_h"), "c_h (costo por encuesta/hogar):", value = 1, min = 0),
               numericInput(ns("c_upm"), "c_UPM (costo fijo por UPM):", value = 1, min = 0)
        ),
        column(6,
               selectInput(ns("m_sel"), "Seleccione m (el que le gustó más):", choices = character(0))
        )
      ),
      h4("Tabla de muestreo para vector de m"),
      tableOutput(ns("tabla_muestreo"))
    )
  )
}

mod_presupuesto_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    tabla_muestreo <- reactive({
      p <- parametro(); pr <- precision(); u <- unidad(); d <- diseno()
      req(p, pr, u, d)

      do.call(rbind, lapply(d$m_vector, function(m_i) {
        n_hogares <- if (p$tipo == "Media") {
          ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
        } else {
          ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
        }
        upm <- ceiling(n_hogares / m_i)
        n_enc <- ceiling(n_hogares * u$r * u$b)
        costo <- n_hogares * input$c_h + upm * input$c_upm
        data.frame(m = m_i, n_hogares = n_hogares, n_encuestas = n_enc, upm = upm, costo_total = round(costo, 2))
      }))
    })

    observe({
      tb <- tabla_muestreo()
      req(nrow(tb) > 0)
      choices <- as.character(tb$m)
      selected <- if (!is.null(input$m_sel) && input$m_sel %in% choices) input$m_sel else choices[1]
      updateSelectInput(session, "m_sel", choices = choices, selected = selected)
    })

    validacion <- reactive({
      if (is.null(input$c_h) || is.na(input$c_h) || input$c_h < 0) return("c_h debe ser >= 0.")
      if (is.null(input$c_upm) || is.na(input$c_upm) || input$c_upm < 0) return("c_UPM debe ser >= 0.")
      tb <- tabla_muestreo()
      if (is.null(input$m_sel) || !(input$m_sel %in% as.character(tb$m))) return("Seleccione un m válido.")
      NULL
    })

    output$tabla_muestreo <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tabla_muestreo()
    }, striped = TRUE, bordered = TRUE)

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        tb <- tabla_muestreo()
        fila <- tb[tb$m == as.numeric(input$m_sel), , drop = FALSE]
        list(
          c_h = input$c_h,
          c_upm = input$c_upm,
          m_sel = as.numeric(input$m_sel),
          tabla_muestreo = tb,
          fila_m = fila
        )
      })
    )
  })
}
