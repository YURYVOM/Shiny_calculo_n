# =========================================================
# MÓDULO 4: Parámetros de diseño y cálculo sobre vector de m
# =========================================================

mod_diseno_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 4. Parámetros de diseño"),
      p("Ingrese parámetros de diseño, calcule MRB y revise resultados para un vector de m."),
      fluidRow(
        column(6,
               numericInput(ns("N"), "N (tamaño de población):", value = NA, min = 1),
               numericInput(ns("M"), "M (UPM del marco):", value = NA, min = 1)
        ),
        column(6,
               numericInput(ns("m_desde"), "m desde:", value = 8, min = 1),
               numericInput(ns("m_hasta"), "m hasta:", value = 20, min = 1),
               numericInput(ns("rho"), "rho (ICC):", value = 0.05, min = 0, max = 1, step = 0.01)
        )
      ),
      h4("Cálculo interno MRB = m × r × b"),
      tableOutput(ns("tabla_mrb")),
      h4("Tabla de muestreo para vector de m"),
      tableOutput(ns("tabla_muestreo")),
      selectInput(ns("m_sel"), "Seleccione m (el que le gustó más):", choices = character(0))
    )
  )
}

mod_diseno_server <- function(id, parametro, precision, unidad) {
  moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.na(input$N) || input$N <= 0) return("N debe ser mayor que 0.")
      if (is.na(input$M) || input$M <= 0) return("M debe ser mayor que 0.")
      if (is.na(input$m_desde) || input$m_desde <= 0) return("m desde debe ser > 0.")
      if (is.na(input$m_hasta) || input$m_hasta < input$m_desde) return("m hasta debe ser >= m desde.")
      if (is.na(input$rho) || input$rho < 0 || input$rho > 1) return("rho debe estar entre 0 y 1.")
      NULL
    })

    m_vector <- reactive({
      validate(need(is.null(validacion()), validacion()))
      seq.int(as.integer(input$m_desde), as.integer(input$m_hasta), by = 1)
    })

    tabla_mrb <- reactive({
      u <- unidad(); req(u)
      data.frame(m = m_vector(), MRB = round(m_vector() * u$r * u$b, 4))
    })

    tabla_muestreo <- reactive({
      p <- parametro(); pr <- precision(); u <- unidad(); req(p, pr, u)
      do.call(rbind, lapply(m_vector(), function(m_i) {
        n_hogares <- if (p$tipo == "Media") {
          ss4HHSm(N = input$N, M = input$M, rho = input$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
        } else {
          ss4HHSp(N = input$N, M = input$M, rho = input$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
        }
        upm <- ceiling(n_hogares / m_i)
        n_enc <- ceiling(n_hogares * u$r * u$b)
        data.frame(m = m_i, n_hogares = n_hogares, n_encuestas = n_enc, upm = upm)
      }))
    })

    observe({
      tb <- tabla_muestreo(); req(nrow(tb) > 0)
      choices <- as.character(tb$m)
      selected <- if (!is.null(input$m_sel) && input$m_sel %in% choices) input$m_sel else choices[1]
      updateSelectInput(session, "m_sel", choices = choices, selected = selected)
    })

    output$tabla_mrb <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tabla_mrb()
    }, striped = TRUE, bordered = TRUE)

    output$tabla_muestreo <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tb <- tabla_muestreo()
      validate(need(!is.null(input$m_sel) && input$m_sel %in% as.character(tb$m), "Seleccione un m válido."))
      tb
    }, striped = TRUE, bordered = TRUE)

    list(
      validacion = reactive({
        msg <- validacion(); if (!is.null(msg)) return(msg)
        tb <- tabla_muestreo()
        if (is.null(input$m_sel) || !(input$m_sel %in% as.character(tb$m))) return("Seleccione un m válido.")
        NULL
      }),
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        tb <- tabla_muestreo()
        validate(need(!is.null(input$m_sel) && input$m_sel %in% as.character(tb$m), "Seleccione un m válido."))
        list(
          N = input$N,
          M = input$M,
          rho = input$rho,
          m_vector = m_vector(),
          m_sel = as.numeric(input$m_sel),
          tabla_mrb = tabla_mrb(),
          tabla_muestreo = tb
        )
      })
    )
  })
}
