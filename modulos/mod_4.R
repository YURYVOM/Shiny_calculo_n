# =========================================================
# MÓDULO 4: Parámetros de diseño
# =========================================================

mod_diseno_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 4. Parámetros de diseño"),
      p("Ingrese parámetros de diseño. El cálculo y comparación por vector de m se mostrará en el módulo siguiente."),
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
      )
    )
  )
}

mod_diseno_server <- function(id) {
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

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          N = input$N,
          M = input$M,
          rho = input$rho,
          m_vector = m_vector()
        )
      })
    )
  })
}
