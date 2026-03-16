# =========================================================
# MÓDULO 4: Parámetros de diseño
# =========================================================

mod_diseno_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("4. Parámetros de diseño"),
      p("Ingrese parámetros de diseño para cálculo con funciones SS4HHSm/SS4HHSp."),
      fluidRow(
        column(6,
               numericInput(ns("N"), "N (tamaño de población):", value = NA, min = 1),
               numericInput(ns("M"), "M (UPM del marco):", value = NA, min = 1)
        ),
        column(6,
               numericInput(ns("m"), "m (hogares por UPM):", value = 12, min = 1),
               numericInput(ns("rho"), "rho (ICC):", value = 0.05, min = 0, max = 1, step = 0.01)
        )
      ),
      verbatimTextOutput(ns("resumen_diseno"))
    )
  )
}

mod_diseno_server <- function(id, unidad) {
  moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.na(input$N) || input$N <= 0) return("N debe ser mayor que 0.")
      if (is.na(input$M) || input$M <= 0) return("M debe ser mayor que 0.")
      if (is.na(input$m) || input$m <= 0) return("m debe ser mayor que 0.")
      if (is.na(input$rho) || input$rho < 0 || input$rho > 1) return("rho debe estar entre 0 y 1.")
      NULL
    })

    nbar <- reactive({
      req(unidad())
      input$m * unidad()$r * unidad()$b
    })

    output$resumen_diseno <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      cat("Cálculo interno: n̄ = m × r × b =", round(nbar(), 4), "\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(N = input$N, M = input$M, m = input$m, rho = input$rho, nbar = nbar())
      })
    )
  })
}
