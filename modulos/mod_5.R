# =========================================================
# MÓDULO 5: Parámetros de presupuesto
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("5. Parámetros de presupuesto"),
      p("Separe aquí los parámetros de costo para el cálculo final."),
      numericInput(ns("c_h"), "c_h (costo por encuesta/hogar):", value = 1, min = 0),
      numericInput(ns("c_upm"), "c_UPM (costo fijo por UPM):", value = 1, min = 0),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_presupuesto_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.null(input$c_h) || is.na(input$c_h) || input$c_h < 0) {
        return("c_h debe ser un número mayor o igual a 0.")
      }
      if (is.null(input$c_upm) || is.na(input$c_upm) || input$c_upm < 0) {
        return("c_UPM debe ser un número mayor o igual a 0.")
      }
      NULL
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      cat("Costos válidos para cálculo final.\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(c_h = input$c_h, c_upm = input$c_upm)
      })
    )
  })
}
