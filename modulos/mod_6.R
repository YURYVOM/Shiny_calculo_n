# =========================================================
# MÓDULO 6: Dominios / división administrativa
# =========================================================

mod_dominios_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("6. Representatividad por dominios (DAM/área)"),
      radioButtons(
        ns("usa_dominios"),
        "¿Necesita representatividad por división administrativa?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        numericInput(ns("n_dominios"), "Número de dominios de la mayor división administrativa:", value = 2, min = 2),
        numericInput(ns("sim"), "SIM (factor de incremento por dominio):", value = 1, min = 1, step = 0.01),
        helpText("Si se activa, se ajustará el tamaño de muestra total por dominios usando SIM.")
      ),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_dominios_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.null(input$usa_dominios)) return("Debe seleccionar si usará dominios.")
      if (identical(input$usa_dominios, "si")) {
        if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios debe ser >= 2.")
        if (is.na(input$sim) || input$sim < 1) return("SIM debe ser >= 1.")
      }
      NULL
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_dominios, "si")) {
        cat("Se aplicará ajuste por dominios con", input$n_dominios, "dominios y SIM =", input$sim, "\n")
      } else {
        cat("Sin ajuste por dominios.\n")
      }
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          usa_dominios = identical(input$usa_dominios, "si"),
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          sim = if (identical(input$usa_dominios, "si")) input$sim else 1
        )
      })
    )
  })
}
