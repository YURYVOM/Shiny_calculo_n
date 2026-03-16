# =========================================================
# PASO 6: Dominios de la mayor división administrativa (DAM)
# =========================================================

mod_dominios_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 5. Representatividad por DAM"),
      radioButtons(
        ns("usa_dominios"),
        "¿Necesita representatividad por división administrativa (DAM)?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        numericInput(ns("n_dominios"), "Ingrese número de dominios:", value = 2, min = 2),
        helpText("El indicador (media+desviación o proporción) se toma del paso 1, según el flujo.")
      ),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_dominios_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.null(input$usa_dominios)) return("Debe seleccionar si usará representatividad por DAM.")
      if (identical(input$usa_dominios, "si")) {
        if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")
      }
      NULL
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_dominios, "si")) {
        cat("DAM activo con", input$n_dominios, "dominios.\n")
      } else {
        cat("Sin representatividad por DAM.\n")
      }
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        if (!identical(input$usa_dominios, "si")) {
          return(list(usa_dominios = FALSE, n_dominios = 1))
        }
        list(usa_dominios = TRUE, n_dominios = input$n_dominios)
      })
    )
  })
}
