# =========================================================
# MÓDULO 2: Unidad de análisis
# Permite seleccionar:
# - Hogares  -> fija r = 1, b = 1
# - Personas -> pide r y b
# Devuelve una lista reactiva con los valores del módulo
# =========================================================

mod_unidad_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("2. Unidad de análisis"),

      shiny::radioButtons(
        inputId = ns("unidad"),
        label = "Seleccione la unidad de análisis:",
        choices = c("Hogares", "Personas"),
        selected = "Hogares",
        inline = TRUE
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Personas'", ns("unidad")),
        shiny::numericInput(
          inputId = ns("r"),
          label = "Ingrese r (promedio de personas por hogar):",
          value = NA,
          min = 0,
          step = 0.01
        ),
        shiny::numericInput(
          inputId = ns("b"),
          label = "Ingrese b (proporción elegible, entre 0 y 1):",
          value = NA,
          min = 0,
          max = 1,
          step = 0.01
        )
      ),

      shiny::verbatimTextOutput(ns("mensaje_unidad"))
    )
  )
}

mod_unidad_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      req(input$unidad)

      if (input$unidad == "Personas") {
        if (is.na(input$r) || is.na(input$b)) {
          return("Debe ingresar r y b.")
        }

        if (input$r <= 0) {
          return("El valor de r debe ser mayor que 0.")
        }

        if (input$b < 0 || input$b > 1) {
          return("El valor de b debe estar entre 0 y 1.")
        }
      }

      return(NULL)
    })

    datos_unidad <- reactive({
      req(input$unidad)

      validate(shiny::need(is.null(validacion()), validacion()))

      if (input$unidad == "Hogares") {
        return(list(
          unidad = "Hogares",
          r = 1,
          b = 1
        ))
      }

      if (input$unidad == "Personas") {
        return(list(
          unidad = "Personas",
          r = input$r,
          b = input$b
        ))
      }
    })

    output$mensaje_unidad <- shiny::renderPrint({
      if (input$unidad == "Hogares") {
        cat("Para hogares se asume automáticamente: r = 1 y b = 1")
      } else {
        req(datos_unidad())
        cat("Valores seleccionados:\n")
        print(datos_unidad())
      }
    })

    return(list(
      datos = datos_unidad,
      validacion = validacion
    ))
  })
}
