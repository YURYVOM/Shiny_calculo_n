
# =========================================================
# MÓDULO 1: Parámetro de interés
# Permite seleccionar:
# - Media  -> pide x_barra y s
# - Proporción -> pide p
# Devuelve una lista reactiva con los valores del módulo
# =========================================================

mod_parametro_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("1. Parámetro de interés"),

      shiny::selectInput(
        inputId = ns("tipo_param"),
        label = "Seleccione el parámetro de interés:",
        choices = c("Media", "Proporción"),
        selected = "Media"
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Media'", ns("tipo_param")),
        shiny::numericInput(
          inputId = ns("xbarra"),
          label = "Ingrese x̄ (media esperada):",
          value = NA,
          min = 0
        ),
        shiny::numericInput(
          inputId = ns("s"),
          label = "Ingrese s (desviación estándar):",
          value = NA,
          min = 0
        )
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Proporción'", ns("tipo_param")),
        shiny::numericInput(
          inputId = ns("p"),
          label = "Ingrese p (entre 0 y 1):",
          value = NA,
          min = 0,
          max = 1,
          step = 0.01
        )
      )
    )
  )
}


mod_parametro_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # -----------------------------
    # Validaciones del módulo
    # -----------------------------
    validacion <- reactive({
      req(input$tipo_param)

      if (input$tipo_param == "Media") {
        if (is.na(input$xbarra) || is.na(input$s)) {
          return("Debe ingresar x̄ y s.")
        }
        if (input$s < 0) {
          return("La desviación estándar s no puede ser negativa.")
        }
      }

      if (input$tipo_param == "Proporción") {
        if (is.na(input$p)) {
          return("Debe ingresar p.")
        }
        if (input$p < 0 || input$p > 1) {
          return("La proporción p debe estar entre 0 y 1.")
        }
      }

      return(NULL)
    })

    # -----------------------------
    # Salida reactiva principal
    # -----------------------------
    parametro <- reactive({
      req(validacion())
      validate(need(is.null(validacion()), validacion()))

      if (input$tipo_param == "Media") {
        return(list(
          tipo  = "Media",
          valor = input$xbarra,
          xbarra = input$xbarra,
          s = input$s,
          p = NULL
        ))
      }

      if (input$tipo_param == "Proporción") {
        return(list(
          tipo  = "Proporción",
          valor = input$p,
          xbarra = NULL,
          s = NULL,
          p = input$p
        ))
      }
    })

    return(list(
      datos = parametro,
      validacion = validacion
    ))
  })
}
