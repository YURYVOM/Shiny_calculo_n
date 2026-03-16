# =========================================================
# MÓDULO 3: Definir precisión
# Ingresa amplitud del IC y nivel de confianza
# Calcula EE, ME, CV y MR
# =========================================================

mod_precision_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("3. Definir precisión"),

      shiny::numericInput(
        ns("amplitud"),
        "Amplitud del intervalo de confianza:",
        value = NA,
        min = 0
      ),

      shiny::selectInput(
        ns("conf"),
        "Nivel de confianza:",
        choices = c(
          "90%" = 0.90,
          "95%" = 0.95,
          "99%" = 0.99
        ),
        selected = 0.95
      ),

      hr(),

      h4("Resultados"),

      verbatimTextOutput(ns("resultados"))
    )
  )
}


mod_precision_server <- function(id, parametro) {

  shiny::moduleServer(id, function(input, output, session) {

    z <- reactive({
      qnorm(1 - (1 - as.numeric(input$conf)) / 2)
    })

    SE <- reactive({
      req(input$amplitud)
      input$amplitud / (2 * z())
    })

    ME <- reactive({
      z() * SE()
    })

    CV <- reactive({
      req(parametro())
      SE() / parametro()
    })

    MR <- reactive({
      req(parametro())
      ME() / parametro()
    })

    output$resultados <- renderPrint({

      list(
        Error_estandar = SE(),
        Margen_error = ME(),
        Coeficiente_variacion = CV(),
        Margen_error_relativo = MR()
      )

    })

    list(
      SE = SE,
      ME = ME,
      CV = CV,
      MR = MR
    )

  })
}
