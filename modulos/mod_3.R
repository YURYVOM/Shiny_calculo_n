# =========================================================
# PASO 3: Definir precisión
# Ingresa amplitud del IC y nivel de confianza (sin alpha explícito)
# Calcula EE, ME, CV y MR
# =========================================================

mod_precision_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("Módulo 3. Definir precisión"),
      p("Ingrese amplitud y nivel de confianza."),

      shiny::numericInput(
        ns("amplitud"),
        "Amplitud del intervalo de confianza:",
        value = NA,
        min = 0,
        step = 0.001
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
      h4("Resultados de precisión"),
      p("Verifique si las medidas de calidad cumplen con lo esperado. En caso contrario, ajuste la amplitud y el nivel de confianza hasta alcanzar la calidad deseada."),
      tableOutput(ns("resultados"))
    )
  )
}


mod_precision_server <- function(id, parametro) {

  shiny::moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.na(input$amplitud) || input$amplitud <= 0) {
        return("La amplitud debe ser mayor que 0.")
      }
      conf_num <- suppressWarnings(as.numeric(input$conf))
      if (is.na(conf_num) || conf_num <= 0 || conf_num >= 1) {
        return("El nivel de confianza debe estar entre 0 y 1.")
      }
      NULL
    })

    z <- reactive({
      validate(need(is.null(validacion()), validacion()))
      qnorm(1 - (1 - as.numeric(input$conf)) / 2)
    })

    SE <- reactive({
      validate(need(is.null(validacion()), validacion()))
      input$amplitud / (2 * z())
    })

    ME <- reactive({
      z() * SE()
    })

    CV <- reactive({
      p <- parametro()
      req(p)
      if (isTRUE(all.equal(p, 0))) return(NA_real_)
      SE() / p
    })

    MR <- reactive({
      p <- parametro()
      req(p)
      if (isTRUE(all.equal(p, 0))) return(NA_real_)
      ME() / p
    })

    output$resultados <- renderTable({
      validate(need(is.null(validacion()), validacion()))

      data.frame(
        Indicador = c(
          "z",
          "Error estándar",
          "Margen de error",
          "Coeficiente de variación",
          "Margen de error relativo",
          "alpha calculada"
        ),
        Valor = c(
          round(z(), 4),
          round(SE(), 6),
          round(ME(), 6),
          round(CV(), 6),
          round(MR(), 6),
          round(1 - as.numeric(input$conf), 4)
        )
      )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%")

    list(
      validacion = validacion,
      SE = SE,
      ME = ME,
      CV = CV,
      MR = MR,
      conf = reactive(as.numeric(input$conf)),
      amplitud = reactive(input$amplitud)
    )

  })
}
