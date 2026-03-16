# =========================================================
# MÓDULO 4: Parámetros de diseño y presupuesto
# =========================================================

mod_diseno_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("4. Parámetros de diseño y presupuesto"),
      p("Ingrese los parámetros para ajustar el tamaño de muestra por efecto de diseño y costos."),

      fluidRow(
        column(6,
               numericInput(ns("N"), "N (tamaño de población):", value = NA, min = 1),
               numericInput(ns("M"), "M (UPM del marco):", value = NA, min = 1),
               numericInput(ns("m"), "m (hogares por UPM):", value = 12, min = 1),
               numericInput(ns("rho"), "rho (ICC):", value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        column(6,
               numericInput(ns("c_h"), "c_h (costo por encuesta):", value = 1, min = 0),
               numericInput(ns("c_upm"), "c_UPM (costo fijo por UPM):", value = 1, min = 0),
               checkboxInput(ns("dominios"), "Requiere representatividad por dominios", value = FALSE),
               conditionalPanel(
                 condition = sprintf("input['%s']", ns("dominios")),
                 numericInput(ns("n_dominios"), "Número de dominios:", value = 2, min = 2)
               )
        )
      ),

      verbatimTextOutput(ns("resumen_diseno"))
    )
  )
}

mod_diseno_server <- function(id, unidad) {
  shiny::moduleServer(id, function(input, output, session) {

    validacion <- reactive({
      if (is.na(input$N) || input$N <= 0) return("N debe ser mayor que 0.")
      if (is.na(input$M) || input$M <= 0) return("M debe ser mayor que 0.")
      if (is.na(input$m) || input$m <= 0) return("m debe ser mayor que 0.")
      if (is.na(input$rho) || input$rho < 0 || input$rho > 1) return("rho debe estar entre 0 y 1.")
      if (is.na(input$c_h) || input$c_h < 0) return("c_h no puede ser negativo.")
      if (is.na(input$c_upm) || input$c_upm < 0) return("c_UPM no puede ser negativo.")
      if (isTRUE(input$dominios) && (is.na(input$n_dominios) || input$n_dominios < 2)) {
        return("Si activa dominios, el número de dominios debe ser >= 2.")
      }
      NULL
    })

    nbar <- reactive({
      req(unidad())
      input$m * unidad()$r * unidad()$b
    })

    output$resumen_diseno <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      req(unidad())
      cat("Cálculo interno:\n")
      cat("n̄ = m × r × b =", round(nbar(), 4), "\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        list(
          N = input$N,
          M = input$M,
          m = input$m,
          rho = input$rho,
          c_h = input$c_h,
          c_upm = input$c_upm,
          dominios = isTRUE(input$dominios),
          n_dominios = if (isTRUE(input$dominios)) input$n_dominios else 1,
          nbar = nbar()
        )
      })
    )
  })
}
