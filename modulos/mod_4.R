# =========================================================
# MÓDULO 4: Parámetros de diseño
# =========================================================

mod_diseno_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 4. Parámetros de diseño"),
      p("Ingrese los parámetros de diseño. El valor de m corresponde al submuestreo (número de unidades seleccionadas por UPM). El cálculo y la comparación para distintos valores de m se presentarán en el módulo siguiente."),
      fluidRow(
        column(6,
               numericInput(ns("N"), "Ingrese el tamaño de la población (N):", value = NA, min = 1),
               numericInput(ns("M"), "Ingrese el total de Unidades Primarias de Muestreo (UPM) en el marco (M):", value = NA, min = 1)
        ),
        column(6,
               numericInput(ns("m_desde"), "Valor mínimo de submuestreo (m):", value = 8, min = 1),
               numericInput(ns("m_hasta"), "Valor máximo de submuestreo (m):", value = 20, min = 1),
               numericInput(ns("rho"), "Coeficiente de correlación intraclase (rho):", value = 0.05, min = 0, max = 1, step = 0.01)
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
