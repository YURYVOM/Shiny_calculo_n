# =========================================================
# MÓDULO 7: Asignación por área (IPFP aproximada)
# =========================================================

mod_asignacion_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("7. Asignación por área"),
      radioButtons(
        ns("usa_area"),
        "¿Desea hacer asignación por área?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_area")),
        numericInput(ns("n_areas"), "Número de áreas:", value = 2, min = 2),
        textAreaInput(
          ns("tabla_prop"),
          "Tabla cruzada de proporciones censales (CSV, filas=dominios, columnas=áreas). Debe sumar 1.",
          rows = 6,
          placeholder = "0.10,0.15\n0.20,0.25\n0.10,0.20"
        )
      ),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_asignacion_server <- function(id, n_dominios_rx) {
  moduleServer(id, function(input, output, session) {

    tabla_matrix <- reactive({
      req(input$tabla_prop)
      filas <- strsplit(trimws(input$tabla_prop), "\\n")[[1]]
      mat <- do.call(rbind, lapply(filas, function(f) as.numeric(strsplit(f, ",")[[1]])))
      mat
    })

    validacion <- reactive({
      if (is.null(input$usa_area)) return("Debe seleccionar si hará asignación por área.")
      if (identical(input$usa_area, "si")) {
        if (is.na(input$n_areas) || input$n_areas < 2) return("Número de áreas debe ser >= 2.")
        mat <- tryCatch(tabla_matrix(), error = function(e) NULL)
        if (is.null(mat)) return("La tabla cruzada no tiene formato CSV válido.")

        n_dom <- n_dominios_rx()
        if (nrow(mat) != n_dom) return(sprintf("La tabla debe tener %s filas (dominios).", n_dom))
        if (ncol(mat) != input$n_areas) return(sprintf("La tabla debe tener %s columnas (áreas).", input$n_areas))
        if (any(is.na(mat)) || any(mat < 0)) return("La tabla no debe tener negativos ni valores faltantes.")
        if (abs(sum(mat) - 1) > 1e-6) return("La tabla de proporciones debe sumar 1.")
      }
      NULL
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_area, "si")) {
        cat("Asignación IPFP aproximada habilitada.\n")
        print(round(tabla_matrix(), 4))
      } else {
        cat("No se aplicará asignación por área.\n")
      }
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        if (identical(input$usa_area, "si")) {
          list(usa_area = TRUE, n_areas = input$n_areas, tabla_prop = tabla_matrix())
        } else {
          list(usa_area = FALSE, n_areas = 1, tabla_prop = matrix(1, nrow = n_dominios_rx(), ncol = 1))
        }
      })
    )
  })
}
