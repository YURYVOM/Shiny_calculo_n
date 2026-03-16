# =========================================================
# MÓDULO 6: Representatividad DAM + selección LM para AM + asignación por área
# =========================================================

mod_asignacion_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 6. Representatividad DAM y nivel AM"),
      radioButtons(
        ns("usa_dominios"),
        "¿Desea representatividad por DAM?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        numericInput(ns("n_dominios"), "¿Cuántos dominios tiene?", value = 2, min = 2),
        textAreaInput(ns("param_dom"), "Parámetro del indicador por dominio (coma separado)", rows = 3, placeholder = "0.2,0.25,0.3"),
        textAreaInput(ns("sd_dom"), "Si es Media: desviación estándar por dominio", rows = 3, placeholder = "120,125,130"),
        textAreaInput(ns("N_dom"), "Población N por dominio/departamento", rows = 3, placeholder = "150000,180000,210000")
      ),
      hr(),
      selectInput(ns("lm_sel"), "Seleccione LM para nivel AM:", choices = character(0)),
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

mod_asignacion_server <- function(id, dominios_datos, parametro) {
  moduleServer(id, function(input, output, session) {

    parse_vec <- function(txt) {
      if (is.null(txt)) return(numeric(0))
      v <- trimws(unlist(strsplit(txt, ",")))
      v <- v[v != ""]
      as.numeric(v)
    }

    observe({
      d5 <- dominios_datos(); req(d5)
      choices <- as.character(d5$LM_vector)
      selected <- if (!is.null(input$lm_sel) && input$lm_sel %in% choices) input$lm_sel else choices[1]
      updateSelectInput(session, "lm_sel", choices = choices, selected = selected)
    })

    tabla_matrix <- reactive({
      req(input$tabla_prop)
      filas <- strsplit(trimws(input$tabla_prop), "\\n")[[1]]
      do.call(rbind, lapply(filas, function(f) as.numeric(strsplit(f, ",")[[1]])))
    })

    n_dominios_activo <- reactive({
      if (identical(input$usa_dominios, "si")) input$n_dominios else 1
    })

    validacion <- reactive({
      d5 <- dominios_datos(); req(d5)
      p <- parametro(); req(p)

      if (is.null(input$lm_sel) || !(input$lm_sel %in% as.character(d5$LM_vector))) {
        return("Seleccione un LM válido para nivel AM.")
      }

      if (identical(input$usa_dominios, "si")) {
        if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")

        v_param <- parse_vec(input$param_dom)
        v_N <- parse_vec(input$N_dom)
        if (length(v_param) != input$n_dominios) return("Debe ingresar un parámetro por dominio.")
        if (length(v_N) != input$n_dominios) return("Debe ingresar N por dominio.")
        if (any(is.na(v_param)) || any(is.na(v_N)) || any(v_N <= 0)) return("Revise parámetros/N por dominio (numéricos y N > 0).")

        if (p$tipo == "Media") {
          v_sd <- parse_vec(input$sd_dom)
          if (length(v_sd) != input$n_dominios) return("Debe ingresar desviación estándar por dominio.")
          if (any(is.na(v_sd)) || any(v_sd <= 0)) return("Desviación estándar por dominio debe ser > 0.")
        } else {
          if (any(v_param < 0 | v_param > 1)) return("Las proporciones por dominio deben estar entre 0 y 1.")
        }
      }

      if (identical(input$usa_area, "si")) {
        if (is.na(input$n_areas) || input$n_areas < 2) return("Número de áreas debe ser >= 2.")
        mat <- tryCatch(tabla_matrix(), error = function(e) NULL)
        if (is.null(mat)) return("La tabla cruzada no tiene formato CSV válido.")

        n_dom <- n_dominios_activo()
        if (nrow(mat) != n_dom) return(sprintf("La tabla debe tener %s filas (dominios).", n_dom))
        if (ncol(mat) != input$n_areas) return(sprintf("La tabla debe tener %s columnas (áreas).", input$n_areas))
        if (any(is.na(mat)) || any(mat < 0)) return("La tabla no debe tener negativos ni faltantes.")
        if (abs(sum(mat) - 1) > 1e-6) return("La tabla de proporciones debe sumar 1.")
      }

      NULL
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      cat("LM seleccionado para AM:", input$lm_sel, "\n")
      cat("DAM:", ifelse(identical(input$usa_dominios, "si"), "Sí", "No"), "\n")
      cat("Asignación por área:", ifelse(identical(input$usa_area, "si"), "Sí", "No"), "\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        p <- parametro(); req(p)

        list(
          lm_sel = as.numeric(input$lm_sel),
          usa_dominios = identical(input$usa_dominios, "si"),
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          param_dom = if (identical(input$usa_dominios, "si")) parse_vec(input$param_dom) else numeric(0),
          sd_dom = if (identical(input$usa_dominios, "si") && p$tipo == "Media") parse_vec(input$sd_dom) else numeric(0),
          N_dom = if (identical(input$usa_dominios, "si")) parse_vec(input$N_dom) else numeric(0),
          usa_area = identical(input$usa_area, "si"),
          n_areas = if (identical(input$usa_area, "si")) input$n_areas else 1,
          tabla_prop = if (identical(input$usa_area, "si")) tabla_matrix() else matrix(1, nrow = n_dominios_activo(), ncol = 1)
        )
      })
    )
  })
}
