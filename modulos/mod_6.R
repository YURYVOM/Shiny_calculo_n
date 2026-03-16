# =========================================================
# MÓDULO 6: Representatividad DAM (ingreso por dominio)
# =========================================================

mod_asignacion_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 6. Representatividad por DAM"),
      p("Indique si desea representatividad a nivel de División Administrativa Mayor (DAM)."),
      radioButtons(
        ns("usa_dominios"),
        "¿Desea representatividad por DAM?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        numericInput(ns("n_dominios"), "Número de dominios DAM:", value = 2, min = 2),
        radioButtons(
          ns("unidad_dam"),
          "Nivel de estimación DAM:",
          choices = c("Hogares" = "Hogares", "Personas" = "Personas"),
          selected = "Hogares",
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Personas'", ns("unidad_dam")),
          p("Si DAM es a nivel Personas, ingrese r y b para cada DAM en la tabla de dominios.")
        ),
        uiOutput(ns("dominios_ui")),
        fluidRow(
          column(6, selectInput(ns("dam_filtro"), "Filtro DAM para visualizar:", choices = character(0))),
          column(6, selectInput(ns("m_filtro"), "Filtro m para visualizar:", choices = character(0)))
        )
      ),
      h4("Resultados por DAM y por m (salida de función)"),
      tableOutput(ns("tabla_dam")),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_asignacion_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    valor_num_input <- function(id) {
      v <- input[[id]]
      if (is.null(v) || length(v) == 0) return(NA_real_)
      as.numeric(v)
    }

    valores_dominios <- reactive({
      p <- parametro(); req(p)
      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      param_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("param_dom_", i)), numeric(1))
      N_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("N_dom_", i)), numeric(1))
      M_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("M_dom_", i)), numeric(1))
      amplitud_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("amplitud_dom_", i)), numeric(1))
      r_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("r_dom_", i)), numeric(1))
      b_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("b_dom_", i)), numeric(1))

      if (p$tipo == "Media") {
        sd_dom <- vapply(seq_len(n), function(i) valor_num_input(paste0("sd_dom_", i)), numeric(1))
      } else {
        sd_dom <- numeric(0)
      }

      list(
        param_dom = param_dom,
        sd_dom = sd_dom,
        N_dom = N_dom,
        M_dom = M_dom,
        amplitud_dom = amplitud_dom,
        delta_dom = amplitud_dom / 2,
        r_dom = r_dom,
        b_dom = b_dom
      )
    })

    output$dominios_ui <- renderUI({
      p <- parametro(); req(p)
      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      filas <- lapply(seq_len(n), function(i) {
        fluidRow(
          column(
            3,
            numericInput(
              session$ns(paste0("param_dom_", i)),
              if (p$tipo == "Media") sprintf("DAM %s: media esperada", i) else sprintf("DAM %s: proporción p", i),
              value = NA,
              min = if (p$tipo == "Proporción") 0 else NA,
              max = if (p$tipo == "Proporción") 1 else NA
            )
          ),
          column(3, if (p$tipo == "Media") numericInput(session$ns(paste0("sd_dom_", i)), sprintf("DAM %s: desviación estándar", i), value = NA, min = 0) else tags$div()),
          column(2, numericInput(session$ns(paste0("N_dom_", i)), sprintf("DAM %s: N", i), value = NA, min = 1)),
          column(2, numericInput(session$ns(paste0("M_dom_", i)), sprintf("DAM %s: M", i), value = NA, min = 1)),
          column(2, numericInput(session$ns(paste0("amplitud_dom_", i)), sprintf("DAM %s: amplitud", i), value = NA, min = 0, step = 0.001)),
          column(1, if (identical(input$unidad_dam, "Personas")) numericInput(session$ns(paste0("r_dom_", i)), sprintf("DAM %s: r", i), value = NA, min = 0, step = 0.01) else tags$div()),
          column(1, if (identical(input$unidad_dam, "Personas")) numericInput(session$ns(paste0("b_dom_", i)), sprintf("DAM %s: b", i), value = NA, min = 0, max = 1, step = 0.01) else tags$div())
        )
      })

      do.call(tagList, filas)
    })

    observe({
      d <- diseno(); req(d)

      n_dom <- suppressWarnings(as.integer(input$n_dominios))
      tiene_dominios <- identical(input$usa_dominios, "si") && !is.na(n_dom) && n_dom >= 2

      dam_choices <- if (isTRUE(tiene_dominios)) {
        c("Todos", as.character(seq_len(n_dom)))
      } else c("Todos")
      dam_sel <- if (!is.null(input$dam_filtro) && input$dam_filtro %in% dam_choices) input$dam_filtro else "Todos"
      updateSelectInput(session, "dam_filtro", choices = dam_choices, selected = dam_sel)

      m_choices <- as.character(d$m_vector)
      m_filter_choices <- c("Todos", m_choices)
      m_filter_sel <- if (!is.null(input$m_filtro) && input$m_filtro %in% m_filter_choices) input$m_filtro else "Todos"
      updateSelectInput(session, "m_filtro", choices = m_filter_choices, selected = m_filter_sel)
    })

    tabla_dam_completa <- reactive({
      p <- parametro(); pr <- precision(); d <- diseno()
      req(p, pr, d)

      if (!identical(input$usa_dominios, "si")) {
        return(data.frame())
      }

      v <- valores_dominios()
      out <- list(); k <- 1
      for (j in seq_len(input$n_dominios)) {
        for (m_i in d$m_vector) {
          n_h <- if (p$tipo == "Media") {
            ss4HHSm(N = v$N_dom[j], M = v$M_dom[j], rho = d$rho, mu = v$param_dom[j], sigma = v$sd_dom[j], delta = v$delta_dom[j], conf = pr$conf, m = m_i)
          } else {
            ss4HHSp(N = v$N_dom[j], M = v$M_dom[j], rho = d$rho, p = v$param_dom[j], delta = v$delta_dom[j], conf = pr$conf, m = m_i)
          }
          out[[k]] <- data.frame(
            dam = j,
            HouseholdsPerPSU = m_i,
            DEFF = round(1 + (m_i - 1) * d$rho, 2),
            PSUinSample = ceiling(n_h / m_i),
            HouseholdsInSample = n_h,
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
      do.call(rbind, out)
    })

    validacion <- reactive({
      p <- parametro(); req(p)

      if (!identical(input$usa_dominios, "si")) {
        return(NULL)
      }

      if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")
      v <- valores_dominios()
      if (any(is.na(v$param_dom)) || any(is.na(v$N_dom)) || any(is.na(v$M_dom)) || any(v$N_dom <= 0) || any(v$M_dom <= 0)) {
        return("Revise parámetro, N y M por DAM (numéricos y > 0).")
      }
      if (any(is.na(v$amplitud_dom)) || any(v$amplitud_dom <= 0)) {
        return("La amplitud por DAM debe ser mayor que 0.")
      }
      if (p$tipo == "Media") {
        if (any(is.na(v$sd_dom)) || any(v$sd_dom <= 0)) return("Desviación estándar por DAM debe ser > 0.")
      } else if (any(v$param_dom < 0 | v$param_dom > 1)) {
        return("Parámetros de proporción por DAM deben estar entre 0 y 1.")
      }
      if (identical(input$unidad_dam, "Personas")) {
        if (any(is.na(v$r_dom)) || any(is.na(v$b_dom))) return("Debe ingresar r y b por cada DAM cuando la unidad es Personas.")
        if (any(v$r_dom <= 0)) return("Todos los valores de r DAM deben ser mayores que 0.")
        if (any(v$b_dom < 0 | v$b_dom > 1)) return("Todos los valores de b DAM deben estar entre 0 y 1.")
      }
      NULL
    })

    output$tabla_dam <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      if (!identical(input$usa_dominios, "si")) return(data.frame(Nota = "No se solicitó representatividad por DAM."))
      tb <- tabla_dam_completa()
      if (!is.null(input$dam_filtro) && input$dam_filtro != "Todos") tb <- tb[tb$dam == as.numeric(input$dam_filtro), , drop = FALSE]
      if (!is.null(input$m_filtro) && input$m_filtro != "Todos") tb <- tb[tb$HouseholdsPerPSU == as.numeric(input$m_filtro), , drop = FALSE]
      tb
    }, striped = TRUE, bordered = TRUE)

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_dominios, "si")) {
        cat("Representatividad DAM: Sí\n")
        cat("Se guardaron parámetro, N, M y amplitud por dominio DAM.\n")
        cat("Unidad DAM:", input$unidad_dam, "\n")
        if (input$unidad_dam == "Personas") {
          v <- valores_dominios()
          cat("r por DAM:", paste(v$r_dom, collapse = ", "), "\n")
          cat("b por DAM:", paste(v$b_dom, collapse = ", "), "\n")
        }
      } else {
        cat("Representatividad DAM: No\n")
        cat("Se usarán los parámetros nacionales en el módulo final.\n")
      }
      cat("Continúe al siguiente módulo para los resultados finales y área.\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        d <- diseno(); p <- parametro(); req(d, p)

        if (identical(input$usa_dominios, "si")) {
          v <- valores_dominios()
        } else {
          v <- list(
            param_dom = numeric(0),
            sd_dom = numeric(0),
            N_dom = numeric(0),
            M_dom = numeric(0),
            amplitud_dom = numeric(0),
            delta_dom = numeric(0),
            r_dom = numeric(0),
            b_dom = numeric(0)
          )
        }

        list(
          usa_dominios = identical(input$usa_dominios, "si"),
          m_sel_nacional = if (identical(input$usa_dominios, "si")) NA_real_ else d$m_vector[1],
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          param_dom = v$param_dom,
          sd_dom = if (identical(input$usa_dominios, "si") && p$tipo == "Media") v$sd_dom else numeric(0),
          N_dom = v$N_dom,
          M_dom = v$M_dom,
          amplitud_dom = v$amplitud_dom,
          delta_dom = v$delta_dom,
          tabla_dam = tabla_dam_completa(),
          m_vector = d$m_vector,
          unidad_dam = if (identical(input$usa_dominios, "si")) input$unidad_dam else "Hogares",
          r_dam = if (identical(input$usa_dominios, "si") && identical(input$unidad_dam, "Personas")) v$r_dom else rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1),
          b_dam = if (identical(input$usa_dominios, "si") && identical(input$unidad_dam, "Personas")) v$b_dom else rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1)
        )
      })
    )
  })
}
