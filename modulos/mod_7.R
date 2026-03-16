# =========================================================
# MÓDULO 6: Representatividad DAM (ingreso por dominio + selección nacional)
# =========================================================

mod_asignacion_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 6. Representatividad por DAM"),
      p("Paso 1: indique si desea representatividad por DAM."),
      radioButtons(
        ns("usa_dominios"),
        "¿Desea representatividad por DAM?",
        choices = c("No" = "no", "Sí" = "si"),
        selected = "no",
        inline = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        p("Paso 2: ingrese los valores por dominio DAM."),
        numericInput(ns("n_dominios"), "Número de dominios DAM:", value = 2, min = 2),
        uiOutput(ns("dominios_ui")),
        fluidRow(
          column(6, selectInput(ns("dam_filtro"), "Filtro DAM para visualizar:", choices = character(0))),
          column(6, selectInput(ns("m_filtro"), "Filtro m para visualizar:", choices = character(0)))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'no'", ns("usa_dominios")),
        p("Paso 2: seleccione el m nacional que más le gustó."),
        selectInput(ns("m_nacional"), "Seleccione m nacional:", choices = character(0))
      ),
      h4("Resultados por DAM y por m (salida de función)"),
      tableOutput(ns("tabla_dam")),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_asignacion_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    valores_dominios <- reactive({
      p <- parametro(); req(p)
      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      param_dom <- vapply(seq_len(n), function(i) input[[paste0("param_dom_", i)]], numeric(1))
      N_dom <- vapply(seq_len(n), function(i) input[[paste0("N_dom_", i)]], numeric(1))
      M_dom <- vapply(seq_len(n), function(i) input[[paste0("M_dom_", i)]], numeric(1))

      if (p$tipo == "Media") {
        sd_dom <- vapply(seq_len(n), function(i) input[[paste0("sd_dom_", i)]], numeric(1))
      } else {
        sd_dom <- numeric(0)
      }

      list(param_dom = param_dom, sd_dom = sd_dom, N_dom = N_dom, M_dom = M_dom)
    })

    output$dominios_ui <- renderUI({
      p <- parametro(); req(p)
      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      filas <- lapply(seq_len(n), function(i) {
        fluidRow(
          column(3, numericInput(session$ns(paste0("param_dom_", i)), sprintf("DAM %s: parámetro", i), value = NA, min = if (p$tipo == "Proporción") 0 else NA, max = if (p$tipo == "Proporción") 1 else NA)),
          column(3, if (p$tipo == "Media") numericInput(session$ns(paste0("sd_dom_", i)), sprintf("DAM %s: desviación", i), value = NA, min = 0) else tags$div()),
          column(3, numericInput(session$ns(paste0("N_dom_", i)), sprintf("DAM %s: N", i), value = NA, min = 1)),
          column(3, numericInput(session$ns(paste0("M_dom_", i)), sprintf("DAM %s: M", i), value = NA, min = 1))
        )
      })

      do.call(tagList, filas)
    })

    observe({
      d <- diseno(); req(d)
      m_choices <- as.character(d$m_vector)

      m_sel <- if (!is.null(input$m_nacional) && input$m_nacional %in% m_choices) input$m_nacional else m_choices[1]
      updateSelectInput(session, "m_nacional", choices = m_choices, selected = m_sel)

      dam_choices <- if (identical(input$usa_dominios, "si") && !is.null(input$n_dominios) && input$n_dominios >= 2) {
        c("Todos", as.character(seq_len(input$n_dominios)))
      } else c("Todos", "1")
      dam_sel <- if (!is.null(input$dam_filtro) && input$dam_filtro %in% dam_choices) input$dam_filtro else "Todos"
      updateSelectInput(session, "dam_filtro", choices = dam_choices, selected = dam_sel)

      m_filter_choices <- c("Todos", m_choices)
      m_filter_sel <- if (!is.null(input$m_filtro) && input$m_filtro %in% m_filter_choices) input$m_filtro else "Todos"
      updateSelectInput(session, "m_filtro", choices = m_filter_choices, selected = m_filter_sel)
    })

    tabla_dam_completa <- reactive({
      p <- parametro(); pr <- precision(); u <- unidad(); d <- diseno()
      req(p, pr, u, d)

      if (!identical(input$usa_dominios, "si")) {
        return(do.call(rbind, lapply(d$m_vector, function(m_i) {
          n_h <- if (p$tipo == "Media") {
            ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
          } else {
            ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
          }
          data.frame(dam = 1, m = m_i, n_hogares = n_h, n_encuestas = ceiling(n_h * u$r * u$b), upm = ceiling(n_h / m_i))
        })))
      }

      v <- valores_dominios()
      out <- list(); k <- 1
      for (j in seq_len(input$n_dominios)) {
        for (m_i in d$m_vector) {
          n_h <- if (p$tipo == "Media") {
            ss4HHSm(N = v$N_dom[j], M = v$M_dom[j], rho = d$rho, mu = v$param_dom[j], sigma = v$sd_dom[j], delta = pr$delta, conf = pr$conf, m = m_i)
          } else {
            ss4HHSp(N = v$N_dom[j], M = v$M_dom[j], rho = d$rho, p = v$param_dom[j], delta = pr$delta, conf = pr$conf, m = m_i)
          }
          out[[k]] <- data.frame(dam = j, m = m_i, n_hogares = n_h, n_encuestas = ceiling(n_h * u$r * u$b), upm = ceiling(n_h / m_i))
          k <- k + 1
        }
      }
      do.call(rbind, out)
    })

    validacion <- reactive({
      p <- parametro(); d <- diseno(); req(p, d)

      if (!identical(input$usa_dominios, "si")) {
        if (is.null(input$m_nacional) || !(input$m_nacional %in% as.character(d$m_vector))) return("Seleccione un m nacional válido.")
        return(NULL)
      }

      if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")
      v <- valores_dominios()
      if (any(is.na(v$param_dom)) || any(is.na(v$N_dom)) || any(is.na(v$M_dom)) || any(v$N_dom <= 0) || any(v$M_dom <= 0)) {
        return("Revise parámetros, N y M por DAM (numéricos y > 0).")
      }
      if (p$tipo == "Media") {
        if (any(is.na(v$sd_dom)) || any(v$sd_dom <= 0)) return("Desviación estándar por DAM debe ser > 0.")
      } else if (any(v$param_dom < 0 | v$param_dom > 1)) {
        return("Parámetros proporción por DAM deben estar entre 0 y 1.")
      }
      NULL
    })

    output$tabla_dam <- renderTable({
      validate(need(is.null(validacion()), validacion()))
      tb <- tabla_dam_completa()
      if (!is.null(input$dam_filtro) && input$dam_filtro != "Todos") tb <- tb[tb$dam == as.numeric(input$dam_filtro), , drop = FALSE]
      if (!is.null(input$m_filtro) && input$m_filtro != "Todos") tb <- tb[tb$m == as.numeric(input$m_filtro), , drop = FALSE]
      tb
    }, striped = TRUE, bordered = TRUE)

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_dominios, "si")) {
        cat("Representatividad DAM: Sí\n")
        cat("Se guardaron los valores por dominio y se calculó para todos los m.\n")
      } else {
        cat("Representatividad DAM: No\n")
        cat("m nacional elegido:", input$m_nacional, "\n")
      }
      cat("Continúe al siguiente módulo para los resultados finales y área.\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        d <- diseno(); p <- parametro(); req(d, p)
        v <- if (identical(input$usa_dominios, "si")) valores_dominios() else list(param_dom = numeric(0), sd_dom = numeric(0), N_dom = numeric(0), M_dom = numeric(0))

        list(
          usa_dominios = identical(input$usa_dominios, "si"),
          m_sel_nacional = if (identical(input$usa_dominios, "si")) NA_real_ else as.numeric(input$m_nacional),
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          param_dom = v$param_dom,
          sd_dom = if (identical(input$usa_dominios, "si") && p$tipo == "Media") v$sd_dom else numeric(0),
          N_dom = v$N_dom,
          M_dom = v$M_dom,
          tabla_dam = tabla_dam_completa(),
          m_vector = d$m_vector
        )
      })
    )
  })
}
