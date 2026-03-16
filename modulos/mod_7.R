# =========================================================
# MÓDULO 6: Representatividad DAM (ingreso por dominio + filtros)
# =========================================================

mod_asignacion_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 6. Representatividad por DAM"),
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
        textAreaInput(ns("param_dom"), "Parámetro del indicador por DAM (coma separado)", rows = 3, placeholder = "0.2,0.25,0.3"),
        textAreaInput(ns("sd_dom"), "Si es Media: desviación estándar por DAM", rows = 3, placeholder = "120,125,130"),
        textAreaInput(ns("N_dom"), "Población N por DAM", rows = 3, placeholder = "150000,180000,210000"),
        textAreaInput(ns("M_dom"), "UPM M por DAM", rows = 3, placeholder = "1200,1400,1600"),
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

    parse_vec <- function(txt) {
      if (is.null(txt)) return(numeric(0))
      v <- trimws(unlist(strsplit(txt, ",")))
      v <- v[v != ""]
      as.numeric(v)
    }

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

      v_param <- parse_vec(input$param_dom)
      v_sd <- parse_vec(input$sd_dom)
      v_N <- parse_vec(input$N_dom)
      v_M <- parse_vec(input$M_dom)

      out <- list(); k <- 1
      for (j in seq_len(input$n_dominios)) {
        for (m_i in d$m_vector) {
          n_h <- if (p$tipo == "Media") {
            ss4HHSm(N = v_N[j], M = v_M[j], rho = d$rho, mu = v_param[j], sigma = v_sd[j], delta = pr$delta, conf = pr$conf, m = m_i)
          } else {
            ss4HHSp(N = v_N[j], M = v_M[j], rho = d$rho, p = v_param[j], delta = pr$delta, conf = pr$conf, m = m_i)
          }
          out[[k]] <- data.frame(dam = j, m = m_i, n_hogares = n_h, n_encuestas = ceiling(n_h * u$r * u$b), upm = ceiling(n_h / m_i))
          k <- k + 1
        }
      }
      do.call(rbind, out)
    })

    observe({
      d <- diseno(); req(d)
      dam_choices <- if (identical(input$usa_dominios, "si") && !is.null(input$n_dominios) && input$n_dominios >= 2) {
        c("Todos", as.character(seq_len(input$n_dominios)))
      } else c("Todos", "1")
      dam_sel <- if (!is.null(input$dam_filtro) && input$dam_filtro %in% dam_choices) input$dam_filtro else "Todos"
      updateSelectInput(session, "dam_filtro", choices = dam_choices, selected = dam_sel)

      m_choices <- c("Todos", as.character(d$m_vector))
      m_sel <- if (!is.null(input$m_filtro) && input$m_filtro %in% m_choices) input$m_filtro else "Todos"
      updateSelectInput(session, "m_filtro", choices = m_choices, selected = m_sel)
    })

    validacion <- reactive({
      p <- parametro(); req(p)
      if (identical(input$usa_dominios, "si")) {
        if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")
        v_param <- parse_vec(input$param_dom)
        v_N <- parse_vec(input$N_dom)
        v_M <- parse_vec(input$M_dom)
        if (length(v_param) != input$n_dominios) return("Debe ingresar un parámetro por cada DAM.")
        if (length(v_N) != input$n_dominios) return("Debe ingresar N por cada DAM.")
        if (length(v_M) != input$n_dominios) return("Debe ingresar M por cada DAM.")
        if (any(is.na(v_param)) || any(is.na(v_N)) || any(is.na(v_M)) || any(v_N <= 0) || any(v_M <= 0)) return("Revise parámetros, N y M por DAM (numéricos y > 0).")
        if (p$tipo == "Media") {
          v_sd <- parse_vec(input$sd_dom)
          if (length(v_sd) != input$n_dominios) return("Debe ingresar desviación estándar por cada DAM.")
          if (any(is.na(v_sd)) || any(v_sd <= 0)) return("Desviación estándar por DAM debe ser > 0.")
        } else if (any(v_param < 0 | v_param > 1)) {
          return("Parámetros proporción por DAM deben estar entre 0 y 1.")
        }
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
      cat("Representatividad DAM:", ifelse(identical(input$usa_dominios, "si"), "Sí", "No"), "\n")
      cat("Use filtros DAM/m para revisar tablas y elegir la combinación más útil.\n")
      cat("En el siguiente módulo seleccione el mejor m y continúe con área.\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        p <- parametro(); req(p)
        d <- diseno(); req(d)
        list(
          usa_dominios = identical(input$usa_dominios, "si"),
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          param_dom = if (identical(input$usa_dominios, "si")) parse_vec(input$param_dom) else numeric(0),
          sd_dom = if (identical(input$usa_dominios, "si") && p$tipo == "Media") parse_vec(input$sd_dom) else numeric(0),
          N_dom = if (identical(input$usa_dominios, "si")) parse_vec(input$N_dom) else numeric(0),
          M_dom = if (identical(input$usa_dominios, "si")) parse_vec(input$M_dom) else numeric(0),
          tabla_dam = tabla_dam_completa(),
          m_vector = d$m_vector
        )
      })
    )
  })
}
