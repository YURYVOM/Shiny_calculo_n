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
        condition = sprintf("input['%s'] == 'no'", ns("usa_dominios")),
        h4("Selección del parámetro m nacional"),
        p("Como no se solicitó representatividad por DAM, seleccione aquí el valor de m que desea usar en el cálculo nacional final."),
        uiOutput(ns("selector_m_nacional_ui")),
        DT::DTOutput(ns("tabla_m_nacional_seleccionado"))
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
        tags$hr(),
        h4("Selección del parámetro m por DAM"),
        p("Seleccione el valor de m que se utilizará en el cálculo final para cada DAM."),
        uiOutput(ns("selector_m_dam_ui")),
        fluidRow(
          column(6, selectInput(ns("dam_filtro"), "Filtro DAM para visualizar:", choices = character(0))),
          column(6, selectInput(ns("m_filtro"), "Filtro m para visualizar:", choices = character(0)))
        )
      ),
      h4("Resultados por DAM y por m"),
      DT::DTOutput(ns("tabla_dam")),
      h4("Resumen de selección final"),
      DT::DTOutput(ns("tabla_m_seleccionado_dam")),
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

    output$selector_m_nacional_ui <- renderUI({
      d <- diseno(); req(d)
      selectInput(
        session$ns("m_seleccionado_nacional"),
        "Seleccione el valor de m nacional:",
        choices = as.character(d$m_vector),
        selected = as.character(d$m_vector[1])
      )
    })

    tabla_m_nacional_seleccionado <- reactive({
      d <- diseno(); p <- parametro(); pr <- precision(); req(d, p, pr)
      req(identical(input$usa_dominios, "no"), !is.null(input$m_seleccionado_nacional))

      m_sel <- as.numeric(input$m_seleccionado_nacional)
      tamano_muestra <- if (p$tipo == "Media") {
        ss4HHSm(
          N = d$N,
          M = d$M,
          rho = d$rho,
          mu = p$xbarra,
          sigma = p$s,
          delta = pr$delta,
          conf = pr$conf,
          m = m_sel
        )
      } else {
        ss4HHSp(
          N = d$N,
          M = d$M,
          rho = d$rho,
          p = p$p,
          delta = pr$delta,
          conf = pr$conf,
          m = m_sel
        )
      }

      data.frame(
        HouseholdsPerPSU = m_sel,
        DEFF = round(1 + (m_sel - 1) * d$rho, 2),
        PSUinSample = ceiling(tamano_muestra / m_sel),
        HouseholdsInSample = tamano_muestra,
        stringsAsFactors = FALSE
      )
    })

    output$tabla_m_nacional_seleccionado <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))
      if (!identical(input$usa_dominios, "no")) return(NULL)
      DT::datatable(
        tabla_m_nacional_seleccionado(),
        options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          'DEFF',
          target = 'row',
          backgroundColor = DT::styleInterval(c(1, 3), c('#f8d7da', 'inherit', '#f8d7da'))
        )
    })

    valores_dominios <- reactive({
      p <- parametro(); req(p)
      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      parametro_objetivo_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("parametro_objetivo_dam_", i)), numeric(1))
      poblacion_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("poblacion_dam_", i)), numeric(1))
      upm_marco_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("upm_marco_dam_", i)), numeric(1))
      amplitud_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("amplitud_dam_", i)), numeric(1))
      rho_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("rho_dam_", i)), numeric(1))
      r_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("r_dam_", i)), numeric(1))
      b_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("b_dam_", i)), numeric(1))

      if (p$tipo == "Media") {
        desviacion_estandar_dam <- vapply(seq_len(n), function(i) valor_num_input(paste0("desviacion_estandar_dam_", i)), numeric(1))
      } else {
        desviacion_estandar_dam <- numeric(0)
      }

      list(
        parametro_objetivo_dam = parametro_objetivo_dam,
        desviacion_estandar_dam = desviacion_estandar_dam,
        poblacion_dam = poblacion_dam,
        upm_marco_dam = upm_marco_dam,
        amplitud_dam = amplitud_dam,
        delta_dam = amplitud_dam / 2,
        rho_dam = rho_dam,
        r_dam = r_dam,
        b_dam = b_dam
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
              session$ns(paste0("parametro_objetivo_dam_", i)),
              if (p$tipo == "Media") sprintf("DAM %s: media esperada", i) else sprintf("DAM %s: proporción esperada", i),
              value = NA,
              min = if (p$tipo == "Proporción") 0 else NA,
              max = if (p$tipo == "Proporción") 1 else NA
            )
          ),
          column(3, if (p$tipo == "Media") numericInput(session$ns(paste0("desviacion_estandar_dam_", i)), sprintf("DAM %s: desviación estándar", i), value = NA, min = 0) else tags$div()),
          column(2, numericInput(session$ns(paste0("poblacion_dam_", i)), sprintf("DAM %s: población (N)", i), value = NA, min = 1)),
          column(2, numericInput(session$ns(paste0("upm_marco_dam_", i)), sprintf("DAM %s: UPM en marco (M)", i), value = NA, min = 1)),
          column(2, numericInput(session$ns(paste0("amplitud_dam_", i)), sprintf("DAM %s: amplitud", i), value = NA, min = 0, step = 0.001)),
          column(2, numericInput(session$ns(paste0("rho_dam_", i)), sprintf("DAM %s: rho", i), value = NA, min = 0, max = 1, step = 0.01)),
          column(1, if (identical(input$unidad_dam, "Personas")) numericInput(session$ns(paste0("r_dam_", i)), sprintf("DAM %s: r", i), value = NA, min = 0, step = 0.01) else tags$div()),
          column(1, if (identical(input$unidad_dam, "Personas")) numericInput(session$ns(paste0("b_dam_", i)), sprintf("DAM %s: b", i), value = NA, min = 0, max = 1, step = 0.01) else tags$div())
        )
      })

      do.call(tagList, filas)
    })

    output$selector_m_dam_ui <- renderUI({
      d <- diseno(); req(d)
      n <- input$n_dominios
      req(identical(input$usa_dominios, "si"), !is.null(n), n >= 2)

      opciones_m <- as.character(d$m_vector)
      filas <- lapply(seq_len(n), function(i) {
        valor_actual <- input[[paste0("m_seleccionado_dam_", i)]]
        valor_seleccionado <- if (!is.null(valor_actual) && valor_actual %in% opciones_m) valor_actual else opciones_m[1]
        fluidRow(
          column(
            4,
            selectInput(
              session$ns(paste0("m_seleccionado_dam_", i)),
              sprintf("DAM %s: valor de m", i),
              choices = opciones_m,
              selected = valor_seleccionado
            )
          )
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
          tamano_muestra <- if (p$tipo == "Media") {
            ss4HHSm(N = v$poblacion_dam[j], M = v$upm_marco_dam[j], rho = v$rho_dam[j], mu = v$parametro_objetivo_dam[j], sigma = v$desviacion_estandar_dam[j], delta = v$delta_dam[j], conf = pr$conf, m = m_i)
          } else {
            ss4HHSp(N = v$poblacion_dam[j], M = v$upm_marco_dam[j], rho = v$rho_dam[j], p = v$parametro_objetivo_dam[j], delta = v$delta_dam[j], conf = pr$conf, m = m_i)
          }
          out[[k]] <- data.frame(
            DAM = j,
            HouseholdsPerPSU = m_i,
            DEFF = round(1 + (m_i - 1) * v$rho_dam[j], 2),
            PSUinSample = ceiling(tamano_muestra / m_i),
            HouseholdsInSample = tamano_muestra,
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
      do.call(rbind, out)
    })

    tabla_m_seleccionado_dam <- reactive({
      d <- diseno(); req(d)

      if (identical(input$usa_dominios, "no")) {
        return(tabla_m_nacional_seleccionado())
      }
      if (!identical(input$usa_dominios, "si")) return(data.frame())

      tabla_completa <- tabla_dam_completa()
      req(nrow(tabla_completa) > 0)

      do.call(rbind, lapply(seq_len(input$n_dominios), function(i) {
        m_seleccionado <- as.numeric(input[[paste0("m_seleccionado_dam_", i)]])
        tabla_completa[tabla_completa$DAM == i & tabla_completa$HouseholdsPerPSU == m_seleccionado, , drop = FALSE]
      }))
    })

    validacion <- reactive({
      p <- parametro(); d <- diseno(); req(p, d)

      if (identical(input$usa_dominios, "no")) {
        if (is.null(input$m_seleccionado_nacional) || !(input$m_seleccionado_nacional %in% as.character(d$m_vector))) {
          return("Seleccione un valor válido de m nacional.")
        }
        return(NULL)
      }

      if (!identical(input$usa_dominios, "si")) {
        return("Debe indicar si desea representatividad por DAM.")
      }

      if (is.na(input$n_dominios) || input$n_dominios < 2) return("Número de dominios DAM debe ser >= 2.")
      v <- valores_dominios()
      if (any(is.na(v$parametro_objetivo_dam)) || any(is.na(v$poblacion_dam)) || any(is.na(v$upm_marco_dam)) || any(v$poblacion_dam <= 0) || any(v$upm_marco_dam <= 0)) {
        return("Revise parámetro objetivo, población y UPM en marco por DAM (numéricos y > 0).")
      }
      if (any(is.na(v$amplitud_dam)) || any(v$amplitud_dam <= 0)) {
        return("La amplitud por DAM debe ser mayor que 0.")
      }
      if (any(is.na(v$rho_dam)) || any(v$rho_dam < 0 | v$rho_dam > 1)) {
        return("El rho por DAM debe estar entre 0 y 1.")
      }
      if (p$tipo == "Media") {
        if (any(is.na(v$desviacion_estandar_dam)) || any(v$desviacion_estandar_dam <= 0)) return("La desviación estándar por DAM debe ser > 0.")
      } else if (any(v$parametro_objetivo_dam < 0 | v$parametro_objetivo_dam > 1)) {
        return("Los parámetros de proporción por DAM deben estar entre 0 y 1.")
      }
      if (identical(input$unidad_dam, "Personas")) {
        if (any(is.na(v$r_dam)) || any(is.na(v$b_dam))) return("Debe ingresar r y b por cada DAM cuando la unidad es Personas.")
        if (any(v$r_dam <= 0)) return("Todos los valores de r DAM deben ser mayores que 0.")
        if (any(v$b_dam < 0 | v$b_dam > 1)) return("Todos los valores de b DAM deben estar entre 0 y 1.")
      }
      for (i in seq_len(input$n_dominios)) {
        valor_m <- input[[paste0("m_seleccionado_dam_", i)]]
        if (is.null(valor_m) || !(valor_m %in% as.character(d$m_vector))) {
          return(sprintf("Seleccione un valor válido de m para el DAM %s.", i))
        }
      }
      NULL
    })

    output$tabla_dam <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))
      if (!identical(input$usa_dominios, "si")) {
        return(DT::datatable(data.frame(Nota = "No se solicitó representatividad por DAM."), options = list(dom = 't'), rownames = FALSE))
      }
      tb <- tabla_dam_completa()
      if (!is.null(input$dam_filtro) && input$dam_filtro != "Todos") tb <- tb[tb$DAM == as.numeric(input$dam_filtro), , drop = FALSE]
      if (!is.null(input$m_filtro) && input$m_filtro != "Todos") tb <- tb[tb$HouseholdsPerPSU == as.numeric(input$m_filtro), , drop = FALSE]
      DT::datatable(
        tb,
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          'DEFF',
          target = 'row',
          backgroundColor = DT::styleInterval(c(1, 3), c('#f8d7da', 'inherit', '#f8d7da'))
        )
    })

    output$tabla_m_seleccionado_dam <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))
      DT::datatable(
        tabla_m_seleccionado_dam(),
        options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          'DEFF',
          target = 'row',
          backgroundColor = DT::styleInterval(c(1, 3), c('#f8d7da', 'inherit', '#f8d7da'))
        )
    })

    output$resumen <- renderPrint({
      validate(need(is.null(validacion()), validacion()))
      if (identical(input$usa_dominios, "si")) {
        cat("Representatividad DAM: Sí\n")
        cat("Se guardaron parámetro objetivo, población, UPM en marco, amplitud y rho por dominio DAM.\n")
        cat("Unidad DAM:", input$unidad_dam, "\n")
        v <- valores_dominios()
        cat("rho por DAM:", paste(v$rho_dam, collapse = ", "), "\n")
        cat("m seleccionado por DAM:", paste(vapply(seq_len(input$n_dominios), function(i) input[[paste0("m_seleccionado_dam_", i)]], character(1)), collapse = ", "), "\n")
        if (input$unidad_dam == "Personas") {
          cat("r por DAM:", paste(v$r_dam, collapse = ", "), "\n")
          cat("b por DAM:", paste(v$b_dam, collapse = ", "), "\n")
        }
      } else {
        cat("Representatividad DAM: No\n")
        cat("m nacional seleccionado:", input$m_seleccionado_nacional, "\n")
      }
      cat("Continúe al siguiente módulo para los resultados finales y la asignación IPFP.\n")
    })

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))
        d <- diseno(); p <- parametro(); req(d, p)

        if (identical(input$usa_dominios, "si")) {
          v <- valores_dominios()
          m_seleccionado_dam <- vapply(seq_len(input$n_dominios), function(i) as.numeric(input[[paste0("m_seleccionado_dam_", i)]]), numeric(1))
          m_seleccionado_nacional <- NA_real_
        } else {
          v <- list(
            parametro_objetivo_dam = numeric(0),
            desviacion_estandar_dam = numeric(0),
            poblacion_dam = numeric(0),
            upm_marco_dam = numeric(0),
            amplitud_dam = numeric(0),
            delta_dam = numeric(0),
            rho_dam = numeric(0),
            r_dam = numeric(0),
            b_dam = numeric(0)
          )
          m_seleccionado_dam <- numeric(0)
          m_seleccionado_nacional <- as.numeric(input$m_seleccionado_nacional)
        }

        list(
          usa_dominios = identical(input$usa_dominios, "si"),
          n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1,
          parametro_objetivo_dam = v$parametro_objetivo_dam,
          desviacion_estandar_dam = if (identical(input$usa_dominios, "si") && p$tipo == "Media") v$desviacion_estandar_dam else numeric(0),
          poblacion_dam = v$poblacion_dam,
          upm_marco_dam = v$upm_marco_dam,
          amplitud_dam = v$amplitud_dam,
          delta_dam = v$delta_dam,
          rho_dam = v$rho_dam,
          tabla_dam = tabla_dam_completa(),
          tabla_m_seleccionado_dam = tabla_m_seleccionado_dam(),
          m_vector = d$m_vector,
          m_seleccionado_nacional = m_seleccionado_nacional,
          m_seleccionado_dam = m_seleccionado_dam,
          unidad_dam = if (identical(input$usa_dominios, "si")) input$unidad_dam else "Hogares",
          r_dam = if (identical(input$usa_dominios, "si") && input$unidad_dam == "Personas") v$r_dam else rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1),
          b_dam = if (identical(input$usa_dominios, "si") && input$unidad_dam == "Personas") v$b_dam else rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1)
        )
      })
    )
  })
}
