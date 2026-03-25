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
        fluidRow(
          column(6, selectInput(ns("dam_filtro"), "Filtro DAM para visualizar:", choices = character(0))),
          column(6, selectInput(ns("m_filtro"), "Filtro m para visualizar:", choices = character(0)))
        )
      ),
      h4("Resultados por DAM y por m"),
      DT::DTOutput(ns("tabla_dam")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        downloadButton(ns("descargar_tabla_dam"), "Descargar tabla DAM (CSV)", class = "btn btn-primary")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'si'", ns("usa_dominios")),
        h4("Selección final del parámetro m para todos los DAM"),
        p("Después de revisar los resultados, seleccione un único valor de m que se aplicará a todos los DAM."),
        uiOutput(ns("selector_m_dam_ui"))
      ),
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
      d <- diseno()
      req(d)

      selectInput(
        session$ns("m_seleccionado_nacional"),
        "Seleccione el valor de m nacional:",
        choices = as.character(d$m_vector),
        selected = as.character(d$m_vector[1])
      )
    })

    tabla_m_nacional_seleccionado <- reactive({
      d <- diseno()
      p <- parametro()
      pr <- precision()
      u <- unidad()
      req(d, p, pr, u)
      req(identical(input$usa_dominios, "no"), !is.null(input$m_seleccionado_nacional))

      m_sel <- as.numeric(input$m_seleccionado_nacional)

      if (identical(u$unidad, "Personas")) {
        res <- if (p$tipo == "Media") {
          ss4HHSm(
            N = d$N,
            M = d$M,
            rho = d$rho,
            r = u$r,
            b = u$b,
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
            r = u$r,
            b = u$b,
            P = p$p,
            delta = pr$delta,
            conf = pr$conf,
            m = m_sel
          )
        }
      } else {
        res <- if (p$tipo == "Media") {
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
            r = 1,
            b = 1,
            rho = d$rho,
            P = p$p,
            delta = pr$delta,
            conf = pr$conf,
            m = m_sel
          )
        }
      }

      as.data.frame(res)
    })

    output$tabla_m_nacional_seleccionado <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))
      if (!identical(input$usa_dominios, "no")) return(NULL)

      DT::datatable(
        tabla_m_nacional_seleccionado(),
        options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "DEFF",
          target = "row",
          backgroundColor = DT::styleInterval(c(1, 3), c("#f8d7da", "inherit", "#f8d7da"))
        )
    })

    valores_dominios <- reactive({
      p <- parametro()
      req(p)

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
      p <- parametro()
      req(p)

      n <- input$n_dominios
      req(!is.null(n), n >= 2)

      filas <- lapply(seq_len(n), function(i) {
        tags$div(
          style = "border: 1px solid #d9dee8; border-radius: 10px; padding: 12px 14px; margin-bottom: 12px; background-color: #f8fafc;",
          tags$h4(sprintf("DAM %s", i), style = "margin-top: 0; margin-bottom: 10px; font-size: 20px;"),
          fluidRow(
            column(
              4,
              numericInput(
                session$ns(paste0("parametro_objetivo_dam_", i)),
                if (p$tipo == "Media") "Media esperada" else "Proporción esperada",
                value = NA,
                min = if (p$tipo == "Proporción") 0 else NA,
                max = if (p$tipo == "Proporción") 1 else NA
              )
            ),
            column(
              4,
              if (p$tipo == "Media") {
                numericInput(
                  session$ns(paste0("desviacion_estandar_dam_", i)),
                  "Desviación estándar",
                  value = NA,
                  min = 0
                )
              } else {
                tags$div()
              }
            ),
            column(4, numericInput(session$ns(paste0("poblacion_dam_", i)), "Población (N)", value = NA, min = 1))
          ),
          fluidRow(
            column(3, numericInput(session$ns(paste0("upm_marco_dam_", i)), "UPM en marco (M)", value = NA, min = 1)),
            column(3, numericInput(session$ns(paste0("amplitud_dam_", i)), "Amplitud", value = NA, min = 0, step = 0.001)),
            column(3, numericInput(session$ns(paste0("rho_dam_", i)), "Rho", value = NA, min = 0, max = 1, step = 0.01)),
            column(
              1,
              if (identical(input$unidad_dam, "Personas")) {
                numericInput(session$ns(paste0("r_dam_", i)), "r", value = NA, min = 0, step = 0.01)
              } else {
                tags$div()
              }
            ),
            column(
              2,
              if (identical(input$unidad_dam, "Personas")) {
                numericInput(session$ns(paste0("b_dam_", i)), "b", value = NA, min = 0, step = 0.01)
              } else {
                tags$div()
              }
            )
          )
        )
      })

      do.call(tagList, filas)
    })

    output$selector_m_dam_ui <- renderUI({
      d <- diseno()
      req(d)

      req(identical(input$usa_dominios, "si"))

      opciones_m <- as.character(d$m_vector)
      valor_actual <- input$m_seleccionado_dam_global
      valor_seleccionado <- if (!is.null(valor_actual) && valor_actual %in% opciones_m) valor_actual else opciones_m[1]

      selectInput(
        session$ns("m_seleccionado_dam_global"),
        "Valor de m para todos los DAM:",
        choices = opciones_m,
        selected = valor_seleccionado
      )
    })

    observe({
      d <- diseno()
      req(d)

      n_dom <- suppressWarnings(as.integer(input$n_dominios))
      tiene_dominios <- identical(input$usa_dominios, "si") && !is.na(n_dom) && n_dom >= 2

      dam_choices <- if (isTRUE(tiene_dominios)) {
        c("Todos", as.character(seq_len(n_dom)))
      } else {
        c("Todos")
      }

      dam_sel <- if (!is.null(input$dam_filtro) && input$dam_filtro %in% dam_choices) input$dam_filtro else "Todos"
      updateSelectInput(session, "dam_filtro", choices = dam_choices, selected = dam_sel)

      m_choices <- as.character(d$m_vector)
      m_filter_choices <- c("Todos", m_choices)
      m_filter_sel <- if (!is.null(input$m_filtro) && input$m_filtro %in% m_filter_choices) input$m_filtro else "Todos"
      updateSelectInput(session, "m_filtro", choices = m_filter_choices, selected = m_filter_sel)
    })

    tabla_dam_completa <- reactive({
      p <- parametro()
      pr <- precision()
      d <- diseno()
      req(p, pr, d)

      if (!identical(input$usa_dominios, "si")) {
        return(data.frame())
      }

      v <- valores_dominios()

      out <- do.call(rbind, lapply(seq_len(input$n_dominios), function(j) {
        do.call(rbind, lapply(d$m_vector, function(m_i) {

          res <- if (identical(input$unidad_dam, "Personas")) {
            if (p$tipo == "Media") {
              ss4HHSm(
                N = v$poblacion_dam[j],
                M = v$upm_marco_dam[j],
                rho = v$rho_dam[j],
                r = v$r_dam[j],
                b = v$b_dam[j],
                mu = v$parametro_objetivo_dam[j],
                sigma = v$desviacion_estandar_dam[j],
                delta = v$delta_dam[j],
                conf = pr$conf,
                m = m_i
              )
            } else {
              ss4HHSp(
                N = v$poblacion_dam[j],
                M = v$upm_marco_dam[j],
                rho = v$rho_dam[j],
                r = v$r_dam[j],
                b = v$b_dam[j],
                P = v$parametro_objetivo_dam[j],
                delta = v$delta_dam[j],
                conf = pr$conf,
                m = m_i
              )
            }
          } else {
            if (p$tipo == "Media") {
              ss4HHSm(
                N = v$poblacion_dam[j],
                M = v$upm_marco_dam[j],
                rho = v$rho_dam[j],
                mu = v$parametro_objetivo_dam[j],
                sigma = v$desviacion_estandar_dam[j],
                delta = v$delta_dam[j],
                conf = pr$conf,
                m = m_i
              )
            } else {
              ss4HHSp(
                N = v$poblacion_dam[j],
                M = v$upm_marco_dam[j],
                r = 1,
                b = 1,
                rho = v$rho_dam[j],
                P = v$parametro_objetivo_dam[j],
                delta = v$delta_dam[j],
                conf = pr$conf,
                m = m_i
              )
            }
          }

          res <- as.data.frame(res)
          res$DAM <- j
          res
        }))
      }))

      rownames(out) <- NULL
      out
    })

    tabla_m_seleccionado_dam <- reactive({
      d <- diseno()
      req(d)

      if (identical(input$usa_dominios, "no")) {
        return(tabla_m_nacional_seleccionado())
      }

      if (!identical(input$usa_dominios, "si")) return(data.frame())

      tabla_completa <- tabla_dam_completa()
      req(nrow(tabla_completa) > 0)
      m_seleccionado_global <- as.numeric(input$m_seleccionado_dam_global)

      do.call(rbind, lapply(seq_len(input$n_dominios), function(i) {
        tabla_completa[
          tabla_completa$DAM == i & tabla_completa$HouseholdsPerPSU == m_seleccionado_global,
          ,
          drop = FALSE
        ]
      }))
    })

    validacion <- reactive({
      p <- parametro()
      d <- diseno()
      u <- unidad()
      req(p, d, u)

      if (identical(input$usa_dominios, "no")) {
        if (is.null(input$m_seleccionado_nacional) || !(input$m_seleccionado_nacional %in% as.character(d$m_vector))) {
          return("Seleccione un valor válido de m nacional.")
        }

        if (identical(u$unidad, "Personas")) {
          if (is.null(u$r) || is.na(u$r) || u$r <= 0) return("El valor de r debe ser mayor que 0.")
          if (is.null(u$b) || is.na(u$b) || u$b < 0) return("El valor de b debe ser mayor o igual que 0.")
        }

        return(NULL)
      }

      if (!identical(input$usa_dominios, "si")) {
        return("Debe indicar si desea representatividad por DAM.")
      }

      if (is.na(input$n_dominios) || input$n_dominios < 2) {
        return("Número de dominios DAM debe ser >= 2.")
      }

      v <- valores_dominios()

      if (any(is.na(v$parametro_objetivo_dam)) ||
          any(is.na(v$poblacion_dam)) ||
          any(is.na(v$upm_marco_dam)) ||
          any(v$poblacion_dam <= 0) ||
          any(v$upm_marco_dam <= 0)) {
        return("Revise parámetro objetivo, población y UPM en marco por DAM (numéricos y > 0).")
      }

      if (any(is.na(v$amplitud_dam)) || any(v$amplitud_dam <= 0)) {
        return("La amplitud por DAM debe ser mayor que 0.")
      }

      if (any(is.na(v$rho_dam)) || any(v$rho_dam < 0 | v$rho_dam > 1)) {
        return("El rho por DAM debe estar entre 0 y 1.")
      }

      if (p$tipo == "Media") {
        if (any(is.na(v$desviacion_estandar_dam)) || any(v$desviacion_estandar_dam <= 0)) {
          return("La desviación estándar por DAM debe ser > 0.")
        }
      } else {
        if (any(v$parametro_objetivo_dam < 0 | v$parametro_objetivo_dam > 1)) {
          return("Los parámetros de proporción por DAM deben estar entre 0 y 1.")
        }
      }

      if (identical(input$unidad_dam, "Personas")) {
        if (any(is.na(v$r_dam)) || any(is.na(v$b_dam))) {
          return("Debe ingresar r y b por cada DAM cuando la unidad es Personas.")
        }
        if (any(v$r_dam <= 0)) {
          return("Todos los valores de r DAM deben ser mayores que 0.")
        }
        if (any(v$b_dam < 0)) {
          return("Todos los valores de b DAM deben ser mayores o iguales que 0.")
        }
      }

      if (is.null(input$m_seleccionado_dam_global) || !(input$m_seleccionado_dam_global %in% as.character(d$m_vector))) {
        return("Seleccione un valor válido de m para aplicar a todos los DAM.")
      }

      NULL
    })

    output$tabla_dam <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))

      if (!identical(input$usa_dominios, "si")) {
        return(
          DT::datatable(
            data.frame(Nota = "No se solicitó representatividad por DAM."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      tb <- tabla_dam_completa()

      if (!is.null(input$dam_filtro) && input$dam_filtro != "Todos") {
        tb <- tb[tb$DAM == as.numeric(input$dam_filtro), , drop = FALSE]
      }

      if (!is.null(input$m_filtro) && input$m_filtro != "Todos") {
        tb <- tb[tb$HouseholdsPerPSU == as.numeric(input$m_filtro), , drop = FALSE]
      }

      DT::datatable(
        tb,
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "DEFF",
          target = "row",
          backgroundColor = DT::styleInterval(c(1, 3), c("#f8d7da", "inherit", "#f8d7da"))
        )
    })

    output$tabla_m_seleccionado_dam <- DT::renderDT({
      validate(need(is.null(validacion()), validacion()))

      DT::datatable(
        tabla_m_seleccionado_dam(),
        options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "DEFF",
          target = "row",
          backgroundColor = DT::styleInterval(c(1, 3), c("#f8d7da", "inherit", "#f8d7da"))
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
        cat(
          "m seleccionado para todos los DAM:",
          input$m_seleccionado_dam_global,
          "\n"
        )

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

    output$descargar_tabla_dam <- downloadHandler(
      filename = function() paste0("tabla_resultados_dam_", Sys.Date(), ".csv"),
      content = function(file) {
        tabla <- tabla_dam_completa()
        if (nrow(tabla) == 0) {
          tabla <- data.frame(Nota = "No se solicitó representatividad por DAM.")
        }
        utils::write.csv(tabla, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    list(
      validacion = validacion,
      datos = reactive({
        validate(need(is.null(validacion()), validacion()))

        d <- diseno()
        p <- parametro()
        req(d, p)

        if (identical(input$usa_dominios, "si")) {
          v <- valores_dominios()
          m_seleccionado_dam <- rep(as.numeric(input$m_seleccionado_dam_global), input$n_dominios)
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
          r_dam = if (identical(input$usa_dominios, "si") && input$unidad_dam == "Personas") {
            v$r_dam
          } else {
            rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1)
          },
          b_dam = if (identical(input$usa_dominios, "si") && input$unidad_dam == "Personas") {
            v$b_dam
          } else {
            rep(1, if (identical(input$usa_dominios, "si")) input$n_dominios else 1)
          }
        )
      })
    )
  })
}
