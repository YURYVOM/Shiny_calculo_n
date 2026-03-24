# =========================================================
# Módulo 5: Cálculo de tamaño de muestra nacional
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("Módulo 5. Tamaño de muestra nacional"),
      p("En este módulo se calcula el tamaño de muestra nacional para distintos valores del parámetro de muestreo m. El parámetro m corresponde al submuestreo, es decir, al número de unidades seleccionadas dentro de cada UPM."),
      h4("Resultados nacionales según el valor de m"),
      shiny::downloadButton(
        outputId = ns("descargar_muestra_nacional"),
        label = "Descargar tabla nacional en Excel",
        class = "btn btn-success"
      ),
      tags$br(),
      tags$br(),
      DT::DTOutput(ns("tabla_muestreo")),
      tags$br(),
      p("La selección final de m nacional se realizará en el módulo siguiente únicamente si no se solicita representatividad por DAM.")
    )
  )
}

mod_presupuesto_server <- function(id, parametro, precision, unidad, diseno) {
  shiny::moduleServer(id, function(input, output, session) {

    tabla_funcion <- shiny::reactive({
      p  <- parametro()
      pr <- precision()
      u  <- unidad()
      d  <- diseno()
      shiny::req(p, pr, u, d)

      out <- do.call(rbind, lapply(d$m_vector, function(m_i) {
        tamano_muestra <- if (p$tipo == "Media") {
          ss4HHSm(
            N = d$N,
            M = d$M,
            rho = d$rho,
            mu = p$xbarra,
            sigma = p$s,
            delta = pr$delta,
            conf = pr$conf,
            m = m_i
          )
        } else {
          ss4HHSp(
            N = d$N,
            M = d$M,
            rho = d$rho,
            p = p$p,
            delta = pr$delta,
            conf = pr$conf,
            m = m_i
          )
        }

        persons_per_household <- if (u$unidad == "Personas") {
          u$r * u$b
        } else {
          1
        }

        persons_per_psu <- if (u$unidad == "Personas") {
          m_i * u$r * u$b
        } else {
          m_i
        }

        deff_i <- 1 + (persons_per_psu - 1) * d$rho

        data.frame(
          HouseholdsPerPSU   = m_i,
          PersonsPerPSU      = round(persons_per_psu, 0),
          DEFF               = round(deff_i, 1),
          PSUinSample        = ceiling(tamano_muestra / m_i),
          HouseholdsInSample = ceiling(tamano_muestra),
          PersonsInSample    = ceiling(tamano_muestra * persons_per_household),
          stringsAsFactors = FALSE
        )
      }))

      as.data.frame(out)
    })

    validacion <- shiny::reactive({
      tb <- tabla_funcion()
      if (is.null(tb) || nrow(tb) == 0) return("No hay resultados nacionales para mostrar.")
      NULL
    })

    output$tabla_muestreo <- DT::renderDT({
      shiny::validate(shiny::need(is.null(validacion()), validacion()))
      DT::datatable(
        tabla_funcion(),
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          'DEFF',
          target = 'row',
          backgroundColor = DT::styleInterval(c(1, 3), c('#f8d7da', 'inherit', '#f8d7da'))
        )
    })

    output$descargar_muestra_nacional <- shiny::downloadHandler(
      filename = function() {
        paste0("muestra_nacional_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          stop("Se requiere el paquete 'openxlsx' para exportar a Excel (.xlsx).")
        }

        openxlsx::write.xlsx(tabla_funcion(), file = file, overwrite = TRUE)
      }
    )

    list(
      validacion = validacion,
      datos = shiny::reactive({
        shiny::validate(shiny::need(is.null(validacion()), validacion()))
        list(
          tabla_muestreo = tabla_funcion()
        )
      })
    )
  })
}
