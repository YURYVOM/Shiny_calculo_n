# =========================================================
# Módulo 5: Cálculo de tamaño de muestra
# =========================================================

mod_presupuesto_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      h3("Módulo 5. Tamaño de muestra"),
      p("En este módulo se calcula el tamaño de muestra nacional para diferentes valores de m. El parámetro m corresponde al tamaño de submuestreo, es decir, al número de unidades seleccionadas dentro de cada UPM."),
      h4("Tamaños de muestra nacional según valores de m"),
      shiny::downloadButton(
        outputId = ns("descargar_muestra_nacional"),
        label = "Descargar dataset en Excel",
        class = "btn btn-success"
      ),
      tags$br(),
      tags$br(),
      DT::DTOutput(ns("tabla_muestreo"))
    )
  )
}

mod_presupuesto_server <- function(id, parametro, precision, unidad, diseno) {
  shiny::moduleServer(id, function(input, output, session) {

    tabla_funcion <- shiny::reactive({
      p  <- parametro()
      pr <- precision()
      d  <- diseno()
      shiny::req(p, pr, d)

      out <- do.call(rbind, lapply(d$m_vector, function(m_i) {
        n_hogares <- if (p$tipo == "Media") {
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

        data.frame(
          HouseholdsPerPSU = m_i,
          DEFF = round(1 + (m_i - 1) * d$rho, 2),
          PSUinSample = ceiling(n_hogares / m_i),
          HouseholdsInSample = n_hogares,
          stringsAsFactors = FALSE
        )
      }))

      as.data.frame(out)
    })

    tabla_muestreo <- shiny::reactive({
      p  <- parametro()
      pr <- precision()
      u  <- unidad()
      d  <- diseno()
      shiny::req(p, pr, u, d)

      tb <- tabla_funcion()

    })

    validacion <- shiny::reactive({
      tb <- tabla_funcion()
      if (is.null(tb) || nrow(tb) == 0) return("No hay resultados para mostrar.")
      NULL
    })

    output$tabla_muestreo <- DT::renderDT({
      shiny::validate(shiny::need(is.null(validacion()), validacion()))
      tabla_muestreo()
    }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))

    output$descargar_muestra_nacional <- shiny::downloadHandler(
      filename = function() {
        paste0("muestra_nacional_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          stop("Se requiere el paquete 'openxlsx' para exportar a Excel (.xlsx).")
        }

        openxlsx::write.xlsx(tabla_muestreo(), file = file, overwrite = TRUE)
      }
    )

    list(
      validacion = validacion,
      datos = shiny::reactive({
        shiny::validate(shiny::need(is.null(validacion()), validacion()))
        list(
          tabla_muestreo = tabla_muestreo()
        )
      })
    )
  })
}
