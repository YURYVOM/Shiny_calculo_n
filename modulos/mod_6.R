# =========================================================
# MÓDULO 5: Cálculo de tamaño de muestra (nacional) con vector de LM
# =========================================================

mod_dominios_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    wellPanel(
      h3("Módulo 5. Cálculo de tamaño de muestra"),
      p("Cálculo nacional con funciones: Media con ss4HHSm y Proporción con ss4HHSp."),
      h4("Resultados para vector de LM/m"),
      tableOutput(ns("tabla_nacional")),
      verbatimTextOutput(ns("resumen"))
    )
  )
}

mod_dominios_server <- function(id, parametro, precision, unidad, diseno) {
  moduleServer(id, function(input, output, session) {

    tabla_nacional <- reactive({
      p <- parametro(); pr <- precision(); d <- diseno()
      req(p, pr, d)

      do.call(rbind, lapply(d$m_vector, function(m_i) {
        n_hogares <- if (p$tipo == "Media") {
          ss4HHSm(N = d$N, M = d$M, rho = d$rho, mu = p$xbarra, sigma = p$s, delta = pr$delta, conf = pr$conf, m = m_i)
        } else {
          ss4HHSp(N = d$N, M = d$M, rho = d$rho, p = p$p, delta = pr$delta, conf = pr$conf, m = m_i)
        }

        data.frame(
          HouseholdsPerPSU = m_i,
          DEFF = round(1 + (m_i - 1) * d$rho, 4),
          PSUinSample = ceiling(n_hogares / m_i),
          HouseholdsInSample = n_hogares
        )
      }))
    })

    output$tabla_nacional <- renderTable({
      tabla_nacional()
    }, striped = TRUE, bordered = TRUE)

    output$resumen <- renderPrint({
      cat("Se calcularon resultados nacionales para todos los valores de m (sin seleccionar uno en este módulo).\n")
      cat("La tabla mantiene los nombres de salida: HouseholdsPerPSU, DEFF, PSUinSample, HouseholdsInSample.\n")
    })

    list(
      validacion = reactive(NULL),
      datos = reactive({
        d <- diseno(); req(d)
        list(
          tabla_nacional = tabla_nacional(),
          LM_vector = d$m_vector
        )
      })
    )
  })
}
