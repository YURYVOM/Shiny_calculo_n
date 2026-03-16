library(shiny)
library(bslib)

# Cargar módulos
source("modulos/mod_1.R")
source("modulos/mod_2.R")
source("modulos/mod_3.R")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#eaf2ff",
    fg = "#1f2d3d",
    primary = "#1565c0",
    base_font = font_google("Nunito")
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: #dfeaf8;
      }
      .contenedor-principal {
        max-width: 900px;
        margin: 25px auto;
      }
      .tarjeta-paso {
        background: white;
        border-radius: 20px;
        padding: 30px;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
      }
      .titulo-app {
        text-align: center;
        color: #0d47a1;
        font-weight: 800;
        margin-bottom: 10px;
      }
      .subtitulo-app {
        text-align: center;
        color: #0d47a1;
        font-weight: 700;
        margin-bottom: 25px;
      }
      .botones-nav {
        display: flex;
        justify-content: space-between;
        margin-top: 20px;
      }
    "))
  ),

  div(
    class = "contenedor-principal",
    h1("Calculadora de tamaño de muestra", class = "titulo-app"),
    div(class = "subtitulo-app", textOutput("titulo_paso")),

    div(
      class = "tarjeta-paso",

      tabsetPanel(
        id = "wizard",
        selected = "paso1",
        type = "hidden",

        tabPanel(
          title = "Paso 1",
          value = "paso1",
          mod_parametro_ui("param")
        ),

        tabPanel(
          title = "Paso 2",
          value = "paso2",
          mod_unidad_ui("unidad")
        ),

        tabPanel(
          title = "Paso 3",
          value = "paso3",
          mod_precision_ui("precision")
        )
      ),

      div(
        class = "botones-nav",
        actionButton("anterior", "← Atrás"),
        actionButton("siguiente", "Siguiente →", class = "btn btn-primary")
      )
    )
  )
)

server <- function(input, output, session) {

  mod1 <- mod_parametro_server("param")
  mod2 <- mod_unidad_server("unidad")

  mod3 <- mod_precision_server(
    "precision",
    parametro = reactive({
      tipo <- input[["param-tipo_param"]]

      if (is.null(tipo)) return(NULL)

      if (tipo == "Media") {
        input[["param-xbarra"]]
      } else if (tipo == "Proporción") {
        input[["param-p"]]
      } else {
        NULL
      }
    })
  )

  paso_actual <- reactiveVal("paso1")

  output$titulo_paso <- renderText({
    if (paso_actual() == "paso1") {
      "Paso 1 de 3: Parámetro de interés"
    } else if (paso_actual() == "paso2") {
      "Paso 2 de 3: Unidad de análisis"
    } else {
      "Paso 3 de 3: Definir precisión"
    }
  })

  # -----------------------------
  # Validación explícita paso 1
  # -----------------------------
  paso1_valido <- reactive({
    tipo <- input[["param-tipo_param"]]

    if (is.null(tipo)) return(FALSE)

    if (tipo == "Media") {
      xbarra <- input[["param-xbarra"]]
      s      <- input[["param-s"]]

      return(
        !is.null(xbarra) && !is.na(xbarra) &&
          !is.null(s) && !is.na(s) &&
          s >= 0
      )
    }

    if (tipo == "Proporción") {
      p <- input[["param-p"]]

      return(
        !is.null(p) && !is.na(p) &&
          p >= 0 && p <= 1
      )
    }

    FALSE
  })

  # -----------------------------
  # Validación explícita paso 2
  # -----------------------------
  paso2_valido <- reactive({
    unidad <- input[["unidad-unidad"]]
    r      <- input[["unidad-r"]]
    b      <- input[["unidad-b"]]

    !is.null(unidad) && unidad != "" &&
      !is.null(r) && !is.na(r) && r > 0 &&
      !is.null(b) && !is.na(b) && b >= 0 && b <= 1
  })

  # -----------------------------
  # Validación explícita paso 3
  # -----------------------------
  paso3_valido <- reactive({
    amplitud <- input[["precision-amplitud"]]
    conf     <- input[["precision-conf"]]

    !is.null(amplitud) && !is.na(amplitud) &&
      amplitud > 0 &&
      !is.null(conf) && !is.na(conf)
  })

  # -----------------------------
  # Botón siguiente
  # -----------------------------
  observeEvent(input$siguiente, {

    if (paso_actual() == "paso1") {

      if (!isTRUE(paso1_valido())) {
        showNotification(
          "Complete correctamente el paso 1 para continuar.",
          type = "warning",
          duration = 3
        )
        return()
      }

      paso_actual("paso2")
      updateTabsetPanel(session, inputId = "wizard", selected = "paso2")
      return()
    }

    if (paso_actual() == "paso2") {

      if (!isTRUE(paso2_valido())) {
        showNotification(
          "Complete correctamente el paso 2 para continuar.",
          type = "warning",
          duration = 3
        )
        return()
      }

      paso_actual("paso3")
      updateTabsetPanel(session, inputId = "wizard", selected = "paso3")
      return()
    }

    if (paso_actual() == "paso3") {

      if (!isTRUE(paso3_valido())) {
        showNotification(
          "Complete correctamente el paso 3.",
          type = "warning",
          duration = 3
        )
        return()
      }

      showNotification(
        "Paso 3 completado correctamente.",
        type = "message",
        duration = 3
      )
    }
  })

  # -----------------------------
  # Botón anterior
  # -----------------------------
  observeEvent(input$anterior, {

    if (paso_actual() == "paso3") {
      paso_actual("paso2")
      updateTabsetPanel(session, inputId = "wizard", selected = "paso2")
      return()
    }

    if (paso_actual() == "paso2") {
      paso_actual("paso1")
      updateTabsetPanel(session, inputId = "wizard", selected = "paso1")
      return()
    }
  })
}

shinyApp(ui, server)
