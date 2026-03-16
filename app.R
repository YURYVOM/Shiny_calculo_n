library(shiny)
library(bslib)

# Cargar módulos
source("modulos/mod_1.R")
source("modulos/mod_2.R")
source("modulos/mod_3.R")
source("modulos/mod_4.R")
source("modulos/mod_5.R")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#eef4ff",
    fg = "#1f2d3d",
    primary = "#2057b3",
    secondary = "#8fb3f4",
    base_font = font_google("Nunito")
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #edf3ff 0%, #dce9ff 100%);
      }
      .contenedor-principal {
        max-width: 1020px;
        margin: 24px auto;
      }
      .tarjeta-paso {
        background: #ffffff;
        border-radius: 22px;
        padding: 28px;
        box-shadow: 0 10px 24px rgba(20, 58, 128, 0.12);
        border: 1px solid #d5e1fb;
      }
      .titulo-app {
        text-align: center;
        color: #0d3f97;
        font-weight: 900;
        margin-bottom: 4px;
      }
      .subtitulo-app {
        text-align: center;
        color: #375d9d;
        font-size: 1.05rem;
        font-weight: 700;
        margin-bottom: 22px;
      }
      .botones-nav {
        display: flex;
        justify-content: space-between;
        gap: 12px;
        margin-top: 20px;
      }
      .well {
        background: #f7faff;
        border: 1px solid #d6e3fd;
        border-radius: 16px;
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

        tabPanel(title = "Paso 1", value = "paso1", mod_parametro_ui("param")),
        tabPanel(title = "Paso 2", value = "paso2", mod_unidad_ui("unidad")),
        tabPanel(title = "Paso 3", value = "paso3", mod_precision_ui("precision")),
        tabPanel(title = "Paso 4", value = "paso4", mod_diseno_ui("diseno")),
        tabPanel(title = "Paso 5", value = "paso5", mod_resultados_ui("resultados"))
      ),

      div(
        class = "botones-nav",
        actionButton("anterior", "← Atrás", class = "btn btn-outline-primary"),
        actionButton("siguiente", "Siguiente →", class = "btn btn-primary")
      )
    )
  )
)

server <- function(input, output, session) {

  pasos <- c("paso1", "paso2", "paso3", "paso4", "paso5")
  paso_actual <- reactiveVal("paso1")

  mod1 <- mod_parametro_server("param")
  mod2 <- mod_unidad_server("unidad")

  parametro_valor <- reactive({
    p <- mod1$datos()
    req(p)
    p$valor
  })

  mod3 <- mod_precision_server("precision", parametro = parametro_valor)
  mod4 <- mod_diseno_server("diseno", unidad = mod2$datos)

  entrada_exportable <- reactive({
    p1 <- mod1$datos()
    p2 <- mod2$datos()
    d3 <- mod3
    d4 <- mod4$datos()
    req(p1, p2, d4)

    list(
      tipo_param = p1$tipo,
      xbarra = p1$xbarra,
      s = p1$s,
      p = p1$p,
      unidad = p2$unidad,
      r = p2$r,
      b = p2$b,
      amplitud = d3$amplitud(),
      conf = d3$conf(),
      N = d4$N,
      M = d4$M,
      m = d4$m,
      rho = d4$rho,
      c_h = d4$c_h,
      c_upm = d4$c_upm,
      dominios = d4$dominios,
      n_dominios = d4$n_dominios
    )
  })

  mod_resultados_server("resultados", entrada = entrada_exportable)

  output$titulo_paso <- renderText({
    switch(
      paso_actual(),
      "paso1" = "Paso 1 de 5: Seleccionar parámetro de interés",
      "paso2" = "Paso 2 de 5: Unidad de análisis",
      "paso3" = "Paso 3 de 5: Precisión (amplitud + confianza)",
      "paso4" = "Paso 4 de 5: Diseño y presupuesto",
      "paso5" = "Paso 5 de 5: Resultados y exportación de código R"
    )
  })

  es_paso_valido <- reactive({
    switch(
      paso_actual(),
      "paso1" = is.null(mod1$validacion()),
      "paso2" = is.null(mod2$validacion()),
      "paso3" = is.null(mod3$validacion()),
      "paso4" = is.null(mod4$validacion()),
      "paso5" = TRUE
    )
  })

  mensaje_error <- reactive({
    switch(
      paso_actual(),
      "paso1" = mod1$validacion(),
      "paso2" = mod2$validacion(),
      "paso3" = mod3$validacion(),
      "paso4" = mod4$validacion(),
      "paso5" = NULL
    )
  })

  observeEvent(input$siguiente, {
    idx <- match(paso_actual(), pasos)

    if (!isTRUE(es_paso_valido())) {
      showNotification(
        paste("Revise el", gsub("paso", "Paso ", paso_actual()), ":", mensaje_error()),
        type = "warning",
        duration = 4
      )
      return()
    }

    if (idx < length(pasos)) {
      siguiente <- pasos[idx + 1]
      paso_actual(siguiente)
      updateTabsetPanel(session, "wizard", selected = siguiente)
    } else {
      showNotification("Proceso completado. Ya puede exportar su script de R.", type = "message")
    }
  })

  observeEvent(input$anterior, {
    idx <- match(paso_actual(), pasos)
    if (idx > 1) {
      anterior <- pasos[idx - 1]
      paso_actual(anterior)
      updateTabsetPanel(session, "wizard", selected = anterior)
    }
  })
}

shinyApp(ui, server)
