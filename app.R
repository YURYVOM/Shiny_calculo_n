library(shiny)
library(bslib)

# Cargar módulos
source("modulos/mod_1.R")
source("modulos/mod_2.R")
source("modulos/mod_3.R")
source("modulos/mod_4.R")
source("modulos/mod_5.R")
source("modulos/mod_6.R")
source("modulos/mod_7.R")
source("modulos/mod_utils.R")
source("modulos/mod_8.R")

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
      body { background: linear-gradient(180deg, #edf3ff 0%, #dce9ff 100%); }
      .contenedor-principal { max-width: 1060px; margin: 24px auto; }
      .tarjeta-paso { background: #fff; border-radius: 22px; padding: 28px; box-shadow: 0 10px 24px rgba(20,58,128,.12); border: 1px solid #d5e1fb; }
      .titulo-app { text-align:center; color:#0d3f97; font-weight:900; margin-bottom:4px; }
      .subtitulo-app { text-align:center; color:#375d9d; font-size:1.05rem; font-weight:700; margin-bottom:22px; }
      .botones-nav { display:flex; justify-content:space-between; gap:12px; margin-top:20px; }
      .well { background:#f7faff; border:1px solid #d6e3fd; border-radius:16px; }
    "))
  ),
  div(
    class = "contenedor-principal",
    h1("Calculadora de tamaño de muestra", class = "titulo-app"),
    div(class = "subtitulo-app", textOutput("titulo_paso")),
    div(
      class = "tarjeta-paso",
      tabsetPanel(
        id = "wizard", selected = "paso1", type = "hidden",
        tabPanel(title = "Paso 1", value = "paso1", mod_parametro_ui("param")),
        tabPanel(title = "Paso 2", value = "paso2", mod_unidad_ui("unidad")),
        tabPanel(title = "Paso 3", value = "paso3", mod_precision_ui("precision")),
        tabPanel(title = "Paso 4", value = "paso4", mod_diseno_ui("diseno")),
        tabPanel(title = "Paso 5", value = "paso5", mod_presupuesto_ui("presupuesto")),
        tabPanel(title = "Paso 6", value = "paso6", mod_dominios_ui("dominios")),
        tabPanel(title = "Paso 7", value = "paso7", mod_asignacion_ui("asignacion")),
        tabPanel(title = "Paso 8", value = "paso8", mod_resultados_ui("resultados"))
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
  pasos <- paste0("paso", 1:8)
  paso_actual <- reactiveVal("paso1")

  mod1 <- mod_parametro_server("param")
  mod2 <- mod_unidad_server("unidad")
  parametro_valor <- reactive({ p <- mod1$datos(); req(p); p$valor })
  mod3 <- mod_precision_server("precision", parametro = parametro_valor)
  mod4 <- mod_diseno_server("diseno", unidad = mod2$datos)
  mod5 <- mod_presupuesto_server("presupuesto")
  mod6 <- mod_dominios_server("dominios")
  n_dominios_rx <- reactive({ d <- mod6$datos(); req(d); d$n_dominios })
  mod7 <- mod_asignacion_server("asignacion", n_dominios_rx = n_dominios_rx)

  entrada_exportable <- reactive({
    p1 <- mod1$datos(); p2 <- mod2$datos(); d4 <- mod4$datos(); b5 <- mod5$datos(); d6 <- mod6$datos(); a7 <- mod7$datos()
    req(p1, p2, d4, b5, d6, a7)

    list(
      tipo_param = p1$tipo,
      xbarra = p1$xbarra,
      s = p1$s,
      p = p1$p,
      unidad = p2$unidad,
      r = p2$r,
      b = p2$b,
      amplitud = mod3$amplitud(),
      conf = mod3$conf(),
      N = d4$N,
      M = d4$M,
      m = d4$m,
      rho = d4$rho,
      c_h = b5$c_h,
      c_upm = b5$c_upm,
      usa_dominios = d6$usa_dominios,
      n_dominios = d6$n_dominios,
      sim = d6$sim,
      usa_area = a7$usa_area,
      n_areas = a7$n_areas,
      tabla_prop = a7$tabla_prop
    )
  })

  mod_resultados_server("resultados", entrada = entrada_exportable)

  output$titulo_paso <- renderText({
    switch(
      paso_actual(),
      "paso1" = "Paso 1 de 8: Seleccionar parámetro de interés",
      "paso2" = "Paso 2 de 8: Unidad de análisis",
      "paso3" = "Paso 3 de 8: Precisión (amplitud + confianza)",
      "paso4" = "Paso 4 de 8: Parámetros de diseño",
      "paso5" = "Paso 5 de 8: Parámetros de presupuesto",
      "paso6" = "Paso 6 de 8: Dominios y SIM",
      "paso7" = "Paso 7 de 8: Asignación por área (IPFP)",
      "paso8" = "Paso 8 de 8: Tablas de muestreo y exportación"
    )
  })

  es_paso_valido <- reactive({
    switch(
      paso_actual(),
      "paso1" = is.null(mod1$validacion()),
      "paso2" = is.null(mod2$validacion()),
      "paso3" = is.null(mod3$validacion()),
      "paso4" = is.null(mod4$validacion()),
      "paso5" = is.null(mod5$validacion()),
      "paso6" = is.null(mod6$validacion()),
      "paso7" = is.null(mod7$validacion()),
      "paso8" = TRUE
    )
  })

  mensaje_error <- reactive({
    switch(
      paso_actual(),
      "paso1" = mod1$validacion(),
      "paso2" = mod2$validacion(),
      "paso3" = mod3$validacion(),
      "paso4" = mod4$validacion(),
      "paso5" = mod5$validacion(),
      "paso6" = mod6$validacion(),
      "paso7" = mod7$validacion(),
      "paso8" = NULL
    )
  })

  observeEvent(input$siguiente, {
    idx <- match(paso_actual(), pasos)
    if (!isTRUE(es_paso_valido())) {
      showNotification(paste("Revise", paso_actual(), ":", mensaje_error()), type = "warning", duration = 4)
      return()
    }
    if (idx < length(pasos)) {
      nxt <- pasos[idx + 1]
      paso_actual(nxt)
      updateTabsetPanel(session, "wizard", selected = nxt)
    } else {
      showNotification("Proceso completado. Puede exportar el script de R.", type = "message")
    }
  })

  observeEvent(input$anterior, {
    idx <- match(paso_actual(), pasos)
    if (idx > 1) {
      prv <- pasos[idx - 1]
      paso_actual(prv)
      updateTabsetPanel(session, "wizard", selected = prv)
    }
  })
}

shinyApp(ui, server)
