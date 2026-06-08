# ============================================================
# app.R
# EcoObservaK — Análisis visual e identificación de especies
# Powered by kuzco + Google Gemini 2.5 Flash
# OVS-CR · ICOMVIS · UNA
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(ellmer)
library(kuzco)
library(gt)
library(gargle)
library(magick)

source("R/utils_display.R")

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(

  theme = bs_theme(
    version   = 5,
    bg        = "#F8F4F4",
    fg        = "#2C2C2C",
    primary   = "#a31e32",
    base_font = font_google("Nunito")
  ),

  tags$style(HTML("
    body { margin: 0; padding: 0; }

    .banner-titulo {
      background-color: #a31e32;
      color: white;
      font-weight: bold;
      text-align: center;
      padding: 18px 30px;
      font-size: 1.6rem;
    }

    .banner-subtitulo {
      background-color: #a31e32;
      color: #f5c0c0;
      text-align: center;
      padding: 0 30px 14px 30px;
      font-size: 0.95rem;
      font-style: italic;
    }

    .sidebar-custom {
      background-color: #F8F0F0;
      border-right: 1px solid #E8C8C8;
      padding: 20px;
      min-height: calc(100vh - 100px);
    }

    .sidebar-custom .logo-container {
      text-align: center;
      margin-bottom: 10px;
    }

    .sidebar-custom .logo-caption {
      text-align: center;
      font-size: 11px;
      font-style: italic;
      color: #a31e32;
      margin-bottom: 15px;
    }

    .sidebar-custom h5 {
      color: #a31e32;
      font-weight: bold;
      margin-top: 15px;
    }

    .descripcion-noctua {
      font-size: 13px;
      color: #555;
      margin-bottom: 15px;
    }

    .main-panel {
      background-color: #ffffff;
      padding: 20px;
      min-height: calc(100vh - 100px);
    }

    .btn-primary {
      background-color: #a31e32 !important;
      border-color: #a31e32 !important;
    }

    .btn-primary:hover {
      background-color: #831828 !important;
      border-color: #831828 !important;
    }

    .footer-text {
      text-align: center;
      font-size: 12px;
      color: #666;
      padding: 20px 40px;
      border-top: 1px solid #E8C8C8;
      margin-top: 20px;
    }

    @media (max-width: 768px) {
      .banner-titulo { font-size: 1.1rem; padding: 12px 15px; }
      .col-sm-3, .col-sm-9 { width: 100% !important; }
      .sidebar-custom { min-height: unset; }
    }
  ")),

  # Banner
  div(class = "banner-titulo", "EcoObservaK"),
  div(class = "banner-subtitulo",
    "¿Qué hay en tu imagen? Análisis visual e identificación de especies con Kuzco"
  ),

  fluidRow(
    style = "margin: 0;",

    # Sidebar
    column(3,
      style = "padding: 0;",
      div(class = "sidebar-custom",

        div(class = "logo-container",
          tags$img(src = "logo_maritza.png",
                   style = "max-width: 130px; height: auto; border-radius: 50%;")
        ),
        div(class = "logo-caption",
          tags$span(style = "font-size: 13px; color: #a31e32; font-style: italic;",
            "Con la ayuda de Noctua, el búho observador"),
          tags$br(),
          tags$span(style = "font-size: 10px; color: #888;",
            "Ilustración por Gemini 2.0 Flash y Maritza Ramírez")
        ),

        hr(style = "border-color: #E8C8C8;"),

        p(class = "descripcion-noctua",
          tags$strong("Noctua"), ", nuestro búho observador, utiliza inteligencia artificial ",
          "para ayudarte a descubrir lo que hay en una imagen. Ideal para aprender, explorar ",
          "y maravillarse con la biodiversidad que nos rodea."
        ),

        hr(style = "border-color: #E8C8C8;"),

        h5(bs_icon("image", class = "me-1"), "Imagen"),
        fileInput(
          inputId     = "imagen",
          label       = NULL,
          buttonLabel = "Seleccionar...",
          placeholder = "Ningún archivo seleccionado",
          accept      = c("image/jpeg", "image/png", "image/jpg"),
          width       = "100%"
        ),

        h5(bs_icon("chat-text", class = "me-1"), "Tu solicitud"),
        textInput(
          inputId     = "prompt",
          label       = NULL,
          placeholder = "Ej: ¿qué especie se ve en la imagen?",
          width       = "100%"
        ),

        actionButton("goButton", "Analizar imagen",
                     class = "btn-primary w-100",
                     icon  = icon("play"))
      )
    ),

    # Main panel
    column(9,
      style = "padding: 0;",
      div(class = "main-panel",
        imageOutput("my_image", height = "auto"),
        div(style = "margin-top: 20px;",
          gt_output("results_table")
        )
      )
    )
  ),

  # Footer
  div(class = "footer-text",
    HTML("© 2025 Observatorio de Vida Silvestre y Biodiversidad de Costa Rica, ICOMVIS-UNA.<br>"),
    "App creada por ",
    tags$a(href = "https://mspinola-sitioweb.netlify.app", "Manuel Spínola", target = "_blank"),
    HTML("<br>Esta aplicación utiliza el paquete kuzco de R y Gemini 2.5 Flash (Google AI) como motor de lenguaje.<br>
         Google no respalda ni administra esta aplicación.")
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  observeEvent(input$imagen, {
    req(input$imagen)
    output$my_image <- renderImage({
      list(
        src         = input$imagen$datapath,
        contentType = input$imagen$type,
        width       = "100%",
        height      = "auto"
      )
    }, deleteFile = FALSE)
  })

  results <- reactiveVal()

  observeEvent(input$goButton, {
    req(input$imagen, input$prompt)
    withProgress(message = "Analizando imagen...", value = 0.3, {
    tryCatch({
      res <- kuzco::llm_image_classification(
        provider          = "google_gemini",
        llm_model         = "gemini-2.5-flash",
        backend           = "ellmer",
        additional_prompt = input$prompt,
        image             = input$imagen$datapath,
        language          = "Spanish"
      )
      incProgress(0.7)
      results(res)
    }, error = function(e) {
      showNotification(
        paste0("Error: ", conditionMessage(e)),
        type     = "error",
        duration = 8
      )
    })
    }) # withProgress
  })

  output$results_table <- gt::render_gt({
    req(results())
    my_view_llm_results(results())
  })
}

shinyApp(ui = ui, server = server)
