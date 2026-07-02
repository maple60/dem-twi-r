# UI-only definitions live here. Keep reactive logic in server.R and shared
# calculation code in global.R.
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      "
      body { background: #f7f7f5; }
      .container-fluid { max-width: 1320px; }
      .well { background: #ffffff; border-radius: 6px; }
      .plot-box { min-height: 360px; }
      pre { white-space: pre-wrap; }
    "
    ))
  ),
  shiny::titlePanel("TWI計算"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons(
        "dem_source",
        "DEM入力",
        choices = c(
          "GeoTIFFをアップロード" = "upload",
          "サンプルDEMを使用" = "sample"
        ),
        selected = "upload"
      ),
      shiny::conditionalPanel(
        "input.dem_source == 'upload'",
        shiny::fileInput(
          "dem_file",
          "DEM GeoTIFF",
          accept = c(".tif", ".tiff")
        )
      ),
      shiny::checkboxGroupInput(
        "algorithms",
        "流量蓄積アルゴリズム",
        choices = algorithm_choices,
        selected = c("d8", "dinf")
      ),
      shiny::numericInput(
        "breach_dist",
        "Breach距離",
        value = 20,
        min = 1,
        step = 1
      ),
      shiny::checkboxInput(
        "breach_fill",
        "残った凹地をfillする",
        value = TRUE
      ),
      shiny::actionButton(
        "run",
        "TWIを計算",
        class = "btn-primary"
      ),
      shiny::hr(),
      shiny::selectInput(
        "result_algorithm",
        "結果プレビュー",
        choices = character(0)
      ),
      shiny::downloadButton("download_twi", "選択中のTWIを保存")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "DEMプレビュー",
          shiny::br(),
          shiny::plotOutput("dem_plot", height = 420),
          shiny::tableOutput("dem_info")
        ),
        shiny::tabPanel(
          "結果",
          shiny::br(),
          shiny::plotOutput("twi_plot", height = 420),
          shiny::tableOutput("output_files")
        ),
        shiny::tabPanel(
          "ログ",
          shiny::br(),
          shiny::verbatimTextOutput("status")
        )
      )
    )
  )
)
