library(shiny)

viridis_colors <- function(n = 256) {
  grDevices::hcl.colors(n, palette = "Viridis")
}

scale01 <- function(x) {
  value_range <- range(x, finite = TRUE)
  if (!all(is.finite(value_range)) || isTRUE(all.equal(value_range[1], value_range[2]))) {
    return(x * 0)
  }

  (x - value_range[1]) / diff(value_range)
}

surface_gradient <- function(z, dx, dy) {
  nr <- nrow(z)
  nc <- ncol(z)
  gx <- gy <- z * 0

  gx[2:(nr - 1), ] <- (z[3:nr, ] - z[1:(nr - 2), ]) / (2 * dx)
  gx[1, ] <- (z[2, ] - z[1, ]) / dx
  gx[nr, ] <- (z[nr, ] - z[nr - 1, ]) / dx

  gy[, 2:(nc - 1)] <- (z[, 3:nc] - z[, 1:(nc - 2)]) / (2 * dy)
  gy[, 1] <- (z[, 2] - z[, 1]) / dy
  gy[, nc] <- (z[, nc] - z[, nc - 1]) / dy

  sqrt(gx^2 + gy^2)
}

make_demo_terrain <- function(
  n,
  regional_slope,
  valley_depth,
  ridge_height,
  noise_sd,
  convergence,
  seed
) {
  x <- seq(-1, 1, length.out = n)
  y <- seq(-1, 1, length.out = n)
  xx <- outer(x, rep(1, n))
  yy <- outer(rep(1, n), y)

  set.seed(seed)
  valley <- exp(-(xx / 0.26)^2) * (0.75 + 0.25 * yy)
  ridge <- exp(-((xx - 0.55)^2 + (yy + 0.10)^2) / 0.16)
  dem <- 100 -
    regional_slope * 35 * yy -
    valley_depth * 28 * valley +
    ridge_height * 22 * ridge +
    matrix(stats::rnorm(n * n, sd = noise_sd), nrow = n)

  slope_tan <- surface_gradient(dem, diff(x)[1], diff(y)[1])
  relief <- scale01(max(dem) - dem)
  sca <- 1 + convergence * (0.7 * relief + 1.8 * valley)
  twi <- log(sca / pmax(slope_tan, 0.03))

  list(
    x = x,
    y = y,
    dem = dem,
    slope_tan = slope_tan,
    sca = sca,
    twi = twi
  )
}

plot_grid <- function(x, y, z, title, palette = viridis_colors()) {
  op <- par(mar = c(3.5, 3.5, 3, 5), las = 1)
  on.exit(par(op), add = TRUE)

  image(
    x,
    y,
    z,
    col = palette,
    xlab = "x",
    ylab = "y",
    main = title,
    useRaster = TRUE
  )
  contour(x, y, z, add = TRUE, drawlabels = FALSE, col = "#2f2f2f66")
  box()
}

summary_table <- function(terrain) {
  data.frame(
    item = c("DEM min", "DEM max", "slope median", "SCA proxy median", "TWI median"),
    value = signif(c(
      min(terrain$dem),
      max(terrain$dem),
      stats::median(terrain$slope_tan),
      stats::median(terrain$sca),
      stats::median(terrain$twi)
    ), 5),
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      body { background: #f7f7f5; }
      .container-fluid { max-width: 1280px; }
      .well { background: #ffffff; border-radius: 6px; }
      .note { color: #555; font-size: 0.95em; }
      "
    ))
  ),
  titlePanel("TWI計算 Shinylive デモ"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("regional_slope", "広域斜面の強さ", min = 0.2, max = 2.0, value = 1.0, step = 0.1),
      sliderInput("valley_depth", "谷地形の深さ", min = 0.0, max = 2.0, value = 1.0, step = 0.1),
      sliderInput("ridge_height", "尾根・小丘の高さ", min = 0.0, max = 2.0, value = 0.8, step = 0.1),
      sliderInput("convergence", "集水しやすさ", min = 0.5, max = 8.0, value = 4.0, step = 0.5),
      sliderInput("noise_sd", "微地形ノイズ", min = 0.0, max = 4.0, value = 0.8, step = 0.2),
      numericInput("seed", "乱数シード", value = 1, min = 1, step = 1),
      tags$p(
        class = "note",
        "ブラウザ上で動く概念デモです。GeoTIFF、terra、WhiteboxTools は使いません。"
      ),
      downloadButton("download_values", "格子値CSVを保存")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "DEM",
          br(),
          plotOutput("dem_plot", height = 440)
        ),
        tabPanel(
          "TWI",
          br(),
          plotOutput("twi_plot", height = 440)
        ),
        tabPanel(
          "概要",
          br(),
          tableOutput("summary"),
          tags$p(
            class = "note",
            "TWI は log(a / tan(beta)) の形だけを示す簡易計算です。実データの前処理、流量蓄積、投影座標系の確認は通常版アプリで扱います。"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  terrain <- reactive({
    make_demo_terrain(
      n = 90,
      regional_slope = input$regional_slope,
      valley_depth = input$valley_depth,
      ridge_height = input$ridge_height,
      noise_sd = input$noise_sd,
      convergence = input$convergence,
      seed = input$seed
    )
  })

  output$dem_plot <- renderPlot({
    value <- terrain()
    plot_grid(value$x, value$y, value$dem, "Synthetic DEM")
  })

  output$twi_plot <- renderPlot({
    value <- terrain()
    plot_grid(
      value$x,
      value$y,
      value$twi,
      "TWI proxy",
      palette = grDevices::hcl.colors(256, palette = "YlGnBu", rev = TRUE)
    )
  })

  output$summary <- renderTable(
    summary_table(terrain()),
    striped = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  output$download_values <- downloadHandler(
    filename = function() "shinylive_twi_demo_values.csv",
    content = function(file) {
      value <- terrain()
      rows <- expand.grid(x = value$x, y = value$y)
      rows$dem <- as.vector(value$dem)
      rows$slope_tan <- as.vector(value$slope_tan)
      rows$sca_proxy <- as.vector(value$sca)
      rows$twi_proxy <- as.vector(value$twi)
      utils::write.csv(rows, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
