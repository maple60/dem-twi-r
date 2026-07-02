library(shiny)
options(shiny.maxRequestSize = 1024 * 1024^2)

required_packages <- c("terra", "whitebox")
algorithm_choices <- c(
  "D8" = "d8",
  "D-infinity" = "dinf",
  "FD8" = "fd8"
)

algorithm_labels <- function(methods) {
  labels <- names(algorithm_choices)[match(methods, algorithm_choices)]
  unname(labels)
}

algorithm_select_choices <- function(methods) {
  stats::setNames(methods, algorithm_labels(methods))
}

check_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Required package(s) are not installed: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

copy_uploaded_dem <- function(upload, work_dir) {
  ext <- tolower(tools::file_ext(upload$name))
  if (!ext %in% c("tif", "tiff")) {
    stop("GeoTIFF file (.tif or .tiff) is required.", call. = FALSE)
  }

  target <- file.path(work_dir, paste0("input_dem.", ext))
  ok <- file.copy(upload$datapath, target, overwrite = TRUE)
  if (!ok) {
    stop("Failed to copy uploaded DEM.", call. = FALSE)
  }
  target
}

dem_metadata <- function(dem) {
  ext <- terra::ext(dem)
  res <- terra::res(dem)
  crs_text <- terra::crs(dem, describe = TRUE)
  crs_name <- NA_character_
  if (is.data.frame(crs_text) && "name" %in% names(crs_text)) {
    crs_name <- crs_text$name[1]
  }

  data.frame(
    item = c(
      "rows",
      "columns",
      "cells",
      "layers",
      "resolution_x",
      "resolution_y",
      "xmin",
      "xmax",
      "ymin",
      "ymax",
      "crs"
    ),
    value = c(
      terra::nrow(dem),
      terra::ncol(dem),
      terra::ncell(dem),
      terra::nlyr(dem),
      signif(res[1], 8),
      signif(res[2], 8),
      signif(ext[1], 8),
      signif(ext[2], 8),
      signif(ext[3], 8),
      signif(ext[4], 8),
      if (!is.na(crs_name) && nzchar(crs_name)) {
        crs_name
      } else {
        terra::crs(dem)
      }
    ),
    stringsAsFactors = FALSE
  )
}

validate_dem <- function(dem) {
  if (terra::nlyr(dem) != 1) {
    stop("DEM must have exactly one raster layer.", call. = FALSE)
  }
  crs_value <- terra::crs(dem)
  if (is.na(crs_value) || !nzchar(crs_value)) {
    stop("DEM has no coordinate reference system.", call. = FALSE)
  }
  if (isTRUE(terra::is.lonlat(dem))) {
    stop(
      "DEM must use a projected coordinate reference system.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_whitebox <- function() {
  if (!whitebox::check_whitebox_binary()) {
    stop(
      "WhiteboxTools executable was not found. ",
      "Run whitebox::install_whitebox() once, then rerun this app.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

run_flow_accumulation <- function(method, breached_path, output_path) {
  if (method == "d8") {
    whitebox::wbt_d8_flow_accumulation(
      input = breached_path,
      output = output_path,
      out_type = "specific contributing area",
      log = FALSE
    )
  } else if (method == "dinf") {
    whitebox::wbt_d_inf_flow_accumulation(
      input = breached_path,
      output = output_path,
      out_type = "Specific Contributing Area",
      log = FALSE
    )
  } else if (method == "fd8") {
    whitebox::wbt_fd8_flow_accumulation(
      dem = breached_path,
      output = output_path,
      out_type = "specific contributing area",
      log = FALSE
    )
  } else {
    stop("Unknown flow accumulation algorithm: ", method, call. = FALSE)
  }
}

run_twi_workflow <- function(
  dem_path,
  algorithms,
  output_dir,
  breach_dist,
  breach_fill,
  progress = NULL
) {
  check_packages(required_packages)
  if (is.null(progress)) {
    progress <- function(detail) invisible(detail)
  }

  dem <- terra::rast(dem_path)
  validate_dem(dem)
  check_whitebox()

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  breached_path <- file.path(output_dir, "dem_breached.tif")
  slope_path <- file.path(output_dir, "dem_breached_slope.tif")

  progress("Depression breaching")
  whitebox::wbt_breach_depressions_least_cost(
    dem = dem_path,
    output = breached_path,
    dist = breach_dist,
    fill = breach_fill
  )

  progress("Slope")
  whitebox::wbt_slope(
    dem = breached_path,
    output = slope_path,
    units = "degrees"
  )

  results <- list()
  for (method in algorithms) {
    accum_path <- file.path(
      output_dir,
      paste0("flow_accumulation_", method, ".tif")
    )
    twi_path <- file.path(output_dir, paste0("twi_", method, ".tif"))

    progress(paste("Flow accumulation:", algorithm_labels(method)))
    run_flow_accumulation(method, breached_path, accum_path)

    progress(paste("TWI:", algorithm_labels(method)))
    whitebox::wbt_wetness_index(
      sca = accum_path,
      slope = slope_path,
      output = twi_path
    )

    results[[method]] <- list(
      algorithm = unname(algorithm_labels(method)),
      accumulation = accum_path,
      twi = twi_path
    )
  }

  list(
    input_dem = dem_path,
    breached = breached_path,
    slope = slope_path,
    algorithms = results,
    output_dir = output_dir,
    finished_at = Sys.time()
  )
}

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
      shiny::fileInput(
        "dem_file",
        "DEM GeoTIFF",
        accept = c(".tif", ".tiff")
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

server <- function(input, output, session) {
  work_dir <- tempfile("twi_shiny_")
  dir.create(work_dir, recursive = TRUE)
  session$onSessionEnded(function() {
    unlink(work_dir, recursive = TRUE, force = TRUE)
  })

  dem_path <- shiny::reactiveVal(NULL)
  run_results <- shiny::reactiveVal(NULL)
  status_messages <- shiny::reactiveVal("DEMのアップロード待ちです。")

  append_status <- function(message) {
    status_messages(c(
      status_messages(),
      paste(format(Sys.time(), "%H:%M:%S"), message)
    ))
  }

  shiny::observeEvent(input$dem_file, {
    tryCatch(
      {
        check_packages("terra")
        path <- copy_uploaded_dem(input$dem_file, work_dir)
        dem <- terra::rast(path)
        validate_dem(dem)
        dem_path(path)
        run_results(NULL)
        shiny::updateSelectInput(
          session,
          "result_algorithm",
          choices = character(0)
        )
        append_status(paste("DEMを読み込みました:", input$dem_file$name))
      },
      error = function(e) {
        dem_path(NULL)
        run_results(NULL)
        append_status(paste("DEM読み込みに失敗しました:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error")
      }
    )
  })

  output$dem_plot <- shiny::renderPlot({
    shiny::req(dem_path())
    dem <- terra::rast(dem_path())
    terra::plot(dem, axes = FALSE, main = "DEM")
  })

  output$dem_info <- shiny::renderTable(
    {
      shiny::req(dem_path())
      dem_metadata(terra::rast(dem_path()))
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  shiny::observeEvent(input$run, {
    shiny::req(dem_path())
    algorithms <- input$algorithms
    if (length(algorithms) == 0) {
      shiny::showNotification(
        "流量蓄積アルゴリズムを1つ以上選んでください。",
        type = "error"
      )
      return(invisible(NULL))
    }

    output_dir <- file.path(
      work_dir,
      paste0("output_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    total_steps <- 2 + 2 * length(algorithms)
    step <- 0

    append_status("TWI計算を開始しました。")
    tryCatch(
      {
        result <- shiny::withProgress(message = "TWI計算中", value = 0, {
          progress <- function(detail) {
            step <<- step + 1
            shiny::incProgress(1 / total_steps, detail = detail)
            append_status(detail)
          }

          run_twi_workflow(
            dem_path = dem_path(),
            algorithms = algorithms,
            output_dir = output_dir,
            breach_dist = input$breach_dist,
            breach_fill = input$breach_fill,
            progress = progress
          )
        })

        run_results(result)
        choices <- algorithm_select_choices(names(result$algorithms))
        shiny::updateSelectInput(
          session,
          "result_algorithm",
          choices = choices,
          selected = names(result$algorithms)[1]
        )
        append_status("TWI計算が完了しました。")
      },
      error = function(e) {
        append_status(paste("TWI計算に失敗しました:", conditionMessage(e)))
        shiny::showNotification(
          conditionMessage(e),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  selected_result <- shiny::reactive({
    result <- run_results()
    shiny::req(result)
    method <- input$result_algorithm
    if (
      is.null(method) ||
        !nzchar(method) ||
        !method %in% names(result$algorithms)
    ) {
      method <- names(result$algorithms)[1]
    }
    result$algorithms[[method]]
  })

  output$twi_plot <- shiny::renderPlot({
    result <- selected_result()
    twi <- terra::rast(result$twi)
    terra::plot(twi, axes = FALSE, main = paste("TWI", result$algorithm))
  })

  output$output_files <- shiny::renderTable(
    {
      result <- run_results()
      shiny::req(result)

      rows <- lapply(result$algorithms, function(item) {
        data.frame(
          algorithm = item$algorithm,
          flow_accumulation = normalizePath(
            item$accumulation,
            winslash = "/",
            mustWork = FALSE
          ),
          twi = normalizePath(item$twi, winslash = "/", mustWork = FALSE),
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  output$status <- shiny::renderText({
    paste(status_messages(), collapse = "\n")
  })

  output$download_twi <- shiny::downloadHandler(
    filename = function() {
      result <- selected_result()
      paste0(
        "twi_",
        tolower(gsub("[^A-Za-z0-9]+", "_", result$algorithm)),
        ".tif"
      )
    },
    content = function(file) {
      result <- selected_result()
      file.copy(result$twi, file, overwrite = TRUE)
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
