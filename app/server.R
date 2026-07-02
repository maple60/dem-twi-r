server <- function(input, output, session) {
  # Each session receives its own temporary workspace so simultaneous users do
  # not overwrite one another's uploaded DEM or output rasters.
  work_dir <- tempfile("twi_shiny_")
  dir.create(work_dir, recursive = TRUE)
  session$onSessionEnded(function() {
    unlink(work_dir, recursive = TRUE, force = TRUE)
  })

  dem_path <- shiny::reactiveVal(NULL)
  run_results <- shiny::reactiveVal(NULL)
  status_messages <- shiny::reactiveVal("DEMのアップロード待ちです。")

  reset_results <- function() {
    run_results(NULL)
    shiny::updateSelectInput(
      session,
      "result_algorithm",
      choices = character(0)
    )
  }

  append_status <- function(message) {
    status_messages(c(
      status_messages(),
      paste(format(Sys.time(), "%H:%M:%S"), message)
    ))
  }

  load_dem <- function(path, label) {
    dem <- terra::rast(path)
    validate_dem(dem)
    dem_path(path)
    reset_results()
    append_status(paste("DEMを読み込みました:", label))
  }

  load_uploaded_dem <- function() {
    tryCatch(
      {
        check_packages("terra")
        path <- copy_uploaded_dem(input$dem_file, work_dir)
        load_dem(path, input$dem_file$name)
      },
      error = function(e) {
        dem_path(NULL)
        reset_results()
        append_status(paste("DEM読み込みに失敗しました:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error")
      }
    )
  }

  shiny::observeEvent(input$dem_source, {
    if (identical(input$dem_source, "sample")) {
      tryCatch(
        {
          path <- copy_sample_dem(work_dir)
          load_dem(path, "WhiteboxサンプルDEM")
        },
        error = function(e) {
          dem_path(NULL)
          reset_results()
          append_status(
            paste("サンプルDEM読み込みに失敗しました:", conditionMessage(e))
          )
          shiny::showNotification(conditionMessage(e), type = "error")
        }
      )
      return(invisible(NULL))
    }

    if (!is.null(input$dem_file)) {
      load_uploaded_dem()
      return(invisible(NULL))
    }

    dem_path(NULL)
    reset_results()
    status_messages("DEMのアップロード待ちです。")
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$dem_file, {
    if (!identical(input$dem_source, "upload")) {
      return(invisible(NULL))
    }
    load_uploaded_dem()
  })

  output$dem_plot <- shiny::renderPlot({
    shiny::req(dem_path())
    dem <- terra::rast(dem_path())
    terra::plot(dem, axes = FALSE, main = "DEM")
  })

  output$dem_map_ui <- shiny::renderUI({
    shiny::req(dem_path())
    if (!leaflet_available()) {
      return(shiny::div(
        class = "map-placeholder",
        "インタラクティブ地図を使うには leaflet パッケージが必要です。"
      ))
    }

    leaflet::leafletOutput("dem_map", height = 420)
  })

  if (leaflet_available()) {
    output$dem_map <- leaflet::renderLeaflet({
      shiny::req(dem_path())
      dem <- terra::rast(dem_path())
      leaflet_raster_map(
        dem,
        title = "DEM",
        colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
      )
    })
  }

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

  output$twi_map_ui <- shiny::renderUI({
    selected_result()
    if (!leaflet_available()) {
      return(shiny::div(
        class = "map-placeholder",
        "インタラクティブ地図を使うには leaflet パッケージが必要です。"
      ))
    }

    leaflet::leafletOutput("twi_map", height = 420)
  })

  if (leaflet_available()) {
    output$twi_map <- leaflet::renderLeaflet({
      result <- selected_result()
      twi <- terra::rast(result$twi)
      leaflet_raster_map(
        twi,
        title = paste("TWI", result$algorithm),
        colors = c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
      )
    })
  }

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
