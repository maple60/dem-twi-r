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
