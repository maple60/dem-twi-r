library(shiny)

# Allow users to upload DEM files up to 1 GB.
options(shiny.maxRequestSize = 1024 * 1024^2)

# Packages used by the TWI workflow. They are loaded lazily through
# namespace-qualified calls so startup stays simple and error messages can
# point to missing dependencies.
required_packages <- c("terra", "whitebox")

# Values are WhiteboxTools method identifiers; names are displayed in the UI.
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

  # WhiteboxTools first removes depressions, then derives slope and flow
  # accumulation rasters used by the wetness index calculation.
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
