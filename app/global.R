library(shiny)

# Allow users to upload DEM files up to 1 GB.
options(shiny.maxRequestSize = 1024 * 1024^2)

# Packages used by the TWI workflow. They are loaded lazily through
# namespace-qualified calls so startup stays simple and error messages can
# point to missing dependencies.
required_packages <- c("terra", "whitebox")

# Keep interactive map rasters small enough for quick browser rendering. The
# analysis rasters are left unchanged; this limit is only for preview layers.
leaflet_preview_max_cells <- 250000

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

leaflet_available <- function() {
  requireNamespace("leaflet", quietly = TRUE)
}

downsample_raster_for_leaflet <- function(
  r,
  max_cells = leaflet_preview_max_cells
) {
  cell_count <- terra::ncell(r)
  if (cell_count <= max_cells) {
    return(r)
  }

  aggregate_factor <- ceiling(sqrt(cell_count / max_cells))
  terra::aggregate(r, fact = aggregate_factor, fun = mean, na.rm = TRUE)
}

raster_value_range <- function(r) {
  value_range <- tryCatch(
    as.numeric(terra::global(r, range, na.rm = TRUE)[1, ]),
    error = function(e) c(NA_real_, NA_real_)
  )

  if (length(value_range) != 2 || any(!is.finite(value_range))) {
    return(c(0, 1))
  }

  if (isTRUE(all.equal(value_range[1], value_range[2]))) {
    return(value_range + c(-0.5, 0.5))
  }

  value_range
}

raster_lonlat_bounds <- function(r) {
  ext <- terra::ext(r)
  corners <- data.frame(
    x = c(ext[1], ext[2], ext[2], ext[1]),
    y = c(ext[3], ext[3], ext[4], ext[4])
  )
  points <- terra::vect(corners, geom = c("x", "y"), crs = terra::crs(r))
  if (!isTRUE(terra::is.lonlat(r))) {
    points <- terra::project(points, "EPSG:4326")
  }

  coords <- terra::crds(points)
  c(
    xmin = min(coords[, 1], na.rm = TRUE),
    ymin = min(coords[, 2], na.rm = TRUE),
    xmax = max(coords[, 1], na.rm = TRUE),
    ymax = max(coords[, 2], na.rm = TRUE)
  )
}

leaflet_raster_map <- function(r, title, colors) {
  check_packages(c("terra", "leaflet"))

  preview <- downsample_raster_for_leaflet(r)
  value_range <- raster_value_range(preview)
  palette <- leaflet::colorNumeric(
    palette = grDevices::colorRampPalette(colors)(256),
    domain = value_range,
    na.color = "transparent"
  )

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addTiles(map)
  map <- leaflet::addRasterImage(
    map,
    preview,
    colors = palette,
    opacity = 0.85,
    project = TRUE,
    maxBytes = 8 * 1024 * 1024
  )
  map <- leaflet::addLegend(
    map,
    position = "bottomright",
    pal = palette,
    values = value_range,
    title = title
  )

  bounds <- tryCatch(raster_lonlat_bounds(preview), error = function(e) NULL)
  if (!is.null(bounds) && all(is.finite(bounds))) {
    map <- leaflet::fitBounds(
      map,
      lng1 = bounds[["xmin"]],
      lat1 = bounds[["ymin"]],
      lng2 = bounds[["xmax"]],
      lat2 = bounds[["ymax"]]
    )
  }

  map
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

copy_sample_dem <- function(work_dir) {
  check_packages(c("terra", "whitebox"))

  source_path <- whitebox::sample_dem_data()
  if (!file.exists(source_path)) {
    stop("Whitebox sample DEM was not found.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(source_path))
  if (!nzchar(ext)) {
    ext <- "tif"
  }

  target <- file.path(work_dir, paste0("sample_dem.", ext))
  ok <- file.copy(source_path, target, overwrite = TRUE)
  if (!ok) {
    stop("Failed to copy sample DEM.", call. = FALSE)
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
