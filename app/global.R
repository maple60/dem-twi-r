library(shiny)
library(leaflet)

# Allow users to upload DEM files up to 1 GB.
options(shiny.maxRequestSize = 1024 * 1024^2)

# Packages used by the TWI workflow and interactive map previews.
required_packages <- c("terra", "whitebox", "leaflet")

# Keep interactive map rasters small enough for quick browser rendering. The
# analysis rasters are left unchanged; this limit is only for preview layers.
leaflet_preview_max_cells <- 250000

viridis_colors <- function(n = 256) {
  grDevices::hcl.colors(n, palette = "Viridis")
}

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

add_reset_view_control <- function(map, bounds) {
  js <- sprintf(
    paste(
      "function(el, x) {",
      "  var map = this;",
      "  var bounds = [[%.15f, %.15f], [%.15f, %.15f]];",
      "  var ResetControl = L.Control.extend({",
      "    options: { position: 'topleft' },",
      "    onAdd: function(map) {",
      "      var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control');",
      "      var button = L.DomUtil.create('button', '', container);",
      "      button.type = 'button';",
      "      button.innerHTML = '移動リセット';",
      "      button.title = '表示位置とズームをリセット';",
      "      button.setAttribute('aria-label', button.title);",
      "      button.style.backgroundColor = '#ffffff';",
      "      button.style.border = '0';",
      "      button.style.cursor = 'pointer';",
      "      button.style.height = '30px';",
      "      button.style.lineHeight = '30px';",
      "      button.style.padding = '0 8px';",
      "      button.style.fontSize = '12px';",
      "      button.style.whiteSpace = 'nowrap';",
      "      L.DomEvent.disableClickPropagation(container);",
      "      L.DomEvent.disableScrollPropagation(container);",
      "      L.DomEvent.on(button, 'click', function(event) {",
      "        L.DomEvent.preventDefault(event);",
      "        map.fitBounds(bounds, { animate: true });",
      "      });",
      "      return container;",
      "    }",
      "  });",
      "  map.addControl(new ResetControl());",
      "}",
      sep = "\n"
    ),
    bounds[["ymin"]],
    bounds[["xmin"]],
    bounds[["ymax"]],
    bounds[["xmax"]]
  )

  htmlwidgets::onRender(map, js)
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
  map <- leaflet::addProviderTiles(
    map,
    provider = leaflet::providers$CartoDB.Positron
  )
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
    map <- add_reset_view_control(map, bounds)
  }

  map
}

hillshade_raster <- function(r) {
  slope <- terra::terrain(r, v = "slope", unit = "radians")
  aspect <- terra::terrain(r, v = "aspect", unit = "radians")
  terra::shade(slope, aspect, angle = 40, direction = 315)
}

leaflet_dem_map <- function(r) {
  check_packages(c("terra", "leaflet"))

  preview <- downsample_raster_for_leaflet(r)
  dem_range <- raster_value_range(preview)
  dem_palette <- leaflet::colorNumeric(
    palette = viridis_colors(),
    domain = dem_range,
    na.color = "transparent"
  )

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addTiles(map, group = "OSM")
  map <- leaflet::addProviderTiles(
    map,
    provider = leaflet::providers$Esri.WorldImagery,
    group = "航空写真"
  )

  hillshade <- tryCatch(hillshade_raster(preview), error = function(e) NULL)
  overlay_groups <- "DEM"
  if (!is.null(hillshade)) {
    hillshade_palette <- leaflet::colorNumeric(
      palette = grDevices::gray.colors(256, start = 0.1, end = 1),
      domain = raster_value_range(hillshade),
      na.color = "transparent"
    )
    map <- leaflet::addRasterImage(
      map,
      hillshade,
      colors = hillshade_palette,
      opacity = 0.75,
      project = TRUE,
      maxBytes = 8 * 1024 * 1024,
      group = "陰影起伏"
    )
    overlay_groups <- c("陰影起伏", overlay_groups)
  }

  map <- leaflet::addRasterImage(
    map,
    preview,
    colors = dem_palette,
    opacity = 0.70,
    project = TRUE,
    maxBytes = 8 * 1024 * 1024,
    group = "DEM"
  )
  map <- leaflet::addLegend(
    map,
    position = "bottomright",
    pal = dem_palette,
    values = dem_range,
    title = "DEM"
  )
  map <- leaflet::addLayersControl(
    map,
    baseGroups = c("OSM", "航空写真"),
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  map <- leaflet::hideGroup(map, "航空写真")
  if (!is.null(hillshade)) {
    map <- leaflet::hideGroup(map, "陰影起伏")
  }

  bounds <- tryCatch(raster_lonlat_bounds(preview), error = function(e) NULL)
  if (!is.null(bounds) && all(is.finite(bounds))) {
    map <- leaflet::fitBounds(
      map,
      lng1 = bounds[["xmin"]],
      lat1 = bounds[["ymin"]],
      lng2 = bounds[["xmax"]],
      lat2 = bounds[["ymax"]]
    )
    map <- add_reset_view_control(map, bounds)
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

crs_name <- function(dem) {
  crs_text <- terra::crs(dem, describe = TRUE)
  crs_label <- NA_character_
  if (is.data.frame(crs_text) && "name" %in% names(crs_text)) {
    crs_label <- crs_text$name[1]
  }

  if (!is.na(crs_label) && nzchar(crs_label)) {
    return(crs_label)
  }

  terra::crs(dem)
}

crs_unit_label <- function(dem) {
  # TWI depends on distance and area. A geographic CRS is therefore shown as
  # degree-based even when the full WKT text is too detailed for the UI.
  if (isTRUE(terra::is.lonlat(dem))) {
    return("degree")
  }

  crs_value <- terra::crs(dem)
  if (grepl("metre|meter", crs_value, ignore.case = TRUE)) {
    return("metre")
  }

  "projected CRS unit"
}

dem_center_lonlat <- function(dem) {
  # Candidate CRS selection uses the DEM centre in lon/lat. Projected DEMs are
  # transformed only for this metadata calculation; the analysis raster is not
  # changed here.
  ext <- terra::ext(dem)
  center <- data.frame(
    x = mean(c(ext[1], ext[2])),
    y = mean(c(ext[3], ext[4]))
  )

  point <- terra::vect(center, geom = c("x", "y"), crs = terra::crs(dem))
  if (!isTRUE(terra::is.lonlat(dem))) {
    point <- terra::project(point, "EPSG:4326")
  }

  coords <- terra::crds(point)
  c(lon = unname(coords[1, 1]), lat = unname(coords[1, 2]))
}

jgd2011_zone_table <- function() {
  # JGD2011 plane rectangular CS EPSG codes are consecutive:
  # zone I is EPSG:6669 and zone XIX is EPSG:6687. The origin and area table is
  # kept in the app so users can verify the suggested zone before reprojecting.
  data.frame(
    zone = c(
      "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
      "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX"
    ),
    epsg = 6669:6687,
    origin_lon = c(
      129.5, 131, 132 + 10 / 60, 133.5, 134 + 20 / 60, 136,
      137 + 10 / 60, 138.5, 139 + 50 / 60, 140 + 50 / 60,
      140.25, 142.25, 144.25, 142, 127.5, 124, 131, 136, 154
    ),
    origin_lat = c(
      33, 33, 36, 33, 36, 36, 36, 36, 36, 40,
      44, 44, 44, 26, 26, 26, 26, 20, 26
    ),
    area = c(
      "長崎県、鹿児島県の一部島しょ",
      "福岡・佐賀・熊本・大分・宮崎・鹿児島",
      "山口・島根・広島",
      "香川・愛媛・徳島・高知",
      "兵庫・鳥取・岡山",
      "京都・大阪・福井・滋賀・三重・奈良・和歌山",
      "石川・富山・岐阜・愛知",
      "新潟・長野・山梨・静岡",
      "東京本土・福島・栃木・茨城・埼玉・千葉・群馬・神奈川",
      "青森・秋田・山形・岩手・宮城",
      "北海道西部",
      "北海道中央部",
      "北海道東部",
      "東京都小笠原諸島の一部",
      "沖縄県本島周辺",
      "沖縄県先島諸島周辺",
      "沖縄県大東諸島周辺",
      "東京都沖ノ鳥島周辺",
      "東京都南鳥島周辺"
    ),
    stringsAsFactors = FALSE
  )
}

lonlat_distance_km <- function(lon, lat, origin_lon, origin_lat) {
  # Equirectangular distance is sufficient for ranking nearby plane-rectangular
  # zone origins. The selected candidate is a guide, not an automatic decision.
  radians <- pi / 180
  x <- (origin_lon - lon) * cos((lat + origin_lat) * radians / 2)
  y <- origin_lat - lat
  111.32 * sqrt(x^2 + y^2)
}

nearest_jgd2011_zone <- function(lon, lat) {
  zones <- jgd2011_zone_table()
  zones$distance_km <- lonlat_distance_km(
    lon,
    lat,
    zones$origin_lon,
    zones$origin_lat
  )
  zones[which.min(zones$distance_km), , drop = FALSE]
}

utm_candidate <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  zone <- max(1, min(60, zone))
  epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
  hemisphere <- if (lat >= 0) "N" else "S"

  data.frame(
    epsg = epsg,
    name = paste0("WGS 84 / UTM zone ", zone, hemisphere),
    stringsAsFactors = FALSE
  )
}

crs_recommendations <- function(dem) {
  if (!isTRUE(terra::is.lonlat(dem))) {
    return(data.frame(
      candidate = "現在のCRS",
      epsg = "",
      name = crs_name(dem),
      reason = "投影座標系のため、通常はそのままTWI計算できます。",
      stringsAsFactors = FALSE
    ))
  }

  center <- dem_center_lonlat(dem)
  jgd <- nearest_jgd2011_zone(center[["lon"]], center[["lat"]])
  utm <- utm_candidate(center[["lon"]], center[["lat"]])

  data.frame(
    candidate = c("第1候補", "代替候補"),
    epsg = c(jgd$epsg, utm$epsg),
    name = c(
      paste0("JGD2011 / Japan Plane Rectangular CS ", jgd$zone),
      utm$name
    ),
    reason = c(
      paste0(
        "DEM中心に最も近いJGD2011平面直角座標系です。対象地域が ",
        jgd$area,
        " に入るか確認してください。"
      ),
      "経度から機械的に選べるUTMです。広域・境界付近ではJGD2011系と比較してください。"
    ),
    stringsAsFactors = FALSE
  )
}

default_target_epsg <- function(dem) {
  recommendations <- crs_recommendations(dem)
  if (nrow(recommendations) == 0 || !nzchar(recommendations$epsg[1])) {
    return("")
  }

  as.character(recommendations$epsg[1])
}

normalize_epsg <- function(epsg) {
  epsg <- trimws(as.character(epsg))
  epsg <- sub("^EPSG:", "", epsg, ignore.case = TRUE)
  if (!grepl("^[0-9]+$", epsg)) {
    stop("変換先EPSGコードを数値で指定してください。", call. = FALSE)
  }

  paste0("EPSG:", epsg)
}

project_dem_to_epsg <- function(dem, target_epsg, output_path) {
  # DEM is continuous elevation data, so bilinear interpolation is the least
  # surprising default for reprojection. Users explicitly opt in before this
  # function is called.
  target_crs <- normalize_epsg(target_epsg)
  terra::project(
    dem,
    target_crs,
    filename = output_path,
    method = "bilinear",
    overwrite = TRUE
  )
  output_path
}

dem_metadata <- function(dem) {
  ext <- terra::ext(dem)
  res <- terra::res(dem)

  data.frame(
    item = c(
      "rows",
      "columns",
      "cells",
      "layers",
      "coordinate_type",
      "coordinate_unit",
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
      if (isTRUE(terra::is.lonlat(dem))) "geographic" else "projected",
      crs_unit_label(dem),
      signif(res[1], 8),
      signif(res[2], 8),
      signif(ext[1], 8),
      signif(ext[2], 8),
      signif(ext[3], 8),
      signif(ext[4], 8),
      crs_name(dem)
    ),
    stringsAsFactors = FALSE
  )
}

crs_detail_table <- function(dem) {
  center <- dem_center_lonlat(dem)
  res <- terra::res(dem)
  ext <- terra::ext(dem)

  data.frame(
    item = c(
      "crs",
      "coordinate_type",
      "coordinate_unit",
      "resolution_x",
      "resolution_y",
      "center_lon",
      "center_lat",
      "xmin",
      "xmax",
      "ymin",
      "ymax"
    ),
    value = c(
      crs_name(dem),
      if (isTRUE(terra::is.lonlat(dem))) "geographic" else "projected",
      crs_unit_label(dem),
      signif(res[1], 8),
      signif(res[2], 8),
      signif(center[["lon"]], 8),
      signif(center[["lat"]], 8),
      signif(ext[1], 8),
      signif(ext[2], 8),
      signif(ext[3], 8),
      signif(ext[4], 8)
    ),
    stringsAsFactors = FALSE
  )
}

validate_dem <- function(dem, require_projected = TRUE) {
  if (terra::nlyr(dem) != 1) {
    stop("DEM must have exactly one raster layer.", call. = FALSE)
  }
  crs_value <- terra::crs(dem)
  if (is.na(crs_value) || !nzchar(crs_value)) {
    stop("DEM has no coordinate reference system.", call. = FALSE)
  }
  if (isTRUE(require_projected) && isTRUE(terra::is.lonlat(dem))) {
    stop(
      "DEM must use a projected coordinate reference system. ",
      "CRS確認タブで候補EPSGを確認し、必要なら投影変換を有効にしてください。",
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
  project_dem = FALSE,
  target_epsg = NULL,
  progress = NULL
) {
  check_packages(required_packages)
  if (is.null(progress)) {
    progress <- function(detail) invisible(detail)
  }

  dem <- terra::rast(dem_path)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  analysis_dem_path <- dem_path
  if (isTRUE(project_dem)) {
    projected_path <- file.path(output_dir, "dem_projected.tif")
    progress(paste("Projection:", normalize_epsg(target_epsg)))
    analysis_dem_path <- project_dem_to_epsg(dem, target_epsg, projected_path)
    dem <- terra::rast(analysis_dem_path)
  }

  validate_dem(dem)
  check_whitebox()

  breached_path <- file.path(output_dir, "dem_breached.tif")
  slope_path <- file.path(output_dir, "dem_breached_slope.tif")

  # WhiteboxTools first removes depressions, then derives slope and flow
  # accumulation rasters used by the wetness index calculation.
  progress("Depression breaching")
  whitebox::wbt_breach_depressions_least_cost(
    dem = analysis_dem_path,
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
    analysis_dem = analysis_dem_path,
    breached = breached_path,
    slope = slope_path,
    algorithms = results,
    output_dir = output_dir,
    finished_at = Sys.time()
  )
}
