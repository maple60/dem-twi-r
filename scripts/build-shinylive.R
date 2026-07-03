args <- commandArgs(trailingOnly = TRUE)

app_dir <- if (length(args) >= 1) args[[1]] else "app-shinylive"
dest_dir <- if (length(args) >= 2) args[[2]] else file.path("_site", "app")

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop(
    "Package 'shinylive' is required. Install it with install.packages('shinylive').",
    call. = FALSE
  )
}

if (!dir.exists(app_dir)) {
  stop("Shinylive source app directory does not exist: ", app_dir, call. = FALSE)
}

if (dir.exists(dest_dir)) {
  unlink(dest_dir, recursive = TRUE, force = TRUE)
}

dir.create(dirname(dest_dir), recursive = TRUE, showWarnings = FALSE)

shinylive::export(
  appdir = app_dir,
  destdir = dest_dir,
  quiet = TRUE,
  template_params = list(title = "TWI Shinylive demo")
)

cat("Exported Shinylive app to ", normalizePath(dest_dir, winslash = "/"), "\n", sep = "")
