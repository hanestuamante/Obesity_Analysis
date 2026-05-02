# Utility helpers ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running the pipeline.",
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

create_project_dirs <- function(paths) {
  dir.create(paths$plots, showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$outputs, showWarnings = FALSE, recursive = TRUE)
}

append_section <- function(title, file) {
  cat("\n\n", strrep("=", 80), "\n", title, "\n", strrep("=", 80), "\n", file = file, append = TRUE)
}

capture_section <- function(expr, title, file) {
  append_section(title, file)
  capture.output(expr, file = file, append = TRUE)
}
