# Backward-compatible entrypoint.
# Prefer running scripts/run_pipeline.R for the full project pipeline.

command_args <- commandArgs(FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)
script_file <- if (length(script_arg) == 1) {
  sub("^--file=", "", script_arg)
} else {
  tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
}

if (!is.null(script_file)) {
  setwd(dirname(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))))
}

source(file.path("scripts", "run_pipeline.R"))
