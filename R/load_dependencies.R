# Load all CleanLabValues module scripts in a stable order
load_cleanlab <- function() {
  srcs <- c(
    "R/check_metadata.R",
    "R/fill_missing_unit.R",
    "R/mo_convert.R",
    "R/clean_lab_main.R",
    "R/CleanLabValuesDataset.R"
  )
  # Try sourcing files relative to a few likely project roots so tests
  # running from `tests/testthat` still find the R/ scripts.
  roots <- c(".", "..", "..", file.path("..", ".."))
  for (s in srcs) {
    found <- FALSE
    for (r in roots) {
      p <- file.path(r, s)
      if (file.exists(p)) {
        source(p)
        found <- TRUE
        break
      }
    }
    if (!found) stop(paste("Missing required file:", s))
  }
  invisible(TRUE)
}
