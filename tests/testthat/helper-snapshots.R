snapshot_path <- function(filename) {
  Sys.getenv(
    "ALVARAS_SNAPSHOT_DIR",
    unset = testthat::test_path("..", "..", "data", "benchmark")
  ) |>
    fs::path(filename)
}

skip_if_snapshot_missing <- function(path) {
  testthat::skip_if_not(
    fs::file_exists(path),
    paste("Local benchmark is unavailable:", path)
  )
}
