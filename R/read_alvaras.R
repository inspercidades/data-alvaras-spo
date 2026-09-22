# Read permits ----

read_alvaras_archive <- function(archive) {
  files <- archive::archive(archive)$path
  csv <- files[stringr::str_detect(
    files,
    "msp_alvaras_sisacoe_aprovadigital.*\\.csv$"
  )]
  if (length(csv) != 1) {
    cli::cli_abort(
      "Expected one permit CSV in {.file {archive}}, found {length(csv)}."
    )
  }
  extract_dir <- fs::path_temp(paste0("alvaras-", digest::digest(archive)))
  fs::dir_create(extract_dir)
  utils::unzip(archive, files = csv, exdir = extract_dir)
  permits <- utils::read.csv2(
    fs::path(extract_dir, csv),
    fileEncoding = "latin1",
    stringsAsFactors = FALSE
  )
  validate_raw_alvaras(permits)
  return(permits)
}
