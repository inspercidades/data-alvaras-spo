# Download permits ----

alvaras_url_template <- paste0(
  "https://monitoramentopde.gestaourbana.prefeitura.sp.gov.br/app/uploads/",
  "msp_alvaras_sisacoe_aprovadigital/",
  "%s_msp_alvaras_sisacoe_aprovadigital.zip"
)

url_exists <- function(url, timeout_seconds = 5) {
  response <- tryCatch(
    httr::HEAD(url, httr::timeout(timeout_seconds)),
    error = function(cnd) NULL
  )
  return(!is.null(response) && httr::status_code(response) == 200)
}

find_latest_alvaras_url <- function(
  start_date = Sys.Date(),
  max_days = 365,
  url_template = alvaras_url_template
) {
  dates <- start_date - seq.int(0, max_days)
  for (date in dates) {
    url <- sprintf(url_template, format(date, "%Y%m%d"))
    if (url_exists(url)) return(url)
  }
  cli::cli_abort(
    "No permit archive was found in the previous {max_days + 1} days."
  )
}

resolve_alvaras_url <- function(config_path = "config/source.yml") {
  config <- yaml::read_yaml(config_path)
  configured_url <- config$url
  if (!is.null(configured_url) && nzchar(configured_url)) {
    if (!url_exists(configured_url)) {
      cli::cli_abort(
        "The pinned municipal archive in {.file {config_path}} is unavailable."
      )
    }
    return(configured_url)
  }

  source_url <- find_latest_alvaras_url(max_days = config$max_days)
  return(source_url)
}

download_alvaras <- function(url, path = "data/raw/alvaras.zip") {
  fs::dir_create(fs::path_dir(path))
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  return(path)
}

write_source_manifest <- function(url, archive, path = "data/raw/source.yml") {
  manifest <- list(
    source_url = url,
    retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    sha256 = digest::digest(file = archive, algo = "sha256")
  )
  yaml::write_yaml(manifest, path)
  return(path)
}
