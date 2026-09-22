# Validate data ----

required_raw_columns <- c(
  "n_documento",
  "assunto",
  "unid_aprov",
  "sql",
  "zona_uso",
  "uso_subcat",
  "area_terreno",
  "ac_total",
  "blocos",
  "pavimentos",
  "unid_resid",
  "data_emissao",
  "ano_emissao",
  "coord_x",
  "coord_y",
  "endereco",
  "distrito",
  "subprefeitura"
)

validate_raw_alvaras <- function(raw_permits, call = rlang::caller_env()) {
  missing <- setdiff(required_raw_columns, names(raw_permits))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "The municipal permit schema changed.",
        "x" = "Missing columns: {paste(missing, collapse = ', ')}"
      ),
      call = call
    )
  }
  return(invisible(raw_permits))
}

validate_final_alvaras <- function(developments, call = rlang::caller_env()) {
  if (!inherits(developments, "sf")) {
    cli::cli_abort(
      "{.arg developments} must be an {.cls sf} object.",
      call = call
    )
  }
  if (sf::st_crs(developments)$epsg != 31983) {
    cli::cli_abort("{.arg developments} must use EPSG:31983.", call = call)
  }
  if (nrow(developments) == 0) {
    cli::cli_abort(
      "{.arg developments} must contain at least one row.",
      call = call
    )
  }
  return(invisible(developments))
}

validate_geocoded_alvaras <- function(
  geo_permits,
  city_boundary,
  call = rlang::caller_env()
) {
  if (!inherits(geo_permits, "sf")) {
    cli::cli_abort(
      "{.arg geo_permits} must be an {.cls sf} object.",
      call = call
    )
  }
  if (sf::st_crs(geo_permits)$epsg != 31983) {
    cli::cli_abort("{.arg geo_permits} must use EPSG:31983.", call = call)
  }
  inside_city <- lengths(sf::st_intersects(geo_permits, city_boundary)) > 0
  if (any(!inside_city)) {
    cli::cli_warn(
      "{sum(!inside_city)} geocoded permits fall outside São Paulo."
    )
  }
  return(invisible(geo_permits))
}
