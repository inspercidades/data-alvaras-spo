# Geocode permits ----

build_geocoding_queries <- function(permits) {
  missing_coordinates <- dplyr::filter(permits, is.na(coord_x))
  eligible_addresses <- dplyr::filter(
    missing_coordinates,
    stringr::str_detect(endereco, "[A-Za-z]"),
    !stringr::str_detect(endereco, "\\b0\\b"),
    !stringr::str_detect(endereco, "\\b999999\\b"),
    stringr::str_detect(endereco, "\\d"),
    !stringr::str_detect(endereco, "\\bKM\\b")
  )

  queries <- dplyr::mutate(
    eligible_addresses,
    endereco_key = dplyr::if_else(
      !is.na(distrito),
      paste0(endereco, " ", distrito, " SAO PAULO SP"),
      paste0(endereco, " ", subprefeitura, " SAO PAULO SP")
    ),
    endereco_key = stringr::str_remove_all(endereco_key, ";|_|/"),
    endereco_key = stringr::str_squish(endereco_key)
  )
  return(queries)
}

read_geocoding_cache <- function(path) {
  if (!fs::file_exists(path)) {
    return(tibble::tibble(
      endereco_key = character(),
      long = double(),
      lat = double()
    ))
  }
  cache <- arrow::read_parquet(path)
  return(cache)
}

update_geocoding_cache <- function(
  queries,
  cache,
  path = "data/processed/geocoding-cache.parquet"
) {
  unique_queries <- dplyr::distinct(queries, endereco_key)
  new_queries <- dplyr::anti_join(unique_queries, cache, by = "endereco_key")

  if (nrow(new_queries) == 0) {
    return(cache)
  }

  geocoded <- tidygeocoder::geocode(
    new_queries,
    address = "endereco_key",
    method = "arcgis",
    verbose = FALSE
  )
  updated_cache <- dplyr::bind_rows(cache, geocoded)
  updated_cache <- dplyr::distinct(
    updated_cache,
    endereco_key,
    .keep_all = TRUE
  )

  fs::dir_create(fs::path_dir(path))
  arrow::write_parquet(updated_cache, path)
  return(updated_cache)
}

geocode_alvaras <- function(
  permits,
  city_boundary,
  cache_path = "data/processed/geocoding-cache.parquet"
) {
  boundary <- sf::st_transform(city_boundary, crs = 31983)

  municipal_coordinates <- dplyr::filter(permits, !is.na(coord_x))
  municipal_points <- sf::st_as_sf(
    municipal_coordinates,
    coords = c("coord_x", "coord_y"),
    crs = 31983
  )

  queries <- build_geocoding_queries(permits)
  cache <- read_geocoding_cache(cache_path)
  cache <- update_geocoding_cache(queries, cache, cache_path)
  geocoded_records <- dplyr::left_join(queries, cache, by = "endereco_key")
  geocoded_records <- dplyr::filter(
    geocoded_records,
    !is.na(long),
    !is.na(lat)
  )
  geocoded_points <- sf::st_as_sf(
    geocoded_records,
    coords = c("long", "lat"),
    crs = 4326
  )
  geocoded_points <- sf::st_transform(geocoded_points, crs = 31983)
  geocoded_points <- dplyr::select(
    geocoded_points,
    -endereco_key,
    -coord_x,
    -coord_y
  )

  geo_permits <- dplyr::bind_rows(municipal_points, geocoded_points)
  validate_geocoded_alvaras(geo_permits, boundary)
  return(geo_permits)
}
