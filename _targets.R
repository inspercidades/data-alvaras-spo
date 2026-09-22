# Pipeline definition ----
#
# The pipeline downloads municipal permits, cleans and geocodes individual
# records, and groups them into developments. Functions live in `R/` and are
# loaded with `tar_source()`.
#
# Run with `targets::tar_make()`.

library(targets)

tar_option_set(
  packages = character(),
  format = "rds"
)

tar_source()

list(
  tar_target(source_url, resolve_alvaras_url()),
  tar_target(raw_archive, download_alvaras(source_url), format = "file"),
  tar_target(
    source_manifest,
    write_source_manifest(source_url, raw_archive),
    format = "file"
  ),
  tar_target(raw_alvaras, read_alvaras_archive(raw_archive)),
  tar_target(clean_alvaras_data, clean_alvaras(raw_alvaras)),
  tar_target(
    subprefeituras,
    sf::st_read(
      "data/reference/subprefeituras/SubprefectureMSP_SIRGAS.shp",
      quiet = TRUE
    )
  ),
  tar_target(geo_alvaras, geocode_alvaras(clean_alvaras_data, subprefeituras)),
  tar_target(final_alvaras, identify_developments(geo_alvaras)),
  tar_target(final_parquet, write_final_alvaras(final_alvaras), format = "file")
)
