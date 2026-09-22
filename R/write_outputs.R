# Write outputs ----

write_final_alvaras <- function(
  developments,
  path = "outputs/alvaras.parquet"
) {
  validate_final_alvaras(developments)
  fs::dir_create(fs::path_dir(path))
  sfarrow::st_write_parquet(developments, path)
  return(path)
}
