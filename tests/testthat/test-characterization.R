test_that("version 3 cleaned snapshot retains its contract", {
  path <- snapshot_path("alvaras_pde.parquet")
  skip_if_snapshot_missing(path)
  expected <- yaml::read_yaml(testthat::test_path(
    "..",
    "fixtures",
    "version-3.yml"
  ))$clean
  permits <- arrow::read_parquet(path)

  expect_equal(nrow(permits), expected$rows)
  expect_equal(ncol(permits), expected$columns)
  expect_equal(dplyr::n_distinct(permits$id), expected$unique_ids)
  expect_equal(sum(permits$n_unidades, na.rm = TRUE), expected$units_sum)
})

test_that("version 3 final snapshot retains its contract", {
  path <- snapshot_path("geo_alvaras_emp_pde.parquet")
  skip_if_snapshot_missing(path)
  expected <- yaml::read_yaml(testthat::test_path(
    "..",
    "fixtures",
    "version-3.yml"
  ))$final
  developments <- sfarrow::st_read_parquet(path)

  expect_equal(nrow(developments), expected$rows)
  expect_equal(ncol(developments), expected$columns)
  expect_equal(
    dplyr::n_distinct(developments$id_empreendimento_num),
    expected$unique_development_ids
  )
  expect_equal(sf::st_crs(developments)$epsg, expected$epsg)
  expect_equal(sum(developments$n_unidades, na.rm = TRUE), expected$units_sum)
})
