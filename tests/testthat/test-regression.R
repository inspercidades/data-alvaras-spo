# These tests rebuild version 3 from its supplied inputs. They need the local
# benchmark files and take about a minute, so CI skips them.

test_that("cleaning reproduces the version 3 cleaned snapshot", {
  archive <- snapshot_path("20250422_msp_alvaras_sisacoe_aprovadigital.zip")
  snapshot <- snapshot_path("alvaras_pde.parquet")
  skip_if_snapshot_missing(archive)
  skip_if_snapshot_missing(snapshot)

  raw_permits <- read_alvaras_archive(archive)
  permits <- clean_alvaras(raw_permits)
  expected <- arrow::read_parquet(snapshot)

  expect_equal(as.data.frame(permits), as.data.frame(expected))
})

test_that("development matching reproduces the version 3 final snapshot", {
  geocoded <- snapshot_path("geo_alvaras_pde.parquet")
  snapshot <- snapshot_path("geo_alvaras_emp_pde.parquet")
  skip_if_snapshot_missing(geocoded)
  skip_if_snapshot_missing(snapshot)

  geo_permits <- sfarrow::st_read_parquet(geocoded)
  developments <- identify_developments(geo_permits)
  expected <- sfarrow::st_read_parquet(snapshot)

  expect_equal(
    as.data.frame(sf::st_drop_geometry(developments)),
    as.data.frame(sf::st_drop_geometry(expected))
  )
  expect_equal(sf::st_coordinates(developments), sf::st_coordinates(expected))
  expect_equal(sf::st_crs(developments)$epsg, 31983)
})
