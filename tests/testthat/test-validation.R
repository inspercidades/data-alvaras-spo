test_that("raw validation reports missing municipal fields", {
  expect_error(
    validate_raw_alvaras(data.frame(n_documento = "1")),
    "Missing columns"
  )
})

test_that("final validation requires spatial data in EPSG:31983", {
  permits <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(-46.6, -23.5)), crs = 4326)
  )
  expect_error(validate_final_alvaras(permits), "EPSG:31983")
})
