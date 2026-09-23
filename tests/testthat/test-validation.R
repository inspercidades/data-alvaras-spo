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

test_that("geocoding validation warns about coarse matches", {
  boundary <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_buffer(sf::st_point(c(330000, 7390000)), 1000),
      crs = 31983
    )
  )
  permits <- sf::st_sf(
    endereco = c("R EXEMPLO 100", "ES EXEMPLO 200"),
    geocode_tipo = c("PointAddress", "Locality"),
    geometry = sf::st_sfc(
      sf::st_point(c(330000, 7390000)),
      sf::st_point(c(330100, 7390000)),
      crs = 31983
    )
  )
  expect_warning(
    validate_geocoded_alvaras(permits, boundary),
    "1 permit was geocoded coarser"
  )
})
