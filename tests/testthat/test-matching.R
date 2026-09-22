test_that("sparse matching preserves the five matching rules", {
  permits <- sf::st_sf(
    sql_incra_composto = c("A", "A", "A", "B", "C", "C"),
    area_do_terreno = c(10, 10, 20, 30, 30, 40),
    area_da_construcao = c(50, 60, 70, 80, 90, 100),
    n_unidades = c(1, 2, 3, 4, 5, 6),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(200, 0)),
      sf::st_point(c(50, 0)),
      sf::st_point(c(60, 0)),
      sf::st_point(c(1000, 0)),
      sf::st_point(c(1200, 0)),
      crs = 31983
    )
  )

  pairs <- classify_match_pairs(permits, distance_threshold = 100)
  observed <- stats::setNames(pairs$match_type, paste(pairs$left, pairs$right))

  expect_equal(unname(observed["1 2"]), 2L)
  expect_equal(unname(observed["1 3"]), 3L)
  expect_false("3 4" %in% names(observed))
  expect_equal(unname(observed["5 6"]), 5L)
})

test_that("partial matches are promoted by building area or unit count", {
  permits <- sf::st_sf(
    sql_incra_composto = c("A", "A", "B", "C"),
    area_do_terreno = c(10, 20, 30, 30),
    area_da_construcao = c(50, 50, 80, 90),
    n_unidades = c(1, 2, 4, 4),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(50, 0)),
      sf::st_point(c(500, 0)),
      sf::st_point(c(550, 0)),
      crs = 31983
    )
  )

  pairs <- classify_match_pairs(permits, distance_threshold = 100)
  expect_true(all(pairs$match_type == 1L))
})

test_that("sparse component labels reproduce dense graph labels", {
  pairs <- data.frame(
    left = c(1L, 3L),
    right = c(4L, 5L),
    match_type = c(1L, 1L)
  )
  ids <- component_ids(pairs, number_of_permits = 6)

  dense <- matrix(FALSE, nrow = 6, ncol = 6)
  dense[cbind(pairs$left, pairs$right)] <- TRUE
  dense[cbind(pairs$right, pairs$left)] <- TRUE
  diag(dense) <- TRUE
  graph <- igraph::graph_from_adjacency_matrix(dense, mode = "undirected")
  expected <- igraph::components(graph)$membership

  expect_equal(ids$empreendimento_id_match_1, unname(expected))
})

test_that("matching accepts inputs without candidate pairs", {
  permits <- sf::st_sf(
    sql_incra_composto = c("A", "B"),
    area_do_terreno = c(10, 20),
    area_da_construcao = c(30, 40),
    n_unidades = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1000, 0)),
      crs = 31983
    )
  )

  pairs <- classify_match_pairs(permits, distance_threshold = 100)
  expect_equal(nrow(pairs), 0L)
})
