test_that("latest URL discovery formats dates before iteration", {
  checked_urls <- character()
  url_checker <- function(url) {
    checked_urls <<- c(checked_urls, url)
    return(length(checked_urls) == 2)
  }

  result <- find_latest_alvaras_url(
    start_date = as.Date("2026-09-23"),
    max_days = 2,
    url_template = "https://example.test/%s.zip",
    url_checker = url_checker
  )

  expect_identical(result, "https://example.test/20260922.zip")
  expect_identical(
    checked_urls,
    c(
      "https://example.test/20260923.zip",
      "https://example.test/20260922.zip"
    )
  )
})
