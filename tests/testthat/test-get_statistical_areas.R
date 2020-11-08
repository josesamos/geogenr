context("test get_statistical_areas")

test_that("get_statistical_areas works", {
  folder <- "../geodimension/data/us/"
  ua <- uscb_acs_5ye(folder = folder)
  sa <- get_statistical_areas(ua)

  expect_equal(
    sa,
    c(
      "Combined New England City and Town Area",
      "Combined Statistical Area",
      "Metropolitan Division",
      "Metropolitan/Micropolitan Statistical Area",
      "New England City and Town Area",
      "New England City and Town Area Division",
      "Public Use Microdata Area",
      "Tribal Block Group",
      "Tribal Census Tract",
      "Urban Area"
    )
  )
})
