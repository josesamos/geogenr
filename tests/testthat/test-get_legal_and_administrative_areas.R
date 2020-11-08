context("test get_legal_and_administrative_areas")

test_that("get_legal_and_administrative_areas works", {
  folder <- "../geodimension/data/us/"
  ua <- uscb_acs_5ye(folder = folder)
  laa <- get_legal_and_administrative_areas(ua)

  expect_equal(
    laa,
    c(
      "Alaska Native Regional Corporation",
      "American Indian/Alaska Native/Native Hawaiian Area",
      "Congressional District (116th Congress)",
      "County",
      "Elementary School District",
      "Place",
      "Secondary School District",
      "State",
      "State Legislative Districts - Lower Chamber",
      "State Legislative Districts - Upper Chamber",
      "Unified School District",
      "Zip Code Tabulation Area"
    )
  )
})
