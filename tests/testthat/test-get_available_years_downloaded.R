context("test get_available_years_downloaded")

test_that("get_available_years_downloaded works", {
  folder <- system.file("extdata", package = "geogenr")
  folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
  ua <- uscb_acs_5ye(folder = folder)
  sa <- ua |> get_statistical_areas()

  # sa[6]
  # [1] "New England City and Town Area Division"

  y <- ua |> get_available_years_downloaded(geodatabase = sa[6])
  expect_equal(y, 2014:2015)
})
