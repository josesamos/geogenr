context("test get_layer")

test_that("get_layer works", {
  folder <- system.file("extdata", package = "geogenr")
  folder <-
    stringr::str_replace_all(paste(folder, "/", ""), " ", "")
  ua <- uscb_acs_5ye(folder = folder)
  sa <- ua |> get_statistical_areas()
  # sa[6]
  # [1] "New England City and Town Area Division"
  ul <-
    uscb_layer(
      uscb_acs_metadata,
      ua = ua,
      geodatabase = sa[6],
      year = 2015
    )

  layers <- ul |> get_layer_names()
  # layers[3]
  # [1] "X02_RACE"
  ul <- ul |> get_layer(layers[3])

  expect_equal(ncol(ul$layer), 433)
  expect_equal(nrow(ul$layer), 10)
  expect_equal(ncol(ul$layer_metadata), 14)
  expect_equal(nrow(ul$layer_metadata), 432)
})
