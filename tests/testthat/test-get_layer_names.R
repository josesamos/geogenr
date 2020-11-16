context("test get_layer_names")

test_that("get_layer_names works", {
  folder <- system.file("extdata", package = "geogenr")
  folder <-
    stringr::str_replace_all(paste(folder, "/", ""), " ", "")
  ua <- uscb_acs_5ye(folder = folder)
  sa <- ua %>% get_statistical_areas()
  # sa[6]
  # [1] "New England City and Town Area Division"
  ul <-
    uscb_layer(
      uscb_acs_metadata,
      ua = ua,
      geodatabase = sa[6],
      year = 2015
    )

  layers <- ul %>% get_layer_names()
  expect_equal(
    layers,
    c("X00_COUNTS", "X01_AGE_AND_SEX", "X02_RACE")
  )
})
