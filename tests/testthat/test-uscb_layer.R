context("test uscb_layer")

test_that("uscb_layer works", {
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
      year = 2018
    )

  expect_equal(
    names(ul),
    c(
      "metadata",
      "ua",
      "geodatabase",
      "year",
      "filepath",
      "layer_names",
      "layer_name",
      "layer",
      "layer_metadata",
      "layer_group_names",
      "layer_group_name",
      "layer_group_columns",
      "layer_group_metadata"
    )
  )
})
