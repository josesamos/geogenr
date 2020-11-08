context("test get_layer_group_names")

test_that("get_layer_group_names works", {
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

  layers <- ul %>% get_layer_names()
  # layers[3]
  # [1] "X02_RACE"
  ul <- ul %>% get_layer(layers[3])
  layer_groups <- ul %>% get_layer_group_names()

  expect_equal(
    layer_groups,
    c(
      "001 - RACE",
      "003 - DETAILED RACE",
      "008 - WHITE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "009 - BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "010 - AMERICAN INDIAN AND ALASKA NATIVE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "011 - ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "012 - NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "013 - SOME OTHER RACE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES",
      "014 - AMERICAN INDIAN AND ALASKA NATIVE ALONE FOR SELECTED TRIBAL GROUPINGS",
      "015 - ASIAN ALONE BY SELECTED GROUPS",
      "016 - NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE BY SELECTED GROUPS",
      "017 - AMERICAN INDIAN AND ALASKA NATIVE (AIAN) ALONE OR IN ANY COMBINATION BY SELECTED TRIBAL GROUPINGS",
      "018 - ASIAN ALONE OR IN ANY COMBINATION BY SELECTED GROUPS",
      "019 - NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN ANY COMBINATION BY SELECTED GROUPS"
    )
  )
})
