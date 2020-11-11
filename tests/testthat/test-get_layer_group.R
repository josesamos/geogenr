context("test get_layer_group")

test_that("get_layer_group works", {
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
  # layers[3]
  # [1] "X02_RACE"
  ul <- ul %>% get_layer(layers[3])
  layer_groups <- ul %>% get_layer_group_names()

  # layer_groups[2]
  # [1] "003 - DETAILED RACE"
  ul <- ul %>% get_layer_group(layer_groups[2])


  expect_equal(
    ul$layer_group_columns,
    c("GEOID", "C02003e1", "C02003m1", "C02003e2", "C02003m2", "C02003e3",
      "C02003m3", "C02003e4", "C02003m4", "C02003e5", "C02003m5", "C02003e6",
      "C02003m6", "C02003e7", "C02003m7", "C02003e8", "C02003m8", "C02003e9",
      "C02003m9", "C02003e10", "C02003m10", "C02003e11", "C02003m11",
      "C02003e12", "C02003m12", "C02003e13", "C02003m13", "C02003e14",
      "C02003m14", "C02003e15", "C02003m15", "C02003e16", "C02003m16",
      "C02003e17", "C02003m17", "C02003e18", "C02003m18", "C02003e19",
      "C02003m19")
  )
  expect_equal(ncol(ul$layer_group_metadata), 13)
  expect_equal(nrow(ul$layer_group_metadata), 38)
})
