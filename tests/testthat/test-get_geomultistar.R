context("test get_geomultistar")

test_that("get_geomultistar works", {
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
  # layer_groups[2]
  # [1] "003 - DETAILED RACE"
  ul <- ul %>% get_layer_group(layer_groups[2])

  gms <- ul %>% get_geomultistar()

  expect_equal(
    names(gms),
    c("fact", "dimension", "geodimension")
  )
  expect_equal(
    names(gms$fact),
    c("detailed_race")
  )
  expect_equal(
    names(gms$dimension),
    c("when", "where", "what")
  )
  expect_equal(
    names(gms$geodimension$where),
    c("all_where", "cnectafp", "nectafp", "nctadvfp", "geoid", "name",
      "namelsad", "lsad", "mtfcc", "aland", "awater", "intptlat", "intptlon",
      "shape_length", "shape_area", "geoid_data")
  )
})
