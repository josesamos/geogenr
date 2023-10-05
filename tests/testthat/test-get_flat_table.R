context("test get_basic_flat_table")

test_that("get_basic_flat_table works", {
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
  layer_groups <- ul |> get_layer_group_names()
  # layer_groups[2]
  # [1] "003 - DETAILED RACE"
  ul <- ul |> get_layer_group(layer_groups[2])

  layer <- ul |> get_flat_table()

  expect_equal(
    names(layer),
    c("year", "cnectafp", "nectafp", "nctadvfp", "geoid", "name",
      "namelsad", "lsad", "mtfcc", "aland", "awater", "intptlat", "intptlon",
      "shape_length", "shape_area", "geoid_data", "short_name", "full_name",
      "inf_code", "group_code", "spec_code", "inf", "group", "demographic_race",
      "demographic_race_spec", "demographic_total_population", "demographic_total_population_spec",
      "estimate", "margin_of_error")
  )
  expect_equal(nrow(layer), 190)
})
