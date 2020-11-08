context("test uscb_folder")

test_that("uscb_folder works", {
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
  uf <- uscb_folder(ul)

  layer_common <- uf %>% get_common_flat_table()

  expect_equal(
    unique(layer_common$year),
    c("2018", "2016")
  )
  expect_equal(
    unique(layer_common$name),
    c("Boston-Cambridge-Newton, MA", "Brockton-Bridgewater-Easton, MA",
      "Framingham, MA", "Haverhill-Newburyport-Amesbury Town, MA-NH",
      "Lawrence-Methuen Town-Salem, MA-NH", "Lowell-Billerica-Chelmsford, MA-NH",
      "Lynn-Saugus-Marblehead, MA", "Nashua, NH-MA", "Peabody-Salem-Beverly, MA",
      "Taunton-Middleborough-Norton, MA")
  )
})
