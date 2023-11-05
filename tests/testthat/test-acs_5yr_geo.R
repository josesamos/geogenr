test_that("multiplication works", {
  dir <- tempdir()
  source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
  files <- list.files(source_dir, "*.zip", full.names = TRUE)
  file.copy(from = files,
            to = dir,
            overwrite = TRUE)
  ac <- acs_5yr(dir)
  files <- ac |>
    unzip_files()

  act <- ac |>
    as_acs_5yr_topic("Alaska Native Regional Corporation",
                     "2021",
                     topic = "X01 Age And Sex")

  act <- act |>
    select_report(report = "B01002-Median Age By Sex")

  geo <- act |>
    as_acs_5yr_geo()


  expect_equal({
    names(geo)
  },
  c("origin", "metadata", "data"))

  expect_equal({
    names(geo$metadata)
  },
  c(
    "variable",
    "year",
    "Short_Name",
    "Full_Name",
    "report",
    "subreport",
    "report_var",
    "report_desc",
    "measure",
    "item1",
    "item2",
    "group"
  ))

  expect_equal({
    geo$origin[,-4]
  },
  structure(
    list(
      area = "Alaska Native Regional Corporation",
      area_code = "ANRC",
      year = "2021",
      topic = "X01 Age And Sex",
      topic_code = "X01_AGE_AND_SEX"
    ),
    class = "data.frame",
    row.names = 1L
  ))

  expect_equal({
    layer <- act |>
      get_geo_layer()
    class(layer)
  },
  c("sf", "data.frame"))
})
