test_that("geo works", {
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
    layer <- geo |>
      get_geo_layer()
    class(layer)
  },
  c("sf", "data.frame"))

  expect_equal({
    layer <- geo |>
      get_metadata()
    c(names(layer), nrow(layer))
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
    "group",
    "60"
  ))

  expect_equal({
    metadata <- geo |>
      get_metadata()
    metadata <- dplyr::filter(metadata, item2 == "Female")
    geo2 <- geo |>
      set_metadata(metadata)
    names(geo2$data)
  },
  c(
    "STATEFP",
    "ANRCFP",
    "ANRCNS",
    "GEOID",
    "NAME",
    "NAMELSAD",
    "LSAD",
    "CLASSFP",
    "MTFCC",
    "FUNCSTAT",
    "ALAND",
    "AWATER",
    "INTPTLAT",
    "INTPTLON",
    "Shape_Length",
    "Shape_Area",
    "GEOID_Data",
    "V03",
    "V06",
    "V09",
    "V12",
    "V15",
    "V18",
    "V21",
    "V24",
    "V27",
    "V30",
    "V33",
    "V36",
    "V39",
    "V42",
    "V45",
    "V48",
    "V51",
    "V54",
    "V57",
    "V60",
    "Shape"
  ))

  expect_equal({
    dir <- tempdir()
    file <- geo |>
      as_GeoPackage(dir)
    basename(file)
  },
  "ANRC.gpkg")

  expect_equal({
    dir <- tempdir()
    file <- geo |>
      as_GeoPackage(dir, "test")
    basename(file)
  },
  "test.gpkg")
})
