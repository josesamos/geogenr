test_that("rolap works", {
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


  expect_equal({
    ft <- act |>
      as_flat_table()
    class(ft)
  },
  "flat_table")

  expect_equal({
    ft <- act |>
      as_flat_table()
    ft$attributes
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
    "GEOID_Data",
    "year",
    "Short_Name",
    "Full_Name",
    "report",
    "subreport",
    "report_desc",
    "item1",
    "item2",
    "group",
    "report_var"
  ))

  expect_equal({
    ft <- act |>
      as_flat_table()
    ft$measures
  },
  c("estimate", "margin_of_error"))

  expect_equal({
    st <- act |>
      as_star_database()
    class(st)
  },
  "star_database")

  expect_equal({
    st <- act |>
      as_star_database()
    names(st$dimensions)
  },
  c("dim_when", "dim_where", "dim_what"))

  expect_equal({
    st <- act |>
      as_star_database()
    names(st$facts)
  },
  "anrc")
})
