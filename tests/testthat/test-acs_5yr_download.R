

test_that("acs_5yr()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    r <- names(ac)
  }, {
    c("dir", "acs_5yr_md", "selected_files")
  })
})


test_that("get_area_groups()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    r <- ac |>
      get_area_groups()
  }, {
    c("Legal and Administrative Areas", "Statistical Areas")
  })
})


test_that("get_areas()", {
  dir <- system.file("extdata/acs_5yr", package = "geogenr")
  ac <- acs_5yr(dir = dir)
  res <- c(
    "American Indian/Alaska Native/Native Hawaiian Area",
    "Alaska Native Regional Corporation",
    "Congressional District (116th Congress)",
    "County",
    "Place",
    "Elementary School District",
    "Secondary School District",
    "Unified School District",
    "State",
    "State Legislative Districts Upper Chamber",
    "State Legislative Districts Lower Chamber",
    "Code Tabulation Area",
    "Tribal Block Group",
    "Tribal Census Tract",
    "New England City and Town Area",
    "New England City and Town Area Division",
    "Combined New England City and Town Area",
    "Metropolitan/Micropolitan Statistical Area",
    "Metropolitan Division",
    "Combined Statistical Area",
    "Public Use Microdata Area",
    "Urban Area"
  )
  expect_equal({
    r <- ac |>
      get_areas()
  }, {
    res
  })
  expect_equal({
    r <- ac |>
      get_areas(c("Legal and Administrative Areas", "Statistical Areas"))
  }, {
    res
  })
})


test_that("get_area_years()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    r <- ac |>
      get_area_years("State")
    intersect(r, 2013:2021)
  }, {
    as.character(2013:2021)
  })
})

test_that("get_area_file_names()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    r <- ac |>
      get_area_file_names("State")
  }, {
    c(
      "https://www2.census.gov/geo/tiger/TIGER_DP/2012ACS/ACS_2012_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2013ACS/ACS_2013_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2014ACS/ACS_2014_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2015ACS/ACS_2015_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2016ACS/ACS_2016_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2020ACS/ACS_2020_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2021ACS/ACS_2021_5YR_STATE.gdb.zip"
    )
  })
})


test_that("get_area_file_names()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    r <- ac |>
      get_area_file_names("State", 2017:2020)
  }, {
    c(
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2020ACS/ACS_2020_5YR_STATE.gdb.zip"
    )
  })
})


test_that("select_area_files(), get_selected_file_names()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    ac <- ac |>
      select_area_files("State") |>
      get_selected_file_names()
  }, {
    c(
      "https://www2.census.gov/geo/tiger/TIGER_DP/2012ACS/ACS_2012_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2013ACS/ACS_2013_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2014ACS/ACS_2014_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2015ACS/ACS_2015_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2016ACS/ACS_2016_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2020ACS/ACS_2020_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2021ACS/ACS_2021_5YR_STATE.gdb.zip"
    )
  })
})


test_that("select_area_files(), get_selected_file_names()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    ac <- ac |>
      select_area_files("State", 2017:2020) |>
      get_selected_file_names()
  }, {
    c(
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2020ACS/ACS_2020_5YR_STATE.gdb.zip"
    )
  })
})


test_that("get_too_heavy_file_names()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir = dir)
    ac <- ac |>
      get_too_heavy_file_names()
  }, {
    c("https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_PLACE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_PLACE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_PLACE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_ZCTA.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_ZCTA.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_ZCTA.gdb.zip"
    )
  })
})


test_that("download_selected_files()", {
  expect_equal({
    dir <- system.file("extdata/acs_5yr", package = "geogenr")
    ac <- acs_5yr(dir)

    ac <- ac |>
      select_area_files("Alaska Native Regional Corporation", 2020:2021)

    files <- ac |>
      download_selected_files(unzip = FALSE)
  }, {
    NULL
  })
})


test_that("unzip_files()", {
  expect_equal({
    dir <- tempdir()
    source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
    files <- list.files(source_dir, "*.zip", full.names = TRUE)
    file.copy(from=files, to=dir, overwrite = TRUE)
    ac <- acs_5yr(dir)

    files <- ac |>
      unzip_files(delete_zip = TRUE)
    r <- basename(files)
  }, {
    c("ACS_2020_5YR_ANRC.gdb", "ACS_2021_5YR_ANRC.gdb")
  })
})


test_that("unzip_files()", {
  expect_equal({
    dir <- tempdir()
    source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
    files <- list.files(source_dir, "*.zip", full.names = TRUE)
    file.copy(from=files, to=dir, overwrite = TRUE)
    ac <- acs_5yr(dir)

    files <- ac |>
      unzip_files(subdir = 'year')
    dir <- gsub("\\\\", "/", dir)
    r <- gsub(dir, "", files)
  }, {
    c("/2020/ACS_2020_5YR_ANRC.gdb", "/2021/ACS_2021_5YR_ANRC.gdb"
    )
  })
})


test_that("unzip_files()", {
  expect_equal({
    dir <- tempdir()
    source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
    files <- list.files(source_dir, "*.zip", full.names = TRUE)
    file.copy(from=files, to=dir, overwrite = TRUE)
    ac <- acs_5yr(dir)

    files <- ac |>
      unzip_files(subdir = 'area')
    dir <- gsub("\\\\", "/", dir)
    r <- gsub(dir, "", files)
  }, {
    c("/ANRC/ACS_2020_5YR_ANRC.gdb", "/ANRC/ACS_2021_5YR_ANRC.gdb"
    )
  })
})



