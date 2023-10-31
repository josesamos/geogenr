test_that("get_area_groups()", {
  expect_equal({
    get_area_groups()
  }, {
    c("Legal and Administrative Areas", "Statistical Areas")
  })
})


test_that("get_areas()", {
  res <-     c(
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
    get_areas()
  }, {
    res
  })
  expect_equal({
    get_areas(c("Legal and Administrative Areas", "Statistical Areas"))
  }, {
    res
  })
})


test_that("get_area_years()", {
  expect_equal({
    r <- get_area_years("State")
    intersect(r, 2013:2021)
  }, {
    2013:2021
  })
})


test_that("get_area_file_names()", {
  expect_equal({
    r <- get_area_file_names("State")
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
    r <- get_area_file_names("State", 2017:2020)
  }, {
    c(
      "https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2018ACS/ACS_2018_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2019ACS/ACS_2019_5YR_STATE.gdb.zip",
      "https://www2.census.gov/geo/tiger/TIGER_DP/2020ACS/ACS_2020_5YR_STATE.gdb.zip"
    )
  })
})
