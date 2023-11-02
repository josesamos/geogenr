



test_that("get_available_areas()", {
  res <-
    c(
      "X01 Age And Sex",
      "X02 Race",
      "X03 Hispanic Or Latino Origin",
      "X04 Ancestry",
      "X05 Foreign Born Citizenship",
      "X06 Place Of Birth",
      "X07 Migration",
      "X08 Commuting",
      "X09 Children Household Relationship",
      "X10 Grandparents Grandchildren",
      "X11 Household Family Subfamilies",
      "X12 Marital Status And History",
      "X13 Fertility",
      "X14 School Enrollment",
      "X15 Educational Attainment",
      "X16 Language Spoken At Home",
      "X17 Poverty",
      "X18 Disability",
      "X19 Income",
      "X20 Earnings",
      "X21 Veteran Status",
      "X22 Food Stamps",
      "X23 Employment Status",
      "X24 Industry Occupation",
      "X25 Housing Characteristics",
      "X26 Group Quarters",
      "X27 Health Insurance",
      "X28 Computer And Internet Use",
      "X99 Imputation"
    )

  dir <- tempdir()
  sub_dir <- snakecase::to_snake_case(paste0(Sys.time()))
  dir.create(file.path(dir, sub_dir))
  dir <- paste0(dir, '/', sub_dir)

  source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
  files <- list.files(source_dir, "*.zip", full.names = TRUE)
  file.copy(from = files,
            to = dir,
            overwrite = TRUE)
  ac <- acs_5yr(dir)
  files <- ac |>
    unzip_files()

  expect_equal({
    areas <- ac |>
      get_available_areas()
  }, {
    c("Alaska Native Regional Corporation")
  })

  expect_equal({
    years <- ac |>
      get_available_area_years(area = "Alaska Native Regional Corporation")
  }, {
    c("2020", "2021")
  })

  expect_equal({
    topics <- ac |>
      get_available_area_topics("Alaska Native Regional Corporation",
                                2021)
  }, {
    res
  })

  expect_equal({
    topics <- ac |>
      get_available_area_topics("Alaska Native Regional Corporation")
  }, {
    res
  })

  expect_equal({
    act <- ac |>
      as_acs_5yr_topic("Alaska Native Regional Corporation",
                       2021,
                       "X01 Age And Sex")
    names <- names(act$files)
    act$files <- basename(act$files)
    names(act$files) <- names
    act
  }, {
    structure(list(
      area = c(`Alaska Native Regional Corporation` = "ANRC"),
      years = 2021,
      topic = c(`X01 Age And Sex` = "X01_AGE_AND_SEX"),
      files = c(`2021` = "ACS_2021_5YR_ANRC.gdb")
    ), class = "acs_5yr_topic")
  })

  expect_equal({
    act <- ac |>
      as_acs_5yr_topic("Alaska Native Regional Corporation",
                       topic = "X01 Age And Sex")
    names <- names(act$files)
    act$files <- basename(act$files)
    names(act$files) <- names
    act
  }, {
    structure(list(
      area = c(`Alaska Native Regional Corporation` = "ANRC"),
      years = c("2020", "2021"),
      topic = c(`X01 Age And Sex` = "X01_AGE_AND_SEX"),
      files = c(`2020` = "ACS_2020_5YR_ANRC.gdb",
                `2021` = "ACS_2021_5YR_ANRC.gdb")
    ), class = "acs_5yr_topic")
  })
})
