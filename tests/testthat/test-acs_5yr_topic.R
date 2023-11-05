test_that("as_acs_5yr_topic()", {
  res <-
    c(
      `X01 Age And Sex` = "X01_AGE_AND_SEX",
      `X02 Race` = "X02_RACE",
      `X03 Hispanic Or Latino Origin` = "X03_HISPANIC_OR_LATINO_ORIGIN",
      `X04 Ancestry` = "X04_ANCESTRY",
      `X05 Foreign Born Citizenship` = "X05_FOREIGN_BORN_CITIZENSHIP",
      `X06 Place Of Birth` = "X06_PLACE_OF_BIRTH",
      `X07 Migration` = "X07_MIGRATION",
      `X08 Commuting` = "X08_COMMUTING",
      `X09 Children Household Relationship` = "X09_CHILDREN_HOUSEHOLD_RELATIONSHIP",
      `X10 Grandparents Grandchildren` = "X10_GRANDPARENTS_GRANDCHILDREN",
      `X11 Household Family Subfamilies` = "X11_HOUSEHOLD_FAMILY_SUBFAMILIES",
      `X12 Marital Status And History` = "X12_MARITAL_STATUS_AND_HISTORY",
      `X13 Fertility` = "X13_FERTILITY",
      `X14 School Enrollment` = "X14_SCHOOL_ENROLLMENT",
      `X15 Educational Attainment` = "X15_EDUCATIONAL_ATTAINMENT",
      `X16 Language Spoken At Home` = "X16_LANGUAGE_SPOKEN_AT_HOME",
      `X17 Poverty` = "X17_POVERTY",
      `X18 Disability` = "X18_DISABILITY",
      `X19 Income` = "X19_INCOME",
      `X20 Earnings` = "X20_EARNINGS",
      `X21 Veteran Status` = "X21_VETERAN_STATUS",
      `X22 Food Stamps` = "X22_FOOD_STAMPS",
      `X23 Employment Status` = "X23_EMPLOYMENT_STATUS",
      `X24 Industry Occupation` = "X24_INDUSTRY_OCCUPATION",
      `X25 Housing Characteristics` = "X25_HOUSING_CHARACTERISTICS",
      `X26 Group Quarters` = "X26_GROUP_QUARTERS",
      `X27 Health Insurance` = "X27_HEALTH_INSURANCE",
      `X28 Computer And Internet Use` = "X28_COMPUTER_AND_INTERNET_USE",
      `X99 Imputation` = "X99_IMPUTATION"
    )

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
                     topic = "X01 Age And Sex")

  expect_equal({
    topics <- act |>
      get_topic_name()
  },
  "X01 Age And Sex")

  expect_equal({
    topics <- act |>
      get_names_of_other_topics()
  },
  names(res[-1]))

  expect_equal({
    act2 <- act |>
      select_topic(topic = "X03 Hispanic Or Latino Origin")
    act2$topic
  },
  c(`X03 Hispanic Or Latino Origin` = "X03_HISPANIC_OR_LATINO_ORIGIN"))

  expect_equal({
    act |>
      get_report_names()
  },
  c(
    "B01001-Sex By Age",
    "B01002-Median Age By Sex",
    "B01003-Total Population"
  ))

  expect_equal({
    act |>
      get_subreport_names()
  },
  c(
    "B01001---Sex By Age",
    "B01001-A-Sex By Age (White Alone)",
    "B01001-B-Sex By Age (Black Or African American Alone)",
    "B01001-C-Sex By Age (American Indian And Alaska Native Alone)",
    "B01001-D-Sex By Age (Asian Alone)",
    "B01001-E-Sex By Age (Native Hawaiian And Other Pacific Islander Alone)",
    "B01001-F-Sex By Age (Some Other Race Alone)",
    "B01001-G-Sex By Age (Two Or More Races)",
    "B01001-H-Sex By Age (White Alone, Not Hispanic Or Latino)",
    "B01001-I-Sex By Age (Hispanic Or Latino)",
    "B01002---Median Age By Sex",
    "B01002-A-Median Age By Sex (White Alone)",
    "B01002-B-Median Age By Sex (Black Or African American Alone)",
    "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)",
    "B01002-D-Median Age By Sex (Asian Alone)",
    "B01002-E-Median Age By Sex (Native Hawaiian And Other Pacific Islander Alone)",
    "B01002-F-Median Age By Sex (Some Other Race Alone)",
    "B01002-G-Median Age By Sex (Two Or More Races)",
    "B01002-H-Median Age By Sex (White Alone, Not Hispanic Or Latino)",
    "B01002-I-Median Age By Sex (Hispanic Or Latino)",
    "B01003---Total Population"
  ))

  expect_equal({
    act |>
      get_subreport_names()
  },
  c(
    "B01001---Sex By Age",
    "B01001-A-Sex By Age (White Alone)",
    "B01001-B-Sex By Age (Black Or African American Alone)",
    "B01001-C-Sex By Age (American Indian And Alaska Native Alone)",
    "B01001-D-Sex By Age (Asian Alone)",
    "B01001-E-Sex By Age (Native Hawaiian And Other Pacific Islander Alone)",
    "B01001-F-Sex By Age (Some Other Race Alone)",
    "B01001-G-Sex By Age (Two Or More Races)",
    "B01001-H-Sex By Age (White Alone, Not Hispanic Or Latino)",
    "B01001-I-Sex By Age (Hispanic Or Latino)",
    "B01002---Median Age By Sex",
    "B01002-A-Median Age By Sex (White Alone)",
    "B01002-B-Median Age By Sex (Black Or African American Alone)",
    "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)",
    "B01002-D-Median Age By Sex (Asian Alone)",
    "B01002-E-Median Age By Sex (Native Hawaiian And Other Pacific Islander Alone)",
    "B01002-F-Median Age By Sex (Some Other Race Alone)",
    "B01002-G-Median Age By Sex (Two Or More Races)",
    "B01002-H-Median Age By Sex (White Alone, Not Hispanic Or Latino)",
    "B01002-I-Median Age By Sex (Hispanic Or Latino)",
    "B01003---Total Population"
  ))

  expect_equal({
    act |>
      get_subreport_names(report = "B01002-Median Age By Sex")
  },
  c(
    "B01002---Median Age By Sex",
    "B01002-A-Median Age By Sex (White Alone)",
    "B01002-B-Median Age By Sex (Black Or African American Alone)",
    "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)",
    "B01002-D-Median Age By Sex (Asian Alone)",
    "B01002-E-Median Age By Sex (Native Hawaiian And Other Pacific Islander Alone)",
    "B01002-F-Median Age By Sex (Some Other Race Alone)",
    "B01002-G-Median Age By Sex (Two Or More Races)",
    "B01002-H-Median Age By Sex (White Alone, Not Hispanic Or Latino)",
    "B01002-I-Median Age By Sex (Hispanic Or Latino)"
  ))

  expect_equal({
    act2 <- anrc_2021_x01 |>
      select_report(report = "B01002-Median Age By Sex")
    reports <- act2 |>
      get_report_names()
  },
  "B01002-Median Age By Sex")

  expect_equal({
    act2 <- anrc_2021_x01 |>
      select_subreport(
        c(
          "B01002-B-Median Age By Sex (Black Or African American Alone)",
          "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)"
        )
      )
    reports <- act2 |>
      get_report_names()
  },
  character(0))

  expect_equal({
    act2 <- anrc_2021_x01 |>
      select_subreport(
        c(
          "B01002-B-Median Age By Sex (Black Or African American Alone)",
          "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)"
        )
      )
    reports <- act2 |>
      get_subreport_names()
  },
  c(
    "B01002-B-Median Age By Sex (Black Or African American Alone)",
    "B01002-C-Median Age By Sex (American Indian And Alaska Native Alone)"
  ))

  expect_equal({
    act2 |>
      get_geo_attribute_names()
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
    "GEOID_Data"
  ))

  expect_equal({
    layer <- act2 |>
      get_geo_layer()
    class(layer)
  },
  c("sf", "data.frame"))
})
