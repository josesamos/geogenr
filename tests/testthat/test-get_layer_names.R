context("test get_layer_names")

test_that("get_layer_names works", {
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
  expect_equal(
    layers,
    c(
      "X00_COUNTS",
      "X01_AGE_AND_SEX",
      "X02_RACE",
      "X03_HISPANIC_OR_LATINO_ORIGIN",
      "X04_ANCESTRY",
      "X05_FOREIGN_BORN_CITIZENSHIP",
      "X06_PLACE_OF_BIRTH",
      "X07_MIGRATION",
      "X08_COMMUTING",
      "X09_CHILDREN_HOUSEHOLD_RELATIONSHIP",
      "X10_GRANDPARENTS_GRANDCHILDREN",
      "X11_HOUSEHOLD_FAMILY_SUBFAMILIES",
      "X12_MARITAL_STATUS_AND_HISTORY",
      "X13_FERTILITY",
      "X14_SCHOOL_ENROLLMENT",
      "X15_EDUCATIONAL_ATTAINMENT",
      "X16_LANGUAGE_SPOKEN_AT_HOME",
      "X17_POVERTY",
      "X18_DISABILITY",
      "X19_INCOME",
      "X20_EARNINGS",
      "X21_VETERAN_STATUS",
      "X22_FOOD_STAMPS",
      "X23_EMPLOYMENT_STATUS",
      "X24_INDUSTRY_OCCUPATION",
      "X25_HOUSING_CHARACTERISTICS",
      "X26_GROUP_QUARTERS",
      "X27_HEALTH_INSURANCE",
      "X28_COMPUTER_AND_INTERNET_USE",
      "X29_VOTING_AGE_POPULATION",
      "X98_UNWEIGHTED_HOUSING_UNIT_SAMPLE",
      "X99_IMPUTATION"
    )
  )
})
