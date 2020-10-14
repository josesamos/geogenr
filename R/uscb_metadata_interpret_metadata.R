
# interpret_metadata ------------------------------------------------------

#' Interpret metadata
#'
#' @param um A string.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
interpret_metadata <- function(um) {
  UseMethod("interpret_metadata")
}


#' @rdname interpret_metadata
#' @export
#' @keywords internal
interpret_metadata.uscb_metadata <- function(um) {
  for (i in seq_along(um$metadata[[1]])) {
    um$metadata[i,] <- interpret_code(um$metadata[i,])

    values <- strsplit(um$metadata$Full_Name[i], ": ")[[1]]
    values <- stringr::str_trim(values, side = "both")
    um$metadata[i,] <- interpret_values(um$metadata[i,], values)
  }
  um$metadata <- Filter(function(x) (!all(x == "")), um$metadata)
  um
}


#' interpret code
#'
#' @param mdr A `tibble` row.
#'
#' @return A `tibble` row.
#'
#' @keywords internal
interpret_code <- function(mdr) {
  short <- strsplit(mdr$Short_Name, "")[[1]]
  mdr$inf_code <- paste(short[1:3], collapse = "")
  mdr$group_code <- paste(short[4:6], collapse = "")
  pos <- which(short %in% c("e", "m"))
  if (pos > 7) {
    mdr$subgroup_code <- paste(short[7:(pos-1)], collapse = "")
  }
  mdr$type_code <- short[pos]
  mdr$spec_code <- paste(short[(pos+1):length(short)], collapse = "")
  if (short[pos] == "e") {
    mdr$Full_Name <- stringr::str_replace(mdr$Full_Name, " -- \\(Estimate\\)", "")
    mdr$type <- "Estimate"
  } else if (short[pos] == "m") {
    mdr$Full_Name <- stringr::str_replace(mdr$Full_Name, " -- \\(Margin of Error\\)", "")
    mdr$type <- "Margin of Error"
  }
  mdr
}


#' interpret code
#'
#' @param mdr A `tibble` row.
#' @param values A vector of values
#'
#' @return A `tibble` row.
#'
#' @keywords internal
interpret_values <- function(mdr, values) {
  vals <- tolower(values)
  vals <- snakecase::to_snake_case(vals, sep_out = "_")

  if (mdr$subgroup_code == "" |
      # Puerto Rico
      mdr$subgroup_code == "PR") {
    mdr$group <- values[1]
  } else {
    # subgroup: content in parentheses
    subgroup <-
      regmatches(values[1], gregexpr("(?<=\\().*?(?=\\))", values[1], perl = T))[[1]]
    if (length(subgroup) > 0) {
      mdr$subgroup <- subgroup[length(subgroup)]
      # group: remove the content of the parentheses
      # mdr$group <- gsub("\\s*\\([^\\)]+\\)", "", values[1])
      mdr$group <-
        stringr::str_replace(values[1], sprintf("\\(%s\\)", mdr$subgroup), "")
    }
  }

  mdr <-
    interpret_general(mdr, vals[2:length(vals)], values[2:length(vals)])
  mdr
}

interpret_general <- function(mdr, vals, values) {
  inf_code <- mdr$inf_code
  for (j in seq_along(vals)) {
    res <- interpret_as_demographic(mdr, vals[j], values[j])
    if (!res$result) {
      switch(
        inf_code,
        "B00" = {
          res <-
            interpret_as_demographic_total_population(mdr, vals[j], values[j])
        },
        "B01" = {
          res <- interpret_as_demographic_race_b01(mdr, vals[j], values[j])
        },
        "B02" = {
          res <- interpret_as_demographic_race_b02(mdr, vals[j], values[j])
        },
        "B03" = {
          res <-
            interpret_as_demographic_hispanic_or_latino_origin(mdr, vals[j], values[j])
        },
        "B04" = {
          res <- interpret_as_social_ancestry(mdr, vals[j], values[j])
        },
        "B05" = {
          res <- interpret_as_social_citizenship_status(mdr, vals[j], values[j])
        }
      )
    }
    mdr <- res$mdr
  }
  mdr
}

#' interpret_as_social_citizenship_status
#'
interpret_as_social_citizenship_status <- function(mdr, val, value) {
  if (substr(val, 1, 6) == "total_") {
    mdr <- add_value_spec(mdr, "demographic_total_population", value)
  } else {
    mdr <- add_value_spec(mdr, "social_citizenship_status", value)
  }

  list(mdr = mdr,
       result = TRUE)
}


#' interpret_as_social_ancestry
#'
interpret_as_social_ancestry <- function(mdr, val, value) {
  if (val %in% c("people_reporting_multiple_ancestries",
                 "people_reporting_single_ancestry")) {
    mdr <- add_value_spec(mdr, "social_ancestry", value, third = TRUE)
  }
  else {
    mdr <- add_value_spec(mdr, "social_ancestry", value)
  }

  list(mdr = mdr,
       result = TRUE)
}


#' interpret_as_demographic_hispanic_or_latino_origin
#'
interpret_as_demographic_hispanic_or_latino_origin <- function(mdr, val, value) {
  mdr <- add_value_spec(mdr, "demographic_hispanic_or_latino_origin", value)

  list(mdr = mdr,
       result = TRUE)
}

#' interpret_as_demographic_race_b02
#'
interpret_as_demographic_race_b02 <- function(mdr, val, value) {
  if (substr(val, 1, 6) == "total_") {
    if (val %in% c("total_groups_tallied")) {
      mdr <- add_value_spec(mdr, "demographic_total_population", value, third = TRUE)
    }
    else if (val %in% c(
      "total_asian_alone_or_in_any_combination_population_the_total_groups_tallied",
      "total_asian_alone_population",
      "total_native_hawaiian_and_other_pacific_islander_alone_population",
      "total_nhpi_alone_or_in_any_combination_population_the_total_groups_tallied"
    )) {
      mdr <- add_value_spec(mdr, "demographic_race", value, second = TRUE)
    }
    else {
      mdr <- add_value_spec(mdr, "demographic_race", value, third = TRUE)
    }
  } else if (val %in% c(
    "people_who_are_american_indian_and_alaska_native_alone_and_people_with_no_tribe_reported",
    "total_aian_alone_or_in_any_combination_population_the_total_groups_tallied"
  )) {
    mdr <- add_value_spec(mdr, "demographic_race", value, third = TRUE)
  } else {
    mdr <- add_value_spec(mdr, "demographic_race", value)
  }

  list(mdr = mdr,
       result = TRUE)
}

#' interpret_as_demographic_race_b01
#'
interpret_as_demographic_race_b01 <- function(mdr, val, value) {
  result <- TRUE
  if (val %in% c(
    "black_or_african_american_alone",
    "hispanic_or_latino_population",
    "people_who_are_american_indian_and_alaska_native_alone",
    "people_who_are_asian_alone",
    "people_who_are_native_hawaiian_and_other_pacific_islander_alone",
    "people_who_are_some_other_race_alone",
    "people_who_are_two_or_more_races",
    "people_who_are_white_alone",
    "white_alone_not_hispanic_or_latino_population"
  )) {
    mdr <- add_value_spec(mdr, "demographic_race", value)
  } else {
    mdr <- add_value_spec(mdr, "rest", value)
    result <- FALSE
  }

  list(mdr = mdr,
       result = TRUE)
}


#' interpret_as_demographic_total_population
#'
interpret_as_demographic_total_population <- function(mdr, val, value) {
  mdr <- add_value_spec(mdr, "demographic_total_population", value)

  list(mdr = mdr,
       result = TRUE)
}


#' interpret_as_demographic
#'
interpret_as_demographic <- function(mdr, val, value) {
  result <- TRUE
  if (val %in% c("total")) {
    mdr <- add_value_spec(mdr, "demographic_total_population", value)
  } else if (val %in% c("total_population")) {
    mdr <- add_value_spec(mdr, "demographic_total_population", value, second = TRUE)
  } else if (val %in% c("male", "female")) {
    mdr <- add_value_spec(mdr, "demographic_sex", value)
  } else if (val %in% c(
    "10_to_14_years",
    "15_to_17_years",
    "18_and_19_years",
    "20_to_24_years",
    "20_years",
    "21_years",
    "22_to_24_years",
    "25_to_29_years",
    "30_to_34_years",
    "35_to_39_years",
    "35_to_44_years",
    "40_to_44_years",
    "45_to_49_years",
    "45_to_54_years",
    "5_to_9_years",
    "50_to_54_years",
    "55_to_59_years",
    "55_to_64_years",
    "60_and_61_years",
    "62_to_64_years",
    "65_and_66_years",
    "65_to_74_years",
    "67_to_69_years",
    "70_to_74_years",
    "75_to_79_years",
    "75_to_84_years",
    "80_to_84_years",
    "85_years_and_over",
    "under_5_years"
  )) {
    mdr <- add_value_spec(mdr, "demographic_age", value)
  } else {
    mdr <- add_value_spec(mdr, "rest", value)
    result <- FALSE
  }
  list(mdr = mdr,
       result = result)
}



interpret_b01 <- function(mdr, val, value) {
  if (val %in% c("male", "female")) {
    mdr$sex <- add_value(mdr$sex, value)
  } else if (substr(val, 1, 5) == "total") {
    mdr <- add_value_spec(mdr, "total", value)
  } else if ((!mdr$inf_code %in% c("C16")) &
             (
               grepl("black_or_african", val, fixed = TRUE) |
               grepl("hispanic_or_latino", val, fixed = TRUE) |
               grepl("american_indian", val, fixed = TRUE) |
               grepl("asian", val, fixed = TRUE) |
               grepl("native_hawaiian", val, fixed = TRUE) |
               grepl("other_race_alone", val, fixed = TRUE) |
               grepl("other_race", val, fixed = TRUE) |
               grepl("one_race", val, fixed = TRUE) |
               grepl("two_race", val, fixed = TRUE) |
               grepl("three_race", val, fixed = TRUE) |
               grepl("four_or_more_races", val, fixed = TRUE) |
               grepl("two_or_more_races", val, fixed = TRUE) |
               grepl("white_alone", val, fixed = TRUE) |
               (
                 grepl("white", val, fixed = TRUE) &
                 mdr$inf_code %in% c("B98", "C02")
               ) |
               grepl("groups_tallied", val, fixed = TRUE) |
               grepl("alaska_native", val, fixed = TRUE) |
               grepl("central_american", val, fixed = TRUE) |
               grepl("south_american", val, fixed = TRUE)
             )) {
    mdr <- add_value_spec(mdr, "human_group", value, double = TRUE)
  } else if (mdr$inf_code %in% c("B02", "B03")) {
    mdr <- add_value_spec(mdr, "human_group", value, double = TRUE)
  } else if (grepl("single_ancestry", val, fixed = TRUE) |
             grepl("multiple_ancestr", val, fixed = TRUE) |
             grepl("ancestry_specified", val, fixed = TRUE) |
             grepl("ancestry_not", val, fixed = TRUE) |
             grepl("ancestry_un", val, fixed = TRUE)) {
    mdr <- add_value_spec(mdr, "ancestry", value, double = TRUE)
  } else if (mdr$inf_code %in% c("B04")) {
    mdr <- add_value_spec(mdr, "ancestry", value, second = TRUE)
  } else if ((!mdr$inf_code %in% c("B99")) &
             (
               grepl("u_s_citizen", val, fixed = TRUE) |
               grepl("naturalized", val, fixed = TRUE) |
               grepl("noncitizen", val, fixed = TRUE) |
               grepl("not_a_citizen", val, fixed = TRUE)
             )) {
    mdr <- add_value_spec(mdr, "u_s_citizen", value)
  } else if ((
    grepl("born_in", val, fixed = TRUE) |
    grepl("born_outside", val, fixed = TRUE) |
    grepl("foreign_born", val, fixed = TRUE) |
    (
      grepl("native", val, fixed = TRUE) &
      !grepl("languages", val, fixed = TRUE)
    )
  ) & (!mdr$inf_code %in% c("B15"))) {
    mdr <- add_value_spec(mdr, "birth", value)
  } else if ((!mdr$inf_code %in% c("B24")) &
             (grepl("child", val, fixed = TRUE) &
              (!grepl("_worker", val, fixed = TRUE)) &
              (!grepl("income", val, fixed = TRUE)))) {
    mdr <- add_value_spec(mdr, "children", value, double = TRUE)
  } else if ((!mdr$inf_code %in% c("B24", "B27", "B29")) &
             ((
               grepl("married_couple", val, fixed = TRUE) |
               grepl("male_householder", val, fixed = TRUE) |
               grepl("famil", val, fixed = TRUE) |
               grepl("other_living", val, fixed = TRUE) |
               grepl("living_a", val, fixed = TRUE) |
               grepl("relatives", val, fixed = TRUE) |
               grepl("spouse", val, fixed = TRUE) |
               grepl("husband", val, fixed = TRUE) |
               grepl("male_householder", val, fixed = TRUE) |
               grepl("household", val, fixed = TRUE)
             ) &
             (!grepl("_worker", val, fixed = TRUE)) &
             (!grepl("income", val, fixed = TRUE))
             )) {
    mdr <- add_value_spec(mdr, "household", value, double = TRUE)
  } else if ((!mdr$inf_code %in% c("B24")) &
             ((grepl("entered_", val, fixed = TRUE) &
               (
                 grepl("_to_", val, fixed = TRUE) |
                 grepl("_before", val, fixed = TRUE) |
                 grepl("_or_later", val, fixed = TRUE)
               )) |
              grepl("living_with_", val, fixed = TRUE) |
              grepl("under_1_00", val, fixed = TRUE) |
              grepl("1_00_to_1_99", val, fixed = TRUE) |
              grepl("2_00_and_over", val, fixed = TRUE) |
              (grepl("naturalized_", val, fixed = TRUE) &
               (
                 grepl("_to_", val, fixed = TRUE) |
                 grepl("before_", val, fixed = TRUE)
               )) |
              grepl("own_children_", val, fixed = TRUE) |
              grepl("_years_and_over_in_", val, fixed = TRUE) |
              (grepl("_poverty", val, fixed = TRUE) &
               (!grepl(
                 "income", val, fixed = TRUE
               ))) |
              grepl("some_other_race_population_in_puerto_rico", val, fixed = TRUE) |
              (mdr$inf_code %in% c("B13") &
               substr(val, 1, 6) == "women_")
             )) {
    mdr <- add_value_spec(mdr, "condition", value, double = TRUE)
  } else if (((
    grepl("under_", val, fixed = TRUE) |
    grepl("_to_", val, fixed = TRUE) |
    grepl("_and_", val, fixed = TRUE)
  ) &
  grepl("_years", val, fixed = TRUE) &
  (!grepl("workers_", val, fixed = TRUE)) &
  (!grepl("citizens_", val, fixed = TRUE)) &
  (!grepl("grandparent_", val, fixed = TRUE)) &
  (!grepl("population_", val, fixed = TRUE)) &
  (!grepl("related_children", val, fixed = TRUE)) &
  (!grepl("civilian_veterans", val, fixed = TRUE)) &
  (!grepl("females_20", val, fixed = TRUE)) &
  (!grepl("no_children_under", val, fixed = TRUE)) &
  (!mdr$inf_code %in% c("B10"))
  ) |
  val %in% c("20_years", "21_years", "5_years", "15_years")) {
    mdr$age <- add_value(mdr$age, value)
  } else if ((!mdr$inf_code %in% c("B24", "C15")) &
             (grepl("population_", val, fixed = TRUE) |
              grepl("citizens_", val, fixed = TRUE))) {
    mdr <- add_value_spec(mdr, "condition", value, double = TRUE)
  } else if (mdr$inf_code %in% c("B05")) {
    mdr <- add_value_spec(mdr, "place_of_birth", value)
  } else if (grepl("income", val, fixed = TRUE)) {
    mdr <- add_value_spec(mdr, "income", value)
  } else if (grepl("_000_or_more", val, fixed = TRUE) |
             grepl("_000_to_", val, fixed = TRUE) |
             grepl("1_to_9_999_", val, fixed = TRUE) |
             grepl("_499", val, fixed = TRUE) |
             grepl("_999", val, fixed = TRUE) |
             grepl("less_than_10_000", val, fixed = TRUE) |
             grepl("$", value, fixed = TRUE)) {
    mdr <- add_value_spec(mdr, "money", value)
  } else if ((!mdr$inf_code %in% c("B09")) &
             (
               grepl("divorced", val, fixed = TRUE) |
               grepl("married", val, fixed = TRUE) |
               grepl("separated", val, fixed = TRUE) |
               grepl("widowed", val, fixed = TRUE)
             )) {
    mdr <- add_value_spec(mdr, "marital_status", value)
  } else if (grepl("speak_only_", val, fixed = TRUE) |
             grepl("speak_other_", val, fixed = TRUE) |
             grepl("speak_spanish", val, fixed = TRUE) |
             grepl("speak_language", val, fixed = TRUE)) {
    mdr <- add_value_spec(mdr, "languages", value)
  } else if (substr(val, 1, 14) == "speak_english_") {
    mdr$english <- add_value(mdr$english, value)
  } else if ((!mdr$inf_code %in% c("B24")) &
             (
               grepl("_degree", val, fixed = TRUE) |
               grepl("_graduate", val, fixed = TRUE) |
               grepl("graduate_", val, fixed = TRUE) |
               grepl("_th_grade", val, fixed = TRUE) |
               grepl("enrolled_in_", val, fixed = TRUE) |
               grepl("_school", val, fixed = TRUE)
             )) {
    mdr <- add_value_spec(mdr, "studies", value)
  } else if ((
    grepl("agriculture", val, fixed = TRUE) |
    grepl("construction", val, fixed = TRUE) |
    grepl("manufacturing", val, fixed = TRUE) |
    grepl("_trade", val, fixed = TRUE) |
    grepl("transportation_and", val, fixed = TRUE) |
    grepl("information", val, fixed = TRUE) |
    grepl("finance_and", val, fixed = TRUE) |
    grepl("professional", val, fixed = TRUE) |
    grepl("educational", val, fixed = TRUE) |
    grepl("arts_", val, fixed = TRUE) |
    grepl("other_services", val, fixed = TRUE) |
    grepl("_occupations", val, fixed = TRUE) |
    grepl("public_administration", val, fixed = TRUE) |
    grepl("_worker", val, fixed = TRUE) |
    grepl("worked_", val, fixed = TRUE) |
    grepl("_labor_force", val, fixed = TRUE) |
    grepl("employed", val, fixed = TRUE) |
    grepl("did_not_work", val, fixed = TRUE) |
    grepl("veteran", val, fixed = TRUE) |
    grepl("armed_forces", val, fixed = TRUE)
  ) &
  (!mdr$inf_code %in% c("B15", "B24", "B25", "B99", "C15", "C24"))) {
    mdr <- add_value_spec(mdr, "activity", value, double = TRUE)
  } else if (grepl("living_", val, fixed = TRUE) &
             (!mdr$inf_code %in% c("B07", "B17", "B18"))) {
    mdr <- add_value_spec(mdr, "residence", value)
  } else if (grepl("_vehicle_available", val, fixed = TRUE) |
             grepl("_vehicles_available", val, fixed = TRUE) |
             grepl("_vehicles_available", val, fixed = TRUE) |
             grepl("car_truck_or_van", val, fixed = TRUE)) {
    mdr <- add_value_spec(mdr, "vehicles", value, double = TRUE)
  } else if (grepl("_difficulty", val, fixed = TRUE) |
             grepl("_disability", val, fixed = TRUE)) {
    mdr$disability <- add_value(mdr$disability, value)
  } else if ((!mdr$inf_code %in% c("B24")) &
             (
               grepl("_insurance_coverage", val, fixed = TRUE) |
               grepl("_health_coverage", val, fixed = TRUE) |
               grepl("_health_insurance", val, fixed = TRUE) |
               grepl("_public_coverage", val, fixed = TRUE) |
               grepl("_medicare_coverage", val, fixed = TRUE) |
               grepl("_health_care", val, fixed = TRUE) |
               grepl("_purchase_coverage", val, fixed = TRUE) |
               grepl("_coverage_combinations", val, fixed = TRUE) |
               grepl("_only_combinations", val, fixed = TRUE)
             )) {
    mdr <- add_value_spec(mdr, "insurance_coverage", value)
  } else if (mdr$inf_code %in% c("B08")) {
    if (grepl("workers_", val, fixed = TRUE)) {
      mdr <- add_value_spec(mdr, "condition", value, double = TRUE)
    } else {
      mdr <- add_value_spec(mdr, "transportation_to_work", value, double = TRUE)
    }
  } else if (mdr$inf_code %in% c("B24", "C24")) {
    mdr <- add_value_spec(mdr, "activity", value, double = TRUE)
  } else if (mdr$inf_code %in% c(
    "B07",
    "B09",
    "B10",
    "B12",
    "B13",
    "B14",
    "B17",
    "B18",
    "B19",
    "B20",
    "B21",
    "B23",
    "B26",
    "B27",
    "B29",
    "C17",
    "C18",
    "C23",
    "C27"
  )) {
    mdr <- add_value_spec(mdr, "condition", value, double = TRUE)
  } else if (mdr$inf_code %in% c("B15", "C15")) {
    mdr <- add_value_spec(mdr, "studies", value)
  } else if (mdr$inf_code %in% c("B16", "C16")) {
    mdr$languages <- add_value(mdr$languages, value)
  } else if (mdr$inf_code %in% c("B00", "B25", "B98", "B99")) {
    if (grepl("housing_unit", val, fixed = TRUE)) {
      mdr <- add_value_spec(mdr, "housing_units", value)
    } else if (mdr$inf_code %in% c("B98", "B99")) {
      mdr <- add_value_spec(mdr, "condition", value, double = TRUE)
    } else {
      mdr <- add_value_spec(mdr, "housing", value)
    }
  } else if (mdr$inf_code %in% c("B28")) {
    mdr <- add_value_spec(mdr, "computers", value)
  } else if (mdr$inf_code %in% c("B11", "B22")) {
    mdr <- add_value_spec(mdr, "household", value, double = TRUE)
  }
  else {
    mdr$rest <- add_value(mdr$rest, val)
  }
  mdr
}

add_value <- function(field, value, sep = ": ") {
  if (field == "") {
    field <- value
  } else {
    field <- paste(field, value, sep = sep)
  }
  field
}

add_value_spec <-
  function(mdr,
           field,
           value,
           double = TRUE,
           second = FALSE,
           third = FALSE,
           sep = ": ") {
    field_spec2 <- sprintf("%s_spec2", field)
    if (third) {
      if (mdr[, field_spec2] == "") {
        mdr[, field_spec2] <- value
      } else {
        mdr[, field_spec2] <- paste(mdr[, field_spec2], value, sep = sep)
      }
    } else {
      if (second) {
        double = TRUE
      }
      field_spec <- sprintf("%s_spec", field)
      if (mdr[, field] == ""  & (!second)) {
        mdr[, field] <- value
      } else if (mdr[, field_spec] == "") {
        mdr[, field_spec] <- value
      } else {
        if (double) {
          if (mdr[, field_spec2] == "") {
            mdr[, field_spec2] <- value
          } else {
            mdr[, field_spec2] <- paste(mdr[, field_spec2], value, sep = sep)
          }
        } else {
          mdr[, field_spec] <- paste(mdr[, field_spec], value, sep = sep)
        }
      }
    }
    mdr
  }
