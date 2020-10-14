
# transform_metadata ------------------------------------------------------

#' Transform metadata
#'
#' @param ta A string.
#'
#' @return A `tiger_acs` object.
#'
#' @keywords internal
transform_metadata <- function(ta) {
  UseMethod("transform_metadata")
}


#' @rdname transform_metadata
#' @export
#' @keywords internal
transform_metadata.tiger_acs <- function(ta) {
  for (i in seq_along(ta$metadata[[1]])) {
    ta$metadata[i,] <- transform_code(ta$metadata[i,])

    values <- strsplit(ta$metadata$Full_Name[i], ": ")[[1]]
    values <- stringr::str_trim(values, side = "both")
    ta$metadata[i,] <- transform_values(ta$metadata[i,], values)
  }
  ta
}


#' Transform code
#'
#' @param mdr A `tibble` row.
#'
#' @return A `tibble` row.
#'
#' @keywords internal
transform_code <- function(mdr) {
  i <- 1
  short <- strsplit(mdr$Short_Name[i], "")[[1]]
  mdr$inf_code[i] <- paste(short[1:3], collapse = "")
  mdr$group_code[i] <- paste(short[4:6], collapse = "")
  pos <- which(short %in% c("e", "m"))
  if (pos > 7) {
    mdr$subgroup_code[i] <- paste(short[7:(pos-1)], collapse = "")
  }
  mdr$type_code <- short[pos]
  mdr$spec_code[i] <- paste(short[(pos+1):length(short)], collapse = "")
  if (short[pos] == "e") {
    mdr$Full_Name[i] <- stringr::str_replace(mdr$Full_Name[i], " -- \\(Estimate\\)", "")
    mdr$type[i] <- "Estimate"
  } else if (short[pos] == "m") {
    mdr$Full_Name[i] <- stringr::str_replace(mdr$Full_Name[i], " -- \\(Margin of Error\\)", "")
    mdr$type[i] <- "Margin of Error"
  }
  mdr
}


#' Transform code
#'
#' @param mdr A `tibble` row.
#' @param values A vector of values
#'
#' @return A `tibble` row.
#'
#' @keywords internal
transform_values <- function(mdr, values) {
  val <- tolower(values)
  val <- snakecase::to_snake_case(val, sep_out = "_")

  if (mdr$subgroup_code == "" |
      # Puerto Rico
      mdr$subgroup_code == "PR") {
    mdr$group <- values[1]
  } else {
    # subgroup: content in parentheses
    subgroup <- regmatches(values[1], gregexpr("(?<=\\().*?(?=\\))", values[1], perl = T))[[1]]
    if (length(subgroup) > 0) {
      mdr$subgroup <- subgroup[length(subgroup)]
      # group: remove the content of the parentheses
      # mdr$group <- gsub("\\s*\\([^\\)]+\\)", "", values[1])
      mdr$group <- stringr::str_replace(values[1], sprintf("\\(%s\\)", mdr$subgroup), "")
    }
  }

  for (j in 2:length(val)) {
    mdr <- read_b01(mdr, val[j], values[j])
  }
  mdr
}




read_b01 <- function(mdr, val, value) {
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
           double = FALSE,
           second = FALSE,
           sep = ": ") {
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
        field_spec2 <- sprintf("%s_spec2", field)
        if (mdr[, field_spec2] == "") {
          mdr[, field_spec2] <- value
        } else {
          mdr[, field_spec2] <- paste(mdr[, field_spec2], value, sep = sep)
        }
      } else {
        mdr[, field_spec] <- paste(mdr[, field_spec], value, sep = sep)
      }
    }
    mdr
  }
