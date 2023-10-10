
# interpret_metadata ------------------------------------------------------

#' Interpret metadata
#'
#' Interpret the metadata to distribute it in columns according to its topic.
#'
#' @param um A `uscb_metadata` object.
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
  other_field <- ""
  for (i in seq_along(um$metadata[[1]])) {
    um$metadata[i, ] <- interpret_code(um$metadata[i, ])

    values <- strsplit(um$metadata$Full_Name[i], ": ")[[1]]
    values <- stringr::str_trim(values, side = "both")

    res <- interpret_values(um$metadata[i, ], values, um$interpret, um$field_values, other_field)
    um$metadata[i, ] <- res$mdr[1,]
    other_field <- res$other_field
  }
  um
}


#' interpret code
#'
#' Interprets the values of the code included in the metadata for a row.
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


#' interpret values
#'
#' Interprets the values in the metadata for a row.
#'
#' @param mdr A `tibble` row.
#' @param values A vector of values.
#' @param interpret Vector of functions to consider.
#' @param field_values A data frame that stores associations between fields and
#'   values.
#' @param other_field Included field that can contain the value "other".
#'
#' @return A `tibble` row.
#'
#' @keywords internal
interpret_values <- function(mdr, values, interpret, field_values, other_field) {
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

  for (j in 2:length(values)) {
    res <- interpret_all(mdr, vals[j], values[j], interpret, field_values, other_field)
    if (!res$result) {
      res <- interpret_as(mdr, field = "rest", vals[j], values[j])
    } else {
      other_field <- res$other_field
    }
    mdr <- res$mdr
  }
  res$other_field <- other_field
  res
}


#' interpret_all
#'
#' Apply each of the available functions to the value to classify it.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value to classify.
#' @param interpret Vector of functions to consider.
#' @param field_values A data frame that stores associations between fields and
#'   values.
#' @param other_field Included field that can contain the value "other".
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_all <- function(mdr, val, value, interpret, field_values, other_field) {
  if (is.null(field_values)) {
    for (f in interpret) {
      res <- f(mdr, val, value)
      if (res$result) {
        return(res)
      }
    }
  } else {
    fields <- unique(field_values[field_values$val_set == val, "field"])
    if (length(fields) == 0) {
      val_std <- standardize_text(val)
      fields <- unique(field_values[field_values$val_set_red == val_std, "field"])
      if (length(fields) == 0) {
        val_std <- standardize_text2(val)
        fields <- unique(field_values[field_values$val_set_red2 == val_std, "field"])
        if (length(fields) == 0) {
          res <- interpret_as(mdr, field = "rest", val, value)
          return(res)
        }
      }
    }
    field <- fields[1]
    if (length(fields) > 1) {
      for (i in 1:length(other_field)) {
        if (other_field[i] %in% fields) {
          field <- other_field[i]
          break
        }
      }
    }
    mdr <- add_value(mdr, field, value)
    other_field <- c(field, other_field[other_field != field])[1:10]
    res <-   list(
      mdr = mdr,
      other_field = other_field,
      field_values = field_values,
      result = TRUE
    )
  }
  res
}


#' interpret_as
#'
#' Classifies a value within a field if it is one of the field's possible
#' values.
#'
#' @param mdr A `tibble` row.
#' @param field A field name to classify the value.
#' @param val A transformed value.
#' @param value A value.
#' @param val_set Set of possible values for the field.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as <- function(mdr, field, val, value, val_set = NULL, field_values = NULL) {
  result <- TRUE
  if (is.null(val_set)) {
    mdr <- add_value(mdr, field, value)
  } else if (val %in% val_set) {
    mdr <- add_value(mdr, field, value)
  } else {
    result <- FALSE
  }

  if (!is.null(field_values)) {
    if (!is.null(val_set) & !(field %in% unique(field_values$field))) {
      df <- data.frame(subject = strsplit(field, "_")[[1]][1], field = field, val_set = val_set)
      field_values <- rbind(field_values, df)
    }
  }

  list(mdr = mdr,
       other_field = "",
       field_values = field_values,
       result = result)
}


#' add_value
#'
#' Adds a value to a field at the first free level or at the indicated level.
#'
#' @param mdr A `tibble` row.
#' @param field A field name to classify the value.
#' @param value A value to add.
#' @param level Field level to add the value to.
#'
#' @return A `tibble` row.
#'
#' @keywords internal
add_value <-
  function(mdr,
           field,
           value,
           level = 0) {
    field_spec <- sprintf("%s_spec", field)
    field_spec_2 <- sprintf("%s_spec_2", field)
    field_spec_3 <- sprintf("%s_spec_3", field)
    field_spec_4 <- sprintf("%s_spec_4", field)

    if (mdr[, field] == "" & level == 0) {
      mdr[, field] <- value
    } else if (mdr[, field_spec] == "" & level <= 1) {
      mdr[, field_spec] <- value
    } else if (mdr[, field_spec_2] == "" & level <= 2) {
      mdr[, field_spec_2] <- value
    } else if (mdr[, field_spec_3] == "" & level <= 3) {
      mdr[, field_spec_3] <- value
    } else if (mdr[, field_spec_4] == "") {
      mdr[, field_spec_4] <- value
    } else {
      mdr[, field_spec_4] <- paste(mdr[, field_spec_4], value, sep = ": ")
    }
    mdr
  }


# interpret ---------------------------------------------------------------

#' interpret_social_ancestry
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_social_ancestry <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "armenian",
      "german",
      "greek",
      "haitian",
      "italian",
      "polish",
      "portuguese",
      "russian",
      "afghan",
      "albanian",
      "alsatian",
      "american",
      "arab",
      "subsaharan_african",
      "west_indian_except_hispanic_groups",
      "african",
      "arab",
      "assyrian_chaldean_syriac",
      "australian",
      "austrian",
      "bahamian",
      "barbadian",
      "basque",
      "belgian",
      "belizean",
      "bermudan",
      "brazilian",
      "british",
      "british_west_indian",
      "bulgarian",
      "cajun",
      "canadian",
      "cape_verdean",
      "carpatho_rusyn",
      "celtic",
      "croatian",
      "cypriot",
      "czech",
      "czechoslovakian",
      "danish",
      "dutch",
      "dutch_west_indian",
      "eastern_european",
      "egyptian",
      "english",
      "estonian",
      "ethiopian",
      "european",
      "finnish",
      "french_except_basque",
      "french_canadian",
      "german_russian",
      "ghanaian",
      "guyanese",
      "hungarian",
      "icelander",
      "iranian",
      "iraqi",
      "irish",
      "israeli",
      "jamaican",
      "jordanian",
      "kenyan",
      "latvian",
      "lebanese",
      "liberian",
      "lithuanian",
      "luxembourger",
      "macedonian",
      "maltese",
      "moroccan",
      "new_zealander",
      "nigerian",
      "northern_european",
      "norwegian",
      "other_arab",
      "other_groups",
      "other_subsaharan_african",
      "other_west_indian",
      "palestinian",
      "pennsylvania_german",
      "romanian",
      "scandinavian",
      "scotch_irish",
      "scottish",
      "senegalese",
      "serbian",
      "sierra_leonean",
      "slavic",
      "slovak",
      "slovene",
      "somali",
      "south_african",
      "soviet_union",
      "sudanese",
      "swedish",
      "swiss",
      "syrian",
      "trinidadian_and_tobagonian",
      "turkish",
      "u_s_virgin_islander",
      "ugandan",
      "ukrainian",
      "unclassified_or_not_reported",
      "welsh",
      "west_indian",
      "yugoslavian",
      "zimbabwean",
      "ancestry_not_specified",
      "ancestry_specified",
      "ancestry_not_reported",
      "ancestry_unclassified",
      "multiple_ancestry",
      "single_ancestry"
    )
    interpret_as(mdr, field = "social_ancestry", val, value, val_set, field_values)
  }



#' interpret_as_survey
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_survey <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "final_actual_interviews",
      "final_number_of_housing_unit_interviews",
      "final_synthetic_interviews",
      "initial_addresses_selected",
      "initial_sample_selected",
      "nonresponse_rate",
      "response_rate",
      "group_quarters_person_other_reason",
      "group_quarters_person_refusal",
      "insufficient_data",
      "language_problem",
      "maximum_contact_attempts_reached",
      "no_one_home",
      "other_reason",
      "refusal",
      "resident_temporarily_absent",
      "temporarily_absent",
      "unable_to_locate",
      "unable_to_locate_group_quarters_person",
      "whole_group_quarters_other_reason",
      "whole_group_quarters_refusal",
      "1_or_more_items_allocated",
      "10_to_less_than_25_percent_of_total_earnings_for_individual_allocated",
      "10_to_less_than_25_percent_of_total_income_for_family_allocated",
      "10_to_less_than_25_percent_of_total_income_for_household_allocated",
      "10_to_less_than_25_percent_of_total_income_for_individual_allocated",
      "100_percent_of_total_earnings_for_individual_allocated",
      "100_percent_of_total_income_for_family_allocated",
      "100_percent_of_total_income_for_household_allocated",
      "100_percent_of_total_income_for_individual_allocated",
      "25_to_less_than_50_percent_of_total_earnings_for_individual_allocated",
      "25_to_less_than_50_percent_of_total_income_for_family_allocated",
      "25_to_less_than_50_percent_of_total_income_for_household_allocated",
      "25_to_less_than_50_percent_of_total_income_for_individual_allocated",
      "50_to_less_than_100_percent_of_total_earnings_for_individual_allocated",
      "50_to_less_than_100_percent_of_total_income_for_family_allocated",
      "50_to_less_than_100_percent_of_total_income_for_household_allocated",
      "50_to_less_than_100_percent_of_total_income_for_individual_allocated",
      "dollar_value_of_zero_allocated",
      "more_than_0_to_less_than_10_percent_of_total_earnings_for_individual_allocated",
      "more_than_0_to_less_than_10_percent_of_total_income_for_family_allocated",
      "more_than_0_to_less_than_10_percent_of_total_income_for_household_allocated",
      "more_than_0_to_less_than_10_percent_of_total_income_for_individual_allocated",
      "no_disability_items_allocated",
      "no_earnings_allocated",
      "no_health_insurance_items_allocated",
      "no_income_allocated",
      "no_private_health_insurance_items_allocated",
      "no_public_coverage_items_allocated",
      "not_allocated",
      "one_or_more_disability_items_allocated",
      "one_or_more_health_insurance_items_allocated",
      "one_or_more_private_health_insurance_items_allocated",
      "one_or_more_public_coverage_items_allocated",
      "allocated",
      "allocated_for_either_departure_time_or_minutes_to_work",
      "no_items_allocated",
      "not_allocated",
      "not_allocated_for_either_departure_time_or_minutes_to_work",
      "one_or_more_items_allocated",
      "all_geographic_parts_allocated",
      "language_status_allocated",
      "language_status_not_allocated",
      "one_or_more_but_not_all_geographic_parts_allocated"
    )
    interpret_as(mdr, field = "survey", val, value, val_set, field_values)
  }


#' interpret_as_housing_computer_and_internet_use
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_computer_and_internet_use <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "broadband_of_any_type",
      "broadband_such_as_cable_fiber_optic_or_dsl",
      "broadband_such_as_cable_fiber_optic_or_dsl_with_no_other_type_of_internet_subscription",
      "broadband_such_as_cable_fiber_optic_or_dsl",
      "cellular_data_plan",
      "cellular_data_plan_with_no_other_type_of_internet_subscription",
      "dial_up_alone",
      "dial_up_with_no_other_type_of_internet_subscription",
      "has_a_computer",
      "has_one_or_more_types_of_computing_devices",
      "internet_access_without_a_subscription",
      "no_internet_access",
      "other_service",
      "other_service_with_no_other_type_of_internet_subscription",
      "satellite_internet_service",
      "satellite_internet_service_with_no_other_type_of_internet_subscription",
      "with_a_broadband_subscription",
      "with_a_fixed_broadband_internet_subscription",
      "with_an_internet_subscription",
      "cellular_data_plan_alone_or_with_dial_up",
      "desktop_or_laptop",
      "desktop_or_laptop_alone",
      "desktop_or_laptop_with_no_other_type_of_computing_device",
      "no_computer",
      "no_computer",
      "other_computer",
      "other_computer_with_no_other_type_of_computing_device",
      "smartphone",
      "smartphone_with_no_other_type_of_computing_device",
      "smartphone_tablet_or_other_portable_wireless_computer_or_other_computer",
      "smartphone_tablet_or_other_portable_wireless_computer_or_other_computer_no_desktop_or_laptop",
      "tablet_or_other_portable_wireless_computer",
      "tablet_or_other_portable_wireless_computer_with_no_other_type_of_computing_device",
      "with_a_broadband_internet_subscription",
      "with_a_cellular_data_plan",
      "with_dial_up_internet_subscription_alone",
      "without_a_cellular_data_plan",
      "without_an_internet_subscription",
      "without_internet_subscription",
      "broadband_such_as_cable_fiberoptic_or_dsl_satellite_and_other_service",
      "dial_up"
    )
    interpret_as(mdr, field = "housing_computer_and_internet_use", val, value, val_set, field_values)
  }


#' interpret_as_demographic_group_quarters_population
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_group_quarters_population <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "adult_correctional_facilities",
      "college_university_student_housing",
      "nursing_facilities_skilled_nursing_facilities",
      "juvenile_facilities",
      "military_quarters_military_ships"
    )
    interpret_as(mdr, field = "demographic_group_quarters_population", val, value, val_set, field_values)
  }


#' interpret_as_economic_food_stamps_snap
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_food_stamps_snap <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "household_did_not_receive_food_stamps_snap_in_the_past_12_months",
      "household_received_food_stamps_snap_in_the_past_12_months",
      "did_not_receive_food_stamps_snap_in_the_past_12_months",
      "received_food_stamps_snap_in_the_past_12_months"
    )
    interpret_as(mdr, field = "economic_food_stamps_snap", val, value, val_set, field_values)
  }


#' interpret_as_social_veteran_status_military_service
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_veteran_status_military_service <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "between_gulf_war_and_vietnam_era_only",
      "between_korean_war_and_world_war_ii_only",
      "between_vietnam_era_and_korean_war_only",
      "gulf_war_8_1990_to_8_2001_and_vietnam_era",
      "gulf_war_8_1990_to_8_2001_no_vietnam_era",
      "gulf_war_9_2001_or_later_and_gulf_war_8_1990_to_8_2001_no_vietnam_era",
      "gulf_war_9_2001_or_later_and_gulf_war_8_1990_to_8_2001_and_vietnam_era",
      "gulf_war_9_2001_or_later_no_gulf_war_8_1990_to_8_2001_no_vietnam_era",
      "korean_war_and_world_war_ii_no_vietnam_era",
      "korean_war_no_vietnam_era_no_world_war_ii",
      "nonveteran",
      "pre_world_war_ii_only",
      "veteran",
      "vietnam_era_and_korean_war_and_world_war_ii",
      "vietnam_era_and_korean_war_no_world_war_ii",
      "vietnam_era_no_korean_war_no_world_war_ii",
      "world_war_ii_no_korean_war_no_vietnam_era",
      "civilian",
      "in_armed_forces"
    )
    interpret_as(mdr, field = "social_veteran_status_military_service", val, value, val_set, field_values)
  }


#' interpret_as_social_school_enrollment
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_school_enrollment <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "enrolled_in_college_undergraduate_years",
      "enrolled_in_grade_1_to_grade_4",
      "enrolled_in_grade_5_to_grade_8",
      "enrolled_in_grade_9_to_grade_12",
      "enrolled_in_kindergarten",
      "enrolled_in_nursery_school_preschool",
      "graduate_or_professional_school",
      "not_enrolled_in_school",
      "enrolled_in_school",
      "enrolled_in_graduate_or_professional_school",
      "private_school",
      "public_school",
      "enrolled_in_grade_1",
      "enrolled_in_grade_10",
      "enrolled_in_grade_11",
      "enrolled_in_grade_12",
      "enrolled_in_grade_2",
      "enrolled_in_grade_3",
      "enrolled_in_grade_4",
      "enrolled_in_grade_5",
      "enrolled_in_grade_6",
      "enrolled_in_grade_7",
      "enrolled_in_grade_8",
      "enrolled_in_grade_9",
      "enrolled_in_private_college_or_graduate_school",
      "enrolled_in_private_school",
      "enrolled_in_public_college_or_graduate_school",
      "enrolled_in_public_school",
      "not_enrolled_in_college_or_graduate_school"
    )
    interpret_as(mdr, field = "social_school_enrollment", val, value, val_set, field_values)
  }


#' interpret_as_social_fertility
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_fertility <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "women_who_did_not_have_a_birth_in_the_past_12_months",
      "women_who_had_a_birth_in_the_past_12_months"
    )
    interpret_as(mdr, field = "social_fertility", val, value, val_set, field_values)
  }


#' interpret_as_economic_health_insurance_coverage
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_health_insurance_coverage <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "with_social_security_income_in_the_past_12_months",
      "without_social_security_income_in_the_past_12_months",
      "with_ssi_and_or_cash_public_assistance_income_in_the_past_12_months",
      "without_ssi_or_cash_public_assistance_income_in_the_past_12_months",
      "with_private_health_insurance_coverage",
      "with_public_health_coverage",
      "with_health_insurance_coverage",
      "no_health_insurance_coverage",
      "no_private_health_insurance",
      "no_public_coverage",
      "other_coverage_combinations",
      "other_private_only_combinations",
      "other_public_only_combinations",
      "with_direct_purchase_and_medicare_coverage",
      "with_direct_purchase_health_insurance_only",
      "with_employer_based_and_direct_purchase_coverage",
      "with_employer_based_and_medicare_coverage",
      "with_employer_based_health_insurance_only",
      "with_medicaid_means_tested_public_coverage_only",
      "with_medicare_and_medicaid_means_tested_public_coverage",
      "with_medicare_coverage_only",
      "with_private_health_insurance",
      "with_public_coverage",
      "with_tricare_military_health_coverage_only",
      "with_va_health_care_only",
      "no_private_health_insurance_coverage",
      "with_one_type_of_health_insurance_coverage",
      "with_two_or_more_types_of_health_insurance_coverage",
      "no_direct_purchase_health_insurance",
      "no_employer_based_health_insurance",
      "no_medicaid_means_tested_public_coverage",
      "no_medicare_coverage",
      "no_tricare_military_health_coverage",
      "no_va_health_care",
      "with_direct_purchase_health_insurance",
      "with_employer_based_health_insurance",
      "with_medicaid_means_tested_public_coverage",
      "with_medicare_coverage",
      "with_tricare_military_health_coverage",
      "with_va_health_care"
    )
    interpret_as(mdr, field = "economic_health_insurance_coverage", val, value, val_set, field_values)
  }


#' interpret_as_economic_income_and_earnings
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_income_and_earnings <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "no_income",
      "with_income",
      "1_to_9_999_or_loss",
      "10_000_to_14_999",
      "15_000_to_24_999",
      "25_000_to_34_999",
      "35_000_to_49_999",
      "50_000_to_64_999",
      "65_000_to_74_999",
      "75_000_or_more",
      "aggregate_income_deficit_in_the_past_12_months_of_unrelated_individuals_dollars",
      "aggregate_income_deficit_in_the_past_12_months",
      "100_000_to_124_999",
      "125_000_to_149_999",
      "15_000_to_19_999",
      "150_000_to_199_999",
      "20_000_to_24_999",
      "200_000_or_more",
      "25_000_to_29_999",
      "30_000_to_34_999",
      "35_000_to_39_999",
      "40_000_to_44_999",
      "45_000_to_49_999",
      "50_000_to_59_999",
      "60_000_to_74_999",
      "75_000_to_99_999",
      "less_than_10_000",
      "no_earnings",
      "with_earnings",
      "no_wage_or_salary_income",
      "with_wage_or_salary_income",
      "no_self_employment_income",
      "with_self_employment_income",
      "no_interest_dividends_or_net_rental_income",
      "with_interest_dividends_or_net_rental_income",
      "no_social_security_income",
      "with_social_security_income",
      "no_supplemental_security_income_ssi",
      "with_supplemental_security_income_ssi",
      "aggregate_earnings_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_interest_dividends_or_net_rental_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_other_types_of_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_public_assistance_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_retirement_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_self_employment_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_social_security_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_supplemental_security_income_ssi_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_wage_or_salary_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "no_cash_public_assistance_or_food_stamps_snap",
      "no_other_types_of_income",
      "no_public_assistance_income",
      "no_retirement_income",
      "with_cash_public_assistance_or_food_stamps_snap",
      "with_other_types_of_income",
      "with_public_assistance_income",
      "with_retirement_income",
      "fourth_quintile",
      "lower_limit_of_top_5_percent",
      "lowest_quintile",
      "second_quintile",
      "third_quintile",
      "highest_quintile",
      "top_5_percent",
      "aggregate_family_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "gini_index",
      "median_family_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "no_cash_public_assistance_income_or_household_food_stamps_snap_benefits_in_the_past_12_months",
      "no_cash_public_assistance_income_or_household_food_stamps_snap_benefits_in_the_past_12_months",
      "with_cash_public_assistance_income_or_households_receiving_food_stamps_snap_benefits_in_the_past_12_months",
      "with_cash_public_assistance_income_or_households_receiving_food_stamps_snap_benefits_in_the_past_12_months",
      "1_to_2_499_or_loss",
      "10_000_to_12_499",
      "100_000_or_more",
      "12_500_to_14_999",
      "15_000_to_17_499",
      "17_500_to_19_999",
      "2_500_to_4_999",
      "20_000_to_22_499",
      "22_500_to_24_999",
      "5_000_to_7_499",
      "50_000_to_54_999",
      "55_000_to_64_999",
      "7_500_to_9_999",
      "aggregate_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_nonfamily_household_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "median_nonfamily_household_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "per_capita_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "25_000_to_49_999",
      "50_000_to_74_999",
      "under_25_000",
      "10_000_to_19_999",
      "20_000_to_34_999",
      "other_dollars"
    )
    interpret_as(mdr, field = "economic_income_and_earnings", val, value, val_set, field_values)
  }


#' interpret_as_housing_units
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_units <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "1_unit_structures",
      "2_or_more_unit_structures",
      "mobile_homes_and_all_other_types_of_units",
      "occupied",
      "vacant",
      "1_attached",
      "1_detached",
      "1_detached_or_attached",
      "10_to_19",
      "2",
      "2_to_4",
      "20_to_49",
      "3_or_4",
      "5_or_more",
      "5_to_9",
      "50_or_more",
      "boat_rv_van_etc",
      "mobile_home",
      "bottled_tank_or_lp_gas",
      "coal_or_coke",
      "electricity",
      "fuel_oil_kerosene_etc",
      "no_fuel_used",
      "no_telephone_service_available",
      "other_fuel",
      "solar_energy",
      "utility_gas",
      "with_telephone_service_available",
      "wood",
      "complete_kitchen_facilities",
      "lacking_complete_kitchen_facilities",
      "10_0_to_14_9_percent",
      "15_0_to_19_9_percent",
      "20_0_to_24_9_percent",
      "25_0_to_29_9_percent",
      "30_0_to_34_9_percent",
      "35_0_percent_or_more",
      "35_0_to_39_9_percent",
      "40_0_to_49_9_percent",
      "5_to_19",
      "50_0_percent_or_more",
      "less_than_10_0_percent",
      "less_than_20_0_percent",
      "not_computed",
      "1_attached_dollars",
      "1_detached_dollars",
      "2_dollars",
      "3_or_4_dollars",
      "5_or_more_dollars",
      "boat_rv_van_etc_dollars",
      "housing_units_with_a_mortgage_dollars",
      "housing_units_with_a_mortgage_contract_to_purchase_or_similar_debt",
      "housing_units_without_a_mortgage_dollars",
      "mobile_home_dollars",
      "not_mortgaged",
      "owner_occupied_mobile_homes",
      "with_a_mortgage",
      "with_either_a_second_mortgage_or_home_equity_loan_but_not_both",
      "2_0_to_2_9",
      "3_0_to_3_9",
      "4_0_or_more",
      "both_second_mortgage_and_home_equity_loan",
      "home_equity_loan_only",
      "less_than_2_0",
      "no_second_mortgage_and_no_home_equity_loan",
      "second_mortgage_only",
      "mobile_home_boat_rv_van_etc",
      "no_real_estate_taxes_paid",
      "no_selected_conditions",
      "occupied_housing_units_with_monthly_housing_costs",
      "owner_occupied_dollars",
      "renter_occupied_dollars",
      "with_four_selected_conditions",
      "with_one_selected_condition",
      "with_three_selected_conditions",
      "with_two_selected_conditions",
      "zero_or_negative_income",
      "20_to_29_percent",
      "30_percent_or_more",
      "less_than_20_percent",
      "mobile_home_boat_rv_van_etc"
    )
    interpret_as(mdr, field = "housing_units", val, value, val_set, field_values)
  }


#' interpret_as_housing_plumbing_facilities
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_plumbing_facilities <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "complete_plumbing_facilities",
      "lacking_complete_plumbing_facilities",
      "lacking_plumbing_facilities"
    )
    interpret_as(mdr, field = "housing_plumbing_facilities", val, value, val_set, field_values)
  }


#' interpret_as_housing_occupancy_vacancy_status
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_occupancy_vacancy_status <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "for_migrant_workers",
      "for_rent",
      "for_sale_only",
      "for_seasonal_recreational_or_occasional_use",
      "other_vacant",
      "rented_not_occupied",
      "sold_not_occupied"
    )
    interpret_as(mdr, field = "housing_occupancy_vacancy_status", val, value, val_set, field_values)
  }



#' interpret_as_housing_vehicles_available
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_vehicles_available <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "1_vehicle_available",
      "2_vehicles_available",
      "3_vehicles_available",
      "4_vehicles_available",
      "5_or_more_vehicles_available",
      "3_or_more_vehicles_available",
      "4_or_more_vehicles_available",
      "no_vehicle_available",
      "1_or_more_vehicles_available"
    )
    interpret_as(mdr, field = "housing_vehicles_available", val, value, val_set, field_values)
  }



#' interpret_as_housing_rooms
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_rooms <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "1_bedroom",
      "1_room",
      "2_bedrooms",
      "2_rooms",
      "3_bedrooms",
      "3_rooms",
      "4_bedrooms",
      "4_rooms",
      "5_or_more_bedrooms",
      "5_rooms",
      "6_rooms",
      "7_rooms",
      "8_rooms",
      "9_or_more_rooms",
      "no_bedroom",
      "3_or_more_bedrooms"
    )
    interpret_as(mdr, field = "housing_rooms", val, value, val_set, field_values)
  }



#' interpret_as_housing_occupants_per_room
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_occupants_per_room <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "0_50_or_less_occupants_per_room",
      "0_51_to_1_00_occupants_per_room",
      "1_00_or_less_occupants_per_room",
      "1_01_or_more_occupants_per_room",
      "1_01_to_1_50_occupants_per_room",
      "1_51_or_more_occupants_per_room",
      "1_51_to_2_00_occupants_per_room",
      "2_01_or_more_occupants_per_room"
    )
    interpret_as(mdr, field = "housing_occupants_per_room", val, value, val_set, field_values)
  }



#' interpret_as_housing_value_of_home
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_value_of_home <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "value_10_000_to_19_999",
      "value_100_000_to_199_999",
      "value_20_000_to_29_999",
      "value_200_000_to_249_999",
      "value_250_000_to_499_999",
      "value_30_000_to_39_999",
      "value_40_000_to_49_999",
      "value_50_000_to_59_999",
      "value_500_000_or_more",
      "value_60_000_to_69_999",
      "value_70_000_to_79_999",
      "value_80_000_to_89_999",
      "value_90_000_to_99_999",
      "value_less_than_10_000",
      "1_000_000_to_1_499_999",
      "1_500_000_to_1_999_999",
      "150_000_to_174_999",
      "175_000_to_199_999",
      "2_000_000_or_more",
      "200_000_to_249_999",
      "250_000_to_299_999",
      "300_000_to_399_999",
      "40_000_to_49_999",
      "400_000_to_499_999",
      "400_000_to_499_999",
      "500_000_to_749_999",
      "60_000_to_69_999",
      "70_000_to_79_999",
      "750_000_to_999_999",
      "80_000_to_89_999",
      "90_000_to_99_999",
      "100_000_to_149_999",
      "150_000_or_more",
      "1_000_000_or_more",
      "10_000_to_24_999",
      "100_000_to_149_999",
      "150_000_or_more",
      "200_000_to_299_999",
      "300_000_to_499_999",
      "50_000_to_99_999",
      "less_than_50_000",
      "less_than_20_000"
    )
    interpret_as(mdr, field = "housing_value_of_home", val, value, val_set, field_values)
  }



#' interpret_as_housing_rent
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_rent <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "meals_included_in_rent",
      "no_meals_included_in_rent",
      "with_cash_rent",
      "1_000_to_1_249",
      "1_250_to_1_499",
      "1_500_to_1_999",
      "100_to_149",
      "150_to_199",
      "2_000_to_2_499",
      "2_500_to_2_999",
      "200_to_249",
      "250_to_299",
      "3_000_to_3_499",
      "3_500_or_more",
      "300_to_349",
      "350_to_399",
      "400_to_449",
      "450_to_499",
      "500_to_549",
      "550_to_599",
      "600_to_649",
      "650_to_699",
      "700_to_749",
      "750_to_799",
      "800_to_899",
      "900_to_999",
      "less_than_100",
      "meals_included_in_rent",
      "no_cash_rent",
      "no_meals_included_in_rent",
      "1_000_to_1_499",
      "1_500_or_more",
      "300_to_499",
      "500_to_749",
      "750_to_999",
      "no_extra_payment_for_any_utilities",
      "pay_extra_for_one_or_more_utilities",
      "less_than_300",
      "1000_to_1_099",
      "1100_to_1_199",
      "1200_to_1_299",
      "1300_to_1_399",
      "1400_to_1_499",
      "200_to_299",
      "3_500_to_3_999",
      "300_to_399",
      "4_000_or_more",
      "400_to_499",
      "500_to_599",
      "600_to_699",
      "700_to_799",
      "100_to_199",
      "2_000_or_more",
      "2_000_to_2_999",
      "3_000_or_more",
      "5_000_to_9_999",
      "800_to_1_499",
      "less_than_5_000",
      "less_than_800",
      "less_than_200"
    )
    interpret_as(mdr, field = "housing_rent", val, value, val_set, field_values)
  }


#' interpret_as_housing_year_structure_built
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_year_structure_built <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "built_1939_or_earlier",
      "built_1940_to_1949",
      "built_1950_or_later",
      "built_1950_to_1959",
      "built_1960_to_1969",
      "built_1970_to_1979",
      "built_1980_to_1989",
      "built_1990_to_1999",
      "built_2000_to_2009",
      "built_2010_to_2013",
      "built_2014_or_later",
      "built_1940_to_1959",
      "built_1960_to_1979",
      "built_1980_to_1999",
      "built_2010_or_later"
    )
    interpret_as(mdr, field = "housing_year_structure_built", val, value, val_set, field_values)
  }



#' interpret_as_housing_year_householder_moved_into_unit
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_year_householder_moved_into_unit <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "moved_in_1989_or_earlier",
      "moved_in_1990_to_1999",
      "moved_in_2000_to_2009",
      "moved_in_2010_to_2014",
      "moved_in_2015_to_2016",
      "moved_in_2017_or_later",
      "moved_in_2015_or_later"
    )
    interpret_as(mdr, field = "housing_year_householder_moved_into_unit", val, value, val_set, field_values)
  }



#' interpret_as_economic_work_status_last_year
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_work_status_last_year <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "in_labor_force",
      "not_in_labor_force",
      "employed_or_in_armed_forces",
      "unemployed",
      "employed",
      "did_not_work",
      "worked_full_time_year_round",
      "worked_part_time_or_part_year",
      "worked_less_than_full_time_year_round",
      "householder_did_not_work_in_the_past_12_months",
      "householder_worked_full_time_year_round_in_the_past_12_months",
      "householder_worked_part_time_or_part_year_in_the_past_12_months",
      "spouse_did_not_work_in_the_past_12_months",
      "spouse_worked_full_time_year_round_in_the_past_12_months",
      "spouse_worked_part_time_or_part_year_in_the_past_12_months",
      "worked_full_time_year_round_in_the_past_12_months",
      "worked_full_time_year_round_in_the_past_12_months_dollars",
      "both_parents_in_labor_force",
      "civilian_labor_force",
      "father_only_in_labor_force",
      "householder_worked_less_than_full_time_year_round_in_the_past_12_months",
      "husband_in_labor_force",
      "husband_not_in_labor_force",
      "mother_only_in_labor_force",
      "neither_parent_in_labor_force",
      "usually_worked_1_to_14_hours_per_week",
      "usually_worked_15_to_34_hours_per_week",
      "usually_worked_35_or_more_hours_per_week",
      "wife_in_labor_force",
      "worked_in_the_past_12_months",
      "spouse_worked_less_than_full_time_year_round_in_the_past_12_months",
      "wife_not_in_labor_force",
      "did_not_work_in_the_past_12_months",
      "aggregate_usual_hours",
      "1_to_13_weeks",
      "14_to_26_weeks",
      "27_to_39_weeks",
      "40_to_47_weeks",
      "48_and_49_weeks",
      "50_to_52_weeks",
      "in_the_civilian_labor_force",
      "in_the_labor_force",
      "usual_hours_worked_per_week_in_the_past_12_months",
      "weeks_worked_in_the_past_12_months"
    )
    interpret_as(mdr, field = "economic_work_status_last_year", val, value, val_set, field_values)
  }


#' interpret_as_social_disability_status
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_disability_status <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "no_disability",
      "with_any_disability",
      "no_ambulatory_difficulty",
      "no_cognitive_difficulty",
      "no_hearing_difficulty",
      "no_independent_living_difficulty",
      "no_self_care_difficulty",
      "no_vision_difficulty",
      "with_a_cognitive_difficulty",
      "with_a_disability",
      "with_a_hearing_difficulty",
      "with_a_self_care_difficulty",
      "with_a_vision_difficulty",
      "with_an_ambulatory_difficulty",
      "with_an_independent_living_difficulty",
      "has_a_service_connected_disability_rating",
      "has_no_service_connected_disability_rating",
      "0_percent",
      "10_or_20_percent",
      "30_or_40_percent",
      "50_or_60_percent",
      "70_percent_or_higher",
      "rating_not_reported",
      "with_one_type_of_disability",
      "with_two_or_more_types_of_disability"
    )
    interpret_as(mdr, field = "social_disability_status", val, value, val_set, field_values)
  }


#' interpret_as_social_grandparents_as_caregivers
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_grandparents_as_caregivers <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "grandparent_householder_responsible_for_own_grandchildren_under_18_years",
      "grandparent_householder_not_responsible_for_own_grandchildren_under_18_years",
      "no_parent_present",
      "parent_present",
      "grandparent_householder_and_or_spouse_responsible_for_grandchildren_under_18_years_dollars",
      "grandparent_householder_and_or_spouse_responsible_for_grandchildren_under_18_years_and_no_parent_present_dollars",
      "grandparent_responsible_for_own_grandchildren_under_18_years",
      "living_with_own_grandchildren_under_18_years",
      "grandparent_not_responsible_for_own_grandchildren_under_18_years",
      "grandparent_responsible_1_or_2_years",
      "grandparent_responsible_3_or_4_years",
      "grandparent_responsible_5_years_or_more",
      "grandparent_responsible_6_to_11_months",
      "grandparent_responsible_less_than_6_months",
      "not_living_with_own_grandchildren_under_18_years",
      "householder_or_spouse_with_no_parent_of_grandchildren_present",
      "other_grandparents",
      "household_with_grandparent_responsible_for_own_grandchildren_under_18_years",
      "household_with_grandparents_living_with_grandchildren",
      "household_with_grandparent_not_responsible_for_own_grandchildren_under_18_years",
      "household_without_grandparents_living_with_grandchildren"
    )
    interpret_as(mdr, field = "social_grandparents_as_caregivers", val, value, val_set, field_values)
  }


#' interpret_as_demographic_household
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_household <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "1_person_household",
      "2_person_household",
      "3_person_household",
      "4_or_more_person_household",
      "1_worker",
      "2_workers",
      "3_or_more_workers",
      "3_workers",
      "no_workers",
      "in_households",
      "in_group_quarters",
      "female_householder_no_husband_present",
      "in_married_couple_families",
      "in_other_families",
      "male_householder_no_wife_present",
      "in_female_householder_no_husband_present_family",
      "in_male_householder_no_wife_present_family",
      "in_married_couple_family",
      "in_nonfamily_households",
      "in_family_households",
      "no_unmarried_partner_of_householder_present",
      "unmarried_partner_of_householder_present",
      "living_in_household_with_no_supplemental_security_income_ssi_cash_public_assistance_income_or_food_stamps_snap_in_the_past_12_months",
      "living_in_household_with_supplemental_security_income_ssi_cash_public_assistance_income_or_food_stamps_snap_in_the_past_12_months",
      "child",
      "householder",
      "living_alone",
      "nonrelatives",
      "not_living_alone",
      "brother_or_sister",
      "foster_child",
      "housemate_or_roommate",
      "other_nonrelatives",
      "parent",
      "parent_in_law",
      "roomer_or_boarder",
      "son_in_law_or_daughter_in_law",
      "spouse",
      "unmarried_partner",
      "own_child",
      "adopted_child",
      "biological_child",
      "foster_child_or_other_unrelated_child",
      "grandchild",
      "other_relatives",
      "stepchild",
      "child_of_householder",
      "householder_living_with_spouse_or_spouse_of_householder",
      "householder_living_with_unmarried_partner_or_unmarried_partner_of_householder",
      "lives_alone",
      "family_households",
      "nonfamily_households",
      "other_family",
      "householder_living_alone",
      "householder_not_living_alone",
      "married_couple_family",
      "relatives",
      "female_householder",
      "households_with_no_people_under_18_years",
      "households_with_one_or_more_people_under_18_years",
      "male_householder",
      "households_with_no_people_60_years_and_over",
      "households_with_one_or_more_people_60_years_and_over",
      "2_or_more_person_household",
      "households_with_no_people_65_years_and_over",
      "households_with_one_or_more_people_65_years_and_over",
      "1_person_households",
      "unmarried_partner_households",
      "all_other_households",
      "female_householder_and_female_partner",
      "female_householder_and_male_partner",
      "male_householder_and_female_partner",
      "male_householder_and_male_partner",
      "households_with_no_nonrelatives",
      "households_with_one_or_more_nonrelatives",
      "4_person_household",
      "5_person_household",
      "6_person_household",
      "7_or_more_person_household",
      "multigenerational_households",
      "not_an_unmarried_partner",
      "partner_in_an_unmarried_partner_household",
      "in_other_family",
      "nonfamily_householder",
      "other",
      "1_or_2_children",
      "3_or_4_children",
      "5_or_more_children",
      "no_child",
      "other_families",
      "2_people",
      "3_to_4_people",
      "5_to_6_people",
      "7_or_more_people",
      "all_relatives",
      "in_non_family_households_and_other_living_arrangement",
      "non_relatives",
      "other_living_arrangement",
      "1_or_2_own_children_of_the_householder",
      "3_or_4_own_children_of_the_householder",
      "5_or_more_own_children_of_the_householder",
      "no_own_child_of_the_householder",
      "2_person_households",
      "3_person_households",
      "4_person_households",
      "5_person_households",
      "6_person_households",
      "7_or_more_person_households",
      "1_earner",
      "1_earner_dollars",
      "2_person_families",
      "2_earners",
      "2_earners_dollars",
      "3_person_families",
      "3_or_more_earners",
      "3_or_more_earners_dollars",
      "4_person_families",
      "5_person_families",
      "6_person_families",
      "7_or_more_person_families",
      "no_earners",
      "no_earners_dollars",
      "no_own_children_of_the_householder_under_18_years",
      "with_own_children_of_the_householder_under_18_years",
      "1_worker_dollars",
      "2_workers_husband_and_wife_worked_dollars",
      "2_workers_other_dollars",
      "3_or_more_workers_husband_and_wife_worked_dollars",
      "3_or_more_workers_other_dollars",
      "female_householder_dollars",
      "female_householder_no_husband_present_dollars",
      "living_alone_dollars",
      "male_householder_dollars",
      "male_householder_no_wife_present_dollars",
      "married_couple_family_dollars",
      "no_workers_dollars",
      "not_living_alone_dollars",
      "other_dollars",
      "other_family_dollars",
      "no_children_under_18_years",
      "with_children_under_18_years",
      "at_least_one_person_in_household_60_years_or_over",
      "households_with_1_or_more_persons_with_a_disability",
      "households_with_no_persons_with_a_disability",
      "husband_and_wife_worked",
      "no_people_in_household_60_years_or_over",
      "living_with_one_parent",
      "living_with_two_parents",
      "own_children_under_18_years_living_in_families_or_subfamilies",
      "own_children_under_18_years_living_in_families_or_subfamilies_for_whom_poverty_status_is_determined",
      "with_own_children_of_the_householder_under_18_years",
      "6_to_17_years_only",
      "no_own_children_of_the_householder_under_18_years",
      "under_6_years_and_6_to_17_years",
      "under_6_years_only",
      "with_related_children_of_the_householder_under_18_years",
      "no_related_children_of_the_householder_under_18_years",
      "married_couple_subfamily",
      "father_child_subfamily",
      "mother_child_subfamily",
      "no_own_children_under_18_years",
      "with_own_children_under_18_years",
      "in_father_child_subfamilies",
      "in_married_couple_subfamilies",
      "in_mother_child_subfamilies",
      "husband_wife_in_a_childless_subfamily",
      "husband_wife_in_a_subfamily_with_children",
      "5_to_17_years_only",
      "under_5_years_and_5_to_17_years",
      "under_5_years_only",
      "married_couple_families",
      "living_with_father",
      "living_with_mother",
      "in_non_family_households_and_other_living_arrangements",
      "with_own_children_of_the_householder_under_18",
      "with_related_children_of_the_householder_under_18",
      "no_own_children_of_the_householder_under_18",
      "no_related_children_of_the_householder_under_18",
      "5_or_more_person_household"
    )
    interpret_as(mdr, field = "demographic_household", val, value, val_set, field_values)
  }

#' interpret_as_economic_industry_and_occupation
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_industry_and_occupation <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "management_business_science_and_arts_occupations",
      "military_specific_occupations",
      "natural_resources_construction_and_maintenance_occupations",
      "production_transportation_and_material_moving_occupations",
      "sales_and_office_occupations",
      "service_occupations",
      "agriculture_forestry_fishing_and_hunting_and_mining",
      "armed_forces",
      "arts_entertainment_and_recreation_and_accommodation_and_food_services",
      "construction",
      "educational_services_and_health_care_and_social_assistance",
      "finance_and_insurance_and_real_estate_and_rental_and_leasing",
      "information",
      "manufacturing",
      "other_services_except_public_administration",
      "professional_scientific_and_management_and_administrative_and_waste_management_services",
      "public_administration",
      "retail_trade",
      "transportation_and_warehousing_and_utilities",
      "wholesale_trade",
      "private_for_profit_wage_and_salary_workers",
      "employee_of_private_company_workers",
      "federal_government_workers",
      "local_government_workers",
      "private_not_for_profit_wage_and_salary_workers",
      "self_employed_in_own_incorporated_business_workers",
      "self_employed_in_own_not_incorporated_business_workers",
      "state_government_workers",
      "unpaid_family_workers",
      "computer_engineering_and_science_occupations",
      "construction_and_extraction_occupations",
      "education_legal_community_service_arts_and_media_occupations",
      "farming_fishing_and_forestry_occupations",
      "healthcare_practitioners_and_technical_occupations",
      "healthcare_support_occupations",
      "installation_maintenance_and_repair_occupations",
      "management_business_and_financial_occupations",
      "material_moving_occupations",
      "office_and_administrative_support_occupations",
      "production_occupations",
      "protective_service_occupations",
      "sales_and_related_occupations",
      "transportation_occupations",
      "architecture_and_engineering_occupations",
      "arts_design_entertainment_sports_and_media_occupations",
      "building_and_grounds_cleaning_and_maintenance_occupations",
      "business_and_financial_operations_occupations",
      "community_and_social_service_occupations",
      "computer_and_mathematical_occupations",
      "educational_instruction_and_library_occupations",
      "firefighting_and_prevention_and_other_protective_service_workers_including_supervisors",
      "food_preparation_and_serving_related_occupations",
      "health_diagnosing_and_treating_practitioners_and_other_technical_occupations",
      "health_technologists_and_technicians",
      "law_enforcement_workers_including_supervisors",
      "legal_occupations",
      "life_physical_and_social_science_occupations",
      "management_occupations",
      "personal_care_and_service_occupations",
      "accommodation_and_food_services",
      "accountants_and_auditors",
      "actors",
      "actuaries",
      "acupuncturists",
      "adhesive_bonding_machine_operators_and_tenders",
      "administrative_and_support_and_waste_management_services",
      "administrative_services_managers",
      "advertising_and_promotions_managers",
      "advertising_sales_agents",
      "aerospace_engineers",
      "agents_and_business_managers_of_artists_performers_and_athletes",
      "agricultural_and_food_science_technicians",
      "agricultural_and_food_scientists",
      "agricultural_engineers",
      "agricultural_inspectors",
      "agriculture_forestry_fishing_and_hunting",
      "air_traffic_controllers_and_airfield_operations_specialists",
      "aircraft_mechanics_and_service_technicians",
      "aircraft_pilots_and_flight_engineers",
      "aircraft_structure_surfaces_rigging_and_systems_assemblers",
      "ambulance_drivers_and_attendants_except_emergency_medical_technicians",
      "animal_breeders",
      "animal_caretakers",
      "animal_control_workers",
      "animal_trainers",
      "architects_except_landscape_and_naval",
      "architectural_and_civil_drafters",
      "architectural_and_engineering_managers",
      "archivists_curators_and_museum_technicians",
      "artists_and_related_workers",
      "arts_entertainment_and_recreation",
      "astronomers_and_physicists",
      "athletes_and_sports_competitors",
      "atmospheric_and_space_scientists",
      "audiologists",
      "audiovisual_equipment_installers_and_repairers",
      "automotive_body_and_related_repairers",
      "automotive_glass_installers_and_repairers",
      "automotive_service_technicians_and_mechanics",
      "avionics_technicians",
      "baggage_porters_bellhops_and_concierges",
      "bailiffs",
      "bakers",
      "barbers",
      "bartenders",
      "bill_and_account_collectors",
      "billing_and_posting_clerks",
      "bioengineers_and_biomedical_engineers",
      "biological_scientists",
      "biological_technicians",
      "boilermakers",
      "bookkeeping_accounting_and_auditing_clerks",
      "brickmasons_blockmasons_and_stonemasons",
      "broadcast_announcers_and_radio_disc_jockeys",
      "broadcast_sound_and_lighting_technicians",
      "brokerage_clerks",
      "budget_analysts",
      "bus_and_truck_mechanics_and_diesel_engine_specialists",
      "bus_drivers_school",
      "bus_drivers_transit_and_intercity",
      "business_operations_specialists_all_other",
      "butchers_and_other_meat_poultry_and_fish_processing_workers",
      "buyers_and_purchasing_agents_farm_products",
      "cabinetmakers_and_bench_carpenters",
      "cardiovascular_technologists_and_technicians",
      "cargo_and_freight_agents",
      "carpenters",
      "carpet_floor_and_tile_installers_and_finishers",
      "cashiers",
      "cement_masons_concrete_finishers_and_terrazzo_workers",
      "chefs_and_head_cooks",
      "chemical_engineers",
      "chemical_processing_machine_setters_operators_and_tenders",
      "chemical_technicians",
      "chemists_and_materials_scientists",
      "chief_executives",
      "child_family_and_school_social_workers",
      "childcare_workers",
      "chiropractors",
      "civil_engineers",
      "claims_adjusters_appraisers_examiners_and_investigators",
      "cleaners_of_vehicles_and_equipment",
      "clergy",
      "clinical_and_counseling_psychologists",
      "clinical_laboratory_technologists_and_technicians",
      "coaches_and_scouts",
      "coin_vending_and_amusement_machine_servicers_and_repairers",
      "commercial_and_industrial_designers",
      "commercial_divers",
      "communications_equipment_operators_all_other",
      "compensation_and_benefits_managers",
      "compensation_benefits_and_job_analysis_specialists",
      "compliance_officers",
      "computer_and_information_research_scientists",
      "computer_and_information_systems_managers",
      "computer_hardware_engineers",
      "computer_network_architects",
      "computer_numerically_controlled_tool_operators_and_programmers",
      "computer_occupations_all_other",
      "computer_programmers",
      "computer_support_specialists",
      "computer_systems_analysts",
      "computer_automated_teller_and_office_machine_repairers",
      "conservation_scientists_and_foresters",
      "construction_and_building_inspectors",
      "construction_equipment_operators",
      "construction_laborers",
      "construction_managers",
      "control_and_valve_installers_and_repairers",
      "conveyor_dredge_and_hoist_and_winch_operators",
      "cooks",
      "correctional_officers_and_jailers",
      "correspondence_clerks",
      "cost_estimators",
      "counselors_all_other",
      "counter_and_rental_clerks",
      "couriers_and_messengers",
      "court_reporters_and_simultaneous_captioners",
      "court_municipal_and_license_clerks",
      "crane_and_tower_operators",
      "credit_analysts",
      "credit_authorizers_checkers_and_clerks",
      "credit_counselors_and_loan_officers",
      "crossing_guards_and_flaggers",
      "crushing_grinding_polishing_mixing_and_blending_workers",
      "customer_service_representatives",
      "cutting_workers",
      "cutting_punching_and_press_machine_setters_operators_and_tenders_metal_and_plastic",
      "dancers_and_choreographers",
      "data_entry_keyers",
      "database_administrators_and_architects",
      "dental_and_ophthalmic_laboratory_technicians_and_medical_appliance_technicians",
      "dental_assistants",
      "dental_hygienists",
      "dentists",
      "derrick_rotary_drill_and_service_unit_operators_oil_and_gas",
      "desktop_publishers",
      "detectives_and_criminal_investigators",
      "diagnostic_medical_sonographers",
      "dietetic_technicians_and_ophthalmic_medical_technicians",
      "dietitians_and_nutritionists",
      "dining_room_and_cafeteria_attendants_and_bartender_helpers",
      "directors_religious_activities_and_education",
      "disc_jockeys_except_radio",
      "dishwashers",
      "dispatchers_except_police_fire_and_ambulance",
      "door_to_door_sales_workers_news_and_street_vendors_and_related_workers",
      "driver_sales_workers_and_truck_drivers",
      "drywall_installers_ceiling_tile_installers_and_tapers",
      "earth_drillers_except_oil_and_gas",
      "economists",
      "editors",
      "education_and_childcare_administrators",
      "educational_services",
      "educational_guidance_and_career_counselors_and_advisors",
      "electric_motor_power_tool_and_related_repairers",
      "electrical_and_electronic_engineering_technologists_and_technicians",
      "electrical_and_electronics_engineers",
      "electrical_and_electronics_installers_and_repairers_transportation_equipment",
      "electrical_and_electronics_repairers_industrial_and_utility",
      "electrical_power_line_installers_and_repairers",
      "electrical_electronics_and_electromechanical_assemblers",
      "electricians",
      "electronic_equipment_installers_and_repairers_motor_vehicles",
      "elementary_and_middle_school_teachers",
      "elevator_and_escalator_installers_and_repairers",
      "eligibility_interviewers_government_programs",
      "embalmers_crematory_operators_and_funeral_attendants",
      "emergency_management_directors",
      "emergency_medical_technicians",
      "emergency_medicine_physicians",
      "engine_and_other_machine_assemblers",
      "engineers_all_other",
      "entertainers_and_performers_sports_and_related_workers_all_other",
      "entertainment_and_recreation_managers",
      "environmental_engineers",
      "environmental_science_and_geoscience_technicians",
      "environmental_scientists_and_specialists_including_health",
      "etchers_and_engravers",
      "excavating_and_loading_machine_and_dragline_operators_surface_mining",
      "executive_secretaries_and_executive_administrative_assistants",
      "exercise_physiologists",
      "exercise_trainers_and_group_fitness_instructors",
      "explosives_workers_ordnance_handling_experts_and_blasters",
      "extruding_forming_pressing_and_compacting_machine_setters_operators_and_tenders",
      "facilities_managers",
      "farmers_ranchers_and_other_agricultural_managers",
      "fashion_designers",
      "fast_food_and_counter_workers",
      "fence_erectors",
      "file_clerks",
      "finance_and_insurance",
      "financial_and_investment_analysts",
      "financial_clerks_all_other",
      "financial_examiners",
      "financial_managers",
      "fire_inspectors",
      "firefighters",
      "first_line_supervisors_of_construction_trades_and_extraction_workers",
      "first_line_supervisors_of_correctional_officers",
      "first_line_supervisors_of_farming_fishing_and_forestry_workers",
      "first_line_supervisors_of_firefighting_and_prevention_workers",
      "first_line_supervisors_of_food_preparation_and_serving_workers",
      "first_line_supervisors_of_housekeeping_and_janitorial_workers",
      "first_line_supervisors_of_landscaping_lawn_service_and_groundskeeping_workers",
      "first_line_supervisors_of_mechanics_installers_and_repairers",
      "first_line_supervisors_of_non_retail_sales_workers",
      "first_line_supervisors_of_office_and_administrative_support_workers",
      "first_line_supervisors_of_police_and_detectives",
      "first_line_supervisors_of_production_and_operating_workers",
      "first_line_supervisors_of_protective_service_workers_all_other",
      "first_line_supervisors_of_retail_sales_workers",
      "first_line_supervisors_of_security_workers",
      "fish_and_game_wardens",
      "fishing_and_hunting_workers",
      "flight_attendants",
      "floral_designers",
      "food_and_tobacco_roasting_baking_and_drying_machine_operators_and_tenders",
      "food_batchmakers",
      "food_cooking_machine_operators_and_tenders",
      "food_preparation_and_serving_related_workers_all_other",
      "food_preparation_workers",
      "food_processing_workers_all_other",
      "food_servers_nonrestaurant",
      "food_service_managers",
      "forest_and_conservation_workers",
      "forming_machine_setters_operators_and_tenders_metal_and_plastic",
      "fundraisers",
      "funeral_home_managers",
      "furnace_kiln_oven_drier_and_kettle_operators_and_tenders",
      "furniture_finishers",
      "gambling_cage_workers",
      "gambling_services_workers",
      "general_and_operations_managers",
      "geoscientists_and_hydrologists_except_geographers",
      "glaziers",
      "graders_and_sorters_agricultural_products",
      "graphic_designers",
      "grinding_lapping_polishing_and_buffing_machine_tool_setters_operators_and_tenders_metal_and_plastic",
      "hairdressers_hairstylists_and_cosmetologists",
      "hazardous_materials_removal_workers",
      "health_care_and_social_assistance",
      "healthcare_diagnosing_or_treating_practitioners_all_other",
      "healthcare_social_workers",
      "heating_air_conditioning_and_refrigeration_mechanics_and_installers",
      "heavy_vehicle_and_mobile_equipment_service_technicians_and_mechanics",
      "helpers_installation_maintenance_and_repair_workers",
      "helpers_production_workers",
      "helpers_construction_trades",
      "highway_maintenance_workers",
      "home_appliance_repairers",
      "home_health_aides",
      "hosts_and_hostesses_restaurant_lounge_and_coffee_shop",
      "hotel_motel_and_resort_desk_clerks",
      "human_resources_assistants_except_payroll_and_timekeeping",
      "human_resources_managers",
      "human_resources_workers",
      "industrial_and_refractory_machinery_mechanics",
      "industrial_engineers_including_health_and_safety",
      "industrial_production_managers",
      "industrial_truck_and_tractor_operators",
      "information_and_record_clerks_all_other",
      "information_security_analysts",
      "inspectors_testers_sorters_samplers_and_weighers",
      "insulation_workers",
      "insurance_claims_and_policy_processing_clerks",
      "insurance_sales_agents",
      "insurance_underwriters",
      "interior_designers",
      "interpreters_and_translators",
      "interviewers_except_eligibility_and_loan",
      "janitors_and_building_cleaners",
      "jewelers_and_precious_stone_and_metal_workers",
      "judges_magistrates_and_other_judicial_workers",
      "judicial_law_clerks",
      "laborers_and_freight_stock_and_material_movers_hand",
      "landscape_architects",
      "landscaping_and_groundskeeping_workers",
      "laundry_and_dry_cleaning_workers",
      "lawyers",
      "legal_secretaries_and_administrative_assistants",
      "legal_support_workers_all_other",
      "legislators",
      "librarians_and_media_collections_specialists",
      "library_assistants_clerical",
      "library_technicians",
      "licensed_practical_and_licensed_vocational_nurses",
      "life_scientists_all_other",
      "loan_interviewers_and_clerks",
      "locksmiths_and_safe_repairers",
      "locomotive_engineers_and_operators",
      "lodging_managers",
      "logging_workers",
      "logisticians",
      "machine_feeders_and_offbearers",
      "machinists",
      "magnetic_resonance_imaging_technologists",
      "maids_and_housekeeping_cleaners",
      "mail_clerks_and_mail_machine_operators_except_postal_service",
      "maintenance_and_repair_workers_general",
      "maintenance_workers_machinery",
      "management_analysts",
      "management_of_companies_and_enterprises",
      "managers_all_other",
      "manicurists_and_pedicurists",
      "manufactured_building_and_mobile_home_installers",
      "marine_engineers_and_naval_architects",
      "market_research_analysts_and_marketing_specialists",
      "marketing_managers",
      "marriage_and_family_therapists",
      "massage_therapists",
      "materials_engineers",
      "mathematicians",
      "mechanical_engineers",
      "media_and_communication_equipment_workers_all_other",
      "media_and_communication_workers_all_other",
      "medical_and_health_services_managers",
      "medical_assistants",
      "medical_records_specialists",
      "medical_scientists",
      "medical_secretaries_and_administrative_assistants",
      "medical_transcriptionists",
      "meeting_convention_and_event_planners",
      "mental_health_and_substance_abuse_social_workers",
      "mental_health_counselors",
      "merchandise_displayers_and_window_trimmers",
      "metal_furnace_operators_tenders_pourers_and_casters",
      "meter_readers_utilities",
      "millwrights",
      "mining_and_geological_engineers_including_mining_safety_engineers",
      "mining_quarrying_and_oil_and_gas_extraction",
      "miscellaneous_agricultural_workers",
      "miscellaneous_construction_and_related_workers",
      "miscellaneous_health_technologists_and_technicians",
      "miscellaneous_plant_and_system_operators",
      "miscellaneous_social_scientists_and_related_workers",
      "miscellaneous_vehicle_and_mobile_equipment_mechanics_installers_and_repairers",
      "model_makers_and_patternmakers_metal_and_plastic",
      "models_demonstrators_and_product_promoters",
      "molders_and_molding_machine_setters_operators_and_tenders_metal_and_plastic",
      "molders_shapers_and_casters_except_metal_and_plastic",
      "morticians_undertakers_and_funeral_arrangers",
      "motor_vehicle_operators_all_other",
      "music_directors_and_composers",
      "musicians_and_singers",
      "natural_sciences_managers",
      "network_and_computer_systems_administrators",
      "new_accounts_clerks",
      "news_analysts_reporters_and_journalists",
      "nuclear_engineers",
      "nuclear_medicine_technologists_and_medical_dosimetrists",
      "nuclear_technicians",
      "nurse_anesthetists",
      "nurse_midwives",
      "nurse_practitioners",
      "nursing_assistants",
      "occupational_health_and_safety_specialists_and_technicians",
      "occupational_therapists",
      "occupational_therapy_assistants_and_aides",
      "office_and_administrative_support_workers_all_other",
      "office_clerks_general",
      "office_machine_operators_except_computer",
      "operations_research_analysts",
      "opticians_dispensing",
      "optometrists",
      "order_clerks",
      "orderlies_and_psychiatric_aides",
      "other_assemblers_and_fabricators",
      "other_community_and_social_service_specialists",
      "other_designers",
      "other_drafters",
      "other_educational_instruction_and_library_workers",
      "other_engineering_technologists_and_technicians_except_drafters",
      "other_entertainment_attendants_and_related_workers",
      "other_extraction_workers",
      "other_financial_specialists",
      "other_grounds_maintenance_workers",
      "other_healthcare_practitioners_and_technical_occupations",
      "other_healthcare_support_workers",
      "other_installation_maintenance_and_repair_workers",
      "other_life_physical_and_social_science_technicians",
      "other_machine_tool_setters_operators_and_tenders_metal_and_plastic",
      "other_material_moving_workers",
      "other_mathematical_science_occupations",
      "other_metal_workers_and_plastic_workers",
      "other_personal_appearance_workers",
      "other_physicians",
      "other_production_equipment_operators_and_tenders",
      "other_production_workers",
      "other_protective_service_workers",
      "other_psychologists",
      "other_rail_transportation_workers",
      "other_teachers_and_instructors",
      "other_textile_apparel_and_furnishings_workers",
      "other_transportation_workers",
      "other_woodworkers",
      "packaging_and_filling_machine_operators_and_tenders",
      "packers_and_packagers_hand",
      "painters_and_paperhangers",
      "painting_workers",
      "paper_goods_machine_setters_operators_and_tenders",
      "paralegals_and_legal_assistants",
      "paramedics",
      "parking_attendants",
      "parking_enforcement_workers",
      "parts_salespersons",
      "passenger_attendants",
      "payroll_and_timekeeping_clerks",
      "personal_care_aides",
      "personal_care_and_service_workers_all_other",
      "personal_financial_advisors",
      "personal_service_managers_all_other",
      "pest_control_workers",
      "petroleum_engineers",
      "pharmacists",
      "pharmacy_aides",
      "pharmacy_technicians",
      "phlebotomists",
      "photographers",
      "photographic_process_workers_and_processing_machine_operators",
      "physical_scientists_all_other",
      "physical_therapist_assistants_and_aides",
      "physical_therapists",
      "physician_assistants",
      "pipelayers",
      "plasterers_and_stucco_masons",
      "plumbers_pipefitters_and_steamfitters",
      "podiatrists",
      "police_officers",
      "postal_service_clerks",
      "postal_service_mail_carriers",
      "postal_service_mail_sorters_processors_and_processing_machine_operators",
      "postmasters_and_mail_superintendents",
      "postsecondary_teachers",
      "power_plant_operators_distributors_and_dispatchers",
      "precision_instrument_and_equipment_repairers",
      "prepress_technicians_and_workers",
      "preschool_and_kindergarten_teachers",
      "pressers_textile_garment_and_related_materials",
      "print_binding_and_finishing_workers",
      "printing_press_operators",
      "private_detectives_and_investigators",
      "probation_officers_and_correctional_treatment_specialists",
      "procurement_clerks",
      "producers_and_directors",
      "production_planning_and_expediting_clerks",
      "professional_scientific_and_technical_services",
      "project_management_specialists",
      "proofreaders_and_copy_markers",
      "property_appraisers_and_assessors",
      "property_real_estate_and_community_association_managers",
      "psychiatric_technicians",
      "public_relations_and_fundraising_managers",
      "public_relations_specialists",
      "public_safety_telecommunicators",
      "pumping_station_operators",
      "purchasing_agents_except_wholesale_retail_and_farm_products",
      "purchasing_managers",
      "radiation_therapists",
      "radio_and_telecommunications_equipment_installers_and_repairers",
      "radiologic_technologists_and_technicians",
      "radiologists",
      "rail_track_laying_and_maintenance_equipment_operators",
      "railroad_conductors_and_yardmasters",
      "real_estate_and_rental_and_leasing",
      "real_estate_brokers_and_sales_agents",
      "receptionists_and_information_clerks",
      "recreation_workers",
      "recreational_therapists",
      "refuse_and_recyclable_material_collectors",
      "registered_nurses",
      "rehabilitation_counselors",
      "reinforcing_iron_and_rebar_workers",
      "religious_workers_all_other",
      "reservation_and_transportation_ticket_agents_and_travel_clerks",
      "residential_advisors",
      "respiratory_therapists",
      "retail_salespersons",
      "riggers",
      "roofers",
      "roustabouts_oil_and_gas",
      "sailors_and_marine_oilers",
      "sales_and_related_workers_all_other",
      "sales_engineers",
      "sales_managers",
      "sales_representatives_of_services_except_advertising_insurance_financial_services_and_travel",
      "sales_representatives_wholesale_and_manufacturing",
      "sawing_machine_setters_operators_and_tenders_wood",
      "school_bus_monitors",
      "school_psychologists",
      "secondary_school_teachers",
      "secretaries_and_administrative_assistants_except_legal_medical_and_executive",
      "securities_commodities_and_financial_services_sales_agents",
      "security_and_fire_alarm_systems_installers",
      "security_guards_and_gambling_surveillance_officers",
      "self_employed_in_own_not_incorporated_business_workers_and_unpaid_family_workers",
      "septic_tank_servicers_and_sewer_pipe_cleaners",
      "sewing_machine_operators",
      "sheet_metal_workers",
      "ship_and_boat_captains_and_operators",
      "ship_engineers",
      "shipping_receiving_and_inventory_clerks",
      "shoe_and_leather_workers",
      "shuttle_drivers_and_chauffeurs",
      "skincare_specialists",
      "small_engine_mechanics",
      "social_and_community_service_managers",
      "social_and_human_service_assistants",
      "social_science_research_assistants",
      "social_workers_all_other",
      "sociologists",
      "software_developers",
      "software_quality_assurance_analysts_and_testers",
      "solar_photovoltaic_installers",
      "special_education_teachers",
      "speech_language_pathologists",
      "stationary_engineers_and_boiler_operators",
      "statistical_assistants",
      "statisticians",
      "stockers_and_order_fillers",
      "structural_iron_and_steel_workers",
      "structural_metal_fabricators_and_fitters",
      "substance_abuse_and_behavioral_disorder_counselors",
      "supervisors_of_personal_care_and_service_workers",
      "supervisors_of_transportation_and_material_moving_workers",
      "surgeons",
      "surgical_technologists",
      "survey_researchers",
      "surveying_and_mapping_technicians",
      "surveyors_cartographers_and_photogrammetrists",
      "switchboard_operators_including_answering_service",
      "tailors_dressmakers_and_sewers",
      "tax_examiners_and_collectors_and_revenue_agents",
      "tax_preparers",
      "taxi_drivers",
      "teaching_assistants",
      "technical_writers",
      "telecommunications_line_installers_and_repairers",
      "telemarketers",
      "telephone_operators",
      "television_video_and_film_camera_operators_and_editors",
      "tellers",
      "textile_machine_setters_operators_and_tenders",
      "therapists_all_other",
      "tire_builders",
      "title_examiners_abstractors_and_searchers",
      "tool_and_die_makers",
      "tour_and_travel_guides",
      "training_and_development_managers",
      "training_and_development_specialists",
      "transportation_and_warehousing",
      "transportation_inspectors",
      "transportation_security_screeners",
      "transportation_service_attendants",
      "transportation_storage_and_distribution_managers",
      "travel_agents",
      "tree_trimmers_and_pruners",
      "tutors",
      "umpires_referees_and_other_sports_officials",
      "underground_mining_machine_operators",
      "upholsterers",
      "urban_and_regional_planners",
      "ushers_lobby_attendants_and_ticket_takers",
      "utilities",
      "veterinarians",
      "veterinary_assistants_and_laboratory_animal_caretakers",
      "veterinary_technologists_and_technicians",
      "waiters_and_waitresses",
      "water_and_wastewater_treatment_plant_and_system_operators",
      "web_and_digital_interface_designers",
      "web_developers",
      "weighers_measurers_checkers_and_samplers_recordkeeping",
      "welding_soldering_and_brazing_workers",
      "wholesale_and_retail_buyers_except_farm_products",
      "wind_turbine_service_technicians",
      "woodworking_machine_setters_operators_and_tenders_except_sawing",
      "word_processors_and_typists",
      "writers_and_authors"
    )
    interpret_as(mdr, field = "economic_industry_and_occupation", val, value, val_set, field_values)
  }


#' interpret_as_economic_journey_and_place_of_work
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_journey_and_place_of_work <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "worked_in_state_of_residence",
      "worked_in_county_of_residence",
      "worked_outside_county_of_residence",
      "worked_outside_state_of_residence",
      "living_in_a_place",
      "not_living_in_a_place",
      "worked_in_place_of_residence",
      "worked_outside_place_of_residence",
      "living_in_the_12_selected_states",
      "not_living_in_the_12_selected_states",
      "worked_in_mcd_of_residence",
      "worked_outside_mcd_of_residence",
      "aggregate_travel_time_to_work_in_minutes",
      "aggregate_number_of_vehicles_car_truck_or_van_used_in_commuting",
      "living_in_a_principal_city",
      "living_outside_any_principal_city",
      "worked_in_a_different_metropolitan_statistical_area",
      "worked_in_a_micropolitan_statistical_area",
      "worked_in_metropolitan_statistical_area_of_residence",
      "worked_in_a_principal_city",
      "worked_outside_any_metropolitan_or_micropolitan_statistical_area",
      "worked_outside_any_principal_city",
      "worked_in_a_different_micropolitan_statistical_area",
      "worked_in_a_metropolitan_statistical_area",
      "worked_in_micropolitan_statistical_area_of_residence",
      "car_truck_or_van",
      "carpooled",
      "public_transportation_excluding_taxicab",
      "motorcycle",
      "other_means",
      "taxicab",
      "railroad",
      "subway_or_elevated",
      "bicycle",
      "ferryboat",
      "car_truck_or_van_carpooled",
      "car_truck_or_van_drove_alone",
      "bus_or_trolley_bus",
      "railroad_or_ferryboat",
      "streetcar_or_trolley_car_carro_publico_in_puerto_rico",
      "taxicab_motorcycle_or_other_means",
      "walked",
      "worked_at_home",
      "streetcar_or_trolley_car_carro_publico_in_puerto_rico_subway_or_elevated",
      "taxicab_motorcycle_bicycle_or_other_means",
      "10_00_a_m_to_10_59_a_m",
      "11_00_a_m_to_11_59_a_m",
      "12_00_a_m_to_4_59_a_m",
      "12_00_p_m_to_3_59_p_m",
      "4_00_p_m_to_11_59_p_m",
      "5_00_a_m_to_5_29_a_m",
      "5_30_a_m_to_5_59_a_m",
      "6_00_a_m_to_6_29_a_m",
      "6_30_a_m_to_6_59_a_m",
      "7_00_a_m_to_7_29_a_m",
      "7_30_a_m_to_7_59_a_m",
      "8_00_a_m_to_8_29_a_m",
      "8_30_a_m_to_8_59_a_m",
      "9_00_a_m_to_9_59_a_m",
      "in_2_person_carpool",
      "in_3_or_more_person_carpool",
      "in_3_person_carpool",
      "in_4_or_more_person_carpool",
      "in_4_person_carpool",
      "in_5_or_6_person_carpool",
      "in_7_or_more_person_carpool",
      "drove_alone",
      "10_to_14_minutes",
      "15_to_19_minutes",
      "20_to_24_minutes",
      "25_to_29_minutes",
      "30_to_34_minutes",
      "35_to_39_minutes",
      "40_to_44_minutes",
      "45_to_59_minutes",
      "5_to_9_minutes",
      "60_to_89_minutes",
      "90_or_more_minutes",
      "less_than_5_minutes",
      "35_to_44_minutes",
      "60_or_more_minutes",
      "less_than_10_minutes",
      "did_not_work_at_home",
      "other_means_including_those_who_worked_at_home",
      "length_of_time",
      "time_arriving_at_work_from_home",
      "time_leaving_home_to_go_to_work",
      "travel_time_to_work"
    )
    interpret_as(mdr, field = "economic_journey_and_place_of_work", val, value, val_set, field_values)
  }


#' interpret_as_housing_tenure_owner_renter
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_housing_tenure_owner_renter <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "householder_lived_in_owner_occupied_housing_units",
      "householder_lived_in_renter_occupied_housing_units",
      "owner_occupied",
      "renter_occupied",
      "housing_units_with_a_mortgage",
      "housing_units_without_a_mortgage",
      "occupied_housing_units",
      "renter_occupied_housing_units",
      "renter_occupied_housing_units_paying_cash_rent",
      "vacant_for_rent_and_rented_not_occupied_housing_units",
      "vacant_for_sale_only_and_sold_not_occupied_housing_units",
      "vacant_housing_units",
      "owner_occupied_housing_units",
      "all_other_vacant_units",
      "living_in_owner_occupied_housing_unit",
      "living_in_renter_occupied_housing_unit"
    )
    interpret_as(mdr, field = "housing_tenure_owner_renter", val, value, val_set, field_values)
  }


#' interpret_as_social_migration_residence_1_year_ago
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_migration_residence_1_year_ago <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "moved_from_abroad",
      "moved_from_different_county_within_same_state",
      "moved_from_different_municipio",
      "moved_from_different_state",
      "moved_from_elsewhere",
      "moved_from_the_united_states",
      "same_house_1_year_ago",
      "movers_from_abroad",
      "movers_to_different_state",
      "movers_within_same_state",
      "same_residence_non_movers",
      "different_house_in_puerto_rico_1_year_ago",
      "different_house_in_united_states_1_year_ago",
      "moved_from_principal_city",
      "moved_from_remainder_of_metropolitan_statistical_area",
      "moved_from_remainder_of_micropolitan_statistical_area",
      "total_living_in_area_1_year_ago",
      "different_county",
      "different_state",
      "elsewhere",
      "elsewhere_1_year_ago",
      "elsewhere_in_puerto_rico",
      "same_city_or_town",
      "current_residence_midwest",
      "current_residence_northeast",
      "current_residence_south",
      "current_residence_west",
      "foreign_country",
      "midwest",
      "northeast",
      "puerto_rico",
      "south",
      "u_s_island_areas",
      "west",
      "moved_to_different_county_within_same_state",
      "moved_to_different_municipio",
      "moved_to_different_state",
      "moved_to_the_united_states",
      "moved_within_same_county",
      "moved_within_same_municipio",
      "same_house",
      "different_county_same_state", "different_municipio", "same_county",
      "same_municipio",
      "same_state",
      "metropolitan_statistical_area",
      "micropolitan_statistical_area",
      "different_metropolitan_statistical_area",
      "different_micropolitan_statistical_area",
      "same_metropolitan_statistical_area",
      "same_micropolitan_statistical_area",
      "abroad_1_year_ago",
      "united_states_1_year_ago",
      "not_in_a_metropolitan_or_micropolitan_statistical_area_1_year_ago",
      "same_address",
      "different_address_in_puerto_rico_or_the_united_states",
      "different_address_in_the_united_states",
      "abroad",
      "in_puerto_rico",
      "in_the_united_states",
      "different_house",
      "vacant_current_residence_elsewhere"
    )
    interpret_as(mdr, field = "social_migration_residence_1_year_ago", val, value, val_set, field_values)
  }


#' interpret_as_social_educational_attainment
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_educational_attainment <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "bachelor_s_degree",
      "graduate_or_professional_degree",
      "high_school_graduate_includes_equivalency",
      "less_than_high_school_graduate",
      "some_college_or_associate_s_degree",
      "not_high_school_graduate",
      "9_th_to_12_th_grade_no_diploma",
      "associate_s_degree",
      "less_than_9_th_grade",
      "some_college_no_degree",
      "10_th_grade",
      "11_th_grade",
      "12_th_grade_no_diploma",
      "5_th_and_6_th_grade",
      "7_th_and_8_th_grade",
      "9_th_grade",
      "doctorate_degree",
      "master_s_degree",
      "no_schooling_completed",
      "nursery_to_4_th_grade",
      "professional_school_degree",
      "some_college_1_or_more_years_no_degree",
      "some_college_less_than_1_year",
      "1_st_grade",
      "2_nd_grade",
      "3_rd_grade",
      "4_th_grade",
      "5_th_grade",
      "6_th_grade",
      "7_th_grade",
      "8_th_grade",
      "ged_or_alternative_credential",
      "kindergarten",
      "nursery_school",
      "regular_high_school_diploma",
      "arts_humanities_and_other",
      "business",
      "education",
      "science_and_engineering",
      "science_and_engineering_related_fields",
      "biological_agricultural_and_environmental_sciences",
      "communications",
      "computers_mathematics_and_statistics",
      "engineering",
      "liberal_arts_and_history",
      "literature_and_languages",
      "multidisciplinary_studies",
      "other",
      "physical_and_related_sciences",
      "psychology",
      "social_sciences",
      "visual_and_performing_arts",
      "bachelor_s_degree_or_higher",
      "some_college_associate_s_degree",
      "college_or_graduate_school",
      "less_than_high_school_diploma",
      "nursery_school_through_12_th_grade",
      "high_school_graduate_includes_equivalency_some_college_or_associate_s_degree",
      "less_than_high_school_graduate_or_equivalency",
      "high_school_graduate_including_equivalency"
    )
    interpret_as(mdr, field = "social_educational_attainment", val, value, val_set, field_values)
  }


#' interpret_as_social_marital_status
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_marital_status <- function(mdr, val, value, field_values = NULL) {
  val_set <- c(
    "divorced",
    "never_married",
    "now_married_except_separated",
    "separated",
    "widowed",
    "now_married_including_separated_and_spouse_absent",
    "unmarried_never_married_widowed_and_divorced",
    "ever_married",
    "married_spouse_absent",
    "now_married",
    "divorced_last_year",
    "married_last_year",
    "married_spouse_present",
    "not_divorced_last_year",
    "not_married_last_year",
    "not_widowed_last_year",
    "widowed_last_year",
    "other",
    "once",
    "three_or_more_times",
    "two_times",
    "now_married_including_spouse_absent"
  )
  interpret_as(mdr, field = "social_marital_status", val, value, val_set, field_values)
}


#' interpret_as_social_language_spoken_at_home
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_language_spoken_at_home <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "speak_only_english",
      "speak_other_languages",
      "speak_spanish",
      "speak_english_very_well",
      "speak_english_less_than_very_well",
      "speak_other_language",
      "speak_another_language",
      "speak_asian_and_pacific_island_languages",
      "speak_other_indo_european_languages",
      "speak_english_not_at_all",
      "speak_english_not_well",
      "speak_english_well",
      "amharic_somali_or_other_afro_asiatic_languages",
      "arabic",
      "armenian",
      "bengali",
      "chinese_incl_mandarin_cantonese",
      "french_incl_cajun",
      "german",
      "greek",
      "gujarati",
      "haitian",
      "hebrew",
      "hindi",
      "hmong",
      "ilocano_samoan_hawaiian_or_other_austronesian_languages",
      "italian",
      "japanese",
      "khmer",
      "korean",
      "malayalam_kannada_or_other_dravidian_languages",
      "navajo",
      "nepali_marathi_or_other_indic_languages",
      "other_and_unspecified_languages",
      "other_indo_european_languages",
      "other_languages_of_asia",
      "other_native_languages_of_north_america",
      "persian_incl_farsi_dari",
      "polish",
      "portuguese",
      "punjabi",
      "russian",
      "serbo_croatian",
      "spanish",
      "swahili_or_other_languages_of_central_eastern_and_southern_africa",
      "tagalog_incl_filipino",
      "tamil",
      "telugu",
      "thai_lao_or_other_tai_kadai_languages",
      "ukrainian_or_other_slavic_languages",
      "urdu",
      "vietnamese",
      "yiddish_pennsylvania_dutch_or_other_west_germanic_languages",
      "yoruba_twi_igbo_or_other_languages_of_western_africa",
      "speak_language_other_than_english",
      "asian_and_pacific_island_languages",
      "english_only",
      "french_haitian_or_cajun",
      "german_or_other_west_germanic_languages",
      "other_asian_and_pacific_island_languages",
      "other_languages",
      "russian_polish_or_other_slavic_languages",
      "limited_english_speaking_household",
      "not_a_limited_english_speaking_household",
      "ability_to_speak_english",
      "specific_languages_spoken"
    )
    interpret_as(mdr, field = "social_language_spoken_at_home", val, value, val_set, field_values)
  }


#' interpret_as_economic_poverty_status
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_economic_poverty_status <- function(mdr, val, value, field_values = NULL) {
  val_set <- c(
    "under_1_00",
    "1_00_to_1_99",
    "2_00_and_over",
    "100_to_149_percent_of_the_poverty_level",
    "at_or_above_150_percent_of_the_poverty_level",
    "below_100_percent_of_the_poverty_level",
    "income_in_the_past_12_months_at_or_above_poverty_level",
    "income_in_the_past_12_months_below_poverty_level",
    "100_to_199_percent_of_poverty_level_in_the_past_12_months",
    "200_percent_or_more_of_poverty_level_in_the_past_12_months",
    "below_100_percent_of_poverty_level_in_the_past_12_months",
    "did_not_receive_public_assistance_income",
    "received_public_assistance_income",
    "income_in_the_past_12_months_at_or_above_the_poverty_level",
    "income_in_the_past_12_months_below_the_poverty_level",
    "1_30_to_1_49",
    "1_50_to_1_84",
    "1_85_and_above",
    "under_1_30",
    "1_00_to_1_24",
    "1_25_to_1_49",
    "1_50_to_1_74",
    "1_75_to_1_84",
    "1_85_to_1_99",
    "2_00_to_2_99",
    "3_00_to_3_99",
    "4_00_to_4_99",
    "5_00_and_over",
    "50_to_74",
    "75_to_99",
    "under_50",
    "50_to_99",
    "1_00_to_1_49",
    "1_50_to_1_99",
    "1_00_to_1_37_of_poverty_threshold",
    "1_38_to_1_99_of_poverty_threshold",
    "2_00_to_3_99_of_poverty_threshold",
    "4_00_of_poverty_threshold_and_over",
    "under_1_00_of_poverty_threshold"
  )
  interpret_as(mdr, field = "economic_poverty_status", val, value, val_set, field_values)
}


#' interpret_as_social_place_of_birth
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_place_of_birth <- function(mdr, val, value, field_values = NULL) {
  val_set <- c(
    "born_in_other_state_in_the_united_states",
    "born_in_other_state_of_the_united_states",
    "born_outside_the_united_states",
    "foreign_born",
    "native",
    "born_in_other_state_in_the_united_states",
    "born_in_puerto_rico",
    "born_in_state_of_residence",
    "born_in_the_united_states",
    "native_born_elsewhere",
    "native_born_outside_the_united_states",
    "population_born_outside_puerto_rico",
    "population_born_outside_the_united_states",
    "child_is_foreign_born",
    "child_is_native",
    "both_parents_foreign_born",
    "foreign_born_parent",
    "one_native_and_one_foreign_born_parent",
    "both_parents_native",
    "native_parent",
    "native_population",
    "americas",
    "asia",
    "australia_and_new_zealand_subregion",
    "born_abroad_of_american_parent_s",
    "caribbean",
    "central_america",
    "china",
    "eastern_africa",
    "eastern_asia",
    "europe",
    "latin_america",
    "middle_africa",
    "northern_africa",
    "other_areas",
    "south_america",
    "southern_africa",
    "southern_europe",
    "united_kingdom_inc_crown_dependencies",
    "western_africa",
    "africa",
    "africa_n_e_c",
    "argentina",
    "australia",
    "azores_islands",
    "bahamas",
    "barbados",
    "belize",
    "bolivia",
    "brazil",
    "cabo_verde",
    "cameroon",
    "chile",
    "china_excluding_hong_kong_and_taiwan",
    "china_excluding_taiwan",
    "colombia",
    "costa_rica",
    "cuba",
    "denmark",
    "dominica",
    "dominican_republic",
    "eastern_europe",
    "ecuador",
    "egypt",
    "el_salvador",
    "england",
    "eritrea",
    "ethiopia",
    "fiji",
    "ghana",
    "greece",
    "grenada",
    "guatemala",
    "guyana",
    "haiti",
    "honduras",
    "hong_kong",
    "ireland",
    "italy",
    "jamaica",
    "japan",
    "kenya",
    "korea",
    "liberia",
    "mexico",
    "morocco",
    "nicaragua",
    "nigeria",
    "northern_america",
    "northern_europe",
    "norway",
    "oceania",
    "oceania_n_e_c",
    "other_australian_and_new_zealand_subregion",
    "other_caribbean",
    "other_central_america",
    "other_eastern_africa",
    "other_eastern_asia",
    "other_middle_africa",
    "other_northern_africa",
    "other_northern_europe",
    "other_south_america",
    "other_southern_africa",
    "other_southern_europe",
    "other_western_africa",
    "panama",
    "peru",
    "portugal",
    "scotland",
    "sierra_leone",
    "somalia",
    "south_africa",
    "south_central_asia",
    "south_eastern_asia",
    "spain",
    "st_vincent_and_the_grenadines",
    "sudan",
    "sweden",
    "taiwan",
    "trinidad_and_tobago",
    "united_kingdom_excluding_england_and_scotland",
    "uruguay",
    "venezuela",
    "west_indies",
    "western_asia",
    "western_europe",
    "afghanistan",
    "albania",
    "armenia",
    "asia_n_e_c",
    "austria",
    "bangladesh",
    "belarus",
    "belgium",
    "bosnia_and_herzegovina",
    "bulgaria",
    "burma",
    "cambodia",
    "canada",
    "croatia",
    "czechoslovakia_includes_czech_republic_and_slovakia",
    "europe_n_e_c",
    "france",
    "germany",
    "hungary",
    "india",
    "indonesia",
    "iran",
    "iraq",
    "israel",
    "jordan",
    "kazakhstan",
    "kuwait",
    "laos",
    "latvia",
    "lebanon",
    "lithuania",
    "macedonia",
    "malaysia",
    "moldova",
    "nepal",
    "netherlands",
    "other_eastern_europe",
    "other_northern_america",
    "other_south_central_asia",
    "other_south_eastern_asia",
    "other_western_asia",
    "other_western_europe",
    "pakistan",
    "philippines",
    "poland",
    "romania",
    "russia",
    "saudi_arabia",
    "serbia",
    "singapore",
    "sri_lanka",
    "switzerland",
    "syria",
    "thailand",
    "turkey",
    "ukraine",
    "uzbekistan",
    "vietnam",
    "yemen",
    "other",
    "native_born",
    "foreign_born_naturalized_citizens"
  )
  interpret_as(mdr, field = "social_place_of_birth", val, value, val_set, field_values)
}


#' interpret_as_social_year_of_entry
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_year_of_entry <- function(mdr, val, value, field_values = NULL) {
  val_set <- c(
    "entered_1990_to_1999",
    "entered_2000_to_2009",
    "entered_2010_or_later",
    "entered_before_1990",
    "entered_before_2000"
  )
  interpret_as(mdr, field = "social_year_of_entry", val, value, val_set, field_values)
}


#' interpret_as_social_citizenship_status
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_social_citizenship_status <- function(mdr, val, value, field_values = NULL) {
  val_set <- c(
    "naturalized_u_s_citizen",
    "naturalized_citizens",
    "not_a_u_s_citizen",
    "u_s_citizen_by_naturalization",
    "u_s_citizen_born_abroad_of_american_parent_s",
    "u_s_citizen_born_in_puerto_rico",
    "u_s_citizen_born_in_puerto_rico_or_u_s_island_areas",
    "u_s_citizen_born_in_the_united_states",
    "u_s_citizen_born_in_u_s_or_u_s_island_areas",
    "naturalized_1990_to_1994",
    "naturalized_1995_to_1999",
    "naturalized_2000_to_2004",
    "naturalized_2005_to_2009",
    "naturalized_2010_to_2014",
    "naturalized_2015_or_later",
    "naturalized_before_1990",
    "naturalized_citizen",
    "not_a_citizen",
    "naturalized",
    "noncitizen"
  )
  interpret_as(mdr, field = "social_citizenship_status", val, value, val_set, field_values)
}


#' interpret_as_demographic_race
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_race <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "black_or_african_american_alone",
      "hispanic_or_latino_population",
      "people_who_are_american_indian_and_alaska_native_alone",
      "people_who_are_asian_alone",
      "people_who_are_native_hawaiian_and_other_pacific_islander_alone",
      "people_who_are_some_other_race_alone",
      "people_who_are_two_or_more_races",
      "people_who_are_white_alone",
      "white_alone_not_hispanic_or_latino_population",
      "american_indian_and_alaska_native_alone_population_in_puerto_rico",
      "american_indian_and_alaska_native_alone_population_in_the_united_states",
      "asian_alone_population_in_puerto_rico",
      "asian_alone_population_in_the_united_states",
      "black_or_african_american_alone_population_in_puerto_rico",
      "black_or_african_american_alone_population_in_the_united_states",
      "hispanic_or_latino_population_in_puerto_rico",
      "hispanic_or_latino_population_in_the_united_states",
      "native_hawaiian_and_other_pacific_islander_alone_population_in_puerto_rico",
      "native_hawaiian_and_other_pacific_islander_alone_population_in_the_united_states",
      "some_other_race_alone_population_in_the_united_states",
      "some_other_race_population_in_puerto_rico",
      "two_or_more_races_population_in_puerto_rico",
      "two_or_more_races_population_in_the_united_states",
      "white_alone_population_in_puerto_rico",
      "white_alone_population_in_the_united_states",
      "white_alone_not_hispanic_or_latino_population_in_puerto_rico",
      "white_alone_not_hispanic_or_latino_population_in_the_united_states",
      "american_indian_and_alaskan_native_alone_population_1_year_and_over_in_puerto_rico",
      "american_indian_and_alaskan_native_alone_population_1_year_and_over_in_the_united_states",
      "asian_alone_population_1_year_and_over_in_puerto_rico",
      "asian_alone_population_1_year_and_over_in_the_united_states",
      "black_or_african_american_alone_population_1_year_and_over_in_puerto_rico",
      "black_or_african_american_alone_population_1_year_and_over_in_the_united_states",
      "hispanic_or_latino_population_1_year_and_over_in_puerto_rico",
      "hispanic_or_latino_population_1_year_and_over_in_the_united_states",
      "native_hawaiian_and_other_pacific_islander_alone_population_1_year_and_over_in_puerto_rico",
      "native_hawaiian_and_other_pacific_islander_alone_population_1_year_and_over_in_the_united_states",
      "some_other_race_alone_population_1_year_and_over_in_puerto_rico",
      "some_other_race_alone_population_1_year_and_over_in_the_united_states",
      "two_or_more_races_population_1_year_and_over_in_puerto_rico",
      "two_or_more_races_population_1_year_and_over_in_the_united_states",
      "white_alone_population_1_year_and_over_in_puerto_rico",
      "white_alone_population_1_year_and_over_in_the_united_states",
      "white_alone_not_hispanic_or_latino_population_1_year_and_over_in_puerto_rico",
      "white_alone_not_hispanic_or_latino_population_1_year_and_over_in_the_united_states",
      "american_indian_and_alaska_native_alone_population_1_year_and_over",
      "asian_alone_population_1_year_and_over",
      "black_or_african_american_alone_population_1_year_and_over",
      "hispanic_or_latino_population_1_year_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_1_year_and_over",
      "some_other_race_alone_population_1_year_and_over",
      "two_or_more_races_population_1_year_and_over",
      "white_alone_population_1_year_and_over",
      "white_alone_not_hispanic_or_latino_population_1_year_and_over",
      "american_indian_and_alaska_native_alone_workers_16_years_and_over",
      "asian_alone_workers_16_years_and_over",
      "black_or_african_american_alone_workers_16_years_and_over",
      "hispanic_or_latino_workers_16_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_workers_16_years_and_over",
      "some_other_race_alone_workers_16_years_and_over",
      "two_or_more_races_workers_16_years_and_over",
      "white_alone_workers_16_years_and_over",
      "white_alone_not_hispanic_or_latino_workers_16_years_and_over",
      "grandparents_american_indian_and_alaska_native_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_asian_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_black_or_african_american_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_hispanic_or_latino_living_with_own_grandchildren_under_18_years",
      "grandparents_native_hawaiian_and_other_pacific_islander_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_some_other_race_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_two_or_more_races_living_with_own_grandchildren_under_18_years",
      "grandparents_white_alone_living_with_own_grandchildren_under_18_years",
      "grandparents_white_alone_not_hispanic_or_latino_living_with_own_grandchildren_under_18_years",
      "households_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "households_with_a_householder_who_is_asian_alone",
      "households_with_a_householder_who_is_black_or_african_american_alone",
      "households_with_a_householder_who_is_hispanic_or_latino",
      "households_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "households_with_a_householder_who_is_some_other_race_alone",
      "households_with_a_householder_who_is_two_or_more_races",
      "households_with_a_householder_who_is_white_alone",
      "households_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "population_in_households_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "population_in_households_with_a_householder_who_is_asian_alone",
      "population_in_households_with_a_householder_who_is_black_or_african_american_alone",
      "population_in_households_with_a_householder_who_is_hispanic_or_latino",
      "population_in_households_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "population_in_households_with_a_householder_who_is_some_other_race_alone",
      "population_in_households_with_a_householder_who_is_two_or_more_races",
      "population_in_households_with_a_householder_who_is_white_alone",
      "population_in_households_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "american_indian_and_alaska_native_alone_population_15_to_54_years",
      "asian_alone_population_15_to_54_years",
      "black_or_african_american_alone_population_15_to_54_years",
      "hispanic_or_latino_population_15_to_54_years",
      "native_hawaiian_and_other_pacific_islander_alone_population_15_to_54_years",
      "some_other_race_alone_population_15_to_54_years",
      "two_or_more_races_population_15_to_54_years",
      "white_alone_population_15_to_54_years",
      "white_alone_not_hispanic_or_latino_population_15_to_54_years",
      "american_indian_and_alaska_native_alone_population_15_years_and_over",
      "asian_alone_population_15_years_and_over",
      "black_or_african_american_alone_population_15_years_and_over",
      "hispanic_or_latino_population_15_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_15_years_and_over",
      "some_other_race_alone_population_15_years_and_over",
      "two_or_more_races_population_15_years_and_over",
      "white_alone_population_15_years_and_over",
      "white_alone_not_hispanic_or_latino_population_15_years_and_over",
      "black_or_african_american_alone_women_15_to_50_years",
      "hispanic_or_latino_women_15_to_50_years",
      "native_hawaiian_and_other_pacific_islander_alone_women_15_to_50_years",
      "some_other_race_alone_women_15_to_50_years",
      "two_or_more_races_women_15_to_50_years",
      "white_alone_women_15_to_50_years",
      "white_alone_not_hispanic_or_latino_women_15_to_50_years",
      "american_indian_and_alaska_native_alone_women_15_to_50_years",
      "asian_alone_women_15_to_50_years",
      "american_indian_and_alaska_native_alone_population_3_years_and_over",
      "asian_alone_population_3_years_and_over",
      "black_or_african_american_alone_population_3_years_and_over",
      "hispanic_or_latino_population_3_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_3_years_and_over",
      "some_other_race_alone_population_3_years_and_over",
      "two_or_more_races_population_3_years_and_over",
      "white_alone_population_3_years_and_over",
      "white_alone_not_hispanic_or_latino_population_3_years_and_over",
      "american_indian_and_alaska_native_alone_population_5_years_and_over",
      "asian_alone_population_5_years_and_over",
      "black_or_african_american_alone_population_5_years_and_over",
      "hispanic_or_latino_population_5_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_5_years_and_over",
      "some_other_race_alone_population_5_years_and_over",
      "two_or_more_races_population_5_years_and_over",
      "white_alone_population_5_years_and_over",
      "white_alone_not_hispanic_or_latino_population_5_years_and_over",
      "american_indian_and_alaska_native_alone_population_for_whom_poverty_status_is_determined",
      "asian_alone_population_for_whom_poverty_status_is_determined",
      "black_or_african_american_alone_population_for_whom_poverty_status_is_determined",
      "hispanic_or_latino_population_for_whom_poverty_status_is_determined",
      "native_hawaiian_and_other_pacific_islander_alone_population_for_whom_poverty_status_is_determined",
      "some_other_race_alone_population_for_whom_poverty_status_is_determined",
      "two_or_more_races_population_for_whom_poverty_status_is_determined",
      "white_alone_population_for_whom_poverty_status_is_determined",
      "white_alone_not_hispanic_or_latino_population_for_whom_poverty_status_is_determined",
      "families_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "families_with_a_householder_who_is_asian_alone",
      "families_with_a_householder_who_is_black_or_african_american_alone",
      "families_with_a_householder_who_is_hispanic_or_latino",
      "families_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "families_with_a_householder_who_is_some_other_race_alone",
      "families_with_a_householder_who_is_two_or_more_races",
      "families_with_a_householder_who_is_white_alone",
      "families_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "american_indian_and_alaska_native_alone_civilian_noninstitutionalized_population",
      "asian_alone_civilian_noninstitutionalized_population",
      "black_or_african_american_alone_civilian_noninstitutionalized_population",
      "hispanic_or_latino_civilian_noninstitutionalized_population",
      "native_hawaiian_and_other_pacific_islander_alone_civilian_noninstitutionalized_population",
      "some_other_race_alone_civilian_noninstitutionalized_population",
      "two_or_more_races_civilian_noninstitutionalized_population",
      "white_alone_civilian_noninstitutionalized_population",
      "white_alone_not_hispanic_or_latino_civilian_noninstitutionalized_population",
      "nonfamily_households_with_a_householder_who_is_asian_alone",
      "nonfamily_households_with_a_householder_who_is_black_or_african_american_alone",
      "nonfamily_households_with_a_householder_who_is_hispanic_or_latino",
      "nonfamily_households_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "nonfamily_households_with_a_householder_who_is_some_other_race_alone",
      "nonfamily_households_with_a_householder_who_is_two_or_more_races",
      "nonfamily_households_with_a_householder_who_is_white_alone",
      "nonfamily_households_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "nonfamily_households_with_householder_who_is_american_indian_and_alaska_native_alone",
      "people_who_are_black_or_african_american_alone",
      "people_who_are_hispanic_or_latino",
      "two_or_more_races_population",
      "households_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "households_with_a_householder_who_is_asian_alone",
      "households_with_a_householder_who_is_black_or_african_american_alone",
      "households_with_a_householder_who_is_hispanic_or_latino",
      "households_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "households_with_a_householder_who_is_some_other_race_alone",
      "households_with_a_householder_who_is_two_or_more_races",
      "households_with_a_householder_who_is_white_alone",
      "households_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "american_indian_and_alaska_native_alone_population",
      "asian_alone_population",
      "black_or_african_american_alone_population",
      "native_hawaiian_and_other_pacific_islander_alone_population",
      "some_other_race_alone_population",
      "white_alone_population",
      "american_indian_and_alaska_native_alone_household_population",
      "asian_alone_household_population",
      "black_or_african_american_alone_household_population",
      "hispanic_or_latino_household_population",
      "native_hawaiian_and_other_pacific_islander_alone_household_population",
      "some_other_race_alone_household_population",
      "two_or_more_races_household_population",
      "white_alone_household_population",
      "white_alone_not_hispanic_or_latino_household_population",
      "american_indian_and_alaska_native",
      "asian",
      "black_or_african_american",
      "hispanic_or_latino",
      "native_hawaiian_and_other_pacific_islander",
      "white",
      "population_of_one_race",
      "population_of_two_or_more_races",
      "population_of_two_races",
      "all_other_two_race_combinations",
      "asian_alone",
      "black_or_african_american_american_indian_and_alaska_native",
      "population_of_four_or_more_races",
      "population_of_three_races",
      "some_other_race",
      "two_races_excluding_some_other_race_and_three_or_more_races",
      "two_races_including_some_other_race",
      "white_american_indian_and_alaska_native",
      "white_asian",
      "white_black_or_african_american",
      "american_indian_and_alaska_native_alone_population_25_years_and_over",
      "american_indian_and_alaska_native_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "asian_alone_population_25_years_and_over",
      "asian_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "black_or_african_american_alone_population_25_years_and_over",
      "black_or_african_american_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "hispanic_or_latino_population_25_years_and_over",
      "hispanic_or_latino_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "native_hawaiian_and_other_pacific_islander_alone_population_25_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "some_other_race_alone_population_25_years_and_over",
      "some_other_race_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "two_or_more_races_population_25_years_and_over",
      "two_or_more_races_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "white_alone_population_25_years_and_over",
      "white_alone_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "white_alone_not_hispanic_or_latino_population_25_years_and_over",
      "white_alone_not_hispanic_or_latino_population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "american_indian_and_alaska_native_alone_civilian_population_18_years_and_over",
      "asian_alone_civilian_population_18_years_and_over",
      "black_or_african_american_alone_civilian_population_18_years_and_over",
      "civilian_population_18_years_and_over_for_whom_poverty_status_is_determined",
      "hispanic_or_latino_civilian_population_18_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_civilian_population_18_years_and_over",
      "some_other_race_alone_civilian_population_18_years_and_over",
      "two_or_more_races_civilian_population_18_years_and_over",
      "white_alone_civilian_population_18_years_and_over",
      "white_alone_not_hispanic_or_latino_civilian_population_18_years_and_over",
      "american_indian_and_alaska_native_alone_population_16_years_and_over",
      "asian_alone_population_16_years_and_over",
      "black_or_african_american_alone_population_16_years_and_over",
      "hispanic_or_latino_population_16_years_and_over",
      "native_hawaiian_and_other_pacific_islander_alone_population_16_years_and_over",
      "some_other_race_alone_population_16_years_and_over",
      "two_or_more_races_population_16_years_and_over",
      "white_alone_population_16_years_and_over",
      "white_alone_not_hispanic_or_latino_population_16_years_and_over",
      "civilian_employed_american_indian_and_alaska_native_alone_population_16_years_and_over",
      "civilian_employed_asian_alone_population_16_years_and_over",
      "civilian_employed_black_or_african_american_alone_population_16_years_and_over",
      "civilian_employed_hispanic_or_latino_population_16_years_and_over",
      "civilian_employed_native_hawaiian_and_other_pacific_islander_alone_population_16_years_and_over",
      "civilian_employed_some_other_race_alone_population_16_years_and_over",
      "civilian_employed_two_or_more_races_population_16_years_and_over",
      "civilian_employed_white_alone_population_16_years_and_over",
      "civilian_employed_white_alone_not_hispanic_or_latino_population_16_years_and_over",
      "alaska_native_tribes_specified",
      "american_indian_and_alaska_native_alone",
      "american_indian_tribes_specified",
      "asian_alone_or_in_combination_with_one_or_more_other_races",
      "asian_indian",
      "bangladeshi",
      "bhutanese",
      "black_or_african_american_alone_or_in_combination_with_one_or_more_other_races",
      "burmese",
      "cambodian",
      "chinese_except_taiwanese",
      "fijian",
      "filipino",
      "guamanian_or_chamorro",
      "indonesian",
      "laotian",
      "malaysian",
      "marshallese",
      "mongolian",
      "native_hawaiian",
      "native_hawaiian_and_other_pacific_islander_alone",
      "native_hawaiian_and_other_pacific_islander_alone_or_in_combination_with_one_or_more_other_races",
      "nepalese",
      "okinawan",
      "other_asian_not_specified",
      "other_asian_specified",
      "other_pacific_islander_not_specified_check_box_only",
      "other_polynesian",
      "pakistani",
      "people_who_are_american_indian_or_alaska_native_alone_or_in_combination_with_one_or_more_other_races",
      "samoan",
      "some_other_race_alone",
      "some_other_race_alone_or_in_combination_with_one_or_more_other_races",
      "sri_lankan",
      "taiwanese",
      "thai",
      "tongan",
      "two_or_more_asian",
      "two_or_more_nhpi",
      "two_or_more_races",
      "white_alone",
      "white_alone_or_in_combination_with_one_or_more_other_races",
      "alaska_native_tribes_not_specified",
      "alaskan_athabascan",
      "aleut",
      "all_other_american_indian_tribes_with_only_one_tribe_reported",
      "american_indian_or_alaska_native_tribes_not_specified",
      "american_indian_tribes_not_specified",
      "apache",
      "arapaho",
      "asian_indian",
      "bangladeshi",
      "bhutanese",
      "blackfeet",
      "burmese",
      "cambodian",
      "canadian_and_french_american_indian",
      "central_american_indian",
      "cherokee",
      "cheyenne",
      "chickasaw",
      "chinese_except_taiwanese",
      "chippewa",
      "choctaw",
      "colville",
      "comanche",
      "cree",
      "creek",
      "crow",
      "delaware",
      "fijian",
      "filipino",
      "guamanian_or_chamorro",
      "hopi",
      "houma",
      "indonesian",
      "inupiat",
      "iroquois",
      "kiowa",
      "laotian",
      "lumbee",
      "malaysian",
      "marshallese",
      "menominee",
      "mexican_american_indian",
      "mongolian",
      "native_hawaiian",
      "nepalese",
      "okinawan",
      "osage",
      "other_asian_not_specified",
      "other_asian_specified",
      "other_melanesian",
      "other_micronesian",
      "other_pacific_islander_not_specified",
      "other_polynesian",
      "ottawa",
      "paiute",
      "pakistani",
      "pima",
      "potawatomi",
      "pueblo",
      "puget_sound_salish",
      "samoan",
      "seminole",
      "shoshone",
      "sioux",
      "south_american_indian",
      "spanish_american_indian",
      "sri_lankan",
      "taiwanese",
      "thai",
      "tlingit_haida",
      "tohono_o_odham",
      "tongan",
      "tsimshian",
      "two_or_more_american_indian_or_alaska_native_tribes",
      "ute",
      "yakama",
      "yaqui",
      "yuman",
      "yup_ik",
      "people_who_are_american_indian_and_alaska_native_alone_and_people_with_no_tribe_reported",
      "central_american",
      "cuban",
      "dominican_dominican_republic",
      "mexican",
      "not_hispanic_or_latino",
      "other_hispanic_or_latino",
      "puerto_rican",
      "south_american",
      "all_other_hispanic_or_latino",
      "argentinean",
      "bolivian",
      "chilean",
      "colombian",
      "costa_rican",
      "ecuadorian",
      "guatemalan",
      "honduran",
      "nicaraguan",
      "other_central_american",
      "other_south_american",
      "panamanian",
      "paraguayan",
      "peruvian",
      "salvadoran",
      "spaniard",
      "spanish_american",
      "uruguayan",
      "venezuelan",
      "american_indian_and_alaska_native_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "asian_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "black_or_african_american_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "hispanic_or_latino_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "median_earnings_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "native_hawaiian_and_other_pacific_islander_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "other",
      "some_other_race_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "two_or_more_races_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "white_alone_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "white_alone_not_hispanic_or_latino_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "occupied_housing_units_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "occupied_housing_units_with_a_householder_who_is_asian_alone",
      "occupied_housing_units_with_a_householder_who_is_black_or_african_american_alone",
      "occupied_housing_units_with_a_householder_who_is_hispanic_or_latino",
      "occupied_housing_units_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "occupied_housing_units_with_a_householder_who_is_some_other_race_alone",
      "occupied_housing_units_with_a_householder_who_is_two_or_more_races",
      "occupied_housing_units_with_a_householder_who_is_white_alone",
      "occupied_housing_units_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "occupied_housing_units_with_a_householder_who_is_american_indian_and_alaska_native_alone",
      "occupied_housing_units_with_a_householder_who_is_asian_alone",
      "occupied_housing_units_with_a_householder_who_is_black_or_african_american_alone",
      "occupied_housing_units_with_a_householder_who_is_hispanic_or_latino",
      "occupied_housing_units_with_a_householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "occupied_housing_units_with_a_householder_who_is_some_other_race_alone",
      "occupied_housing_units_with_a_householder_who_is_two_or_more_races",
      "occupied_housing_units_with_a_householder_who_is_white_alone",
      "occupied_housing_units_with_a_householder_who_is_white_alone_not_hispanic_or_latino",
      "householder_who_is_american_indian_and_alaska_native_alone",
      "householder_who_is_asian_alone",
      "householder_who_is_black_or_african_american_alone",
      "householder_who_is_native_hawaiian_and_other_pacific_islander_alone",
      "householder_who_is_some_other_race_alone",
      "householder_who_is_two_or_more_races",
      "householder_who_is_white_alone" ,
      "householder_who_is_two_races_excluding_some_other_race_and_three_or_more_races",
      "householder_who_is_two_races_including_some_other_race",
      "hmong",
      "japanese",
      "korean",
      "navajo",
      "vietnamese",
      "american_indian_tribes_or_alaska_native_tribes_not_specified",
      "other_alaska_native_tribe",
      "other_american_indian_tribe",
      "other_pacific_islander"
    )
    interpret_as(mdr, field = "demographic_race", val, value, val_set, field_values)
  }

#' interpret_as_demographic_total_population
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_total_population <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
      "total",
      "housing_units",
      "total_population",
      "total_population_in_puerto_rico",
      "total_population_in_the_united_states",
      "population_in_puerto_rico_for_whom_poverty_status_is_determined",
      "population_in_the_united_states_for_whom_poverty_status_is_determined",
      "population_5_years_and_over_in_puerto_rico",
      "population_5_years_and_over_in_the_united_states",
      "population_15_years_and_over_in_puerto_rico",
      "population_15_years_and_over_in_the_united_states",
      "population_25_years_and_over_in_puerto_rico",
      "population_25_years_and_over_in_the_united_states",
      "population_15_years_and_over_in_puerto_rico_with_income",
      "population_15_years_and_over_in_the_united_states_with_income",
      "population_1_year_and_over_in_puerto_rico",
      "population_1_year_and_over_in_the_united_states",
      "population_1_year_and_over_in_puerto_rico_for_whom_poverty_status_is_determined",
      "population_1_year_and_over_in_the_united_states_for_whom_poverty_status_is_determined",
      "population_1_year_and_over_in_households_in_puerto_rico",
      "population_1_year_and_over_in_households_in_the_united_states",
      "population_1_year_and_over_living_in_a_metropolitan_statistical_area_in_puerto_rico",
      "population_1_year_and_over_living_in_a_metropolitan_statistical_area_in_the_united_states",
      "population_1_year_and_over_living_in_a_micropolitan_statistical_area_in_puerto_rico",
      "population_1_year_and_over_living_in_a_micropolitan_statistical_area_in_the_united_states",
      "population_1_year_and_over",
      "population_15_years_and_over",
      "population_25_years_and_over",
      "population_15_years_and_over_with_income_in_the_past_12_months",
      "population_1_year_and_over_for_whom_poverty_status_is_determined",
      "population_1_year_and_over_in_households",
      "population_1_year_and_over_not_living_in_a_metropolitan_or_micropolitan_statistical_area_in_puerto_rico",
      "population_1_year_and_over_not_living_in_a_metropolitan_or_micropolitan_statistical_area_in_the_united_states",
      "workers_16_years_and_over",
      "workers_16_years_and_over_who_did_not_work_at_home",
      "workers_16_years_and_over_in_households",
      "workers_whose_means_of_transportation_is_car_truck_or_van",
      "workers_16_years_and_over_living_in_a_metropolitan_statistical_area",
      "workers_16_years_and_over_living_in_a_micropolitan_statistical_area",
      "workers_16_years_and_over_not_living_in_a_metropolitan_or_micropolitan_statistical_area",
      "workers_16_years_and_over_with_earnings",
      "workers_16_years_and_over_for_whom_poverty_status_is_determined",
      "households",
      "population_under_18_years",
      "own_children_under_18_years",
      "population_under_18_years_in_households_excluding_householders_spouses_and_unmarried_partners",
      "population_under_18_years_in_households",
      "population_65_years_and_over",
      "population_18_years_and_over_in_households",
      "grandchildren_under_18_living_with_grandparent_householder",
      "families_with_grandparent_householders_and_or_spouses_living_with_grandchildren",
      "population_30_years_and_over",
      "grandparents_living_with_own_grandchildren_under_18_years",
      "civilian_grandparents_living_with_own_grandchildren_under_18_years",
      "grandparents_living_with_own_grandchildren_under_18_years_for_whom_poverty_status_is_determined",
      "population_in_households",
      "families",
      "subfamilies",
      "population_in_subfamilies",
      "total_households",
      "population_16_years_and_over",
      "population_15_years_and_over_who_are_now_married_or_separated",
      "population_15_to_54_years",
      "women_15_to_50_years",
      "women_15_to_50_years_in_households",
      "women_16_to_50_years",
      "women_15_to_50_years_for_whom_poverty_status_is_determined",
      "population_3_years_and_over",
      "population_16_to_19_years",
      "population_3_years_and_over_for_whom_poverty_status_is_determined",
      "population_18_years_and_over",
      "population_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "total_bachelor_s_degree_majors_tallied_for_people_25_years_and_over_with_a_bachelor_s_degree_or_higher_attainment",
      "population_5_years_and_over",
      "population_5_years_and_over_in_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well",
      "population_5_years_and_over_for_whom_poverty_status_is_determined",
      "population_for_whom_poverty_status_is_determined",
      "population_25_years_and_over_for_whom_poverty_status_is_determined",
      "population_16_years_and_over_for_whom_poverty_status_is_determined",
      "civilian_population_16_years_and_over_for_whom_poverty_status_is_determined",
      "related_children_under_18_years",
      "unrelated_individuals_15_years_and_over_for_whom_poverty_status_is_determined",
      "families_with_income_in_the_past_12_months_below_the_poverty_level",
      "civilian_noninstitutionalized_population",
      "civilian_noninstitutionalized_population_16_years_and_over_with_earnings_in_the_past_12_months",
      "civilian_noninstitutionalized_population_18_years_and_over",
      "civilian_noninstitutionalized_population_5_years_and_over",
      "median_household_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "aggregate_household_income_in_the_past_12_months_in_2018_inflation_adjusted_dollars",
      "total_dollars",
      "civilian_veterans_18_years_and_over",
      "civilian_population_18_to_64_years",
      "civilian_population_18_years_and_over",
      "civilian_population_25_years_and_over",
      "civilian_population_18_years_and_over_with_income_in_the_past_12_months",
      "civilian_population_65_years_and_over",
      "own_children_under_18_years_in_families_and_subfamilies",
      "population_16_to_64_years",
      "females_20_to_64_years_in_households",
      "opposite_sex_married_couple_families_and_families_maintained_by_women_and_men_with_no_spouse_present",
      "population_20_to_64_years_for_whom_poverty_status_is_determined",
      "population_25_to_64_years",
      "population_16_to_64_years_who_have_worked_in_the_past_12_months",
      "civilian_employed_population_16_years_and_over_with_earnings",
      "full_time_year_round_civilian_employed_population_16_years_and_over_with_earnings",
      "civilian_employed_female_population_16_years_and_over",
      "civilian_employed_male_population_16_years_and_over",
      "civilian_employed_population_16_years_and_over",
      "full_time_year_round_civilian_employed_female_population_16_years_and_over",
      "full_time_year_round_civilian_employed_female_population_16_years_and_over_with_earnings",
      "full_time_year_round_civilian_employed_male_population_16_years_and_over",
      "full_time_year_round_civilian_employed_male_population_16_years_and_over_with_earnings",
      "full_time_year_round_civilian_employed_population_16_years_and_over",
      "civilian_noninstitutionalized_employed_population_16_years_and_over",
      "population_16_years_and_over_with_earnings",
      "population_3_years_and_over_enrolled_in_school",
      "population_in_group_quarters",
      "population_18_to_64_years",
      "population_65_years",
      "foreign_born_population",
      "foreign_born_population_excluding_population_born_at_sea",
      "foreign_born_population_in_puerto_rico_excluding_population_born_at_sea",
      "household_population",
      "group_quarters_population",
      "institutionalized_group_quarters_population",
      "noninstitutionalized_group_quarters_population",
      "civilian_noninstitutionalized_population_19_to_25_years",
      "civilian_noninstitutionalized_population_19_to_64_years",
      "civilian_noninstitutionalized_population_26_years_and_over",
      "civilian_population_living_in_households",
      "civilian_household_population_16_years_and_over",
      "household_population_25_years_and_over",
      "citizens_18_years_and_over",
      "citizens_18_years_and_over_for_whom_poverty_status_is_determined",
      "households_with_a_citizen_voting_age_householder",
      "civilian_noninstitutionalized_population_15_years_and_over",
      "civilian_noninstitutionalized_population_18_to_64_years",
      "civilian_noninstitutionalized_population_for_whom_poverty_status_is_determined",
      "local_state_and_federal_government_workers",
      "people_reporting_multiple_ancestries",
      "people_reporting_single_ancestry",
      "total_aian_alone_or_in_any_combination_population_the_total_groups_tallied",
      "total_asian_alone_or_in_any_combination_population_the_total_groups_tallied",
      "total_nhpi_alone_or_in_any_combination_population_the_total_groups_tallied",
      "total_asian_alone_population",
      "total_native_hawaiian_and_other_pacific_islander_alone_population",
      "total_groups_tallied",
      "population_25_years_and_over_with_earnings",
      "population_16_years_and_over_who_worked_full_time_year_round_with_earnings",
      "total_population_in_occupied_housing_units",
      "aggregate_real_estate_taxes_paid_for_units_with_a_mortgage_dollars",
      "aggregate_real_estate_taxes_paid_for_units_without_a_mortgage_dollars",
      "lower_contract_rent_quartile",
      "lower_value_quartile_dollars",
      "median_contract_rent",
      "median_gross_rent",
      "median_gross_rent_as_a_percentage_of_household_income",
      "median_household_income_for_units_with_a_mortgage",
      "median_household_income_for_units_without_a_mortgage",
      "median_monthly_housing_costs",
      "median_number_of_rooms",
      "median_real_estate_taxes_paid_for_units_with_a_mortgage",
      "median_real_estate_taxes_paid_for_units_without_a_mortgage",
      "median_value_dollars",
      "median_value_for_units_with_a_mortgage",
      "median_value_for_units_without_a_mortgage",
      "median_year_structure_built",
      "aggregate_contract_rent",
      "aggregate_gross_rent",
      "aggregate_gross_rent_dollars",
      "aggregate_number_of_rooms",
      "aggregate_number_of_vehicles_available",
      "aggregate_price_asked_dollars",
      "aggregate_real_estate_taxes_paid_dollars",
      "aggregate_rent_asked",
      "aggregate_selected_monthly_owner_costs_dollars",
      "aggregate_value_dollars",
      "upper_contract_rent_quartile",
      "upper_value_quartile_dollars",
      "household_income_in_the_past_12_months_in_2017_inflation_adjusted_dollars",
      "household_income_the_past_12_months_in_2017_inflation_adjusted_dollars",
      "percent_of_earnings_allocated", "percent_of_family_income_allocated_for_families_with_income_in_the_past_12_months_at_or_above_poverty_level",
      "percent_of_family_income_allocated_for_families_with_income_in_the_past_12_months_below_poverty_level",
      "percent_of_income_allocated", "percent_of_income_allocated_for_individuals_with_income_in_the_past_12_months_at_or_above_poverty_level",
      "percent_of_income_allocated_for_individuals_with_income_in_the_past_12_months_below_poverty_level",
      "selected_monthly_owner_costs",
      "grandparents_living_with_own_grandchildren_under_18_years_in_households"
    )
    interpret_as(mdr, field = "demographic_total_population", val, value, val_set, field_values)
  }

#' interpret_as_demographic_sex
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_sex <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c("male",
                 "female",
                 "female_dollars",
                 "male_dollars")
    interpret_as(mdr, field = "demographic_sex", val, value, val_set, field_values)
  }

#' interpret_as_demographic_age
#'
#' Classifies the value in a field if it is one of the possible values
#' considered for that field.
#'
#' @param mdr A `tibble` row.
#' @param val A transformed value.
#' @param value A value.
#' @param field_values A data frame to store associations between fields and
#'   values.
#'
#' @return A result structure.
#'
#' @keywords internal
interpret_as_demographic_age <-
  function(mdr, val, value, field_values = NULL) {
    val_set <- c(
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
      "under_5_years",
      "6_to_17_years",
      "under_6_years",
      "15_to_19_years",
      "60_to_64_years",
      "65_to_69_years",
      "18_years_and_over",
      "under_18_years",
      "10_to_19_years",
      "20_to_29_years",
      "30_to_39_years",
      "40_to_49_years",
      "50_to_59_years",
      "60_to_69_years",
      "70_years_and_over",
      "18_to_24_years",
      "25_to_34_years",
      "5_to_17_years",
      "75_years_and_over",
      "under_10_years",
      "1_to_4_years",
      "16_to_19_years",
      "25_to_44_years",
      "65_years_and_over",
      "12_to_14_years",
      "3_and_4_years",
      "5_years",
      "6_to_8_years",
      "9_to_11_years",
      "under_3_years",
      "12_to_17_years",
      "6_to_11_years",
      "18_to_34_years",
      "35_to_64_years",
      "grandchildren_12_to_17_years",
      "grandchildren_6_to_11_years",
      "grandchildren_under_6_years",
      "30_to_59_years",
      "60_years_and_over",
      "15_to_19_years_old",
      "20_to_24_years_old",
      "25_to_29_years_old",
      "30_to_34_years_old",
      "35_to_39_years_old",
      "40_to_44_years_old",
      "45_to_50_years_old",
      "20_to_34_years_old",
      "35_to_50_years_old",
      "35_years_and_over",
      "45_to_64_years",
      "25_to_39_years",
      "40_to_64_years",
      "18_to_64_years",
      "15_years",
      "16_and_17_years",
      "18_to_59_years",
      "60_to_74_years",
      "19_to_64_years",
      "under_19_years",
      "35_to_54_years",
      "20_and_21_years",
      "6_to_17_years_only",
      "under_6_years_and_6_to_17_years",
      "under_6_years_only",
      "under_15_years",
      "19_to_25_years",
      "19_to_34_years",
      "26_to_34_years",
      "26_to_64_years",
      "6_to_18_years",
      "18_to_29_years",
      "30_to_44_years",
      "16_to_64_years",
      "householder_25_to_44_years",
      "householder_45_to_64_years",
      "householder_under_25_years",
      "householder_15_to_64_years",
      "householder_65_years_and_over",
      "householder_15_to_34_years",
      "householder_35_to_64_years",
      "householder_35_to_64_years_dollars",
      "householder_15_to_24_years_dollars",
      "householder_15_to_64_years_dollars",
      "householder_25_to_34_years_dollars",
      "householder_35_to_44_years_dollars",
      "householder_45_to_54_years_dollars",
      "householder_55_to_59_years_dollars",
      "householder_60_to_64_years_dollars",
      "householder_65_to_74_years_dollars",
      "householder_65_years_and_over_dollars",
      "householder_75_years_and_over_dollars",
      "householder_15_to_24_years",
      "householder_15_to_54_years",
      "householder_25_to_34_years",
      "householder_35_to_44_years",
      "householder_45_to_54_years",
      "householder_55_to_59_years",
      "householder_55_to_64_years",
      "householder_60_to_64_years",
      "householder_65_to_74_years",
      "householder_75_to_84_years",
      "householder_75_years_and_over",
      "householder_85_years_and_over",
      "2_to_4",
      "20_to_49",
      "5_to_19",
      "50_or_more"
    )
    interpret_as(mdr, field = "demographic_age", val, value, val_set, field_values)
  }

