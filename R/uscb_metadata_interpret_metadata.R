
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
    um$metadata[i, ] <- interpret_code(um$metadata[i, ])

    values <- strsplit(um$metadata$Full_Name[i], ": ")[[1]]
    values <- stringr::str_trim(values, side = "both")
    um$metadata[i, ] <- interpret_values(um$metadata[i, ], values)
  }
  for (v in um$variables) {
    um$metadata <- assign_level(um$metadata, field = v)
  }
  # delete empty columns
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
            interpret_as(mdr, field = "demographic_total_population", vals[j], values[j])
        },
        "B01" = {
          res <- interpret_as_demographic_race(mdr, vals[j], values[j])
        },
        "B02" = {
          res <-
            interpret_as(mdr, field = "demographic_race", vals[j], values[j])
        },
        "B03" = {
          res <-
            interpret_as(mdr, field = "demographic_hispanic_or_latino_origin", vals[j], values[j])
        },
        "B04" = {
          res <-
            interpret_as(mdr, field = "social_ancestry", vals[j], values[j])
        },
        "B05" = {
          res <- interpret_as_demographic_race(mdr, vals[j], values[j])
          if (!res$result) {
            res <-
              interpret_b05(mdr, vals[j], values[j])
          }
        },
        "B06" = {
          res <- interpret_b06(mdr, vals[j], values[j])
        },
        "B07" = {
          res <- interpret_b07(mdr, vals[j], values[j])
        },
        "B08" = {
          res <- interpret_b08(mdr, vals[j], values[j])
        },
        "B09" = {
          res <- interpret_as_demographic_household(mdr, vals[j], values[j])
          if (!res$result) {
            res <-
              interpret_as_social_grandparents_as_caregivers(mdr, vals[j], values[j])
          }
        },
        "B10" = {
          res <- interpret_b10(mdr, vals[j], values[j])
        },
        "B11" = {
          res <- interpret_b11(mdr, vals[j], values[j])
        },
        "B12" = {
          res <- interpret_b12(mdr, vals[j], values[j])
        },
        "B13" = {
          res <- interpret_b13(mdr, vals[j], values[j])
        },
        "B14" = {
          res <- interpret_b14(mdr, vals[j], values[j])
        },
        "B15" = {
          res <-
            interpret_as_social_educational_attainment(mdr, vals[j], values[j])
        },
        "B16" = {
          res <- interpret_b16(mdr, vals[j], values[j])
        }
      )
    }
    if (!res$result) {
      res <- interpret_as(mdr, field = "rest", vals[j], values[j])
    }
    mdr <- res$mdr
  }
  mdr
}



#' interpret_b05
#'
interpret_b05 <-
  function(mdr, val, value) {
    if (substr(val, 1, 6) == "total_") {
      res <- interpret_as(mdr, field = "demographic_total_population", val, value)
    } else {
      res <- interpret_as_social_citizenship_status(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_social_place_of_birth(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_demographic_family(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_economic_poverty_status(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_social_year_of_entry(mdr, val, value)
              if (!res$result) {
                if (mdr$group_code %in% c("002", "006", "007", "008")) {
                  res <- interpret_as(mdr, field = "social_place_of_birth", val, value)
                }
              }
            }
          }
        }
      }
    }
    res
  }



#' interpret_b06
#'
interpret_b06 <- function(mdr, val, value) {
  res <- interpret_as_social_place_of_birth(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_demographic_race(mdr, val, value)
    if (!res$result) {
      res <-
        interpret_as_social_language_spoken_at_home(mdr, val, value)
      if (!res$result) {
        res <-
          interpret_as_social_marital_status(mdr, val, value)
        if (!res$result) {
          res <-
            interpret_as_social_educational_attainment(mdr, val, value)
          if (!res$result) {
            res <-
              interpret_as_economic_income_and_earnings(mdr, val, value)
            if (!res$result) {
              res <-
                interpret_as_economic_poverty_status(mdr, val, value)
            }
          }
        }
      }
    }
  }
  res
}

#' interpret_b07
#'
interpret_b07 <- function(mdr, val, value) {
  res <- interpret_as_social_migration_residence_1_year_ago(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_demographic_race(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_social_citizenship_status(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_social_place_of_birth(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_social_marital_status(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_social_educational_attainment(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_economic_income_and_earnings(mdr, val, value)
              if (!res$result) {
                res <- interpret_as_economic_poverty_status(mdr, val, value)
                if (!res$result) {
                  res <- interpret_as_housing_tenure_owner_renter(mdr, val, value)
                }
              }
            }
          }
        }
      }
    }
  }
  res
}


#' interpret_b08
#'
interpret_b08 <- function(mdr, val, value) {
  res <- interpret_as_economic_journey_and_place_of_work(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_demographic_race(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_social_citizenship_status(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_social_place_of_birth(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_social_language_spoken_at_home(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_economic_income_and_earnings(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_economic_poverty_status(mdr, val, value)
              if (!res$result) {
                res <- interpret_as_economic_industry_and_occupation(mdr, val, value)
                if (!res$result) {
                  res <- interpret_as_housing_tenure_owner_renter(mdr, val, value)
                  if (!res$result) {
                    res <- interpret_as_demographic_household(mdr, val, value)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  res
}


#' interpret_b10
#'
interpret_b10 <- function(mdr, val, value) {
  res <- interpret_as_demographic_household(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_social_grandparents_as_caregivers(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_demographic_race(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_social_disability_status(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_social_place_of_birth(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_social_language_spoken_at_home(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_social_marital_status(mdr, val, value)
              if (!res$result) {
                res <- interpret_as_economic_work_status_last_year(mdr, val, value)
                if (!res$result) {
                  res <-
                    interpret_as_economic_poverty_status(mdr, val, value)
                }
              }
            }
          }
        }
      }
    }
  }
  res
}


#' interpret_b11
#'
interpret_b11 <- function(mdr, val, value) {
  if (mdr$group_code %in% c("014")) {
    res <- interpret_as_housing_units_in_structure(mdr, val, value)
  } else {
    res <- interpret_as_demographic_household(mdr, val, value)
  }
  if (!res$result) {
    res <- interpret_as_demographic_race(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_demographic_family(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_housing_units_in_structure(mdr, val, value)
      }
    }
  }
  res
}


#' interpret_b12
#'
interpret_b12 <- function(mdr, val, value) {
  res <- interpret_as_social_marital_status(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_demographic_race(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_economic_work_status_last_year(mdr, val, value)
    }
  }
  res
}

#' interpret_b13
#'
interpret_b13 <- function(mdr, val, value) {
  res <- interpret_as_social_marital_status(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_economic_work_status_last_year(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_demographic_race(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_social_educational_attainment(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_economic_poverty_status(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_social_place_of_birth(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_social_fertility(mdr, val, value)
              if (!res$result) {
                res <- interpret_as_demographic_household(mdr, val, value)
                if (!res$result) {
                  res <- interpret_as_economic_poverty_status(mdr, val, value)
                }
              }
            }
          }
        }
      }
    }
  }
  res
}


#' interpret_b14
#'
interpret_b14 <- function(mdr, val, value) {
  res <- interpret_as_social_school_enrollment(mdr, val, value)
  if (!res$result) {
    res <- interpret_as_economic_work_status_last_year(mdr, val, value)
    if (!res$result) {
      res <- interpret_as_demographic_race(mdr, val, value)
      if (!res$result) {
        res <- interpret_as_economic_poverty_status(mdr, val, value)
        if (!res$result) {
          res <- interpret_as_social_educational_attainment(mdr, val, value)
          if (!res$result) {
            res <- interpret_as_economic_poverty_status(mdr, val, value)
            if (!res$result) {
              res <- interpret_as_demographic_race(mdr, val, value)
            }
          }
        }
      }
    }
  }
  res
}


#' interpret_b16
#'
interpret_b16 <- function(mdr, val, value) {
  res <- interpret_as_social_language_spoken_at_home(mdr, val, value)
  if (!res$result) {
    if (mdr$group_code %in% c("001")) {
      res <- interpret_as(mdr, field = "social_language_spoken_at_home", val, value)
    }
  }
  res
}


#' interpret_as_social_school_enrollment
#'
interpret_as_social_school_enrollment <-
  function(mdr, val, value) {
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
    interpret_as(mdr, field = "social_school_enrollment", val_set, val, value)
  }


#' interpret_as_social_fertility
#'
interpret_as_social_fertility <-
  function(mdr, val, value) {
    val_set <- c(
      "women_who_did_not_have_a_birth_in_the_past_12_months",
      "women_who_had_a_birth_in_the_past_12_months"
    )
    interpret_as(mdr, field = "social_fertility", val_set, val, value)
  }


#' interpret_as_housing_units_in_structure
#'
interpret_as_housing_units_in_structure <-
  function(mdr, val, value) {
    val_set <- c(
      "1_unit_structures",
      "2_or_more_unit_structures",
      "mobile_homes_and_all_other_types_of_units"
    )
    interpret_as(mdr, field = "housing_units_in_structure", val_set, val, value)
  }



#' interpret_as_economic_work_status_last_year
#'
interpret_as_economic_work_status_last_year <-
  function(mdr, val, value) {
    val_set <- c(
      "in_labor_force",
      "not_in_labor_force",
      "employed_or_in_armed_forces",
      "unemployed",
      "employed"
    )
    interpret_as(mdr, field = "economic_work_status_last_year", val_set, val, value)
  }


#' interpret_as_social_disability_status
#'
interpret_as_social_disability_status <-
  function(mdr, val, value) {
    val_set <- c(
      "no_disability",
      "with_any_disability"
    )
    interpret_as(mdr, field = "social_disability_status", val_set, val, value)
  }


#' interpret_as_social_grandparents_as_caregivers
#'
interpret_as_social_grandparents_as_caregivers <-
  function(mdr, val, value) {
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
    interpret_as(mdr, field = "social_grandparents_as_caregivers", val_set, val, value)
  }


#' interpret_as_demographic_household
#'
interpret_as_demographic_household <-
  function(mdr, val, value) {
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
      "householder_15_to_64_years",
      "householder_65_years_and_over",
      "households_with_no_nonrelatives",
      "households_with_one_or_more_nonrelatives",
      "4_person_household",
      "5_person_household",
      "6_person_household",
      "7_or_more_person_household",
      "multigenerational_households",
      "not_an_unmarried_partner",
      "partner_in_an_unmarried_partner_household"
    )
    interpret_as(mdr, field = "demographic_household", val_set, val, value)
  }

#' interpret_as_economic_industry_and_occupation
#'
interpret_as_economic_industry_and_occupation <-
  function(mdr, val, value) {
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
      "unpaid_family_workers"
    )
    interpret_as(mdr, field = "economic_industry_and_occupation", val_set, val, value)
  }

#' interpret_as_economic_income_and_earnings
#'
interpret_as_economic_income_and_earnings <-
  function(mdr, val, value) {
    val_set <- c(
      "1_to_9_999_or_loss",
      "10_000_to_14_999",
      "15_000_to_24_999",
      "25_000_to_34_999",
      "35_000_to_49_999",
      "50_000_to_64_999",
      "65_000_to_74_999",
      "75_000_or_more"
    )
    interpret_as(mdr, field = "economic_income_and_earnings", val_set, val, value)
  }


#' interpret_as_economic_journey_and_place_of_work
#'
interpret_as_economic_journey_and_place_of_work <-
  function(mdr, val, value) {
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
      "1_vehicle_available",
      "2_vehicles_available",
      "3_vehicles_available",
      "4_vehicles_available",
      "5_or_more_vehicles_available",
      "3_or_more_vehicles_available",
      "4_or_more_vehicles_available",
      "no_vehicle_available",
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
      "less_than_10_minutes"
    )
    interpret_as(mdr, field = "economic_journey_and_place_of_work", val_set, val, value)
  }


#' interpret_as_housing_tenure_owner_renter
#'
interpret_as_housing_tenure_owner_renter <-
  function(mdr, val, value) {
    val_set <- c(
      "householder_lived_in_owner_occupied_housing_units",
      "householder_lived_in_renter_occupied_housing_units"
    )
    interpret_as(mdr, field = "housing_tenure_owner_renter", val_set, val, value)
  }



#' interpret_as_social_migration_residence_1_year_ago
#'
interpret_as_social_migration_residence_1_year_ago <-
  function(mdr, val, value) {
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
      "not_in_a_metropolitan_or_micropolitan_statistical_area_1_year_ago"
    )
    interpret_as(mdr, field = "social_migration_residence_1_year_ago", val_set, val, value)
  }


#' interpret_as_economic_income_and_earnings
#'
interpret_as_economic_income_and_earnings <-
  function(mdr, val, value) {
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
      "75_000_or_more"
    )
    interpret_as(mdr, field = "economic_income_and_earnings", val_set, val, value)
  }


#' interpret_as_social_educational_attainment
#'
interpret_as_social_educational_attainment <-
  function(mdr, val, value) {
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
      "visual_and_performing_arts"
    )
    interpret_as(mdr, field = "social_educational_attainment", val_set, val, value)
  }


#' interpret_as_social_marital_status
#'
interpret_as_social_marital_status <- function(mdr, val, value) {
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
    "two_times"
  )
  interpret_as(mdr, field = "social_marital_status", val_set, val, value)
}


#' interpret_as_social_language_spoken_at_home
#'
interpret_as_social_language_spoken_at_home <-
  function(mdr, val, value) {
    val_set <- c(
      "speak_only_english",
      "speak_other_languages",
      "speak_spanish",
      "speak_english_very_well",
      "speak_english_less_than_very_well",
      "speak_other_language"
    )
    interpret_as(mdr, field = "social_language_spoken_at_home", val_set, val, value)
  }



#' interpret_as_economic_poverty_status
#'
interpret_as_economic_poverty_status <- function(mdr, val, value) {
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
    "income_in_the_past_12_months_below_the_poverty_level"
  )
  interpret_as(mdr, field = "economic_poverty_status", val_set, val, value)
}


#' interpret_as_demographic_family
#'
interpret_as_demographic_family <- function(mdr, val, value) {
  val_set <- c(
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
    "child", # also in household
    "parent" # also in household
  )
  interpret_as(mdr, field = "demographic_family", val_set, val, value)
}


#' interpret_as_social_place_of_birth
#'
interpret_as_social_place_of_birth <- function(mdr, val, value) {
  val_set <- c(
    "born_in_other_state_in_the_united_states",
    "born_in_other_state_of_the_united_states",
    "born_outside_the_united_states",
    "foreign_born",
    "native",
    "foreign_born_population",
    "foreign_born_population_excluding_population_born_at_sea",
    "foreign_born_population_in_puerto_rico_excluding_population_born_at_sea",
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
    "native_parent"
  )
  interpret_as(mdr, field = "social_place_of_birth", val_set, val, value)
}


#' interpret_as_social_year_of_entry
#'
interpret_as_social_year_of_entry <- function(mdr, val, value) {
  val_set <- c(
    "entered_1990_to_1999",
    "entered_2000_to_2009",
    "entered_2010_or_later",
    "entered_before_1990",
    "entered_before_2000"
  )
  interpret_as(mdr, field = "social_year_of_entry", val_set, val, value)
}


#' interpret_as_social_citizenship_status
#'
interpret_as_social_citizenship_status <- function(mdr, val, value) {
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
    "naturalized_before_1990"
  )
  interpret_as(mdr, field = "social_citizenship_status", val_set, val, value)
}


#' interpret_as_demographic_race
#'
interpret_as_demographic_race <- function(mdr, val, value) {
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
    "white_alone_not_hispanic_or_latino_population_3_years_and_over"
  )
  interpret_as(mdr, field = "demographic_race", val_set, val, value)
}


#' interpret_as_demographic
#'
interpret_as_demographic <- function(mdr, val, value) {
  val_set <- c(
    "total",
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
    "population_5_years_and_over"
  )
  res <-
    interpret_as(mdr, field = "demographic_total_population", val_set, val, value)
  if (!res$result) {
    val_set <- c("male",
                 "female")
    res <-
      interpret_as(mdr, field = "demographic_sex", val_set, val, value)
    if (!res$result) {
      if (!res$result) {
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
          "40_to_64_years"
        )
        res <-
          interpret_as(mdr, field = "demographic_age", val_set, val, value)
      }
    }
  }
  res
}

#' add_value
#'
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


#' assign_level
#'
assign_level <- function(mdr, field) {
  f <- c(
    field,
    sprintf("%s_spec", field),
    sprintf("%s_spec_2", field),
    sprintf("%s_spec_3", field),
    sprintf("%s_spec_4", field)
  )
  scroll <- TRUE
  while (scroll) {
    scroll <- FALSE
    for (i in length(f):2) {
      values <- unique(mdr[, f[i]][[1]])
      for (v in values) {
        if (v != "") {
          for (j in (i - 1):1) {
            x <- as.vector(mdr[, f[j]] == v)
            if (sum(x) > 0) {
              if (!all(mdr[x, f[i]] == "")) {
                scroll <- TRUE
                mdr <-
                  scroll_level(
                    mdr,
                    fields = f,
                    field_index = i,
                    values_indices = x
                  )
              }
              mdr[x, f[i]] <- v
              mdr[x, f[j]] <- ""
            }
          }
        }
      }
    }
  }
  mdr
}


#' scroll_level
#'
scroll_level <- function(mdr, fields, field_index, values_indices) {
  if (all(mdr[values_indices, fields[field_index + 1]] == "")) {
    mdr[values_indices, fields[field_index + 1]] <-
      mdr[values_indices, fields[field_index]]
    mdr[values_indices, fields[field_index]] <- ""
  } else {
    mdr <- scroll_level(mdr, fields, field_index + 1, values_indices)
  }
  mdr
}


#' interpret_as
#'
interpret_as <- function(mdr, field, val_set = NULL, val, value) {
  result <- TRUE
  if (is.null(val_set)) {
    mdr <- add_value(mdr, field, value)
  } else if (val %in% val_set) {
    mdr <- add_value(mdr, field, value)
  } else {
    result <- FALSE
  }

  list(mdr = mdr,
       result = result)
}
