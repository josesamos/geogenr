#' `uscb_metadata` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#'
#' @param filepath A string, path to gbd file.
#' @param code A string.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
new_uscb_metadata <- function(filepath = NULL, code = NULL, group_code = NULL) {
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read' to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  metadata <-
    sf::st_read(dsn = filepath,
                layer = layer_names[2],
                quiet = TRUE) %>%
    tibble::as_tibble()

  var_name <- c(
    "social_ancestry",
    "social_citizen_voting_age_population",
    "social_citizenship_status",
    "social_disability_status",
    "social_educational_attainment",
    "social_fertility",
    "social_grandparents_as_caregivers",
    "social_language_spoken_at_home",
    "social_marital_history",
    "social_marital_status",
    "social_migration_residence_1_year_ago",
    "social_place_of_birth",
    "social_school_enrollment",
    "social_undergraduate_field_of_degree",
    "social_veteran_status_military_service",
    "social_year_of_entry",
    "economic_class_of_worker",
    "economic_journey_and_place_of_work",
    "economic_employment_status",
    "economic_food_stamps_snap",
    "economic_health_insurance_coverage",
    "economic_income_and_earnings",
    "economic_industry_and_occupation",
    "economic_poverty_status",
    "economic_work_status_last_year",
    "housing_bedrooms",
    "housing_computer_and_internet_use",
    "housing_house_heating_fuel",
    "housing_kitchen_facilities",
    "housing_occupancy_vacancy_status",
    "housing_occupants_per_room",
    "housing_plumbing_facilities",
    "housing_rent",
    "housing_rooms",
    "housing_selected_monthly_owner_costs",
    "housing_telephone_service_available",
    "housing_tenure_owner_renter",
    "housing_units_in_structure",
    "housing_value_of_home",
    "housing_vehicles_available",
    "housing_year_householder_moved_into_unit",
    "housing_year_structure_built",
    "demographic_age",
    "demographic_sex",
    "demographic_group_quarters_population",
    "demographic_hispanic_or_latino_origin",
    "demographic_race",
    "demographic_relationship_to_householder",
    "demographic_total_population",
    "demographic_family",
    "demographic_household",
    "rest"
  )

  metadata$inf_code <- ""
  metadata$type_code <- ""
  metadata$spec_code <- ""

  metadata$type <- ""

  metadata$group <- ""
  metadata$subgroup <- ""
  metadata$group_code <- ""
  metadata$subgroup_code <- ""

  for (var in var_name) {
    metadata[var] <- ""
    metadata[sprintf("%s_spec", var)] <- ""
    metadata[sprintf("%s_spec_2", var)] <- ""
    metadata[sprintf("%s_spec_3", var)] <- ""
    metadata[sprintf("%s_spec_4", var)] <- ""
  }

  # select only a code
  if (!is.null(code)) {
    for (i in seq_along(metadata[[1]])) {
      short <- strsplit(metadata$Short_Name[i], "")[[1]]
      metadata$inf_code[i] <- paste(short[1:3], collapse = "")
      metadata$group_code[i] <- paste(short[4:6], collapse = "")
    }
    print(sort(unique(metadata$inf_code)))
    metadata <- metadata[metadata$inf_code == code, ]
    if (!is.null(group_code)) {
      print(sort(unique(metadata$group_code)))
      metadata <- metadata[metadata$group_code == group_code, ]
    }
  }

  acs <-
    list(
      layers = layer_names,
      metadata = metadata
    )

  structure(acs,
            class = "uscb_metadata")
}

#' `uscb_metadata` S3 class
#'
#' A `uscb_metadata` object is created from a given
#'
#' @param filepath A string, path to gbd file.
#' @param code A string.
#'
#' @return A `uscb_metadata` object.
#'
#'
#' @export
uscb_metadata <-
  function(filepath = NULL, code = NULL, group_code = NULL) {
    new_uscb_metadata(filepath, code, group_code)
  }


