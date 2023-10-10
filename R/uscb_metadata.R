#' `uscb_metadata` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param filepath A string, path to gbd file.
#' @param code A vector of strings.
#' @param group_code A vector of strings.
#' @param short_name A vector of strings.
#' @param uscb_acs_metadata A metadata object.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
new_uscb_metadata <- function(filepath = NULL, code = NULL, group_code = NULL, short_name = NULL, uscb_acs_metadata = NULL) {
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read' to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)
  layer_cod_name <-
    data.frame(cod = substr(layer_names, 2, 3),
               name = stringr::str_replace_all(substr(layer_names, 5, length(layer_names)), "_", " "))

  metadata <-
    sf::st_read(dsn = filepath,
                layer = layer_names[2],
                quiet = TRUE) |>
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
    "housing_units",
    "survey",
    "rest"
  )

  interpret <- c(
    interpret_as_demographic_total_population,
    interpret_as_demographic_sex,
    interpret_as_demographic_age,
    interpret_as_economic_food_stamps_snap,
    interpret_as_social_veteran_status_military_service,
    interpret_as_social_school_enrollment,
    interpret_as_social_fertility,
    interpret_social_ancestry,
    interpret_as_economic_health_insurance_coverage,
    interpret_as_economic_income_and_earnings,
    interpret_as_housing_units,
    interpret_as_housing_plumbing_facilities,
    interpret_as_housing_rent,
    interpret_as_housing_vehicles_available,
    interpret_as_housing_rooms,
    interpret_as_housing_occupancy_vacancy_status,
    interpret_as_housing_occupants_per_room,
    interpret_as_housing_value_of_home,
    interpret_as_housing_year_structure_built,
    interpret_as_housing_year_householder_moved_into_unit,
    interpret_as_economic_work_status_last_year,
    interpret_as_social_disability_status,
    interpret_as_social_grandparents_as_caregivers,
    interpret_as_demographic_household,
    interpret_as_demographic_group_quarters_population,
    interpret_as_economic_journey_and_place_of_work,
    interpret_as_housing_tenure_owner_renter,
    interpret_as_housing_computer_and_internet_use,
    interpret_as_social_migration_residence_1_year_ago,
    interpret_as_social_educational_attainment,
    interpret_as_social_marital_status,
    interpret_as_social_language_spoken_at_home,
    interpret_as_economic_poverty_status,
    interpret_as_social_year_of_entry,
    interpret_as_social_citizenship_status,
    interpret_as_demographic_race,
    interpret_as_social_place_of_birth,
    interpret_as_economic_industry_and_occupation,
    interpret_as_survey
  )

  metadata$inf_code <- ""
  metadata$group_code <- ""
  metadata$subgroup_code <- ""
  metadata$type_code <- ""
  metadata$spec_code <- ""

  metadata$inf <- ""
  metadata$group <- ""
  metadata$subgroup <- ""
  metadata$type <- ""

  for (var in var_name) {
    metadata[var] <- ""
    metadata[sprintf("%s_spec", var)] <- ""
    metadata[sprintf("%s_spec_2", var)] <- ""
    metadata[sprintf("%s_spec_3", var)] <- ""
    metadata[sprintf("%s_spec_4", var)] <- ""
  }

  for (i in seq_along(metadata[[1]])) {
    short <- strsplit(metadata$Short_Name[i], "")[[1]]
    metadata$inf_code[i] <- paste(short[1:3], collapse = "")
    metadata$group_code[i] <- paste(short[4:6], collapse = "")
    metadata$inf[i] <- layer_cod_name[layer_cod_name$cod == paste(short[2:3], collapse = ""), "name"]
  }
  # select only a code
  if (!is.null(code)) {
    metadata <- metadata[metadata$inf_code %in% code, ]
    if (!is.null(group_code)) {
      metadata <- metadata[metadata$group_code %in% group_code, ]
    }
  }
  if (!is.null(short_name)) {
    metadata <- metadata[metadata$Short_Name %in% short_name, ]
  }

  acs <-
    list(
      uscb_acs_metadata = uscb_acs_metadata,
      layers = layer_names,
      variables = var_name,
      interpret = interpret,
      field_values = NULL,
      metadata = metadata
    )

  structure(acs,
            class = "uscb_metadata")
}

#' `uscb_metadata` S3 class
#'
#' A `uscb_metadata` object is created from a given geodatabase, code, group
#' code, short name and existing metadata.
#'
#' @param filepath A string, path to gbd file.
#' @param code A vector of strings.
#' @param group_code A vector of strings.
#' @param short_name A vector of strings.
#' @param uscb_acs_metadata A metadata object.
#'
#' @return A `uscb_metadata` object.
#'
#' @export
#' @keywords internal
uscb_metadata <-
  function(filepath = NULL, code = NULL, group_code = NULL, short_name = NULL, uscb_acs_metadata = NULL) {
    new_uscb_metadata(filepath, code, group_code, short_name, uscb_acs_metadata)
  }


