#' `tiger_acs` S3 class
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
#' @return A `tiger_acs` object.
#'
#' @keywords internal
new_tiger_acs <- function(filepath = NULL, code = NULL) {
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read' to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  metadata <-
    sf::st_read(dsn = filepath,
                layer = layer_names[2],
                quiet = TRUE) %>%
    tibble::as_tibble()

  metadata$inf_code <- ""
  metadata$group_code <- ""
  metadata$subgroup_code <- ""
  metadata$type_code <- ""
  metadata$spec_code <- ""

  metadata$type <- ""
  metadata$group <- ""
  metadata$subgroup <- ""

  # total
  metadata$total <- ""
  metadata$total_spec <- ""

  #who
  metadata$sex <- ""
  metadata$age <- ""
  metadata$marital_status <- ""
  metadata$marital_status_spec <- ""
  metadata$human_group <- ""
  metadata$human_group_spec <- ""
  metadata$human_group_spec2 <- ""
  metadata$ancestry <- ""
  metadata$ancestry_spec <- ""
  metadata$ancestry_spec2 <- ""
  metadata$u_s_citizen <- ""
  metadata$u_s_citizen_spec <- ""
  metadata$residence <- ""
  metadata$residence_spec <- ""
  metadata$birth <- ""
  metadata$birth_spec <- ""
  metadata$place_of_birth <- ""
  metadata$place_of_birth_spec <- ""
  metadata$languages <- ""
  metadata$languages_spec <- ""
  metadata$english <- ""
  metadata$studies <- ""
  metadata$studies_spec <- ""
  metadata$disability <- ""

  # how
  metadata$activity <- ""
  metadata$activity_spec <- ""
  metadata$activity_spec2 <- ""
  metadata$household <- ""
  metadata$household_spec <- ""
  metadata$household_spec2 <- ""
  metadata$children <- ""
  metadata$children_spec <- ""
  metadata$children_spec2 <- ""
  metadata$condition <- ""
  metadata$condition_spec <- ""
  metadata$condition_spec2 <- ""
  metadata$income <- ""
  metadata$income_spec <- ""
  metadata$money <- ""
  metadata$money_spec <- ""
  metadata$vehicles <- ""
  metadata$vehicles_spec <- ""
  metadata$insurance_coverage <- ""
  metadata$insurance_coverage_spec <- ""
  metadata$transportation_to_work <- ""
  metadata$transportation_to_work_spec <- ""
  metadata$transportation_to_work_spec2 <- ""
  metadata$housing <- ""
  metadata$housing_spec <- ""
  metadata$housing_units <- ""
  metadata$housing_units_spec <- ""
  metadata$computers <- ""
  metadata$computers_spec <- ""

  metadata$rest <- ""

  # select only a code
  if (!is.null(code)) {
    for (i in seq_along(metadata[[1]])) {
      short <- strsplit(metadata$Short_Name[i], "")[[1]]
      metadata$inf_code[i] <- paste(short[1:3], collapse = "")
    }
    print(sort(unique(metadata$inf_code)))
    metadata <- metadata[metadata$inf_code == code, ]
  }

  acs <-
    list(
      layers = layer_names,
      metadata = metadata
    )

  structure(acs,
            class = "tiger_acs")
}

#' `tiger_acs` S3 class
#'
#' A `tiger_acs` object is created from a given
#'
#' @param filepath A string, path to gbd file.
#' @param code A string.
#'
#' @return A `tiger_acs` object.
#'
#'
#' @export
tiger_acs <-
  function(filepath = NULL, code = NULL) {
    new_tiger_acs(filepath, code)
  }


