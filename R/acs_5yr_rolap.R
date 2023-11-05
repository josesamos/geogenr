

#' As `rolap::flat_table` object
#'
#' Obtain an `rolap::flat_table` object to be able to modify the data or integrate
#' it with other data.
#'
#' We can indicate the attributes of the geographic layer to include in the export.
#' Otherwise, the default attributes are included (not area, perimeter or location
#' attributes).
#'
#' @param act An `acs_5yr_topic` object.
#' @param attributes A string vector.
#'
#' @return A `flat_table` object.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' ft <- anrc_2021_x01 |>
#'   as_flat_table()
#'
#' @export
as_flat_table <- function(act, attributes)
  UseMethod("as_flat_table")

#' @rdname as_flat_table
#' @export
as_flat_table.acs_5yr_topic <- function(act, attributes = NULL) {
  geo <- sf::st_drop_geometry(act$geo)
  names <- names(geo)
  if (is.null(attributes)) {
    i <- grep('ALAND|AWATER|INTPTLAT|INTPTLON|FUNCSTAT|Shape', names, ignore.case = TRUE)
    attributes <- names[-i]
  } else {
    attributes <- validate_names(names, attributes, 'attribute')
  }
  geo <- tibble::as_tibble(geo[, attributes])

  data <- act$data
  data <- transform_metadata_rest(data)
  names <- names(data)
  i <- grep('GEOID', names, ignore.case = TRUE)
  names[i] <- 'GEOID_Data'
  names(data) <- names
  i <- grep('estimate|margin_of_error', names)
  names <- c(names[-i], 'estimate', 'margin_of_error')
  data <- data[, names]
  data$estimate <- as.numeric(data$estimate)
  data$margin_of_error <- as.numeric(data$margin_of_error)

  data <- dplyr::inner_join(geo, data, by = "GEOID_Data")

  ft <-
    rolap::flat_table(name = act$area,
                      instances = data,
                      unknown_value = "Not available")
  ft <- ft |>
    rolap::transform_to_attribute("report_var", width = 2)
  ft
}



#' As `rolap::star_database` object
#'
#' Obtain an `rolap::star_database` object to be able to export it to a RDBMS and
#' make queries with other tools.
#'
#' We can indicate the attributes of the geographic layer to include in the export.
#' Otherwise, the default attributes are included (not area, perimeter or location
#' attributes).
#'
#' @param act An `acs_5yr_topic` object.
#' @param attributes A string vector.
#'
#' @return A `star_database` object.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' st <- anrc_2021_x01 |>
#'   as_star_database()
#'
#' @export
as_star_database <- function(act, attributes)
  UseMethod("as_star_database")

#' @rdname as_star_database
#' @export
as_star_database.acs_5yr_topic <- function(act, attributes = NULL) {
  ft <- as_flat_table(act, attributes)
  ft <- ft |>
    rolap::snake_case()
  names <- names(ft$table)
  l <- length(names)
  i <- grep('year', names, fixed = TRUE)
  geo_names <- names[1:(i - 1)]
  var_names <- names[(i + 1):(l - 2)]
  i <- grep('report_var', var_names, fixed = TRUE)
  var_names <- var_names[-i]
  i <- grep('subreport', var_names, fixed = TRUE)
  var_names <- c(var_names[1:i], 'report_var', var_names[(i + 1):length(var_names)])
  measure_names <- names[(l - 1):l]
  when <- rolap::dimension_schema(name = "dim_when",
                           attributes = 'year')
  where <- rolap::dimension_schema(name = "dim_where",
                            attributes = geo_names)
  what <- rolap::dimension_schema(name = "dim_what",
                            attributes = var_names)
  facts <- rolap::fact_schema(name = ft$name,
                           measures = measure_names)
  schema <- rolap::star_schema() |>
    rolap::define_facts(facts) |>
    rolap::define_dimension(when) |>
    rolap::define_dimension(where) |>
    rolap::define_dimension(what)

  db <- ft |>
    rolap::as_star_database(schema)
  db
}
