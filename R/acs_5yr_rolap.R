

#' As flat_table
#'
#' Gets a flat_table
#'
#'
#' @param act A `acs_5yr_topic` object.
#' @param geo_attribute_names A string vector.
#'
#' @return A `flat_table` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    2021,
#'                    "X01 Age And Sex")
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' @export
as_flat_table <- function(act, geo_attribute_names)
  UseMethod("as_flat_table")

#' @rdname as_flat_table
#' @export
as_flat_table.acs_5yr <- function(act, geo_attribute_names = NULL) {
  geo <- sf::st_drop_geometry(act$geo)
  names <- names(geo)
  if (is.null(geo_attribute_names)) {
    i <- grep('ALAND|AWATER|INTPTLAT|INTPTLON|FUNCSTAT|Shape', names, ignore.case = TRUE)
    geo_attribute_names <- names[-i]
  } else {
    geo_attribute_names <- validate_names(names, geo_attribute_names, 'attribute')
  }
  geo <- tibble::as_tibble(geo[, geo_attribute_names])

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



#' As star_database
#'
#' Gets a star_database
#'
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A `star_database` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    2021,
#'                    "X01 Age And Sex")
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' @export
as_star_database <- function(act)
  UseMethod("as_star_database")

#' @rdname as_star_database
#' @export
as_star_database.acs_5yr <- function(act) {
}

