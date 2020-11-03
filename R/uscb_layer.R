#' `uscb_layer` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#'
#' @param filepath A string, path to gbd file.
#' @param metadata A metadata object.
#'
#' @return A `uscb_layer` object.
#'
#' @keywords internal
new_uscb_layer <- function(filepath = NULL, metadata = NULL) {
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read` to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  acs <-
    list(
      metadata = metadata,
      filepath = filepath,
      year = get_year_from_filepath(filepath),
      layer_names = layer_names,
      layer_name = NULL,
      layer = NULL,
      layer_metadata = NULL,
      layer_group_names = NULL,
      layer_group_name = NULL,
      layer_group_columns = NULL,
      layer_group_metadata = NULL
    )

  structure(acs,
            class = "uscb_layer")
}

#' `uscb_layer` S3 class
#'
#' A `uscb_layer` object is created from a given
#'
#' @param filepath A string, path to gbd file.
#' @param metadata A metadata object.
#'
#' @return A `uscb_layer` object.
#'
#'
#' @export
uscb_layer <-
  function(filepath = NULL, metadata = NULL) {
    new_uscb_layer(filepath, metadata)
  }


#' get year from filepath
#'
#' Get last year from a string.
#'
#' @param mdr A string.
#'
#' @return A string.
#'
#' @keywords internal
get_year_from_filepath <- function(filepath) {
  matches <- regmatches(filepath, gregexpr("[[:digit:]]+", filepath))
  matches <- unlist(matches)
  matches <- as.integer(matches)
  matches <- matches[matches >= 2000]
  sprintf("%d", matches[length(matches)])
}

