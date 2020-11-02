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
  # Set the `layer' argument in `st_read' to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  acs <-
    list(
      filepath = filepath,
      layers = layer_names,
      metadata = metadata
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


