#' `uscb_layer` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param metadata A metadata object.
#' @param ua A `uscb_acs_5ye` object.
#' @param geodatabase A string.
#' @param year A integer
#' @param folder A string.
#'
#' @return A `uscb_layer` object.
#'
#' @keywords internal
new_uscb_layer <- function(metadata, ua, geodatabase, year, folder = NULL) {
  if (is.null(folder)) {
    folder <- ua$folder
  }
  filepath <- get_geodatabase_file(folder, ua$extension, ua$variables, geodatabase, year)
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read` to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  acs <-
    list(
      metadata = metadata,
      ua = ua,
      geodatabase = geodatabase,
      year = sprintf("%d", year),
      filepath = filepath,
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
#' A `uscb_layer` object is created from the available metadata, a
#' `uscb_acs_5ye` object, a geodatabase name, and a year.
#'
#' If the folder is not indicated, it is considered that of the `uscb_acs_5ye`
#' object class.
#'
#' @param metadata A metadata object.
#' @param ua A `uscb_acs_5ye` object.
#' @param geodatabase A string.
#' @param year A integer
#' @param folder A string.
#'
#' @return A `uscb_layer` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#'
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#'
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#'
#' @export
uscb_layer <-
  function(metadata, ua, geodatabase, year, folder = NULL) {
    new_uscb_layer(metadata, ua, geodatabase, year, folder)
  }


#' Get year from filepath
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

# get_place ------------------------------------------------------

#' Get place
#'
#' Get geographical layer from a geodatabase.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
get_place <- function(ul) {
  # oldw <- getOption("warn")
  # options(warn = -1)
  place <- sf::st_read(
    dsn = ul$filepath,
    layer = ul$layer_names[1],
    options = "METHOD=SKIP",
    quiet = TRUE,
    as_tibble = TRUE
  )
  # options(warn = oldw)

  place_names <- names(place)
  # in some cases the GEOID column name is GEOID10, rename it.
  place_names_short <- substr(place_names, 1, 5)
  place_geoid <- (place_names[place_names_short == "GEOID"])[1]
  place_names[which(place_names == place_geoid)] <- "GEOID"
  names(place) <- place_names

  place
}
