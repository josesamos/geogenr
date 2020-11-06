# get_layer_names ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_layer_names <- function(ul) {
  UseMethod("get_layer_names")
}

#' @rdname get_layer_names
#' @export
#' @keywords internal
get_layer_names.uscb_layer <- function(ul) {

  ul$layer_names[substr(ul$layer_names, 1, 1) == "X"]
}



# get_layer ------------------------------------------------------

#' get layer
#'
#' get layer to interpret variables.
#'
#' @param ul A `uscb_layer` object.
#' @param layer_name A layer name.
#'
#' @return A `uscb_layer` object.
#'
#' @keywords internal
get_layer <- function(ul, layer_name) {
  UseMethod("get_layer")
}

#' @rdname get_layer
#' @export
#' @keywords internal
get_layer.uscb_layer <- function(ul, layer_name) {
  ul$layer_name <- layer_name
  ul$layer <-
    sf::st_read(dsn = ul$filepath,
                layer = layer_name,
                quiet = TRUE) %>%
    tibble::as_tibble()

  cod <- names(ul$layer)[-1]
  unsel_cod <- cod[!(cod %in% ul$metadata$metadata$Short_Name)]
  if (length(unsel_cod) > 0) {
    um <- uscb_metadata(
      ul$filepath,
      code = NULL,
      group_code = NULL,
      short_name = unsel_cod,
      uscb_acs_metadata = ul$metadata
    )
    um <- um %>% get_metadata()
    ul$metadata$metadata <- um$metadata
  }

  t <- ul$metadata$metadata[ul$metadata$metadata$Short_Name %in% cod, ]
  ul$layer_metadata <- Filter(function(x)!all(x == ""), t)

  ul$layer_group_names <- sort(unique(paste(ul$layer_metadata$group_code, ul$layer_metadata$group, sep = " - ")))

  ul
}


# get_layer_group_names ------------------------------------------------------

#' get layer group names
#'
#' get layer group names.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_layer_group_names <- function(ul) {
  UseMethod("get_layer_group_names")
}

#' @rdname get_layer_group_names
#' @export
#' @keywords internal
get_layer_group_names.uscb_layer <- function(ul) {

  ul$layer_group_names
}


# get_layer_group ------------------------------------------------------

#' get layer group
#'
#' get layer group to interpret variables.
#'
#' @param ul A `uscb_layer` object.
#' @param layer_group_name A layer name.
#'
#' @return A `uscb_layer` object.
#'
#' @keywords internal
get_layer_group <- function(ul, layer_group_name) {
  UseMethod("get_layer_group")
}

#' @rdname get_layer_group
#' @export
#' @keywords internal
get_layer_group.uscb_layer <- function(ul, layer_group_name) {
  ul$layer_group_name <- layer_group_name
  group_code <- substr(layer_group_name, 1, 3)

  short_names <- ul$layer_metadata$Short_Name[ul$layer_metadata$group_code == group_code]
  names <- names(ul$layer)
  ul$layer_group_columns <- c(names[1], names[names %in% short_names])
  t <- ul$layer_metadata[ul$layer_metadata$Short_Name %in% ul$layer_group_columns[-1], ]
  ul$layer_group_metadata <- Filter(function(x)!all(x == ""), t)

  ul
}


# get_flat_table ------------------------------------------------------

#' get tidy data
#'
#' get tidy data.
#'
#' @param ul A `uscb_layer` object.
#' @param remove_zeros A boolean, remove data with zero value.
#' @param remove_geometry A boolean, remove geometry column.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
get_flat_table <- function(ul, remove_zeros = TRUE, remove_geometry = FALSE) {
  UseMethod("get_flat_table")
}

#' @rdname get_flat_table
#' @export
#' @keywords internal
get_flat_table.uscb_layer <- function(ul, remove_zeros = TRUE, remove_geometry = TRUE) {
  layer <- ul$layer[, ul$layer_group_columns]
  layer <- tidyr::pivot_longer(layer, !c("GEOID"), names_to = "Short_Name", values_to = "value")
  if (remove_zeros) {
    layer <- layer[layer$value != 0, ]
  }
  layer <- dplyr::left_join(layer, ul$layer_group_metadata, by = c("Short_Name" = "Short_Name"))
  layer <- tibble::add_column(layer, year = ul$year, .before = "GEOID")
  layer <- dplyr::relocate(layer, c("value"), .after = tidyselect::last_col())
  layer$GEOID <- substr(layer$GEOID, 8, length(layer$GEOID))
  names <- names(layer)

  oldw <- getOption("warn")
  options(warn = -1)
  place <- sf::st_read(
    dsn = ul$filepath,
    layer = ul$layer_names[1],
    options = "METHOD=SKIP",
    quiet = TRUE,
    as_tibble = TRUE
  )
  options(warn = oldw)

  place_names <- names(place)
  # in one case the GEOID column name is GEOID10
  place_names_short <- substr(place_names, 1, 5)
  place_geoid <- (place_names[place_names_short == "GEOID"])[1]
  place_names[which(place_names == place_geoid)] <- "GEOID"
  layer <- dplyr::left_join(layer, place, by = c("GEOID" = place_geoid))
  names <- c(names[1], place_names[-length(place_names)], names[3:length(names)], place_names[length(place_names)])
  layer <- layer[, names]
  names_layer <- snakecase::to_snake_case(tolower(names(layer)), sep_out = "_")
  names(layer) <- names_layer
  if (remove_geometry) {
    layer[, names_layer[-length(names_layer)]]
  } else {
    sf::st_as_sf(layer)
  }
}






