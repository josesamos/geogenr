# get_folder_layer_names ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param uf A `uscb_folder` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_folder_layer_names <- function(uf) {
  UseMethod("get_folder_layer_names")
}

#' @rdname get_folder_layer_names
#' @export
#' @keywords internal
get_folder_layer_names.uscb_folder <- function(uf) {

  uf$initial %>% get_layer_names()
}



# get_folder_layer ------------------------------------------------------

#' get layer
#'
#' get layer to interpret variables.
#'
#' @param uf A `uscb_folder` object.
#' @param layer_name A layer name.
#'
#' @return A `uscb_folder` object.
#'
#' @keywords internal
get_folder_layer <- function(uf, layer_name) {
  UseMethod("get_folder_layer")
}

#' @rdname get_folder_layer
#' @export
#' @keywords internal
get_folder_layer.uscb_folder <- function(uf, layer_name) {
  uf$initial <- uf$initial %>% get_layer(layer_name)
  for (i in seq_along(uf$rest)) {
    uf$rest[i] <- uf$rest[i] %>% get_layer(layer_name)
  }

  uf
}


# get_folder_layer_group_names ------------------------------------------------------

#' get layer group names
#'
#' get layer group names.
#'
#' @param uf A `uscb_folder` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_folder_layer_group_names <- function(uf) {
  UseMethod("get_folder_layer_group_names")
}

#' @rdname get_folder_layer_group_names
#' @export
#' @keywords internal
get_folder_layer_group_names.uscb_folder <- function(uf) {

  uf$initial %>% get_layer_group_names()
}


# get_folder_layer_group ------------------------------------------------------

#' get layer group
#'
#' get layer group to interpret variables.
#'
#' @param uf A `uscb_folder` object.
#' @param layer_group_name A layer name.
#'
#' @return A `uscb_folder` object.
#'
#' @keywords internal
get_folder_layer_group <- function(uf, layer_group_name) {
  UseMethod("get_folder_layer_group")
}

#' @rdname get_folder_layer_group
#' @export
#' @keywords internal
get_folder_layer_group.uscb_folder <- function(uf, layer_group_name) {
  uf$initial <- uf$initial %>% get_layer_group(layer_group_name)
  for (i in seq_along(uf$rest)) {
    uf$rest[i] <- uf$rest[i] %>% get_layer_group(layer_group_name)
  }

  uf
}


# get_common_tidy_data ------------------------------------------------------

#' get tidy data
#'
#' get tidy data.
#'
#' @param uf A `uscb_folder` object.
#' @param remove_zeros A boolean, remove data with zero value.
#' @param remove_geometry A boolean, remove geometry column.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
get_common_tidy_data <- function(uf, remove_zeros = TRUE, remove_geometry = FALSE) {
  UseMethod("get_common_tidy_data")
}

#' @rdname get_common_tidy_data
#' @export
#' @keywords internal
get_common_tidy_data.uscb_folder <- function(uf, remove_zeros = TRUE, remove_geometry = TRUE) {
  tidy <- uf$initial %>% get_tidy_data(remove_zeros, remove_geometry)
  for (i in seq_along(uf$rest)) {
    if (same_layer_group_columns(uf$initial, uf$rest[[i]])) {
      tidy_rest <- uf$rest[[i]] %>% get_tidy_data(remove_zeros, remove_geometry)
      tidy <- tidy %>% tibble::add_row(tidy_rest)
    }
   }

  tidy
}




#' same_layer_group_columns
#'
#' Get last year from a string.
#'
#' @param initial A vector of strings.
#' @param rest A vector of strings.
#'
#' @return A boolean
#'
#' @keywords internal
same_layer_group_columns <- function(initial, rest) {
  res <- TRUE
  if (length(initial$layer_group_columns) != length(rest$layer_group_columns)) {
    res <- FALSE
  } else {
    res <- all(initial$layer_group_columns == rest$layer_group_columns)
  }
  res
}



