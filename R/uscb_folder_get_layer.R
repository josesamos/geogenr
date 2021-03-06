

# get_common_flat_table ------------------------------------------------------

#' Get common flat table
#'
#' Get the layer group data in the form of a flat table that includes all the
#' available data columns for the geodatabases corresponding to the selected
#' years.
#'
#' Optionally you can delete the rows whose measurement value is zero and remove
#' the geometry column.
#'
#' @param uf A `uscb_folder` object.
#' @param remove_zeros A boolean, remove data with zero value.
#' @param remove_geometry A boolean, remove geometry column.
#'
#' @return A `tibble` object.
#'
#' @family result generation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua %>% get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul %>% get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul %>% get_layer(layers[3])
#' lg <- ul %>% get_layer_group_names()
#' # lg[2]
#' # [1] "003 - DETAILED RACE"
#' ul <- ul %>% get_layer_group(lg[2])
#' uf <- uscb_folder(ul)
#'
#' layer_common <- uf %>% get_common_flat_table()
#'
#' @export
get_common_flat_table <- function(uf, remove_zeros = TRUE, remove_geometry = FALSE) {
  UseMethod("get_common_flat_table")
}

#' @rdname get_common_flat_table
#' @export
get_common_flat_table.uscb_folder <- function(uf, remove_zeros = TRUE, remove_geometry = TRUE) {
  tidy <- uf$initial %>% get_flat_table(remove_zeros, remove_geometry)
  for (i in seq_along(uf$rest)) {
    if (same_layer_group_columns(uf$initial, uf$rest[[i]])) {
      tidy_rest <- uf$rest[[i]] %>% get_flat_table(remove_zeros, remove_geometry)
      tidy <- tidy %>% tibble::add_row(tidy_rest)
    }
   }

  tidy
}




#' same_layer_group_columns
#'
#' Check if two layer groups have the same columns.
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


# get_common_geomultistar ------------------------------------------------------

#' Get common `geomultistar`
#'
#' Get all the layer group data in the form of a `geomultistar` object for all
#' geodatabases from the same folder selected: It contains fact and dimension
#' tables, and a dimension with an associated geographic layer.
#'
#' The name of the facts is the layer group name.
#'
#' @param uf A `uscb_folder` object.
#'
#' @return A `geomultistar` object.
#'
#' @family result generation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua %>% get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul %>% get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul %>% get_layer(layers[3])
#' lg <- ul %>% get_layer_group_names()
#' # lg[2]
#' # [1] "003 - DETAILED RACE"
#' ul <- ul %>% get_layer_group(lg[2])
#' uf <- uscb_folder(ul)
#'
#' gms <- uf %>% get_common_geomultistar()
#'
#' @export
get_common_geomultistar <- function(uf) {
  UseMethod("get_common_geomultistar")
}

#' @rdname get_common_geomultistar
#' @export
get_common_geomultistar.uscb_folder <- function(uf) {
  ft <- get_basic_flat_table(uf$initial, remove_zeros = FALSE)
  names_ft <- names(ft)
  what_ini <- which(names_ft == "Short_Name")
  fact_ini <- which(names_ft == "Estimate")

  for (i in seq_along(uf$rest)) {
    if (same_layer_group_columns(uf$initial, uf$rest[[i]])) {
      ft_rest <- get_basic_flat_table(uf$rest[[i]], remove_zeros = FALSE)
      ft <- ft %>% tibble::add_row(ft_rest)
    }
  }

  place <- get_place(uf$initial)
  place_names <- names(place)
  place_names <- place_names[1:(length(place_names) - 1)]
  ft <- dplyr::left_join(ft, place[, place_names], by = c("GEOID" = "GEOID"))

  define_geomultistar(ft,
                      fact_name = substr(uf$initial$layer_group_name, 7, nchar(uf$initial$layer_group_name)),
                      where_names = place_names,
                      what_names = names_ft[what_ini:(fact_ini - 1)],
                      place)
}

