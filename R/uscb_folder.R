#' `uscb_folder` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#'
#' @param ul A `uscb_folder` object.
#' @param year A vector of years.
#'
#' @return A `uscb_folder` object.
#'
#' @keywords internal
new_uscb_folder <- function(ul, year = NULL) {
  if (is.null(year)) {
    year <- ul$ua %>% get_available_years_downloaded(geodatabase = ul$geodatabase)
  }

  year <- sprintf("%d", year)
  year <- year[year != ul$year]
  pos <- gregexpr(pattern = ul$year, ul$filepath)[[1]]
  pos <- pos[length(pos)]
  rest = list()
  if (length(year) > 0) {
    for (y in year) {
      filepath <- ul$filepath
      substr(filepath, pos, pos + 4) <- y
      stopifnot(file.exists(filepath))
      uly <- uscb_layer(metadata = ul$metadata, ua = ul$ua, geodatabase = ul$geodatabase, year = strtoi(y))
      uly <- uly %>% get_layer(ul$layer_name)
      uly <- uly %>% get_layer_group(ul$layer_group_name)
      rest <- c(rest, list(uly))
    }
  }

  acs <-
    list(
      initial = ul,
      rest = rest
    )

  structure(acs,
            class = "uscb_folder")
}

#' `uscb_folder` S3 class
#'
#' A `uscb_folder` object is created from a `uscb_layer` object and the
#' geodatabases of the same layer group corresponding to other years located in
#' the same folder
#'
#' If the vector of years is not indicated, the geodatabases of all the years
#' located in the same folder are considered.
#'
#'
#' @param ul A `uscb_folder` object.
#' @param year A vector of years.
#'
#' @return A `uscb_folder` object.
#'
#' @family data selection functions
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
#'
#' uf <- uscb_folder(ul)
#'
#' @export
uscb_folder <- function(ul, year = NULL) {
    new_uscb_folder(ul, year)
  }



