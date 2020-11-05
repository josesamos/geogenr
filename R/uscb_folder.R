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
new_uscb_folder <- function(ul = NULL, year = NULL) {
  year <- sprintf("%d", year)
  year <- year[year != ul$year]
  pos <- gregexpr(pattern = ul$year, ul$filepath)[[1]]
  pos <- pos[length(pos)]
  rest = list()
  for (y in year) {
    filepath <- ul$filepath
    substr(filepath, pos, pos + 4) <- y
    stopifnot(file.exists(filepath))
    uly <- uscb_layer(filepath, metadata = ul$metadata)
    uly <- uly %>% get_layer(ul$layer_name)
    uly <- uly %>% get_layer_group(ul$layer_group_name)
    rest <- c(rest, list(uly))
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
#' A `uscb_folder` object is created from a given
#'
#' @param ul A `uscb_folder` object.
#' @param year A vector of years.
#'
#' @return A `uscb_folder` object.
#'
#'
#' @export
uscb_folder <- function(ul = NULL, year = NULL) {
    new_uscb_folder(ul, year)
  }



