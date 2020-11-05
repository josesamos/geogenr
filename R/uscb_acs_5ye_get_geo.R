# get_legal_and_administrative_areas ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_legal_and_administrative_areas <- function(ua) {
  UseMethod("get_legal_and_administrative_areas")
}

#' @rdname get_legal_and_administrative_areas
#' @export
#' @keywords internal
get_legal_and_administrative_areas.uscb_acs_5ye<- function(ua) {

  sort(ua$variables[ua$variables$type == 1, "name"])
}

# get_statistical_areas ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_statistical_areas <- function(ua) {
  UseMethod("get_statistical_areas")
}

#' @rdname get_statistical_areas
#' @export
#' @keywords internal
get_statistical_areas.uscb_acs_5ye<- function(ua) {

  sort(ua$variables[ua$variables$type == 2, "name"])
}


# get_years_available ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_years_available <- function(ua) {
  UseMethod("get_years_available")
}

#' @rdname get_years_available
#' @export
#' @keywords internal
get_years_available.uscb_acs_5ye<- function(ua) {

  ua$years
}

