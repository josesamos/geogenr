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


# get_available_years_in_the_web ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param name A string.
#'
#' @return A vector of years.
#'
#' @keywords internal
get_available_years_in_the_web <- function(ua, name) {
  UseMethod("get_available_years_in_the_web")
}

#' @rdname get_available_years_in_the_web
#' @export
#' @keywords internal
get_available_years_in_the_web.uscb_acs_5ye<- function(ua, name) {
  res <- NULL
  for (year in ua$years) {
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, name, year)
    res <- c(res, url_file_exists(url))
  }
  years <- ua$years[res]
  while (TRUE) {
    year <- years[length(years)] + 1
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, name, year)
    if (url_file_exists(url)) {
      years <- c(years, year)
    } else {
      break
    }
  }
  years
}


# get_available_years_downloaded ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param name A string.
#' @param folder A string.
#'
#' @return A vector of years.
#'
#' @keywords internal
get_available_years_downloaded <- function(ua, name, folder = NULL) {
  UseMethod("get_available_years_downloaded")
}

#' @rdname get_available_years_downloaded
#' @export
#' @keywords internal
get_available_years_downloaded.uscb_acs_5ye<- function(ua, name, folder = NULL) {
  if (is.null(folder)) {
    folder <- ua$folder
  }
  res <- NULL
  for (year in ua$years) {
    destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, name, year)
    res <- c(res, file.exists(destfile))
  }
  years <- ua$years[res]
  while (TRUE) {
    if (length(years) == 0) {
      years <- NULL
    }
    year <- ua$years[length(ua$years)] + 1
    destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, name, year)
    if (file.exists(destfile)) {
      years <- c(years, year)
    } else {
      break
    }
  }
  years
}


# download_geodatabases ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param name A string.
#' @param years A vector of years.
#' @param folder A string.
#'
#' @return A vector of years.
#'
#' @keywords internal
download_geodatabases <- function(ua, name, years, folder = NULL) {
  UseMethod("download_geodatabases")
}

#' @rdname download_geodatabases
#' @export
#' @keywords internal
download_geodatabases.uscb_acs_5ye<- function(ua, name, years, folder = NULL) {
  if (is.null(folder)) {
    folder <- ua$folder
  }
  res <- NULL
  for (year in years) {
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, name, year)
    url_exists <- url_file_exists(url)
    res <- c(res, url_exists)
    if (url_exists) {
      destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, name, year)
      utils::download.file(url, destfile = destfile)
    }
  }
  years[res]
}

