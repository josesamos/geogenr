#' `uscb_acs_5ye` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param folder A string.
#'
#' @return A `uscb_acs_5ye` object.
#'
#' @keywords internal
new_uscb_acs_5ye <- function(folder = "") {

  years <- 2010:2018
  url <- "https://www2.census.gov/geo/tiger/TIGER_DP/%dACS/"
  extension <- ".gdb.zip"

  legal_and_administrative_areas <-
    data.frame(
      type = 1,
      name = c(
        "American Indian/Alaska Native/Native Hawaiian Area",
        "Alaska Native Regional Corporation",
        "Congressional District (116th Congress)",
        "County",
        "Place",
        "Elementary School District",
        "Secondary School District",
        "Unified School District",
        "State",
        "State Legislative Districts - Upper Chamber",
        "State Legislative Districts - Lower Chamber",
        "Zip Code Tabulation Area"
      ),
      url = c(
        "ACS_%d_5YR_AIARES",
        "ACS_%d_5YR_ANRC",
        "ACS_%d_5YR_CD_116",
        "ACS_%d_5YR_COUNTY",
        "ACS_%d_5YR_PLACE",
        "ACS_%d_5YR_SDE",
        "ACS_%d_5YR_SDS",
        "ACS_%d_5YR_SDU",
        "ACS_%d_5YR_STATE",
        "ACS_%d_5YR_SLDU",
        "ACS_%d_5YR_SLDL",
        "ACS_%d_5YR_ZCTA"
      )
    )

  statistical_areas <-
    data.frame(
      type = 2,
      name = c(
        "Tribal Block Group",
        "Tribal Census Tract",
        "New England City and Town Area",
        "New England City and Town Area Division",
        "Combined New England City and Town Area",
        "Metropolitan/Micropolitan Statistical Area",
        "Metropolitan Division",
        "Combined Statistical Area",
        "Public Use Microdata Area",
        "Urban Area"
      ),
      url = c(
        "ACS_%d_5YR_TBG",
        "ACS_%d_5YR_TTRACT",
        "ACS_%d_5YR_NECTA",
        "ACS_%d_5YR_NECTADIV",
        "ACS_%d_5YR_CNECTA",
        "ACS_%d_5YR_MSA",
        "ACS_%d_5YR_METDIV",
        "ACS_%d_5YR_CSA",
        "ACS_%d_5YR_PUMA",
        "ACS_%d_5YR_UA"
      )
    )

  acs <-
    list(
      folder = folder,
      years = years,
      url = url,
      extension = extension,
      variables = rbind(legal_and_administrative_areas, statistical_areas)
    )

  structure(acs,
            class = "uscb_acs_5ye")
}

#' `uscb_acs_5ye` S3 class
#'
#' A `uscb_acs_5ye` object is created from a given local folder.
#'
#' @param folder A string.
#'
#' @return A `uscb_acs_5ye` object.
#'
#' @family data collection functions
#'
#' @examples
#'
#' folder <- "../geodimension/data/us/"
#' ua <- uscb_acs_5ye(folder = folder)
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#'
#' @export
uscb_acs_5ye <- function(folder = "") {
    new_uscb_acs_5ye(folder)
  }



# -----------------------------------------------------------------------

#' url_file_exists
#'
#' https://stackoverflow.com/questions/60318926/how-to-check-if-file-exists-in-the-url-before-use-download-file-in-r
#'
#' @param url A string.
#'
#' @return A boolean
#'
#' @keywords internal
url_file_exists <- function(url) {
  head_url <- httr::HEAD(url)
  (head_url$all_headers[[1]]$status == 200)
}


# -----------------------------------------------------------------------

#' get_geodatabase_url
#'
#' @param url A String
#' @param extension A String
#' @param names A String
#' @param name A String
#' @param year An Integer
#'
#' @return A boolean
#'
#' @keywords internal
get_geodatabase_url <- function(url, extension, names, name, year) {
  name <- names[names$name == name, "url"]
  url <- paste(url, name, extension, sep = "")
  sprintf(url, year, year)
}


# -----------------------------------------------------------------------

#' get_geodatabase_file
#'
#' @param folder A String
#' @param extension A String
#' @param names A String
#' @param name A String
#' @param year An Integer
#'
#' @return A boolean
#'
#' @keywords internal
get_geodatabase_file <- function(folder, extension, names, name, year) {
  name <- names[names$name == name, "url"]
  file <- paste(folder, name, extension, sep = "")
  sprintf(file, year)
}


# get_legal_and_administrative_areas ------------------------------------------------------

#' Get Legal and Administrative Area names
#'
#' Returns a list of area names for which geodatabases are available.
#'
#' @param ua A `uscb_acs_5ye` object.
#'
#' @return A vector of names.
#'
#' @family data collection functions
#'
#' @examples
#'
#' folder <- "../geodimension/data/us/"
#' ua <- uscb_acs_5ye(folder = folder)
#' laa <- ua |> get_legal_and_administrative_areas()
#'
#' @export
get_legal_and_administrative_areas <- function(ua) {
  UseMethod("get_legal_and_administrative_areas")
}

#' @rdname get_legal_and_administrative_areas
#' @export
get_legal_and_administrative_areas.uscb_acs_5ye<- function(ua) {

  sort(ua$variables[ua$variables$type == 1, "name"])
}

# get_statistical_areas ------------------------------------------------------

#' Get Statistical Area names
#'
#' Returns a list of area names for which geodatabases are available.
#'
#' @param ua A `uscb_acs_5ye` object.
#'
#' @return A vector of names.
#'
#' @family data collection functions
#'
#' @examples
#'
#' folder <- "../geodimension/data/us/"
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#'
#' @export
get_statistical_areas <- function(ua) {
  UseMethod("get_statistical_areas")
}

#' @rdname get_statistical_areas
#' @export
get_statistical_areas.uscb_acs_5ye<- function(ua) {

  sort(ua$variables[ua$variables$type == 2, "name"])
}


# get_available_years_in_the_web ------------------------------------------------------

#' Get available years in the web
#'
#' For the name of a geodatabase, returns a vector with the years for which data
#' is available on the web.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param geodatabase A string.
#'
#' @return A vector of integers.
#'
#' @family data collection functions
#'
#' @examples
#'
#' folder <- "../geodimension/data/us/"
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#'
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' \donttest{
#' y <- ua |> get_available_years_in_the_web(geodatabase = sa[6])
#' }
#' @export
get_available_years_in_the_web <- function(ua, geodatabase) {
  UseMethod("get_available_years_in_the_web")
}

#' @rdname get_available_years_in_the_web
#' @export
get_available_years_in_the_web.uscb_acs_5ye<- function(ua, geodatabase) {
  res <- NULL
  for (year in ua$years) {
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, geodatabase, year)
    res <- c(res, url_file_exists(url))
  }
  years <- ua$years[res]
  while (TRUE) {
    year <- years[length(years)] + 1
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, geodatabase, year)
    if (url_file_exists(url)) {
      years <- c(years, year)
    } else {
      break
    }
  }
  years
}


# get_available_years_downloaded ------------------------------------------------------

#' Get available years downloaded
#'
#' For the name of a geodatabase, returns a vector with the years for which data
#' is available on the local folder.
#'
#' If the folder is not indicated, it is considered that of the class.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param geodatabase A string.
#' @param folder A string.
#'
#' @return A vector of integers.
#'
#' @family data collection functions
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
#' y <- ua |> get_available_years_downloaded(geodatabase = sa[6])
#'
#' @export
get_available_years_downloaded <- function(ua, geodatabase, folder = NULL) {
  UseMethod("get_available_years_downloaded")
}

#' @rdname get_available_years_downloaded
#' @export
get_available_years_downloaded.uscb_acs_5ye<- function(ua, geodatabase, folder = NULL) {
  if (is.null(folder)) {
    folder <- ua$folder
  }
  res <- NULL
  for (year in ua$years) {
    destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, geodatabase, year)
    res <- c(res, file.exists(destfile))
  }
  years <- ua$years[res]
  while (TRUE) {
    if (length(years) == 0) {
      years <- NULL
    }
    year <- ua$years[length(ua$years)] + 1
    destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, geodatabase, year)
    if (file.exists(destfile)) {
      years <- c(years, year)
    } else {
      break
    }
  }
  years
}


# download_geodatabases ------------------------------------------------------

#' Download geodatabases
#'
#' For the name of a geodatabase and the given years, downloads from the web the
#' corresponding geodatabase data files. Returns a vector with the years for
#' which data is now available on the folder.
#'
#' If the folder is not indicated, it is considered that of the class.
#'
#' @param ua A `uscb_acs_5ye` object.
#' @param geodatabase A string.
#' @param years A vector of integers.
#' @param folder A string.
#'
#' @return A vector of integers.
#'
#' @family data collection functions
#'
#' @examples
#'
#' folder <- "../geodimension/data/us/"
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#'
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' \donttest{
#' y <- ua |> get_available_years_in_the_web(geodatabase = sa[6])
#' \dontrun{
#' y_res <- ua |> download_geodatabases(geodatabase = sa[6], years = y)
#' }
#' }
#' @export
download_geodatabases <- function(ua, geodatabase, years, folder = NULL) {
  UseMethod("download_geodatabases")
}

#' @rdname download_geodatabases
#' @export
download_geodatabases.uscb_acs_5ye<- function(ua, geodatabase, years, folder = NULL) {
  if (is.null(folder)) {
    folder <- ua$folder
  }
  res <- NULL
  for (year in years) {
    url <- get_geodatabase_url(ua$url, ua$extension, ua$variables, geodatabase, year)
    url_exists <- url_file_exists(url)
    res <- c(res, url_exists)
    if (url_exists) {
      destfile <- get_geodatabase_file(folder, ua$extension, ua$variables, geodatabase, year)
      utils::download.file(url, destfile = destfile)
    }
  }
  years[res]
}

