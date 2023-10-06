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
#' @param mdr A string.
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
#'
#' @param mdr A string.
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
#'
#' @param mdr A string.
#'
#' @return A boolean
#'
#' @keywords internal
get_geodatabase_file <- function(folder, extension, names, name, year) {
  name <- names[names$name == name, "url"]
  file <- paste(folder, name, extension, sep = "")
  sprintf(file, year)
}
