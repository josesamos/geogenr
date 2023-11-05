
#' Titles and Years of Selected Demographic and Economic Data
#'
#' Available selected Demographic and Economic Data from the American Community
#' Survey (ACS) 5-year estimates data titles and years.
#'
#' @format A vector list.
#' @source
#'   \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-data.2021.html#list-tab-1656998034}
"acs_5yr_md"


#' "Alaska Native Regional Corporation", 2021, "X01 Age And Sex"
#'
#' Topic selected for the area and years indicated: "Alaska Native Regional
#' Corporation", 2021.
#'
#' @family selection data
#' @examples
#' # Defined by:
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' anrc_2021_x01 <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    2021,
#'                    "X01 Age And Sex")
#'
#' @format An `acs_5yr_topic` object.
"anrc_2021_x01"


#' "Alaska Native Regional Corporation", 2021, 2022, "X01 Age And Sex", "X07 Migration"
#'
#' Topics selected for the area and years indicated: "Alaska Native Regional
#' Corporation", 2021, 2022.
#'
#' Since no year is indicated in the definition, all available years are considered.
#'
#' @family selection data
#' @examples
#' # Defined by:
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' anrc_2021_2022_x01_x07 <- ac |>
#'   as_acs_5yr_topic(area = "Alaska Native Regional Corporation",
#'                    topic = c("X01 Age And Sex", "X07 Migration"))
#'
#' @format An `acs_5yr_topic` object.
"anrc_2021_2022_x01_x07"

