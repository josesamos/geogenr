

#' As flat_table
#'
#' Gets a flat_table
#'
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A `flat_table` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    2021,
#'                    "X01 Age And Sex")
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' @export
as_flat_table <- function(act)
  UseMethod("as_flat_table")

#' @rdname as_flat_table
#' @export
as_flat_table.acs_5yr <- function(act) {
}



#' As star_database
#'
#' Gets a star_database
#'
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A `star_database` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- tempdir()
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    2021,
#'                    "X01 Age And Sex")
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' @export
as_star_database <- function(act)
  UseMethod("as_star_database")

#' @rdname as_star_database
#' @export
as_star_database.acs_5yr <- function(act) {
}

