

#' As ACS census geo
#'
#' Gets an ACS census
#'
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A `acs_5yr_geo` object.
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
as_acs_5yr_geo <- function(act)
  UseMethod("as_acs_5yr_geo")

#' @rdname as_acs_5yr_geo
#' @export
as_acs_5yr_geo.acs_5yr <- function(act) {
  data <- act$data
  data <-
    tidyr::gather(data, "measure", "value", (length(names(data)) - 1):length(names(data)))
  data$Short_Name[data$measure == 'estimate'] <- gsub(
    pattern = '_',
    replacement = 'e',
    data$Short_Name[data$measure == 'estimate'],
    fixed = TRUE
  )
  data$Short_Name[data$measure == 'margin_of_error'] <- gsub(
    pattern = '_',
    replacement = 'm',
    data$Short_Name[data$measure == 'margin_of_error'],
    fixed = TRUE
  )
  data <- tibble::add_column(
    data,
    variable = paste0('Y', data$year, '_', data$Short_Name),
    .before = 1
  )
  metadata <- data |>
    dplyr::select(-tidyselect::all_of(c("GEOID", "value")))
  metadata <- transform_metadata_rest(metadata)

  data <- data |>
    dplyr::select(tidyselect::all_of(c("GEOID", "variable", "value")))
  names(data) <- c("GEOID_Data", "variable", "value")
  data$value <- as.numeric(data$value)
  data <- data |>
    tidyr::spread("variable", "value")
  data <- dplyr::inner_join(act$geo, data, by = "GEOID_Data")

  area <- names(act$area)
  area_code <- act$area
  year <- act$years
  file <- act$files
  topic_code <- act$topic[1]
  topic <- names(topic_code)
  origin <-
    data.frame(
      area,
      area_code,
      year,
      file,
      topic,
      topic_code,
      row.names = 1:length(year),
      stringsAsFactors = FALSE
    )
  if (length(act$topic) > 1) {
    for (i in 2:length(act$topic)) {
      topic_code <- act$topic[i]
      topic <- names(topic_code)
      origin <-
        rbind(
          origin,
          data.frame(
            area,
            area_code,
            year,
            file,
            topic,
            topic_code,
            row.names = 1:length(year),
            stringsAsFactors = FALSE
          )
        )
    }
  }
  structure(list(
    origin = origin,
    metadata = metadata,
    data = data
  ),
  class = "acs_5yr_geo")
}



#' @rdname get_geo_layer
#' @export
get_geo_layer.acs_5yr_geo<- function(act) {
  act$data
}


#' Save as GeoPackage
#'
#' Get the names of the geographic layer attributes (except for the geometry field).
#'
#' @param act A `acs_5yr_geo` object.
#' @param dir A string.
#' @param name A string, file name.
#'
#' @return A vector, geographical attribute names.
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
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' names <- act |>
#'   as_GeoPackage()
#'
#' @export
as_GeoPackage <- function(act, dir, name)
  UseMethod("as_GeoPackage")

#' @rdname as_GeoPackage
#' @export
as_GeoPackage.acs_5yr_geo<- function(act, dir = NULL, name = NULL) {
  if (is.null(name)) {
    name <- act$origin[1, "area_code"]
  }
  if (!is.null(dir)) {
    dir <- name_with_nexus(dir)
  }
  name <- tools::file_path_sans_ext(name)
  file <- paste0(dir, name, '.gpkg')

  sf::st_write(
    obj = act$data,
    dsn = file,
    layer = "data",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = act$metadata,
    dsn = file,
    layer = "metadata",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = act$origin,
    dsn = file,
    layer = "origin",
    append = FALSE,
    quiet = TRUE
  )
}

