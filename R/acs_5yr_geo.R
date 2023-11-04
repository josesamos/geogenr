

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
as_acs_5yr_geo.acs_5yr_topic <- function(act) {
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

  metadata <- data |>
    dplyr::select(-tidyselect::all_of(c("GEOID", "value")))
  metadata <- transform_metadata_rest(metadata)
  n <- as.character(nrow(metadata))
  l <- nchar(n)
  metadata <- tibble::add_column(
    metadata,
    variable = paste0('V', sprintf(sprintf("%%0%dd", l), 1:as.integer(n))),
    .before = 1
  )

  data <- data |>
    dplyr::inner_join(metadata, by = c('year', 'Short_Name'), suffix = c("", ".y")) |>
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
get_geo_layer.acs_5yr_geo <- function(glc) {
  glc$data
}


#' Get metadata layer
#'
#' Get the metadata layer.
#'
#' @param geo A acs_5yr_geo` object.
#'
#' @return A `sf` object.
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
#'   get_metadata()
#'
#' @export
get_metadata <- function(geo)
  UseMethod("get_metadata")

#' @rdname get_metadata
#' @export
get_metadata.acs_5yr_geo <- function(geo) {
  geo$metadata
}

#' Set metadata layer
#'
#' Set the metadata layer.
#'
#' @param geo A `acs_5yr_geo` object.
#' @param metadata A `tibble` object.
#'
#' @return A `sf` object.
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
#' metadata <- act |>
#'   get_metadata()
#'
#' act2 <- act |>
#'   set_metadata(metadata)
#'
#' @export
set_metadata <- function(geo, metadata)
  UseMethod("set_metadata")

#' @rdname set_metadata
#' @export
set_metadata.acs_5yr_geo <- function(geo, metadata) {
  geo$metadata <- metadata
  variable <- unique(metadata$variable)
  names <- names(geo$data)
  i <- grep('GEOID_Data', names, fixed = TRUE)
  names <- c(names[1:i], variable)
  geo$data <- geo$data[, names]
  geo
}



#' Save as GeoPackage
#'
#' Get the names of the geographic layer attributes (except for the geometry field).
#'
#' @param geo A `acs_5yr_geo` object.
#' @param dir A string.
#' @param name A string, file name.
#'
#' @return A string, file name.
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
as_GeoPackage <- function(geo, dir, name)
  UseMethod("as_GeoPackage")

#' @rdname as_GeoPackage
#' @export
as_GeoPackage.acs_5yr_geo <- function(geo, dir = NULL, name = NULL) {
  stopifnot(
    "The maximum number of columns supported by this format (1998 cols.) has been exceeded." = ncol(geo$data) < 1999
  )
  if (is.null(name)) {
    name <- geo$origin[1, "area_code"]
  }
  if (!is.null(dir)) {
    dir <- name_with_nexus(dir)
  }
  name <- tools::file_path_sans_ext(name)
  file <- paste0(dir, name, '.gpkg')

  sf::st_write(
    obj = geo$data,
    dsn = file,
    layer = "data",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = geo$metadata,
    dsn = file,
    layer = "metadata",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = geo$origin,
    dsn = file,
    layer = "origin",
    append = FALSE,
    quiet = TRUE
  )
  file
}

