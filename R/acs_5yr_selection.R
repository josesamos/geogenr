
#' Get available area names
#'
#' Gets the names of the Demographic and Economic Areas that are downloaded and
#' unzipped, available to be queried.
#'
#' @param ac An `acs_5yr` object.
#'
#' @return A vector, area names.
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
#' areas <- ac |>
#'   get_available_areas()
#'
#' @export
get_available_areas <- function(ac)
  UseMethod("get_available_areas")

#' @rdname get_available_areas
#' @export
get_available_areas.acs_5yr<- function(ac) {
  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  ua <- unique(areas)
  res <- NULL
  for (a in ac$acs_5yr_md$all_codes) {
    if (a %in% ua) {
      res <- c(res, ac$acs_5yr_md$all_names[a])
    }
  }
  sort(unique(res))
}


#' Get available area years
#'
#' Gets the years of the Demographic and Economic Areas that are downloaded and
#' unzipped, available to be queried.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#'
#' @return A vector, area years.
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
#' years <- ac |>
#'   get_available_area_years(area = "Alaska Native Regional Corporation")
#'
#' @export
get_available_area_years <- function(ac, area)
  UseMethod("get_available_area_years")

#' @rdname get_available_area_years
#' @export
get_available_area_years.acs_5yr<- function(ac, area) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$acs_5yr_md$all_codes), area, 'area')
  cod <- ac$acs_5yr_md$all_codes[area]

  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  sort(unique(names(areas[areas == cod])))
}


#' Get available area topics (report groups)
#'
#' Gets the topics (report groups) for the given years of the Demographic and
#' Economic Areas that are downloaded and unzipped, available to be queried.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return A vector, available report groups.
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
#' topics <- ac |>
#'   get_available_area_topics("Alaska Native Regional Corporation",
#'                             2021)
#'
#' topics <- ac |>
#'   get_available_area_topics("Alaska Native Regional Corporation")
#'
#' @export
get_available_area_topics <- function(ac, area, years)
  UseMethod("get_available_area_topics")

#' @rdname get_available_area_topics
#' @export
get_available_area_topics.acs_5yr<- function(ac, area, years = NULL) {
  act <- new_acs_5yr_topic(ac, area, years, topic = NULL)
  names(act$area_topics)
}

#' As ACS census topic (report group)
#'
#' Gets an ACS census topic object (report group) for the given years of the
#' Demographic and Economic Areas that are downloaded and unzipped, available to
#' be queried.
#'
#' If no year is indicated, all available years are taken. If no topic is given,
#' the first one that appears in the files is taken.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#' @param topic A vector, topic name.
#'
#' @return An `acs_5yr_topic` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' \donttest{
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
#' anrc_2021_2022_x01_x07 <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = c("X01 Age And Sex", "X07 Migration"))
#' }
#' @export
as_acs_5yr_topic <- function(ac, area, years, topic)
  UseMethod("as_acs_5yr_topic")

#' @rdname as_acs_5yr_topic
#' @export
as_acs_5yr_topic.acs_5yr <- function(ac, area, years = NULL, topic = NULL) {
  act <- new_acs_5yr_topic(ac, area, years, topic)
  get_topic_data(act)
}


#' New acs_5yr_topic object
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#' @param topic A string, topic name.
#'
#' @return An `acs_5yr_topic` object.
#'
#' @keywords internal
new_acs_5yr_topic <- function(ac, area, years = NULL, topic = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$acs_5yr_md$all_codes), area, 'area')
  cod <- ac$acs_5yr_md$all_codes[area]

  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  av_years <- sort(unique(names(areas[areas == cod])))
  years <- validate_names(av_years, years, 'year')
  names(files) <- areas
  files <- files[names(files) == cod]
  areas <- get_file_area(files)
  names(files) <- names(areas)
  files <- files[names(files) %in% years]
  # multiple files downloaded
  if (length(files) > length(years)) {
    nf <- NULL
    for (y in years) {
      nf <- c(nf, files[names(files) == y][1])
    }
    names(nf) <- years
    files <- nf
  }
  res <- NULL
  for (f in files) {
    layers <- sf::st_layers(f)
    layers <- layers$name
    layers <- layers[substr(layers, 1, 1) == 'X']
    if (is.null(res)) {
      res <- layers
    } else {
      res <- intersect(res, layers)
    }
  }
  topics <- name_to_title(res)
  names(res) <- topics
  res <- sort(res)
  if (is.null(topic)) {
    topic <- topics[1]
  } else {
    topic <- validate_names(topics, topic, 'topic')
  }
  topic_name <- res[topic]

  act <- structure(list(
    area = cod,
    years = years,
    topic = topic_name,
    area_topics = res,
    files = files
  ),
  class = "acs_5yr_topic")
}
