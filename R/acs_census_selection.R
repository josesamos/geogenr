
#' Get available area names
#'
#' Gets the names of the Demographic and Economic Areas that are downloaded and
#' unzipped, available to be queried.
#'
#' @param ac A `acs_census` object.
#'
#' @return A vector, area names.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' areas <- ac |>
#'   get_available_areas()
#'
#' @export
get_available_areas <- function(ac)
  UseMethod("get_available_areas")

#' @rdname get_available_areas
#' @export
get_available_areas.acs_census<- function(ac) {
  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  ua <- unique(areas)
  res <- NULL
  for (a in ac$dedata$all_codes) {
    if (a %in% ua) {
      res <- c(res, ac$dedata$all_names[a])
    }
  }
  res
}


#' Get available area years
#'
#' Gets the years of the Demographic and Economic Areas that are downloaded and
#' unzipped, available to be queried.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#'
#' @return A vector, area years.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' years <- ac |>
#'   get_available_area_years(area = "State")
#'
#' @export
get_available_area_years <- function(ac, area)
  UseMethod("get_available_area_years")

#' @rdname get_available_area_years
#' @export
get_available_area_years.acs_census<- function(ac, area) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$dedata$all_codes), area, 'area')
  cod <- ac$dedata$all_codes[area]

  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  sort(names(areas[areas == cod]))
}


#' Get available area topics (report groups)
#'
#' Gets the topics (report groups) for the given years of the Demographic and
#' Economic Areas that are downloaded and unzipped, available to be queried.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return A vector, available report groups.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' topics <- ac |>
#'   get_available_area_topics("State", 2019:2021)
#'
#' topics <- ac |>
#'   get_available_area_topics("State")
#'
#' @export
get_available_area_topics <- function(ac, area, years)
  UseMethod("get_available_area_topics")

#' @rdname get_available_area_topics
#' @export
get_available_area_topics.acs_census<- function(ac, area, years = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$dedata$all_codes), area, 'area')
  cod <- ac$dedata$all_codes[area]

  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  av_years <- sort(names(areas[areas == cod]))
  years <- validate_names(av_years, years, 'year')
  names(files) <- areas
  files <- files[names(files) == cod]
  areas <- get_file_area(files)
  names(files) <- names(areas)
  files <- files[names(files) %in% years]
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
  name_to_title(res)
}



#' As ACS census topic (report group)
#'
#' Gets an ACS census topic object (report group) for the given years of the
#' Demographic and Economic Areas that are downloaded and unzipped, available to
#' be queried.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#' @param topic A string, topic name.
#'
#' @return A `acs_census_topic` object.
#'
#' @family data selection functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' act <- ac |>
#'   as_acs_census_topic("State", 2019:2021, "X01 Age And Sex")
#'
#' act <- ac |>
#'   as_acs_census_topic("State", topic = "X01 Age And Sex")
#'
#' @export
as_acs_census_topic <- function(ac, area, years, topic)
  UseMethod("as_acs_census_topic")

#' @rdname as_acs_census_topic
#' @export
as_acs_census_topic.acs_census<- function(ac, area, years = NULL, topic) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$dedata$all_codes), area, 'area')
  cod <- ac$dedata$all_codes[area]

  files <- get_gbd_files(ac$dir)
  areas <- get_file_area(files)
  av_years <- sort(names(areas[areas == cod]))
  years <- validate_names(av_years, years, 'year')
  names(files) <- areas
  files <- files[names(files) == cod]
  areas <- get_file_area(files)
  names(files) <- names(areas)
  files <- files[names(files) %in% years]
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
  topic <- validate_names(topics, topic, 'topic')
  names(res) <- topics
  topic_name <- res[topic]

  structure(list(
    area = cod,
    years = years,
    topic = topic_name,
    files = files
  ),
  class = "acs_census_topic")
}
