.onLoad <- function(libname, pkgname) {
  utils::data(
    "dedata",
    package = pkgname,
    envir = parent.env(environment())
  )
}

# -----------------------------------------------------------------------


#' `acs_census` S3 class
#'
#' A `acs_census` object is created from a given local dir. This dir will contain
#' the geodatabase files that we download.
#'
#' @param dir A string.
#'
#' @return A `acs_census` object.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir = dir)
#'
#' @export
acs_census <- function(dir = "") {
  stopifnot("The dir must exist." = dir.exists(dir))
  structure(list(
    dir = dir,
    dedata = dedata,
    selected_files = NULL
  ),
  class = "acs_census")
}


# -----------------------------------------------------------------------

#' Get area groups
#'
#' Gets the names of the Demographic and Economic Area Groups where data is available.
#'
#' @param ac A `acs_census` object.
#'
#' @return A vector, area group names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' groups <- ac |>
#'   get_area_groups()
#'
#' @export
get_area_groups <- function(ac)
  UseMethod("get_area_groups")

#' @rdname get_area_groups
#' @export
get_area_groups.acs_census<- function(ac) {
  names(ac$dedata$groups)
}


#' Get area names of a group
#'
#' Gets the names of the Demographic and Economic Areas of a group or set of groups.
#'
#' If no group is indicated, all available areas are obtained.
#'
#' @param ac A `acs_census` object.
#' @param group A string, area group name.
#'
#' @return A vector, area names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' areas <- ac |>
#'   get_areas(group = "Statistical Areas")
#'
#' @export
get_areas <- function(ac, group)
  UseMethod("get_areas")

#' @rdname get_areas
#' @export
get_areas.acs_census<- function(ac, group = NULL) {
  group <- validate_names(names(ac$dedata$groups), group, 'group')
  res <- NULL
  for (g in group) {
    cod <- ac$dedata$groups[[g]]
    res <- c(res, names(ac$dedata[[cod]]))
  }
  res
}


#' Get available area years
#'
#' Get the years for which data has been found to be available for an area.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#'
#' @return A vector, area years.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' years <- ac |>
#'   get_area_years(area = "State")
#'
#' @export
get_area_years <- function(ac, area)
  UseMethod("get_area_years")

#' @rdname get_area_years
#' @export
get_area_years.acs_census<- function(ac, area) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$dedata$all_codes), area, 'area')
  cod <- ac$dedata$all_codes[area]
  years <- ac$dedata$years[[cod]]
  last_year <- as.integer(years[length(years)])
  date <- Sys.Date()
  cur_year <- as.integer(substr(date, 1, 4))
  if (cur_year - last_year > 1) {
    for (y in (last_year + 1):(cur_year - 1)) {
      url <- sprintf(ac$dedata$patterns$url, y, y, cod)
      if (url_file_exists(url)) {
        years <- c(years, y)
      }
    }
  }
  years
}


#' Get area file names
#'
#' Get area url file names for the given years. If no year is indicated, all
#' available ones are obtained.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return A vector, file urls.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' url <- ac |>
#'   get_area_file_names("State", 2019:2021)
#'
#' url <- ac |>
#'   get_area_file_names("State")
#'
#' @export
get_area_file_names <- function(ac, area, years)
  UseMethod("get_area_file_names")

#' @rdname get_area_file_names
#' @export
get_area_file_names.acs_census<- function(ac, area, years = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$dedata$all_codes), area, 'area')
  cod <- ac$dedata$all_codes[area]
  years <- validate_names(ac$dedata$years[[cod]], years, 'year')
  res <- NULL
  for (y in years) {
    res <- c(res, sprintf(ac$dedata$patterns$url, y, y, cod))
  }
  res
}


#' Select area files
#'
#' Select area files for the given years. If no year is indicated, all available
#' ones are selected.
#'
#' @param ac A `acs_census` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return  A `acs_census` object.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' ac <- ac |>
#'   select_area_files("State", 2019:2021)
#'
#' ac <- ac |>
#'   select_area_files("State")
#'
#' @export
select_area_files <- function(ac, area, years)
  UseMethod("select_area_files")

#' @rdname select_area_files
#' @export
select_area_files.acs_census<- function(ac, area, years = NULL) {
  ac$selected_files <- get_area_file_names(ac, area, years)

  cod <- ac$dedata$all_codes[area]
  for (y in years) {
    if (y %in% ac$dedata$too_big[[cod]]) {
      f <- sprintf(ac$dedata$patterns$url, y, y, cod)
      cat(
        sprintf(
          "File '%s' cannot be selected, it is too heavy to be downloaded by 'utils::download.file', which is the function we are using.\n",
          f
        )
      )
      ac$selected_files <- setdiff(ac$selected_files, f)
    }
  }
  ac
}

#' Get selected file names
#'
#' Gets the names of the files selected to be downloaded.
#'
#' @param ac A `acs_census` object.
#'
#' @return A vector, file names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' groups <- ac |>
#'   get_selected_file_names()
#'
#' @export
get_selected_file_names <- function(ac)
  UseMethod("get_selected_file_names")

#' @rdname get_selected_file_names
#' @export
get_selected_file_names.acs_census<- function(ac) {
  ac$selected_files
}


#' Get too heavy file names
#'
#' Gets the names of the files that are too heavy to be download with the available
#' function.
#'
#' @param ac A `acs_census` object.
#'
#' @return A vector, too heavy file names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata", package = "geogenr")
#' ac <- acs_census(dir)
#'
#' groups <- ac |>
#'   get_too_heavy_file_names()
#'
#' @export
get_too_heavy_file_names <- function(ac)
  UseMethod("get_too_heavy_file_names")

#' @rdname get_too_heavy_file_names
#' @export
get_too_heavy_file_names.acs_census<- function(ac) {
  res <- NULL
  for (cod in names(ac$dedata$too_big)) {
    for (y in ac$dedata$too_big[[cod]]) {
      res <- c(res, sprintf(ac$dedata$patterns$url, y, y, cod))
    }
  }
  res
}


#' Download selected files
#'
#' Download the files that have been selected, unzip them (if desired) and, if
#' everything went well and is indicated in the parameter, delete the downloaded
#' files.
#'
#' In the `subdir` parameter, the values NULL, 'year' or 'area' can be indicated.
#' With NULL it does not create any subdirs, with 'year' it creates them by years
#' of downloaded files and with 'area' it creates them by areas.
#'
#' @param ac A `acs_census` object.
#' @param subdir NULL/'year'/'area', output subdir.
#' @param unzip A boolean, unzip files.
#' @param delete_zip A boolean, delete zip files if correctly unzipped.
#'
#' @return A vector, files correctly obtained.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- tempdir()
#' ac <- acs_census(dir)
#'
#' ac <- ac |>
#'   select_area_files("State", 2020:2021)
#'
#' \donttest{
#' files <- ac |>
#'   download_selected_files(unzip = FALSE)
#' }
#'
#' @export
download_selected_files <-
  function(ac,
           subdir = NULL,
           unzip = TRUE,
           delete_zip = FALSE) {
    stopifnot("There are no files selected." = length(ac$selected_files) > 0)
    out_dir <- name_with_nexus(ac$dir)
    res_files <- NULL
    for (f in ac$selected_files) {
      year <- get_file_year(f)
      area <- get_file_area(f)
      if (subdir == 'year') {
        dir <- year
      } else if (subdir == 'area') {
        dir <- area
      } else {
        dir <- NULL
      }
      if (!is.null(dir)) {
        dir.create(file.path(mainDir = out_dir, subDir = dir),
                   showWarnings = FALSE)
        dir <- paste0(dir, '/')
      }
      exdir <- paste0(out_dir, dir)
      destfile <- paste0(exdir, basename(f))
      res <- tryCatch(
        utils::download.file(url = f, destfile = destfile),
        error = function(e)
          1
      )
      if (res != 1) {
        if (unzip) {
          res <- tryCatch(
            utils::unzip(destfile, exdir = exdir),
            error = function(e)
              1
          )
          if (res[1] != 1) {
            res_files <- c(res_files, gsub('.zip', '', destfile))
            if (delete_zip) {
              unlink(destfile)
            }
          }
        } else {
          res_files <- c(res_files, destfile)
        }
      }
    }
    res_files
  }


#' Unzip files
#'
#' Unzip files that are not already unzipped in the object and, if everything
#' went well and is indicated in the parameter, delete the zipped files.
#'
#' In the `subdir` parameter, the values NULL, 'year' or 'area' can be indicated.
#' With NULL it does not create any subdirs, with 'year' it creates them by years
#' of files and with 'area' it creates them by areas.
#'
#' @param subdir NULL/'year'/'area', output subdir.
#' @param delete_zip A boolean, delete zip files if correctly unzipped.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- tempdir()
#' ac <- acs_census(dir)
#'
#' ac <- ac |>
#'   select_area_files("State", 2020:2021)
#'
#' \donttest{
#' files <- ac |>
#'   download_selected_files(unzip = FALSE)
#' }
#' files <- ac |>
#'   unzip_files()
#'
#' @export
unzip_files <- function(ac, subdir = NULL, delete_zip = FALSE) {
  file <-
    list.files(
      path = ac$dir,
      pattern = "*.zip",
      recursive = TRUE,
      ignore.case = TRUE,
      full.names = TRUE
    )
  file_name <- basename(file)
  path_name <- dirname(file)
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
  file_name <- substr(file_name, 1, n - 4)
  out_dir <- name_with_nexus(path_name)
  res <- NULL
  for (i in 1:length(extension)) {
    f <- file[i]
    if (subdir == 'year') {
      dir <- get_file_year(f)
    } else if (subdir == 'area') {
      dir <- get_file_area(f)
    } else {
      dir <- NULL
    }
    if (!is.null(dir)) {
      dir.create(file.path(mainDir = out_dir, subDir = dir), showWarnings = FALSE)
      dir <- paste0(dir, '/')
    }
    exdir <- paste0(out_dir, dir)
    dir_res <- paste0(exdir, file_name)
    if (!dir.exists(dir_res)) {
      utils::unzip(f, exdir = exdir)
      res <- c(res, dir_res)
      if (delete_zip) {
        unlink(f)
      }
    }
  }
  res
}


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


#-------------------------------------------------------------------------------

#' Validate names
#'
#' @param defined_names A vector of strings, defined attribute names.
#' @param names A vector of strings, new attribute names.
#' @param concept A string, treated concept.
#' @param repeated A boolean, repeated names allowed.
#'
#' @return A vector of strings, names.
#'
#' @keywords internal
validate_names <- function(defined_names, names, concept = 'name', repeated = FALSE) {
  if (is.null(names)) {
    names <- defined_names
  } else {
    if (!repeated) {
      stopifnot("There are repeated values." = length(names) == length(unique(names)))
    }
    for (name in names) {
      if (!(name %in% defined_names)) {
        stop(sprintf(
          "'%s' is not defined as %s.",
          name, concept
        ))
      }
    }
  }
  names
}

#' Name with nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/". Add the nexus.
#'
#' @param name A string.
#'
#' @return A string.
#'
#' @keywords internal
name_with_nexus <- function(name) {
  l <- nchar(name)
  c <- substr(name, start = l, stop = l)
  if (c == "/") {
    res <- name
  } else {
    res <- paste0(name, "/")
  }
  res
}


#' Get gbd files
#'
#' Given a dir, we get the gbd files in it, at any level.
#'
#' @param name A string.
#'
#' @return A string vector.
#'
#' @keywords internal
get_gbd_files <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  files <- files[dir.exists(files)]
  n <- nchar(files)
  files <- files[substr(files, n-3, n) == '.gdb']
  files
}

#' Get file year
#'
#' Given a file, get the associated year (in the name).
#'
#' @param name A string vector.
#'
#' @return A string vector.
#'
#' @keywords internal
get_file_year <- function(file) {
  name <- basename(file)
  year <- readr::parse_number(name)
  year
}

#' Get file area (with year)
#'
#' Given a file, get the associated code (in the name), includes the year as the
#' name of the vector elements..
#'
#' @param name A string vector.
#'
#' @return A string vector.
#'
#' @keywords internal
get_file_area <- function(file) {
  name <- basename(file)
  year <- readr::parse_number(name)
  pre <- paste0("ACS_", year, "_5YR_")
  for (i in seq_along(name)) {
    name[i] <- sub(pre[i], "", name[i])
    name[i] <- sub('.gdb', "", name[i])
  }
  names(name) <- year
  name
}


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


#' Name to title
#'
#' @param name A string.
#'
#' @return A string
#'
#' @keywords internal
name_to_title <- function(name) {
  stringr::str_trim(stringr::str_to_title(string = gsub('_', ' ', name), locale = "en"))
}
