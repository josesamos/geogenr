.onLoad <- function(libname, pkgname) {
  utils::data(
    "dedata",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' Get area groups
#'
#' Gets the names of the Demographic and Economic Area Groups where data is available.
#'
#' @return A vector, area group names.
#'
#' @family data download functions
#'
#' @examples
#'
#' groups <- get_area_groups()
#'
#' @export
get_area_groups <- function() {
  names(dedata$groups)
}

#' Get area names of a group
#'
#' Gets the names of the Demographic and Economic Areas of a group or set of groups.
#'
#' If no group is indicated, all available areas are obtained.
#'
#' @param group A string, area group name.
#'
#' @return A vector, area names.
#'
#' @family data download functions
#'
#' @examples
#'
#' areas <- get_areas(group = "Statistical Areas")
#'
#' @export
get_areas <- function(group = NULL) {
  group <- validate_names(names(dedata$groups), group, 'group')
  res <- NULL
  for (g in group) {
    cod <- dedata$groups[[g]]
    res <- c(res, names(dedata[[cod]]))
  }
  res
}


#' Get available area years
#'
#' Get the years for which data has been found to be available for an area.
#'
#' @param area A string, area name.
#'
#' @return A vector, area years.
#'
#' @family data download functions
#'
#' @examples
#'
#' years <- get_area_years("State")
#'
#' @export
get_area_years <- function(area = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(dedata$all_codes), area, 'area')
  cod <- dedata$all_codes[area]
  dedata$years[[cod]]
}


#' Get area file names
#'
#' Get area url file names for the given years. If no year is indicated, all
#' available ones are obtained.
#'
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return A vector, file urls.
#'
#' @family data download functions
#'
#' @examples
#'
#' url <- get_area_file_names("State", 2019:2021)
#'
#' url <- get_area_file_names("State")
#'
#' @export
get_area_file_names <- function(area = NULL, years = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(dedata$all_codes), area, 'area')
  cod <- dedata$all_codes[area]
  if (is.null(years)) {
    years <- dedata$years[[cod]]
  }
  res <- NULL
  for (y in years) {
    res <- c(res, sprintf(dedata$url_pattern, y, y, cod))
  }
  res
}


#' Download area file
#'
#' Download the files whose url is indicated, unzip them (if desired) and, if
#' everything went well and is indicated in the parameter, delete the downloaded
#' files.
#'
#' @param file A string or string vector.
#' @param out_dir A string, output folder.
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
#' url <- get_area_file_names("State", 2021)
#'
#' url <-
#'   paste0(
#'     'file://',
#'     system.file("extdata/ACS_2014_5YR_NECTADIV.gdb.zip", package = "geogenr")
#'   )
#'
#' files <- download_area_file(url, dir, unzip = FALSE)
#'
#' files <- download_area_file(url, dir, delete_zip = TRUE)
#'
#' @export
download_area_file <- function(file = NULL, out_dir = NULL, unzip = TRUE, delete_zip = FALSE) {
  stopifnot("'file' must be defined." = !is.null(file))
  stopifnot("'out_dir' must be defined." = !is.null(out_dir))
  out_dir <- name_with_nexus(out_dir)
  res_files <- NULL
  for (f in file) {
    destfile <- paste0(out_dir, basename(f))
    res <- tryCatch(
      utils::download.file(url = f, destfile = destfile),
      error = function(e)
        1
    )
    if (res != 1) {
      if (unzip) {
        res <- tryCatch(
          utils::unzip(destfile, exdir = out_dir),
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


#' Unzip area file
#'
#' Given a vector of compressed file names or the name of a folder containing
#' compressed files, unzip the files to the given output folder.
#'
#' @param file A string or string vector.
#' @param out_dir A string or string vector, output folder.
#' @param delete_zip A boolean, delete zip files if correctly unzipped.
#' @param only_show_files A boolean, only show the files that would be unzipped,
#' and the destination folders, not unzip them.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- tempdir()
#' url <- get_area_file_names("State", 2021)
#'
#' url <-
#'   paste0(
#'     'file://',
#'     system.file("extdata/ACS_2014_5YR_NECTADIV.gdb.zip", package = "geogenr")
#'   )
#'
#' files <- download_area_file(url, dir, unzip = FALSE)
#'
#' res <- unzip_area_file(files, tempdir())
#'
#' @export
unzip_area_file <- function(file, out_dir = NULL, delete_zip = FALSE, only_show_files = FALSE) {
  stopifnot("'file' must be defined." = !is.null(file))
  stopifnot("'out_dir' must be defined." = !is.null(out_dir))
  if (length(file) == 1) {
    if (dir.exists(file)) {
      file <-
        list.files(
          path = file,
          pattern = "*.zip",
          recursive = TRUE,
          ignore.case = TRUE,
          full.names = TRUE
        )
    }
  }
  file_name <- basename(file)
  path_name <- dirname(file)
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
  if (is.null(out_dir)) {
    out_dir <- path_name
  }
  res <- NULL
  for (i in 1:length(extension)) {
    if (extension[i] == ".zip" | extension[i] == ".ZIP") {
      if (!only_show_files) {
        utils::unzip(file[i], exdir = out_dir)
      }
    } else {
      stop(sprintf("Unsupported file type: %s", extension[i]))
    }
    if (!only_show_files) {
      res <- c(res, gsub('.zip', '', file[i]))
    } else {
      res <- c(res, sprintf("%s to %s", file[i], out_dir))
    }
  }
  res
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
#' Given a folder, we get the gbd files in it, at any level.
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

#' Get file code (with year)
#'
#' Given a file, get the associated code (in the name), includes the year as the
#' name of the vector elements..
#'
#' @param name A string vector.
#'
#' @return A string vector.
#'
#' @keywords internal
get_file_code <- function(file) {
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

