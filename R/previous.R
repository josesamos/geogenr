.onLoad <- function(libname, pkgname) {
  utils::data(
    "dedata",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' Get area groups
#'
#' Gets the names of the Demographic and Economic area groups where data is available.
#'
#' @return A vector.
#'
#' @family data selection functions
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
#' Gets the names of the Demographic and Economic areas of a group or set of groups.
#'
#' If no group is indicated, all available areas are obtained.
#'
#' @return A vector.
#'
#' @family data selection functions
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
#' @return A vector.
#'
#' @family data selection functions
#'
#' @examples
#'
#' groups <- get_area_years()
#'
#' @export
get_area_years <- function(area = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  area <- validate_names(names(dedata$all_codes), area, 'area')
  cod <- dedata$all_codes[area]
  dedata$years[[cod]]
}


#' Get area file names
#'
#' Get area file names for the given years. If no year is indicated, all available
#' ones are obtained.
#'
#' @return A vector.
#'
#' @family data selection functions
#'
#' @examples
#'
#' groups <- get_area_years()
#'
#' @export
get_area_file_names <- function(area = NULL, years = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
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
#' Download the files whose url is indicated, unzip them and, if everything went
#' well and is indicated in the parameter, delete the downloaded files.
#'
#' @return A vector, files correctly obtained.
#'
#' @family data selection functions
#'
#' @examples
#'
#' groups <- get_area_years()
#'
#' @export
download_area_file <- function(file = NULL, out_dir = NULL, delete_zip = FALSE) {
  stopifnot("'file' must be defined." = !is.null(file))
  stopifnot("'out_dir' must be defined." = !is.null(out_dir))
  out_dir <- name_with_nexus(out_dir)
  to <- getOption('timeout')
  options(timeout = to * 100)
  res_files <- NULL
  for (f in file) {
    destfile <- paste0(out_dir, basename(f))
    res <- tryCatch(
      utils::download.file(url = f, destfile = destfile),
      error = function(e)
        1
    )
    if (res != 1) {
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
    }
  }
  options(timeout = to)
  res_files
}


#' Unzip area file
#'
#' Given a vector of compressed file names or the name of a folder containing
#' compressed files, unzip the files to the given output folder.
#'
#' @param file A string or string vector.
#' @param out_dir A string or string vector, output folder.
#' @param only_show_files A boolean, only show the files that would be unzipped,
#' and the destination folders, not unzip them.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family data selection functions
#'
#' @examples
#'
#' f <- system.file("extdata", package = "satres")
#' r <- sat_untarzip(f, only_show_files = TRUE)
#'
#' f1 <- system.file("extdata", "satres.zip", package = "satres")
#' f2 <- system.file("extdata", "satres.tar", package = "satres")
#' r <- sat_untarzip(c(f1, f2), only_show_files = TRUE)
#'
#' @export
unzip_area_file <- function(file, out_dir = NULL, only_show_files = FALSE) {
  stopifnot("'file' must be defined." = !is.null(file))
  stopifnot("'out_dir' must be defined." = !is.null(out_dir))
  if (length(file) == 1) {
    if (dir.exists(file)) {
      file <-
        list.files(
          path = file,
          pattern = "*.zip",
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
      res <- c(res, file[i])
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
