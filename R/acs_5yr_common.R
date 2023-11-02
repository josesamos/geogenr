
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
  res <- name
  for (i in seq_along(c)) {
    if (c[i] != "/") {
      res[i] <- paste0(name[i], "/")
    }
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
    name[i] <- sub('.zip', "", name[i])
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
