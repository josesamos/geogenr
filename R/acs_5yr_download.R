.onLoad <- function(libname, pkgname) {
  utils::data(
    "acs_5yr_md",
    package = pkgname,
    envir = parent.env(environment())
  )
}

# -----------------------------------------------------------------------


#' `acs_5yr` S3 class
#'
#' An `acs_5yr` object is created from a given local dir. This dir will contain
#' the geodatabase files that we download.
#'
#' @param dir A string.
#'
#' @return An `acs_5yr` object.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir = dir)
#'
#' @export
acs_5yr <- function(dir = "") {
  stopifnot("The dir must exist." = dir.exists(dir))
  structure(list(
    dir = dir,
    acs_5yr_md = acs_5yr_md,
    selected_files = NULL
  ),
  class = "acs_5yr")
}


# -----------------------------------------------------------------------

#' Get area groups
#'
#' Gets the names of the Demographic and Economic Area Groups where data is available.
#'
#' @param ac An `acs_5yr` object.
#'
#' @return A vector, area group names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' groups <- ac |>
#'   get_area_groups()
#'
#' @export
get_area_groups <- function(ac)
  UseMethod("get_area_groups")

#' @rdname get_area_groups
#' @export
get_area_groups.acs_5yr<- function(ac) {
  names(ac$acs_5yr_md$groups)
}


#' Get area names of a group
#'
#' Gets the names of the Demographic and Economic Areas of a group or set of groups.
#'
#' If no group is indicated, all available areas are obtained.
#'
#' @param ac An `acs_5yr` object.
#' @param group A string, area group name.
#'
#' @return A vector, area names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' areas <- ac |>
#'   get_areas(group = "Statistical Areas")
#'
#' @export
get_areas <- function(ac, group)
  UseMethod("get_areas")

#' @rdname get_areas
#' @export
get_areas.acs_5yr<- function(ac, group = NULL) {
  group <- validate_names(names(ac$acs_5yr_md$groups), group, 'group')
  res <- NULL
  for (g in group) {
    cod <- ac$acs_5yr_md$groups[[g]]
    res <- c(res, names(ac$acs_5yr_md[[cod]]))
  }
  res
}


#' Get available area years
#'
#' Get the years for which data has been found to be available for an area.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#'
#' @return A vector, area years.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' years <- ac |>
#'   get_area_years(area = "State")
#'
#' @export
get_area_years <- function(ac, area)
  UseMethod("get_area_years")

#' @rdname get_area_years
#' @export
get_area_years.acs_5yr<- function(ac, area) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$acs_5yr_md$all_codes), area, 'area')
  cod <- ac$acs_5yr_md$all_codes[area]
  years <- ac$acs_5yr_md$years[[cod]]
  last_year <- as.integer(years[length(years)])
  date <- Sys.Date()
  cur_year <- as.integer(substr(date, 1, 4))
  if (cur_year - last_year > 1) {
    for (y in (last_year + 1):(cur_year - 1)) {
      url <- sprintf(ac$acs_5yr_md$patterns$url, y, y, cod)
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
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return A vector, file urls.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
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
get_area_file_names.acs_5yr<- function(ac, area, years = NULL) {
  stopifnot("The area name must be defined." = !is.null(area))
  stopifnot("We can only select one area." = length(area) == 1)
  area <- validate_names(names(ac$acs_5yr_md$all_codes), area, 'area')
  cod <- ac$acs_5yr_md$all_codes[area]
  years <- validate_names(ac$acs_5yr_md$years[[cod]], years, 'year')
  res <- NULL
  for (y in years) {
    res <- c(res, sprintf(ac$acs_5yr_md$patterns$url, y, y, cod))
  }
  res
}


#' Select area files
#'
#' Select area files for the given years. If no year is indicated, all available
#' ones are selected.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#' @param years A vector, year number.
#'
#' @return An `acs_5yr` object.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
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
select_area_files.acs_5yr<- function(ac, area, years = NULL) {
  ac$selected_files <- get_area_file_names(ac, area, years)

  cod <- ac$acs_5yr_md$all_codes[area]
  for (y in years) {
    if (y %in% ac$acs_5yr_md$too_big[[cod]]) {
      f <- sprintf(ac$acs_5yr_md$patterns$url, y, y, cod)
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
#' @param ac An `acs_5yr` object.
#'
#' @return A vector, file names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' groups <- ac |>
#'   get_selected_file_names()
#'
#' @export
get_selected_file_names <- function(ac)
  UseMethod("get_selected_file_names")

#' @rdname get_selected_file_names
#' @export
get_selected_file_names.acs_5yr<- function(ac) {
  ac$selected_files
}


#' Get too heavy file names
#'
#' Gets the names of the files that are too heavy to be download with the available
#' function. We have downloaded them directly with the web browser.
#'
#' @param ac An `acs_5yr` object.
#'
#' @return A vector, too heavy file names.
#'
#' @family data download functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' groups <- ac |>
#'   get_too_heavy_file_names()
#'
#' @export
get_too_heavy_file_names <- function(ac)
  UseMethod("get_too_heavy_file_names")

#' @rdname get_too_heavy_file_names
#' @export
get_too_heavy_file_names.acs_5yr<- function(ac) {
  res <- NULL
  for (cod in names(ac$acs_5yr_md$too_big)) {
    for (y in ac$acs_5yr_md$too_big[[cod]]) {
      res <- c(res, sprintf(ac$acs_5yr_md$patterns$url, y, y, cod))
    }
  }
  res
}


#' Download selected files
#'
#' Download the files that have been selected and have not been downloaded yet,
#' unzip them (if desired) and, if everything went well and is indicated in the
#' parameter, delete the downloaded files.
#'
#' In the `subdir` parameter, the values NULL, 'year' or 'area' can be indicated.
#' With NULL it does not create any subdirs, with 'year' it creates them by years
#' of downloaded files and with 'area' it creates them by areas.
#'
#' @param ac An `acs_5yr` object.
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
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' ac <- ac |>
#'   select_area_files("Alaska Native Regional Corporation", 2020:2021)
#'
#' files <- ac |>
#'   download_selected_files(unzip = FALSE)
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
      if (is.null(subdir)) {
        dir <- NULL
      } else if (subdir == 'year') {
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
      if (!file.exists(destfile)) {
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
    }
    res_files
  }


#' Unzip files
#'
#' Unzip files that are not already unzipped in the object and, if everything
#' went well and is indicated in the parameter, delete the unzipped files.
#'
#' In the `subdir` parameter, the values NULL, 'year' or 'area' can be indicated.
#' With NULL it does not create any subdirs, with 'year' it creates them by years
#' of files and with 'area' it creates them by areas.
#'
#' @param ac An `acs_5yr` object.
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
#' source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' files <- list.files(source_dir, "*.zip", full.names = TRUE)
#' file.copy(from = files, to = dir, overwrite = TRUE)
#' ac <- acs_5yr(dir)
#'
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
    if (is.null(subdir)) {
      dir <- NULL
    } else if (subdir == 'year') {
      dir <- get_file_year(f)
    } else if (subdir == 'area') {
      dir <- get_file_area(f)
    } else {
      dir <- NULL
    }
    if (!is.null(dir)) {
      dir.create(file.path(mainDir = out_dir[i], subDir = dir), showWarnings = FALSE)
      dir <- paste0(dir, '/')
    }
    exdir <- paste0(out_dir[i], dir)
    dir_res <- paste0(exdir, file_name[i])
    if (!dir.exists(dir_res)) {
      r <- tryCatch(
        utils::unzip(f, exdir = exdir),
        error = function(e)
          1
      )
      if (r[1] != 1) {
        res <- c(res, dir_res)
        if (delete_zip) {
          unlink(f)
        }
      }
    }
  }
  res
}


#' Get code from area name
#'
#' Obtain the code that appears in the name of the file associated with the area.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#'
#' @return A vector, area code.
#'
#' @family information functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' code <- ac |>
#'   get_code_from_area_name(area = "State")
#'
#' @export
get_code_from_area_name <- function(ac, area)
  UseMethod("get_code_from_area_name")

#' @rdname get_code_from_area_name
#' @export
get_code_from_area_name.acs_5yr<- function(ac, area) {
  stopifnot("The area name must be defined." = !is.null(area))
  area <- validate_names(names(ac$acs_5yr_md$all_codes), area, 'area')
  cod <- ac$acs_5yr_md$all_codes[area]
}


#' Get name from area code
#'
#' Get the name of the area from the code that appears in the name of the area
#' files.
#'
#' @param ac An `acs_5yr` object.
#' @param area A string, area name.
#'
#' @return A vector, area code.
#'
#' @family information functions
#'
#' @examples
#'
#' dir <- system.file("extdata/acs_5yr", package = "geogenr")
#' ac <- acs_5yr(dir)
#'
#' name <- ac |>
#'   get_name_from_area_code(area = "METDIV")
#'
#' @export
get_name_from_area_code <- function(ac, area)
  UseMethod("get_name_from_area_code")

#' @rdname get_name_from_area_code
#' @export
get_name_from_area_code.acs_5yr<- function(ac, area) {
  stopifnot("The area code must be defined." = !is.null(area))
  area <- validate_names(ac$acs_5yr_md$all_codes, area, 'area')
  name <- ac$acs_5yr_md$all_names[area]
}
