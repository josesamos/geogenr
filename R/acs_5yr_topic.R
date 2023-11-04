# act <- structure(list(
#   area = cod,
#   years = years,
#   topic = topic_name,
#   area_topics = res,
#   files = files
# ),
# class = "acs_5yr_topic")


#' Get geographical attributes
#'
#' Get the names of the geographic layer attributes (except for the geometry field).
#'
#' @param act A `acs_5yr_topic` object.
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
#'   get_geo_attribute_names()
#'
#' @export
get_geo_attribute_names <- function(act)
  UseMethod("get_geo_attribute_names")

#' @rdname get_geo_attribute_names
#' @export
get_geo_attribute_names.acs_5yr_topic<- function(act) {
  names <- names(act$geo)
  names <- names[-length(names)]
}


#' Get geographic layer
#'
#' Get the geographic layer.
#'
#' @param glc A `acs_5yr_topic` or `acs_5yr_geo` object.
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
#'   get_geo_layer()
#'
#' @export
get_geo_layer <- function(glc)
  UseMethod("get_geo_layer")

#' @rdname get_geo_layer
#' @export
get_geo_layer.acs_5yr_topic <- function(glc) {
  glc$data
}


#' Get topic name (report groups)
#'
#' Get the selected topic.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A vector, topic name.
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
#' topic <- act |>
#'   get_topic_name()
#'
#' @export
get_topic_name <- function(act)
  UseMethod("get_topic_name")

#' @rdname get_topic_name
#' @export
get_topic_name.acs_5yr_topic<- function(act) {
  names(act$topic)
}


#' Get names of other topics (report groups)
#'
#' Get the rest of the available topics with the one that is selected.
#'
#' @param act A `acs_5yr_topic` object.
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
#' files <- ac |>
#'   unzip_files()
#'
#' act <- ac |>
#'   as_acs_5yr_topic("Alaska Native Regional Corporation",
#'                    topic = "X01 Age And Sex")
#'
#' topics <- act |>
#'   get_names_of_other_topics()
#'
#' @export
get_names_of_other_topics <- function(act)
  UseMethod("get_names_of_other_topics")

#' @rdname get_names_of_other_topics
#' @export
get_names_of_other_topics.acs_5yr_topic<- function(act) {
  sort(names(act$area_topics[!(act$area_topics %in% act$topic)]))
}


#' Select topic (report group)
#'
#' Select the given topic with. If no topic is given, the first one that appears
#' in the files is taken.
#'
#' @param act A `acs_5yr_topic` object.
#' @param topic A string, topic name.
#'
#' @return A `acs_5yr_topic` object.
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
#' act <- act |>
#'   select_topic(topic = "X03 Hispanic Or Latino Origin")
#'
#' @export
select_topic <- function(act, topic)
  UseMethod("select_topic")

#' @rdname select_topic
#' @export
select_topic.acs_5yr_topic<- function(act, topic = NULL) {
  if (is.null(topic)) {
    topic <- names(act$area_topics[1])
  } else {
    topic <- validate_names(names(act$area_topics), topic, 'topic')
  }
  topic_name <- act$area_topics[topic]
  act$topic <- topic_name
  get_topic_data(act)
}




#' Get report names
#'
#' Get the names of the reports. The report code is included with the name. Each
#' report can contain multiple subreports.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A vector, report names.
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
#' reports <- act |>
#'   get_report_names()
#'
#' @export
get_report_names <- function(act)
  UseMethod("get_report_names")

#' @rdname get_report_names
#' @export
get_report_names.acs_5yr_topic<- function(act) {
  r <- act$data[act$data$subreport == '-', c('report', "subreport", "report_desc")]
  report <- sort(unique(paste0(r$report, r$subreport, r$report_desc)))
}


#' Get
#'
#' Get the selected topic.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A vector, topic name.
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
#' reports <- act |>
#'   get_subreport_names()
#'
#' @export
get_subreport_names <- function(act, report)
  UseMethod("get_subreport_names")

#' @rdname get_subreport_names
#' @export
get_subreport_names.acs_5yr_topic<- function(act, report = NULL) {
  if (is.null(report)) {
    r <- act$data[ , c('report', "subreport", "report_desc")]
  } else {
    report <- substr(report, 1, 6)
    r <- act$data[act$data$report %in% report, c('report', "subreport", "report_desc")]
  }
  subreport <- sort(unique(paste0(r$report, '-', r$subreport, '-', r$report_desc)))
}



#' Select report
#'
#' Get the selected topic.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A vector, topic name.
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
#' reports <- act |>
#'   select_report()
#'
#' @export
select_report <- function(act, report)
  UseMethod("select_report")

#' @rdname select_report
#' @export
select_report.acs_5yr_topic<- function(act, report = NULL) {
  stopifnot("The report must be defined." = !is.null(report))
  report <- substr(report, 1, 6)
  act$data <- act$data[act$data$report %in% report, ]
}



#' Get
#'
#' Get the selected topic.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A vector, topic name.
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
#' reports <- act |>
#'   select_subreport()
#'
#' @export
select_subreport <- function(act, subreport)
  UseMethod("select_subreport")

#' @rdname select_subreport
#' @export
select_subreport.acs_5yr_topic<- function(act, subreport = NULL) {
  stopifnot("The subreport must be defined." = !is.null(subreport))
  subreport <- substr(subreport, 1, 8)
  sr <- paste0(act$data$report, '-', act$data$subreport)
  act$data <- act$data[sr %in% subreport, ]
}


#-------------------------------------------------------------------------------


#' Transform metadata layer
#'
#' @param metadata A vector
#'
#' @return A vector
#'
#' @keywords internal
transform_metadata_rest <- function(metadata) {
  metadata <- metadata |>
    dplyr::group_by_at(dplyr::vars(tidyselect::all_of(names(metadata)))) |>
    dplyr::summarise(.groups = "drop")
  metadata$report_var <- as.integer(readr::parse_number(substr(metadata$Short_Name, 7, 12)))

  var_names <- names(metadata)
  i <- grep('report_var', var_names, fixed = TRUE)
  var_names <- var_names[-i]
  i <- grep('subreport', var_names, fixed = TRUE)
  var_names <- c(var_names[1:i], 'report_var', var_names[(i + 1):length(var_names)])
  metadata <- metadata[, var_names]

  fn <- unique(metadata$Full_Name)
  fn2 <- name_to_title(fn)
  fn_parts <- strsplit(fn2, ": ")
  fn_length <- unlist(lapply(fn_parts, length))
  n_parts <- max(fn_length)
  min_parts <- min(fn_length)
  # all with the same length
  if (min_parts < n_parts) {
    for (n in min_parts:(n_parts - 1)) {
      fn_parts[fn_length == n] <-
        lapply(fn_parts[fn_length == n], append, values = "Total", after = n - 1)
      fn_length[fn_length == n] <- n + 1
    }
  }

  fn_parts <- unlist(fn_parts)
  fn_parts <- stringr::str_trim(fn_parts)
  full_name <- data.frame(matrix(fn_parts, ncol = n_parts, byrow = TRUE))
  colnames(full_name) <- c('report_group', paste0('item', 1:(n_parts - 2)), 'group')
  full_name$Full_Name <- fn
  full_name <- tibble::as_tibble(full_name)
  full_name <- full_name[, -1]

  metadata <- metadata |>
    dplyr::inner_join(full_name, by = "Full_Name")

  v <- unique(metadata$subreport)
  if (length(v) == 1) {
    metadata$subreport <- NULL
  }
  metadata
}


#' Transform metadata layer
#'
#' @param metadata A vector
#'
#' @return A vector
#'
#' @keywords internal
transform_metadata_basic <- function(metadata) {
  metadata <- tibble::as_tibble(metadata)

  metadata$Full_Name <- name_to_title(metadata$Full_Name)
  metadata$Full_Name <- gsub("\\s+", " ", metadata$Full_Name)

  metadata <- metadata |>
    dplyr::mutate(measure = "estimate")

  metadata$measure[lapply(metadata[, 'Full_Name'],
                          grepl,
                          pattern = ' -- (Margin Of Error)',
                          fixed = TRUE)[[1]]] <- 'margin_of_error'

  metadata[, 'Full_Name'] <- lapply(
    metadata[, 'Full_Name'],
    gsub,
    pattern = ' -- (Estimate)',
    replacement = '',
    fixed = TRUE
  )

  metadata[, 'Full_Name'] <- lapply(
    metadata[, 'Full_Name'],
    gsub,
    pattern = ' -- (Margin Of Error)',
    replacement = '',
    fixed = TRUE
  )

  metadata$report <- substr(metadata$Short_Name, 1, 6)
  metadata$subreport <- substr(metadata$Short_Name, 7, 7)
  metadata$subreport[is.na(metadata$subreport)] <- '-'
  metadata$subreport[metadata$subreport == 'e' | metadata$subreport == 'm'] <- '-'

  pos <- regexpr(":", metadata$Full_Name)
  metadata$report_desc <- substr(metadata$Full_Name, 1, pos - 1)
  metadata$report_desc <- stringr::str_trim(metadata$report_desc)

  metadata
}


#' Transform layer according to metadata
#'
#' @param layer A `tibble`, layer data.
#' @param metadata A `tibble`, layer metadata.
#'
#' @return A vector
#'
#' @keywords internal
transform_layer <- function(layer, metadata) {
  layer <- tibble::as_tibble(layer)
  layer <- tidyr::gather(layer, "Short_Name", "value", 2:length(names(layer)))
  layer$value <- as.character(layer$value)

  layer <- dplyr::inner_join(layer, metadata, by = "Short_Name")

  layer[, 'Short_Name'] <- lapply(
    layer[, 'Short_Name'],
    gsub,
    pattern = 'e',
    replacement = '_',
    fixed = TRUE
  )

  layer[, 'Short_Name'] <- lapply(
    layer[, 'Short_Name'],
    gsub,
    pattern = 'm',
    replacement = '_',
    fixed = TRUE
  )

  layer <- layer |>
    tidyr::spread(tidyselect::all_of("measure"), tidyselect::all_of("value"))

  layer[stats::complete.cases(layer), ]
}


#' Get layer data
#'
#' @param layer A string, layer name.
#' @param file A string, file name.
#'
#' @return A `tibble`, layer data.
#'
#' @keywords internal
get_layer_data <- function(layer, file) {
  layers <- sf::st_layers(file)
  layers <- layers$name
  rest <- layers[substr(layers, 1, 1) != 'X']
  meta_name <- rest[grepl('METADATA', rest, fixed = TRUE)]

  metadata <- sf::st_read(file, layer = meta_name, quiet = TRUE)
  metadata <- transform_metadata_basic(metadata)

  sel_layer <- sf::st_read(file, layer = layer, quiet = TRUE)
  sel_layer <- transform_layer(layer = sel_layer, metadata)

  tibble::add_column(sel_layer, topic = name_to_title(layer), .before = 1)

  year <- as.character(get_file_year(file))
  tibble::add_column(sel_layer, year = year, .before = 1)
}


#' Get geo layer
#'
#' @param file A string, file name.
#'
#' @return A `st`, geo data.
#'
#' @keywords internal
get_geo_layer_from_file <- function(file) {
  layers <- sf::st_layers(file)
  layers <- layers$name
  rest <- layers[substr(layers, 1, 1) != 'X']
  geo_name <- rest[!grepl('METADATA', rest, fixed = TRUE)]

  sf::st_read(file, layer = geo_name, quiet = TRUE)
}


#' Select topic (report group)
#'
#' Select the given topic with. If no topic is given, the first one that appears
#' in the files is taken.
#'
#' @param act A `acs_5yr_topic` object.
#'
#' @return A `acs_5yr_topic` object.
#'
#' @keywords internal
get_topic_data <-  function(act) {
  for (i in seq_along(act$files)) {
    for (t in act$topic) {
      if (is.null(act$data)) {
        act$data <- get_layer_data(layer = t, file = act$files[i])
      } else {
        act$data <- rbind(act$data, get_layer_data(layer = t, file = act$files[i]))
      }
    }
  }
  sel <- max(names(act$files))
  act$geo <- get_geo_layer_from_file(file = act$files[sel])

  act
}

