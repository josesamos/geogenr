# structure(list(
#   area = cod,
#   years = years,
#   topic = topic_name,
#   area_topics = topics,
#   files = files,
#   data = list()
# ),
# class = "acs_5yr_topic")


#' Get other topics (report groups)
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
#'   get_other_topics()
#'
#' @export
get_other_topics <- function(act)
  UseMethod("get_other_topics")

#' @rdname get_other_topics
#' @export
get_other_topics.acs_5yr_topic<- function(act) {
  sort(names(act$area_topics[act$area_topics != act$topic]))
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


#-------------------------------------------------------------------------------

#' Delete last parenthesis of a string (vectorial)
#'
#' @param name A string.
#'
#' @return A string
#'
#' @keywords internal
delete_last_parenthesis <- function(name) {
  name <- stringr::str_trim(name)
  n <- nchar(name)
  lc <- substr(name, n, n)
  for (i in seq_along(lc)) {
    if (lc[i] == ')') {
      g <- regexpr("\\([^\\(]*$", name[i])
      if (g[1] > 0) {
        name[i] <- substr(name[i], 1, g[1] - 1)
      }
    }
  }
  stringr::str_trim(name)
}


#' Transform metadata layer
#'
#' @param metadata A vector
#'
#' @return A vector
#'
#' @keywords internal
transform_metadata <- function(metadata) {
  metadata <- tibble::as_tibble(metadata)

  metadata <- metadata |>
    dplyr::mutate(measure = "estimate")

  metadata$measure[lapply(metadata[, 'Full_Name'],
                          grepl,
                          pattern = ' -- (Margin of Error)',
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
    pattern = ' -- (Margin of Error)',
    replacement = '',
    fixed = TRUE
  )

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

  metadata <- metadata |>
    dplyr::inner_join(full_name, by = "Full_Name")

  metadata$report <- substr(metadata$Short_Name, 1, 6)
  # metadata$report_desc <- delete_last_parenthesis(metadata$report_group)
  metadata
}


#' Transform layer according to metada
#'
#' @param layer
#' @param sel_layer
#' @param metadata
#'
#' @return A vector
#'
#' @keywords internal
transform_layer <- function(layer, sel_layer, metadata) {
  sel_layer <- tidyr::gather(sel_layer, "Short_Name", "value", 2:length(names(sel_layer)))
  sel_layer$value <- as.character(sel_layer$value)

  variables <- sel_layer |>
    dplyr::select(Short_Name) |>
    dplyr::group_by(Short_Name) |>
    dplyr::summarise() |>
    dplyr::inner_join(metadata, by = "Short_Name")

  variables$topic <- name_to_title(layer)
  var_names <- names(variables)
  items <- var_names[grep('item', var_names)]
  res <- NULL
  for (i in items) {
    val <- unique(variables[, i])
    if (nrow(val) == 1) {
      if (val[[1]] == "Total") {
        res <- c(res, i)
      }
    }
  }
  variables <- variables[ , setdiff(var_names, res)]

  sel_layer <- dplyr::inner_join(variables, sel_layer, by = "Short_Name")

  sel_layer[, 'Short_Name'] <- lapply(
    sel_layer[, 'Short_Name'],
    gsub,
    pattern = 'e',
    replacement = '_',
    fixed = TRUE
  )

  sel_layer[, 'Short_Name'] <- lapply(
    sel_layer[, 'Short_Name'],
    gsub,
    pattern = 'm',
    replacement = '_',
    fixed = TRUE
  )

  sel_layer <- sel_layer |>
    tidyr::spread(measure, value)

  sel_layer
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
  metadata <- transform_metadata(metadata)

  sel_layer <- sf::st_read(file, layer = layer, quiet = TRUE)
  sel_layer <- transform_layer(layer, sel_layer, metadata)
  year <- as.character(get_file_year(file))
  cbind(year = year, sel_layer)
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
  act$data <- vector(mode = "list", length = length(act$files))
  names(act$data) <- names(act$files)
  for (i in seq_along(act$files)) {
    act$data[[i]] <- get_layer_data(act$topic, act$files[i])
  }
  act
}
