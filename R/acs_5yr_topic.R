# structure(list(
#   area = cod,
#   years = years,
#   topic = topic_name,
#   area_topics = topics,
#   files = files
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
  act
}


