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
get_other_topics.acs_5yr<- function(act) {
  act$area_topics
}
