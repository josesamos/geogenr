
# get_metadata ------------------------------------------------------

#' Get metadata
#'
#' get the metadata to distribute it in columns according to its topic.
#'
#' @param um A `uscb_metadata` object.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
get_metadata <- function(um) {
  UseMethod("get_metadata")
}

#' @rdname get_metadata
#' @export
#' @keywords internal
get_metadata.uscb_metadata <- function(um) {
  unsel_cod <- um$metadata$Short_Name[!(um$metadata$Short_Name %in% um$uscb_acs_metadata$metadata$Short_Name)]
  other_field <- ""
  for (short_name in unsel_cod) {
    um$metadata[um$metadata$Short_Name == short_name, ] <-
      interpret_code(um$metadata[um$metadata$Short_Name == short_name, ])
    values <- strsplit(um$metadata$Full_Name[um$metadata$Short_Name == short_name], ": ")[[1]]
    values <- stringr::str_trim(values, side = "both")
    res <-
      interpret_values(
        um$metadata[um$metadata$Short_Name == short_name, ],
        values,
        um$interpret,
        um$uscb_acs_metadata$field_values,
        other_field
      )
    um$metadata[um$metadata$Short_Name == short_name, ] <- res$mdr[1,]
    other_field <- res$other_field
  }
  new_metadata <- um$metadata[um$metadata$Short_Name %in% unsel_cod, ]
  um$metadata <- tibble::add_row(um$uscb_acs_metadata$metadata, new_metadata)
  um$uscb_acs_metadata$metadata <- um$metadata

  um
}
