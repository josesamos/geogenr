
# reformat_metadata ------------------------------------------------------

#' reformat metadata
#'
#' reformat the metadata to distribute it in columns according to its topic.
#'
#' @param um A string.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
reformat_metadata <- function(um) {
  UseMethod("reformat_metadata")
}


#' @rdname reformat_metadata
#' @export
#' @keywords internal
reformat_metadata.uscb_metadata <- function(um) {
  inf_codes <- unique(um$metadata$inf_code)
  for (code in inf_codes) {
    for (v in um$variables) {
      um$metadata[um$metadata$inf_code == code, ] <- assign_level(um$metadata[um$metadata$inf_code == code, ], field = v)
    }
    print(code)
  }
  # delete empty columns
  um$metadata <- Filter(function(x)
    (!all(x == "")), um$metadata)
  um
}

