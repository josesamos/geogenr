
# get_metadata ------------------------------------------------------

#' get metadata
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
  other_field <- ""
  n <-  names(um$uscb_acs_metadata$metadata)
  for (i in seq_along(um$metadata[[1]])) {
    short_name <- um$metadata[i, "Short_Name"][[1]]

    if (short_name %in% um$uscb_acs_metadata$metadata$Short_Name) {
#      um$metadata[i, n] <- um$uscb_acs_metadata$metadata[um$uscb_acs_metadata$metadata$Short_Name == short_name, n]
    } else {
      um$metadata[i, ] <- interpret_code(um$metadata[i, ])

      values <- strsplit(um$metadata$Full_Name[i], ": ")[[1]]
      values <- stringr::str_trim(values, side = "both")

      res <- interpret_values(um$metadata[i, ], values, um$interpret, um$field_values, other_field)
      um$metadata[i, ] <- res$mdr[1,]
      other_field <- res$other_field
    }

    if (i %% 100 == 0) {
      print(i)
    }
  }
  um
}


#' interpret values based on metadata
#'
#' Interprets the values in the metadata for a row.
#'
#' @param mdr A `tibble` row.
#' @param values A vector of values.
#' @param field_values A data frame that stores associations between fields and
#'   values.
#'
#' @return A `tibble` row.
#'
#' @keywords internal
interpret_values_based_on_metadata <- function(mdr, values, field_values) {
  vals <- tolower(values)
  vals <- snakecase::to_snake_case(vals, sep_out = "_")

  if (mdr$subgroup_code == "" |
      # Puerto Rico
      mdr$subgroup_code == "PR") {
    mdr$group <- values[1]
  } else {
    # subgroup: content in parentheses
    subgroup <-
      regmatches(values[1], gregexpr("(?<=\\().*?(?=\\))", values[1], perl = T))[[1]]
    if (length(subgroup) > 0) {
      mdr$subgroup <- subgroup[length(subgroup)]
      # group: remove the content of the parentheses
      # mdr$group <- gsub("\\s*\\([^\\)]+\\)", "", values[1])
      mdr$group <-
        stringr::str_replace(values[1], sprintf("\\(%s\\)", mdr$subgroup), "")
    }
  }

  for (j in 2:length(values)) {
    res <- interpret_all(mdr, vals[j], values[j], interpret, field_values, other_field)
    if (!res$result) {
      res <- interpret_as(mdr, field = "rest", vals[j], values[j])
    } else {
      other_field <- res$other_field
    }
    mdr <- res$mdr
  }
  res$other_field <- other_field
  res
}

