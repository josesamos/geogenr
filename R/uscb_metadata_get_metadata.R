
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
# cuando tengan la misma estructura (no eliminar campos vacíos en metadatos), asignar todos los que tengan el mismo código
  # sel_cod <- um$metadata$Short_Name[um$metadata$Short_Name %in% um$uscb_acs_metadata$metadata$Short_Name][[1]]
  # um$metadata[sel_cod, ] <- um$uscb_acs_metadata$metadata[sel_cod, ]
  unsel_cod <- um$metadata$Short_Name[!(um$metadata$Short_Name %in% um$uscb_acs_metadata$metadata$Short_Name)]
  # for solo para estos códigos (arreglarlo)
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
    print(short_name)
  }


  i <- 1
  n <-  names(um$uscb_acs_metadata$metadata)
  sel_cod <- um$metadata$Short_Name[um$metadata$Short_Name %in% um$uscb_acs_metadata$metadata$Short_Name]
  for (short_name in sel_cod) {
    um$metadata[um$metadata$Short_Name == short_name, n] <-
      um$uscb_acs_metadata$metadata[um$uscb_acs_metadata$metadata$Short_Name == short_name, n]
    print(i)
    i <- i + 1
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

