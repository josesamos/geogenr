
# reformat_metadata ------------------------------------------------------

#' reformat metadata
#'
#' reformat the metadata to distribute it in columns according to its topic.
#'
#' @param um A `uscb_metadata` object.
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
  for (inf_code in inf_codes) {
    print(inf_code)
    group_codes <- unique(um$metadata[um$metadata$inf_code == inf_code, "group_code"][[1]])
    for (group_code in group_codes) {
      print(group_code)
      for (v in um$variables) {
        um$metadata[um$metadata$inf_code == inf_code &
                      um$metadata$group_code == group_code, ] <-
          assign_level(um$metadata[um$metadata$inf_code == inf_code &
                                     um$metadata$group_code == group_code, ], field = v)
      }
    }
  }
  um
}


# delete_empty_columns ------------------------------------------------------

#' delete empty columns
#'
#' delete empty columns in metadata table.
#'
#' @param um A `uscb_metadata` object.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
delete_empty_columns <- function(um) {
  UseMethod("delete_empty_columns")
}

#' @rdname delete_empty_columns
#' @export
#' @keywords internal
delete_empty_columns.uscb_metadata <- function(um) {
  um$metadata <- Filter(function(x)
    (!all(x == "")), um$metadata)
  um
}


# show_fields ------------------------------------------------------

#' show fields
#'
#' show fields in metadata table.
#'
#' @param um A `uscb_metadata` object.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
show_fields <- function(um) {
  UseMethod("show_fields")
}

#' @rdname show_fields
#' @export
#' @keywords internal
show_fields.uscb_metadata <- function(um) {
  um2 <- um %>% delete_empty_columns()
  metadata <- um2$metadata
  for (k in 9:length(names(metadata))) {
    print("____________________________________________")
    print(names(metadata)[k])
    v <- metadata[, k][[1]]
    dput(snakecase::to_snake_case(sort(unique(v)), sep_out = "_"))
  }
  um
}


# get_field_values ------------------------------------------------------

#' show fields
#'
#' show fields in metadata table.
#'
#' @param um A `uscb_metadata` object.
#'
#' @return A `uscb_metadata` object.
#'
#' @keywords internal
get_field_values <- function(um) {
  UseMethod("get_field_values")
}

#' @rdname get_field_values
#' @export
#' @keywords internal
get_field_values.uscb_metadata <- function(um) {
  field_values <- data.frame(subject = character(), field = character(), val_set = character())

  for (f in um$interpret) {
    res <- f(um$metadata[1,], val="zzzzzzzzz", value="zzzzzzzzz", field_values = field_values)
    field_values <- res$field_values
  }
  um$field_values <- unique(field_values)

  um
}



# assign_level ------------------------------------------------------------

#' assign_level
#'
#' Structures the field values in levels so that a value is only in one level.
#'
#' @param mdr A `tibble`.
#' @param field A field name.
#'
#' @return A `tibble`.
#'
#' @keywords internal
assign_level <- function(mdr, field) {
  f <- c(
    field,
    sprintf("%s_spec", field),
    sprintf("%s_spec_2", field),
    sprintf("%s_spec_3", field),
    sprintf("%s_spec_4", field)
  )
  scroll <- TRUE
  while (scroll) {
    scroll <- FALSE
    for (i in length(f):2) {
      values <- unique(mdr[, f[i]][[1]])
      for (v in values) {
        if (v != "") {
          for (j in (i - 1):1) {
            x <- as.vector(mdr[, f[j]] == v)
            if (sum(x) > 0) {
              if (!all(mdr[x, f[i]] == "") & all(mdr[x, f[i]] != v)) {
                scroll <- TRUE
                mdr <-
                  scroll_level(
                    mdr,
                    fields = f,
                    field_index = i,
                    values_indices = x,
                    index_limit = length(f)
                  )
              }
              if (all(mdr[x, f[i]] == "")) {
                mdr[x, f[i]] <- v
                mdr[x, f[j]] <- ""
              }
            }
          }
        }
      }
    }
  }
  mdr
}

# assign level ------------------------------------------------------------



#' scroll_level
#'
#' Recursive function. Shifts the required values to be able to move the set of
#' values to the next level.
#'
#' @param mdr A `tibble`.
#' @param fields Vector with the name of the field and its levels.
#' @param field_index Index of the vector of names to consider.
#' @param values_indices Indices of the tibble positions to move.
#' @param index_limit Level limit for the name index.
#'
#' @return A `tibble`.
#'
#' @keywords internal
scroll_level <- function(mdr, fields, field_index, values_indices, index_limit) {
  enough_spec_fields <- (field_index + 1 <= index_limit)
  if (!enough_spec_fields) {
    print(unique(mdr[values_indices, fields[field_index]]))
  }
  stopifnot(enough_spec_fields)
  if (all(mdr[values_indices, fields[field_index + 1]] == "")) {
    mdr[values_indices, fields[field_index + 1]] <-
      mdr[values_indices, fields[field_index]]
    mdr[values_indices, fields[field_index]] <- ""
  } else {
    mdr <- scroll_level(mdr, fields, field_index + 1, values_indices, index_limit)
  }
  mdr
}

