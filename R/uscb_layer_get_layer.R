# get_layer_names ------------------------------------------------------

#' get layer names
#'
#' get layer names.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A vector of names.
#'
#' @keywords internal
get_layer_names <- function(ul) {
  UseMethod("get_layer_names")
}

#' @rdname get_layer_names
#' @export
#' @keywords internal
get_layer_names.uscb_layer <- function(ul) {

  ul$layer_names[3:length(ul$layer_names)]
}



# get_layer ------------------------------------------------------

#' get layer
#'
#' get layer to interpret variables.
#'
#' @param ul A `uscb_layer` object.
#' @param layer_name A layer name.
#'
#' @return A `uscb_layer` object.
#'
#' @keywords internal
get_layer <- function(ul, layer_name) {
  UseMethod("get_layer")
}

#' @rdname get_layer
#' @export
#' @keywords internal
get_layer.uscb_layer <- function(ul, layer_name) {
  ul$layer_name <- layer_name
  ul$layer <-
    sf::st_read(dsn = ul$filepath,
                layer = layer_name,
                quiet = TRUE) %>%
    tibble::as_tibble()

  cod <- names(ul$layer)[-1]
  unsel_cod <- cod[!(cod %in% ul$metadata$metadata$Short_Name)]
  if (length(unsel_cod) > 0) {
    um <- uscb_metadata(
      ul$filepath,
      code = NULL,
      group_code = NULL,
      short_name = unsel_cod,
      uscb_acs_metadata = ul$metadata
    )
    um <- um %>% get_metadata()
    ul$metadata$metadata <- um$metadata
  }

  t <- ul$metadata$metadata[ul$metadata$metadata$Short_Name %in% cod, ]
  ul$layer_metadata <- Filter(function(x)!all(x == ""), t)

  ul
}
