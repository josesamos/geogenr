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

  ul$layers[3:length(ul$layers)]
}



# get_layer ------------------------------------------------------

#' get layer
#'
#' get layer to interpret variables.
#'
#' @param ul A `uscb_layer` object.
#' @param layer A layer name.
#'
#' @return A layer data.
#'
#' @keywords internal
get_layer <- function(ul, layer) {
  UseMethod("get_layer")
}

#' @rdname get_layer
#' @export
#' @keywords internal
get_layer.uscb_layer <- function(ul, layer) {
  ly <-
    sf::st_read(dsn = ul$filepath,
                layer = layer,
                quiet = TRUE) %>%
    tibble::as_tibble()


  ly
}
