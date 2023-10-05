# get_layer_names ------------------------------------------------------

#' Get layer names
#'
#' Once a specific geodatabase has been selected from which the class object has
#' been created, we can obtain the names of the layers it contains.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A vector of names.
#'
#' @family data selection functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#'
#' layers <- ul |> get_layer_names()
#'
#' @export
get_layer_names <- function(ul) {
  UseMethod("get_layer_names")
}

#' @rdname get_layer_names
#' @export
get_layer_names.uscb_layer <- function(ul) {

  ul$layer_names[substr(ul$layer_names, 1, 1) == "X"]
}



# get_layer ------------------------------------------------------

#' Get layer
#'
#' Get a layer to interpret its variables. Refines the content of the object.
#'
#' @param ul A `uscb_layer` object.
#' @param layer_name A layer name.
#'
#' @return A `uscb_layer` object.
#'
#' @family data selection functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul |> get_layer_names()
#'
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul |> get_layer(layers[3])
#'
#' @export
get_layer <- function(ul, layer_name) {
  UseMethod("get_layer")
}

#' @rdname get_layer
#' @export
get_layer.uscb_layer <- function(ul, layer_name) {
  ul$layer_name <- layer_name
  ul$layer <-
    sf::st_read(dsn = ul$filepath,
                layer = layer_name,
                quiet = TRUE) |>
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
    um <- um |> get_metadata()
    ul$metadata$metadata <- um$metadata
  }

  t <- ul$metadata$metadata[ul$metadata$metadata$Short_Name %in% cod, ]
  ul$layer_metadata <- Filter(function(x)!all(x == ""), t)

  ul$layer_group_names <- sort(unique(paste(ul$layer_metadata$group_code, ul$layer_metadata$group, sep = " - ")))

  ul
}


# get_layer_group_names ------------------------------------------------------

#' Get layer group names
#'
#' A layer is broken down into groups. Get the name of the layer groups.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A vector of names.
#'
#' @family data selection functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul |> get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul |> get_layer(layers[3])
#'
#' layer_groups <- ul |> get_layer_group_names()
#'
#' @export
get_layer_group_names <- function(ul) {
  UseMethod("get_layer_group_names")
}

#' @rdname get_layer_group_names
#' @export
get_layer_group_names.uscb_layer <- function(ul) {

  ul$layer_group_names
}


# get_layer_group ------------------------------------------------------

#' Get layer group
#'
#' Get a layer group to interpret its variables. Refines the content of the
#' object.
#'
#' @param ul A `uscb_layer` object.
#' @param layer_group_name A layer name.
#'
#' @return A `uscb_layer` object.
#'
#' @family data selection functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul |> get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul |> get_layer(layers[3])
#' lg <- ul |> get_layer_group_names()
#'
#' # lg[2]
#' # [1] "003 - DETAILED RACE"
#' ul <- ul |> get_layer_group(lg[2])
#'
#' @export
get_layer_group <- function(ul, layer_group_name) {
  UseMethod("get_layer_group")
}

#' @rdname get_layer_group
#' @export
get_layer_group.uscb_layer <- function(ul, layer_group_name) {
  ul$layer_group_name <- layer_group_name
  group_code <- substr(layer_group_name, 1, 3)

  short_names <- ul$layer_metadata$Short_Name[ul$layer_metadata$group_code == group_code]
  names <- names(ul$layer)
  ul$layer_group_columns <- c(names[1], names[names %in% short_names])
  t <- ul$layer_metadata[ul$layer_metadata$Short_Name %in% ul$layer_group_columns[-1], ]
  ul$layer_group_metadata <- Filter(function(x)!all(x == ""), t)

  ul
}


# get_basic_flat_table ------------------------------------------------------

#' Get basic flat table
#'
#' Get the layer group data in the form of a flat table that only includes the
#' geoid column of the geographic data.
#'
#' Optionally you can delete the rows whose measurement value is zero.
#'
#' @param ul A `uscb_layer` object.
#' @param remove_zeros A boolean, remove data with zero value.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
get_basic_flat_table <- function(ul, remove_zeros = FALSE) {
  UseMethod("get_basic_flat_table")
}

#' @rdname get_basic_flat_table
#' @export
#' @keywords internal
get_basic_flat_table.uscb_layer <- function(ul, remove_zeros = FALSE) {
  layer <- ul$layer[, ul$layer_group_columns]
  layer <- tidyr::pivot_longer(layer, !c("GEOID"), names_to = "Short_Name", values_to = "value")
  if (remove_zeros) {
    layer <- layer[layer$value != 0, ]
  }

  layer <- dplyr::left_join(layer, ul$layer_group_metadata, by = c("Short_Name" = "Short_Name"))
  if (all(!grepl("\\D", layer$spec_code))) {
    format <- sprintf("%%0%dd", max(nchar(layer$spec_code)))
    spec_code <- sprintf(format, strtoi(layer$spec_code))
  } else {
    spec_code <- layer$spec_code
  }
  if ("subgroup_code" %in% names(layer)) {
    layer$Short_Name <- paste(layer$inf_code, layer$group_code, layer$subgroup_code, "_", spec_code, sep = "")
  } else {
    layer$Short_Name <- paste(layer$inf_code, layer$group_code, "_", spec_code, sep = "")
  }
  layer <- dplyr::select(layer, !c("type_code"))
  layer <- tidyr::pivot_wider(layer, names_from = "type", values_from = "value")

  layer <- tibble::add_column(layer, year = ul$year, .before = "GEOID")
  layer <- dplyr::relocate(layer, c("Estimate", "Margin of Error"), .after = tidyselect::last_col())
  layer$GEOID <- substr(layer$GEOID, 8, length(layer$GEOID))
  layer
}


# get_flat_table ------------------------------------------------------

#' Get flat table
#'
#' Get the layer group data in the form of a flat table that includes all the
#' available data columns.
#'
#' Optionally you can delete the rows whose measurement value is zero and remove
#' the geometry column.
#'
#' @param ul A `uscb_layer` object.
#' @param remove_zeros A boolean, remove data with zero value.
#' @param remove_geometry A boolean, remove geometry column.
#'
#' @return A `tibble` object.
#'
#' @family result generation functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul |> get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul |> get_layer(layers[3])
#' lg <- ul |> get_layer_group_names()
#' # lg[2]
#' # [1] "003 - DETAILED RACE"
#' ul <- ul |> get_layer_group(lg[2])
#'
#' layer <- ul |> get_flat_table()
#'
#' @export
get_flat_table <- function(ul, remove_zeros = FALSE, remove_geometry = TRUE) {
  UseMethod("get_flat_table")
}

#' @rdname get_flat_table
#' @export
get_flat_table.uscb_layer <- function(ul, remove_zeros = FALSE, remove_geometry = TRUE) {
  layer <- get_basic_flat_table(ul, remove_zeros)
  names <- names(layer)

  place <- get_place(ul)
  place_names <- names(place)

  layer <- dplyr::left_join(layer, place, by = c("GEOID" = "GEOID"))
  names <- c(names[1], place_names[-length(place_names)], names[3:length(names)], place_names[length(place_names)])
  layer <- layer[, names]
  names_layer <- snakecase::to_snake_case(tolower(names(layer)), sep_out = "_")
  names(layer) <- names_layer
  if (remove_geometry) {
    layer[, names_layer[-length(names_layer)]]
  } else {
    sf::st_as_sf(layer)
  }
}


# get_geomultistar ------------------------------------------------------

#' Get `geomultistar`
#'
#' Get all the layer group data in the form of a `geomultistar` object: It
#' contains fact and dimension tables, and a dimension with an associated
#' geographic layer.
#'
#' The name of the facts is the layer group name.
#'
#' @param ul A `uscb_layer` object.
#'
#' @return A `geomultistar` object.
#'
#' @family result generation functions
#' @seealso
#'
#' @examples
#'
#' folder <- system.file("extdata", package = "geogenr")
#' folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
#' ua <- uscb_acs_5ye(folder = folder)
#' sa <- ua |> get_statistical_areas()
#' # sa[6]
#' # [1] "New England City and Town Area Division"
#' ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
#' layers <- ul |> get_layer_names()
#' # layers[3]
#' # [1] "X02_RACE"
#' ul <- ul |> get_layer(layers[3])
#' lg <- ul |> get_layer_group_names()
#' # lg[2]
#' # [1] "003 - DETAILED RACE"
#' ul <- ul |> get_layer_group(lg[2])
#'
#' gms <- ul |> get_geomultistar()
#'
#' @export
get_geomultistar <- function(ul) {
  UseMethod("get_geomultistar")
}

#' @rdname get_geomultistar
#' @export
get_geomultistar.uscb_layer <- function(ul) {
  ft <- get_basic_flat_table(ul, remove_zeros = FALSE)
  names_ft <- names(ft)
  what_ini <- which(names_ft == "Short_Name")
  fact_ini <- which(names_ft == "Estimate")

  place <- get_place(ul)
  place_names <- names(place)
  place_names <- place_names[1:(length(place_names) - 1)]

  ft <- dplyr::left_join(ft, place[, place_names], by = c("GEOID" = "GEOID"))

  define_geomultistar(ft,
                      fact_name = substr(ul$layer_group_name, 7, nchar(ul$layer_group_name)),
                      where_names = place_names,
                      what_names = names_ft[what_ini:(fact_ini - 1)],
                      place)
}


#' define_geomultistar
#'
#' Definition of a `geomultistar` object from a fact table.
#'
#' @param ft A `tibble` object.
#' @param fact_name A string.
#' @param where_names A vector of strings.
#' @param what_names A vector of strings.
#' @param place A `st` object.
#'
#' @return A boolean
#'
#' @keywords internal
define_geomultistar <- function(ft, fact_name, where_names, what_names, place) {
  dm <- starschemar::dimensional_model() |>
    starschemar::define_fact(
      name = fact_name,
      measures = c("Estimate", "Margin of Error"),
    ) |>
    starschemar::define_dimension(name = "when",
                                  attributes = c("year")) |>
    starschemar::define_dimension(name = "where",
                                  attributes = where_names) |>
    starschemar::define_dimension(name = "what",
                                  attributes = what_names)

  st <- starschemar::star_schema(ft, dm) |>
    starschemar::snake_case()

  gms <-
    geomultistar::geomultistar(st, geodimension = "where") |>
    geomultistar::define_geoattribute(
      attribute = "geoid",
      from_layer = place,
      by = c("geoid" = "GEOID")
    )
  gms
}

