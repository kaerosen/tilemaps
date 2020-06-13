#' Plot Many Maps
#'
#' Plot many maps of a single area.
#'
#' Each element of the \code{map_list} argument must have the same number of
#' features, with the first feature of each element corresponding to the same
#' region, the second feature of each element corresponding to the same region,
#' etc. Region labels must be in the same order as the regions of each
#' \code{sfc_POLYGON} object.
#'
#' @param map_list A list of \code{sfc_POLYGON} objects, each containing
#'   regions of a map to be plotted.
#' @param labels A character vector containing the labels for the regions of
#'   the \code{sfc_POLYGON} objects.
#' @param size numeric. Controls size of labels in plot.
#'
#' @examples
#' library(sf)
#' northeast <- governors[c(6,7,17,18,19,27,28,30,36,37,43),]
#' ne_maps <- many_maps(northeast$geometry, northeast$abbreviation,
#'                      prop = 0, interpolate = 1, smoothness = c(0,20),
#'                      shift = list(c(0,0), c(0,0.5)), plot = FALSE)
#' plot_many_maps(ne_maps$map, northeast$abbreviation)
#'
#' @return Prints a plot with labels of the maps in the \code{map_list}
#'  argument.
#'
#' @export


plot_many_maps <- function(map_list, labels, size = 2) {
  polygons <- sf::st_sfc(unlist(map_list, recursive = FALSE, use.names = FALSE),
                         crs = sf::st_crs(map_list[1][[1]]))
  id <- rep(1:length(map_list), lengths(map_list))
  labels <- rep(labels, length(map_list))
  df <- data.frame(polygons, id, labels)
  geometry <- df$geometry
  ggplot2::ggplot(df) +
    ggplot2::geom_sf(ggplot2::aes(geometry = geometry)) +
    ggplot2::geom_sf_text(ggplot2::aes(geometry = geometry, label = labels),
                          size = size,
                          fun.geometry = function(x) sf::st_centroid(x)) +
    ggplot2::facet_wrap(~ id) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank())
}
