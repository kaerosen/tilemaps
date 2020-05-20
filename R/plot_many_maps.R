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
#' library(spData)
#' library(dplyr)
#' us <- us_states %>%
#'   arrange(NAME) %>%
#'   mutate(abbreviation = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",
#'                           "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY",
#'                           "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
#'                           "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
#'                           "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
#'                           "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
#'                           "WY"))
#' us_3857 <- st_transform(us, 3857)
#' us_maps <- many_maps(us_3857$geometry, us_3857$abbreviation,
#'                      prop = c(0, 0.1), interpolate = c(0.5, 1),
#'                      smoothness = c(0, 20), shift = list(c(0,0), c(0,0.5)),
#'                      weights = c(1,1,1,1), plot = TRUE, size = 1.5)
#' plot_many_maps(us_maps$map, us_3857$abbreviation, size = 1.5)
#'
#' @return Prints a plot with labels of the maps in the \code{map_list}
#'  argument.
#'
#' @export


plot_many_maps <- function(map_list, labels, size = 2) {
  polygons <- st_sfc(unlist(map_list, recursive = FALSE, use.names = FALSE),
                     crs = st_crs(map_list[1][[1]]))
  id <- rep(1:length(map_list), lengths(map_list))
  labels <- rep(labels, length(map_list))
  df <- data.frame(polygons, id, labels)
  ggplot(df) +
    geom_sf(aes(geometry = geometry)) +
    geom_sf_text(aes(geometry = geometry, label = labels), size = size,
                 fun.geometry = function(x) st_centroid(x)) +
    facet_wrap(~ id) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
}
