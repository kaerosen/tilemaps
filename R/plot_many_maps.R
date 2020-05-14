# map_list is a list of "sfc_POLYGON" objects
# labels are region labels in order of original data
# size is size of labels on plot

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
