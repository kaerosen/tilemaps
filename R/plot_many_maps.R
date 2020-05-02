# maps_df is output from many_maps function
# labels are region labels in order of original data
# size is size of labels on plot

plot_many_maps <- function(maps_df, labels, size = 2) {
  polygons <- st_sfc(unlist(maps_df$map, recursive = FALSE, use.names = FALSE),
                     crs = st_crs(maps_df$map[1][[1]]))
  id <- rep(1:nrow(maps_df), lengths(maps_df$map))
  labels <- rep(labels, nrow(maps_df))
  df <- data.frame(polygons, id, labels)
  ggplot(df) +
    geom_sf(aes(geometry = geometry)) +
    geom_sf_text(aes(geometry = geometry, label = labels), size = size) +
    facet_wrap(~ id) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
}
