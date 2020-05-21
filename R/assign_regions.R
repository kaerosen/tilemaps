# STEP 4 - assign regions to tiles

assign_regions <- function(region_centroids, tile_centroids) {
  dist_matrix <- matrix(as.numeric(sf::st_distance(tile_centroids, region_centroids)),
                        nrow = length(region_centroids))^2
  # output permutation of original indices
  as.numeric(clue::solve_LSAP(dist_matrix))
}
