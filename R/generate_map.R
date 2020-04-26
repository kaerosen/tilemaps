# data is object of class sfc_MULTIPOLYGON or sfc_POLYGON
# square is TRUE for square tile map, FALSE for hexagon tile map
# flat_topped is TRUE for hexagons that are are flat topped

generate_map <- function(data, square = TRUE, flat_topped = FALSE) {
  # get crs
  crs <- st_crs(data)

  # estimate grid step size
  R <- length(data)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # STEP 1 - transform centroids
  centroids <- transform_centroids(data, crs, s)
  noisy_centroids <- centroids$noisy_centroids
  transformed_centroids <- centroids$transformed_centroids

  # STEP 2 - transform boundary
  transformed_boundary <- transform_boundary(data, noisy_centroids, transformed_centroids)

  # STEP 3 - fit tiles to boundary
  grid <- fit_tiles(transformed_boundary, R, s, square, flat_topped)

  # STEP 4 - assign regions to tiles
  original_centroids <- st_centroid(data)
  tile_centroids <- st_centroid(grid)
  perm <- assign_regions(original_centroids, tile_centroids)

  # output tile map and permutation of original indices indicating which region is assigned to each tile
  outputs <- list("map" = grid,
                  "assignment" = perm)

  return(outputs)
}
