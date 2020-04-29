# data is object of class sfc_MULTIPOLYGON or sfc_POLYGON
# square is TRUE for square tile map, FALSE for hexagon tile map
# flat_topped is TRUE for hexagons that are are flat topped
# smoothness controls how much the transformed boundary should be smoothed
# shift_right is the number of grid steps to shift tile map to the right before fitting tiles to boundary
# shift_up is the number of grid steps to shift tile map up before fitting tiles to boundary

generate_map <- function(data, square = TRUE, flat_topped = FALSE, smoothness = 0,
                         shift_right = 0, shift_up = 0) {
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
  transformed_boundary <- transform_boundary(data, noisy_centroids, transformed_centroids, smoothness)

  # STEP 3 - fit tiles to boundary
  grid <- fit_tiles(transformed_boundary, R, s, square, flat_topped, shift_right, shift_up)

  # STEP 4 - assign regions to tiles
  tile_centroids <- st_centroid(grid)
  perm <- assign_regions(transformed_centroids, tile_centroids)
  grid <- grid[order(perm)]

  # output tile map in order of original data
  grid

}
