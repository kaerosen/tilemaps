# data is object of class sfc_MULTIPOLYGON or sfc_POLYGON
# square is TRUE for square tile map, FALSE for hexagon tile map
# flat_topped is TRUE for hexagons that are are flat topped
# prop is proportion used when adding Gaussian noise to centroids
# interpolate is value in [0,1] specifying weight for interpolation between
# noisy centroids and fully-transformed centroids
# smoothness controls how much the transformed boundary should be smoothed
# shift is a vector of length two with the number of grid steps to shift tile map in
# the x direction and y direction

generate_map <- function(data, square = TRUE, flat_topped = FALSE, prop = 0, interpolate = 1,
                         smoothness = 0, shift = c(0,0)) {
  # get crs
  crs <- st_crs(data)

  # estimate grid step size
  R <- length(data)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # STEP 1 - transform centroids
  centroids <- transform_centroids(data, crs, s, prop, interpolate)
  noisy_centroids <- centroids$noisy_centroids
  transformed_centroids <- centroids$transformed_centroids

  # STEP 2 - transform boundary
  transformed_boundary <- transform_boundary(data, noisy_centroids, transformed_centroids, smoothness)

  # STEP 3 - fit tiles to boundary
  grid <- fit_tiles(transformed_boundary, R, s, square, flat_topped, shift)

  # STEP 4 - assign regions to tiles
  tile_centroids <- st_centroid(grid)
  perm <- assign_regions(transformed_centroids, tile_centroids)
  grid <- grid[order(perm)]

  # output tile map in order of original data
  grid

}
