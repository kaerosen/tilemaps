# data is object of class sfc_MULTIPOLYGON or sfc_POLYGON
# square is TRUE for square tile maps, FALSE for hexagon tile maps
# flat_topped is TRUE for hexagons that are are flat topped
# prop is vector of proportions to use when adding Gaussian noise to centroids
# interpolate is vector of values in [0,1] specifying weight for interpolation between
# noisy centroids and fully-transformed centroids
# smoothness is vector of values for controlling how much the transformed boundary should be smoothed
# shift is a list of vectors of length two with the number of grid steps to shift tile maps in
# the x direction and y direction

many_maps <- function(data, square = TRUE, flat_topped = FALSE, prop = c(0, 0.05), interpolate = c(0.5, 1),
                      smoothness = c(0, 5), shift = list(c(0,0), c(0.5,0.5))) {

  num_maps <- length(prop) * length(interpolate) * length(smoothness) * length(shift)

  if (num_maps > 20) {
    warning("generating more than 20 maps")
  }

  maps <- list()
  costs <- list()
  index <- 1

  # get crs
  crs <- st_crs(data)

  # estimate grid step size
  R <- length(data)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # find set of neighbors
  neighbors <- st_touches(data)

  for (i in 1:length(prop)) {
    centroids <- transform_centroids(data, neighbors, crs, s, prop[i])
    noisy_centroids <- centroids$noisy_centroids
    transformed_centroids <- centroids$transformed_centroids

    for (j in 1:length(interpolate)) {
      interpolated_centroids <- interpolate_centroids(noisy_centroids, transformed_centroids, crs, interpolate[j])
      transformed_boundary <- transform_boundary(data, noisy_centroids, interpolated_centroids)

      for (k in 1:length(smoothness)) {
        if (smoothness[k] != 0) {
          smoothed_boundary <- smoothr::smooth(transformed_boundary, method = "ksmooth", smoothness = smoothness[k])
        } else {
          smoothed_boundary <- transformed_boundary
        }

        for (l in 1:length(shift)) {
          grid <- fit_tiles(smoothed_boundary, R, s, square, flat_topped, shift[[l]])

          tile_centroids <- st_centroid(grid)
          perm <- assign_regions(interpolated_centroids, tile_centroids)
          grid <- grid[order(perm)]

          maps[[index]] <- grid
          index <- index + 1
        }
      }
    }
  }

  maps

}
