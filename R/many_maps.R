# data is object of class sfc_MULTIPOLYGON or sfc_POLYGON
# labels is vector with names of regions in same order as they appear in data
# square is TRUE for square tile maps, FALSE for hexagon tile maps
# flat_topped is TRUE for hexagons that are are flat topped
# prop is vector of proportions to use when adding Gaussian noise to centroids
# interpolate is vector of values in [0,1] specifying weight for interpolation between
# noisy centroids and fully-transformed centroids
# smoothness is vector of values for controlling how much the transformed boundary should be smoothed
# shift is a list of vectors of length two with the number of grid steps to shift tile maps in
# the x direction and y direction
# weights is weights used to calculate total cost
# output is dataframe including maps, parameters, and costs, ordered by total cost
# plot is TRUE to create plot of all maps
# size is size of plot labels

many_maps <- function(data, labels, square = TRUE, flat_topped = FALSE, prop = c(0, 0.05), interpolate = c(0.5, 1),
                      smoothness = c(0, 5), shift = list(c(0,0), c(0.5,0), c(0,0.5)), weights = c(1,1,1,1),
                      plot = FALSE, size = 2) {

  num_maps <- length(prop) * length(interpolate) * length(smoothness) * length(shift)

  maps <- list()
  shift_param <- list()
  df <- data.frame(matrix(rep(0, num_maps*8), nrow = num_maps))
  colnames(df) <- c("prop", "interpolate", "smoothness", "location_cost", "adjacency_cost",
                    "angle_cost", "roughness_cost", "total_cost")
  index <- 1

  # get crs
  crs <- st_crs(data)

  # estimate grid step size
  R <- length(data)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # find set of neighbors
  neighbors <- st_touches(data)
  if (0 %in% lengths(neighbors)) {
    stop("geometry is not contiguous")
  }

  # get original centroids
  original_centroids <- st_centroid(data)

  for (i in 1:length(prop)) {
    # STEP 1 - transform centroids
    centroids <- transform_centroids(data, neighbors, crs, s, prop[i])
    noisy_centroids <- centroids$noisy_centroids
    transformed_centroids <- centroids$transformed_centroids

    for (j in 1:length(interpolate)) {
      # interpolate centroids
      interpolated_centroids <- interpolate_centroids(noisy_centroids, transformed_centroids, crs, interpolate[j])

      # STEP 2 - transform boundary
      transformed_boundary <- transform_boundary(data, noisy_centroids, interpolated_centroids)

      for (k in 1:length(smoothness)) {
        # smooth boundary
        if (smoothness[k] != 0) {
          smoothed_boundary <- smoothr::smooth(transformed_boundary, method = "ksmooth", smoothness = smoothness[k])
        } else {
          smoothed_boundary <- transformed_boundary
        }

        for (l in 1:length(shift)) {
          # STEP 3 - fit tiles to boundary
          grid <- fit_tiles(smoothed_boundary, R, s, square, flat_topped, shift[[l]])

          # STEP 4 - assign regions to tiles
          tile_centroids <- st_centroid(grid)
          perm <- assign_regions(interpolated_centroids, tile_centroids)
          grid <- grid[order(perm)]

          # calculate costs
          tile_neighbors <- st_touches(grid)
          loc <- location_cost(interpolated_centroids, tile_centroids, s)
          adj <- adjacency_cost(neighbors, tile_neighbors)
          angle <- angle_cost(original_centroids, tile_centroids, neighbors)
          rough <- roughness_cost(square, grid)
          total_cost <- sum(c(loc,adj,angle,rough) * weights)

          maps[[index]] <- grid
          shift_param[[index]] <- shift[[l]]
          df[index, ] <- c(prop[i], interpolate[j], smoothness[k], loc, adj, angle, rough, total_cost)
          index <- index + 1
        }
      }
    }
  }

  df$map <- maps
  df$shift <- shift_param
  df <- df[order(df$total_cost),c(9, 8, 1:3, 10, 4:7)]
  rownames(df) <- 1:nrow(df)

  if (plot) {
    print(plot_many_maps(df$map, labels, size))
  }

  df

}
