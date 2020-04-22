# STEP 3 - Fit tiles to boundary

fit_tiles <- function(data, boundary_poly, R, square = TRUE, flat_topped = FALSE) {

  # calculate grid step size
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # create grid
  grid <- st_make_grid(boundary_poly, cellsize = s,
                       square = square, flat_topped = flat_topped)

  # calculate number of tile centroids inside boundary
  tile_centroids <- st_centroid(grid)
  num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])

  # create range for grid step size
  s_range <- c(0,0)
  if (num_contained > R) {
    s_range[1] <- s
    s_range[2] <- 1.1*s
    grid <- st_make_grid(boundary_poly, cellsize = s_range[2],
                         square = square, flat_topped = flat_topped)
    tile_centroids <- st_centroid(grid)
    new_num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])
    while(new_num_contained > R) {
      s_range[2] <- 1.1*s_range[2]
      grid <- st_make_grid(boundary_poly, cellsize = s_range[2],
                           square = square, flat_topped = flat_topped)
      tile_centroids <- st_centroid(grid)
      new_num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])
    }
  }
  if (num_contained < R) {
    s_range[1] <- .9*s
    s_range[2] <- s
    grid <- st_make_grid(boundary_poly, cellsize = s_range[1],
                         square = square, flat_topped = flat_topped)
    tile_centroids <- st_centroid(grid)
    new_num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])
    while(new_num_contained < R) {
      s_range[1] <- .9*s_range[1]
      grid <- st_make_grid(boundary_poly, cellsize = s_range[1],
                           square = square, flat_topped = flat_topped)
      tile_centroids <- st_centroid(grid)
      new_num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])
    }
  }

  # resize grid
  iter <- 1
  #print(c(iter, num_contained))
  while (num_contained != R) {
    if (iter > 50) {
      stop("failed to converge")
    }
    grid <- st_make_grid(boundary_poly, cellsize = mean(s_range),
                         square = square, flat_topped = flat_topped)
    tile_centroids <- st_centroid(grid)
    num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])
    iter <- iter + 1
    print(c(iter, num_contained))
    if (num_contained > R) {
      s_range[1] <- mean(s_range)
    } else {
      s_range[2] <- mean(s_range)
    }
  }

  # subset grid
  contained <- st_contains(boundary_poly, tile_centroids)[[1]]
  final_grid <- grid[contained]

  final_grid
}
