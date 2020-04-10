# STEP 3 - Fit tiles to boundary

fit_tiles <- function(data, square = TRUE) {
  boundary_poly <- st_union(data)
  boundary_line <- st_boundary(boundary_poly)
  original_centroids <- st_centroid(data)

  # calculate grid step size
  R <- length(original_centroids)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  # create grid
  grid <- st_make_grid(boundary_poly, cellsize = s, square = square)

  # calculate number of tile centroids inside boundary
  tile_centroids <- st_centroid(grid)
  num_contained <- length(st_contains(boundary_poly, tile_centroids)[[1]])

  # create range for grid step size
  s_range <- c(0,0)
  if (num_contained > R) {
    s_range[1] <- s
    s_range[2] <- 1.1*s
  } else {
    s_range[1] <- .9*s
    s_range[2] <- s
  }

  # resize grid
  iter <- 1
  print(c(iter, num_contained))
  while (num_contained != R) {
    if (iter > 50) {
      stop("failed to converge")
    }
    grid <- st_make_grid(boundary_poly, cellsize = mean(s_range), square = square)
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
