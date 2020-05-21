# STEP 3 - Fit tiles to boundary

fit_tiles <- function(boundary_poly, R, s, square = TRUE, flat_topped = FALSE, shift = c(0,0)) {
  # create grid
  buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*s)
  grid <- sf::st_make_grid(buffer, cellsize = s,
                           square = square, flat_topped = flat_topped)

  crs <- sf::st_crs(grid)
  grid <- sf::st_set_crs(grid + shift*s, crs)

  # calculate number of tile centroids inside boundary
  tile_centroids <- sf::st_centroid(grid)
  num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])

  # create range for grid step size
  s_range <- c(0,0)
  if (num_contained > R) {
    s_range[1] <- s
    s_range[2] <- 1.1*s
    buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*s_range[2])
    grid <- sf::st_make_grid(buffer, cellsize = s_range[2],
                             square = square, flat_topped = flat_topped)
    grid <- sf::st_set_crs(grid + shift*s_range[2], crs)
    tile_centroids <- sf::st_centroid(grid)
    new_num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])
    while(new_num_contained > R) {
      s_range[2] <- 1.1*s_range[2]
      buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*s_range[2])
      grid <- sf::st_make_grid(buffer, cellsize = s_range[2],
                               square = square, flat_topped = flat_topped)
      grid <- sf::st_set_crs(grid + shift*s_range[2], crs)
      tile_centroids <- sf::st_centroid(grid)
      new_num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])
    }
  }
  if (num_contained < R) {
    s_range[1] <- .9*s
    s_range[2] <- s
    buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*s_range[1])
    grid <- sf::st_make_grid(buffer, cellsize = s_range[1],
                             square = square, flat_topped = flat_topped)
    grid <- sf::st_set_crs(grid + shift*s_range[1], crs)
    tile_centroids <- sf::st_centroid(grid)
    new_num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])
    while(new_num_contained < R) {
      s_range[1] <- .9*s_range[1]
      buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*s_range[1])
      grid <- sf::st_make_grid(buffer, cellsize = s_range[1],
                               square = square, flat_topped = flat_topped)
      grid <- sf::st_set_crs(grid + shift*s_range[1], crs)
      tile_centroids <- sf::st_centroid(grid)
      new_num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])
    }
  }

  # resize grid
  #iter <- 1
  #print(c(iter, num_contained))
  while (num_contained != R) {
    buffer <- sf::st_buffer(boundary_poly, max(abs(shift))*mean(s_range))
    grid <- sf::st_make_grid(buffer, cellsize = mean(s_range),
                             square = square, flat_topped = flat_topped)
    grid <- sf::st_set_crs(grid + shift*mean(s_range), crs)
    tile_centroids <- sf::st_centroid(grid)
    num_contained <- length(sf::st_contains(boundary_poly, tile_centroids)[[1]])
    #iter <- iter + 1
    #print(c(iter, num_contained))
    if (num_contained > R) {
      s_range[1] <- mean(s_range)
    } else {
      s_range[2] <- mean(s_range)
    }
  }

  # subset grid
  contained <- sf::st_contains(boundary_poly, tile_centroids)[[1]]
  final_grid <- grid[contained]

  final_grid
}
