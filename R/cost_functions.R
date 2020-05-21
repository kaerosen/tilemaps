# location cost
location_cost <- function(transformed_centroids, tile_centroids, s) {
  as.numeric(mean(sf::st_distance(transformed_centroids, tile_centroids, by_element = TRUE)) / s)
}

# adjacency cost
adjacency_cost <- function(original_neighbors, tile_neighbors) {
  missing <- rep(0, length(original_neighbors))
  for (i in 1:length(original_neighbors)) {
    missing[i] <- 1 - mean(original_neighbors[[i]] %in% tile_neighbors[[i]])
  }
  mean(missing)
}

# angle (relative orientation) cost
angle_cost <- function(original_centroids, tile_centroids, original_neighbors) {

  original_coords <- data.frame(sf::st_coordinates(original_centroids))
  tile_coords <- data.frame(sf::st_coordinates(tile_centroids))
  region_means <- rep(0, length(original_centroids))

  for (i in 1:length(original_centroids)) {

    angle <- rep(0, length(original_neighbors[[i]]))

    for (j in 1:length(original_neighbors[[i]])) {

      # calculate slope of line from original centroid to neighbor centroid
      slope1 <- (original_coords$Y[original_neighbors[[i]][j]] - original_coords$Y[i]) /
        (original_coords$X[original_neighbors[[i]][j]] - original_coords$X[i])

      # calculate slope of line from tile centroid to neighbor centroid
      slope2 <- (tile_coords$Y[original_neighbors[[i]][j]] - tile_coords$Y[i]) /
        (tile_coords$X[original_neighbors[[i]][j]] - tile_coords$X[i])

      # calculate angle between lines
      if (slope2 == Inf | slope2 == -Inf) {
        angle[j] <- atan(abs(1/slope1))
      } else {
        angle[j] <- atan(abs((slope1-slope2) / (1+slope1*slope2)))
      }

    }

    region_means[i] <- mean(angle)

  }

  mean(region_means)

}

# roughness cost
roughness_cost <- function(square, tile_map) {
  # find number of edges of each tile
  n <- ifelse(square == TRUE, 4, 6)

  # find number of tiles
  R <- length(tile_map)

  # find number of shared edges
  m <- 2*sum(sf::st_geometry_type(sf::st_intersection(tile_map)) == "LINESTRING")

  # find minimum perimeter
  a <- ifelse(square == TRUE, 1, 3*sqrt(3)/2)
  P <- 2*sqrt(pi*a*R)

  # calculate cost
  (n*R - m - P) / P

}

