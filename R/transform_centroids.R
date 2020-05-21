#' @importFrom stats rnorm

# STEP 1 - Transform centroids so that neighbors are equidistant

# data is sfc object with geometry type MULTIPOLYGON
# crs is coordinate reference system epsg code, s is estimated grid step size
# prop is proportion used when adding Gaussian noise

transform_centroids <- function(data, neighbors, crs, s, prop = 0) {
  # get centroids
  original_centroids <- sf::st_centroid(data)

  # find set of neighbors
  num_neighbors <- lengths(neighbors)

  # calculate mean distance from centroids to neighbor centroids
  neighbor_dist <- list()
  for (i in 1:length(data)) {
    neighbor_dist <- append(neighbor_dist, list(rep(0, length(neighbors[[i]]))))
  }

  dist_matrix <- sf::st_distance(original_centroids)
  for (i in 1:length(data)) {
    neighbor_dist[[i]] <- dist_matrix[i, neighbors[[i]]]
  }

  mean_neighbor_dist <- rep(0, length(data))
  for (i in 1:length(mean_neighbor_dist)) {
    mean_neighbor_dist[i] <- mean(neighbor_dist[[i]])
  }

  # add Gaussian noise to original centroids
  noise <- rnorm(length(original_centroids), mean = 0, sd = prop*mean_neighbor_dist)
  noisy_centroids <- original_centroids + noise
  noisy_centroids <- sf::st_set_crs(noisy_centroids, crs)

  # calculate new centroids
  new_centroids <- update_centroids(noisy_centroids, neighbors, s)
  dist <- as.numeric(sf::st_distance(noisy_centroids, new_centroids, by_element = TRUE))
  iter <- 1
  while (sum(dist > .1*mean_neighbor_dist) > 0 & iter < 100) {
    old_centroids <- new_centroids
    new_centroids <- update_centroids(old_centroids, neighbors, s)
    dist <- as.numeric(sf::st_distance(old_centroids, new_centroids, by_element = TRUE))
    iter <- iter + 1
  }

  if (iter == 100) {
    warning("centroids failed to converge")
  }

  outputs <- list("noisy_centroids" = noisy_centroids,
                  "transformed_centroids" = new_centroids)

  return(outputs)

}

interpolate_centroids <- function(noisy_centroids, transformed_centroids, crs, interpolate) {

  if (interpolate == 0) {
    new_centroids <- noisy_centroids
  } else if (interpolate == 1) {
    new_centroids <- transformed_centroids
  } else {
    new_centroids <- noisy_centroids - (noisy_centroids - transformed_centroids) * interpolate
    new_centroids <- sf::st_set_crs(new_centroids, crs)
  }

  new_centroids

}

update_centroids <- function(centroids, neighbors, s) {
  num_neighbors <- lengths(neighbors)
  new_centroids <- centroids

  dist_matrix <- sf::st_distance(centroids)
  for (i in 1:length(centroids)) {
    total <- 0
    j <- neighbors[[i]]
    u <- (centroids[i] - centroids[j]) / as.numeric(dist_matrix[i, j])
    values <- unlist(centroids[j] + u * s)
    x <- sum(values[2*1:length(j) - 1])
    y <- sum(values[2*1:length(j)])
    new_centroids[i] <- sf::st_point(c(x,y)) / num_neighbors[i]
  }

  new_centroids
}
