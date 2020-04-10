# STEP 1 - Transform centroids so that neighbors are equidistant

# data is sfc object with geometry type MULTIPOLYGON
# crs is coordinate reference system epsg code, default is mercator
transform_centroids <- function(data, crs = 3857) {
  # get centroids
  original_centroids <- st_centroid(data)

  # find set of neighbors
  neighbors <- st_touches(data, data)
  num_neighbors <- lengths(neighbors)

  # calculate mean distance from centroids to neighbor centroids
  neighbor_dist <- list()
  for (i in 1:length(data)) {
    neighbor_dist <- append(neighbor_dist, list(rep(0, length(neighbors[[i]]))))
  }

  for (i in 1:length(data)) {
    for(j in 1:length(neighbor_dist[[i]])) {
      neighbor_dist[[i]][j] <- st_distance(original_centroids[i],
                                           original_centroids[neighbors[[i]][j]])
    }
  }

  mean_neighbor_dist <- rep(0, length(data))
  for (i in 1:length(mean_neighbor_dist)) {
    mean_neighbor_dist[i] <- mean(neighbor_dist[[i]])
  }

  # add Gaussian noise to original centroids
  prop <- 0.1
  noise <- rnorm(length(original_centroids), mean = 0, sd = prop*mean_neighbor_dist)
  noisy_centroids <- original_centroids + noise
  noisy_centroids <- st_set_crs(noisy_centroids, crs)

  # estimate grid step size
  R <- length(original_centroids)
  A <- sum(st_area(data))
  s <- sqrt(A/R)

  # calculate new centroids
  old_centroids <- update_centroids(noisy_centroids, neighbors, s)
  dist <- as.numeric(st_distance(noisy_centroids, old_centroids, by_element = TRUE))
  new_centroids <- update_centroids(old_centroids, neighbors, s)
  new_dist <- as.numeric(st_distance(old_centroids, new_centroids, by_element = TRUE))
  per_change <- (new_dist - dist) / dist
  iter <- 2
  while (sum(abs(per_change) > .15) > 0) {
    if (iter > 50) {
      stop("failed to converge")
    }
    #print(c(iter, sum(abs(per_change) > .15)))
    old_centroids <- new_centroids
    dist <- new_dist
    new_centroids <- update_centroids(old_centroids, neighbors, s)
    new_dist <- as.numeric(st_distance(old_centroids, new_centroids, by_element = TRUE))
    per_change <- (new_dist - dist) / dist
    iter <- iter + 1
  }

  outputs <- list("noisy_centroids" = noisy_centroids,
                  "transformed_centroids" = new_centroids)

  return(outputs)

}

update_centroids <- function(centroids, neighbors, s) {
  num_neighbors <- lengths(neighbors)

  for (i in 1:length(centroids)) {
    total <- 0
    for (j in 1:num_neighbors[i]) {
      k <- neighbors[[i]][j]
      u <- (centroids[i] - centroids[k]) /
        as.numeric(st_distance(centroids[i], centroids[k]))
      total <- centroids[k] + u * as.numeric(s) + total
    }
    centroids[i] <- total * 1/num_neighbors[i]
  }

  centroids
}
