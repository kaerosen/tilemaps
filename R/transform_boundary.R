# STEP 2 - Transform boundary polygon

transform_boundary <- function(data, noisy_centroids, new_centroids) {
  # take sample of original boundary points
  original_poly <- st_union(data)
  original_boundary <- st_boundary(original_poly)
  boundary_points <- st_cast(original_boundary, "MULTIPOINT")
  boundary_coords <- data.frame(st_coordinates(boundary_points))
  num_points <- nrow(boundary_coords)
  index <- 1:1000 * floor(num_points/1000)
  samp_coords <- boundary_coords[index,]
  samp_points <- boundary_points[1]
  samp_points[1] <- st_multipoint(as.matrix(samp_coords[,1:2]))
  samp_points <- st_cast(samp_points, "POINT")

  # find k nearest noisy_centroids to each boundary point
  # M is set of idices of the k nearest noisy centroids to original boundary points
  k <- 3
  dist_matrix <- st_distance(samp_points, noisy_centroids)
  M <- matrix(rep(0,length(samp_points)*k), ncol = k)
  for (i in 1:length(samp_points)) {
    M[i,] <- order(dist_matrix[i,])[1:k]
  }

  # calculate weights for centroids in M
  W <- matrix(rep(0,length(samp_points)*k), ncol = k)

  for (i in 1:length(samp_points)) {
    for (j in 1:k) {
      W[i,j] <- exp(as.numeric(-dist_matrix[i,M[i,j]]^2 / (2*min(dist_matrix[i,M[i,]]^2))))
    }
  }

  # normalize weights
  W <- W / rowSums(W)

  # calculate weighted mean of displacement vectors
  v <- samp_points
  for (i in 1:length(v)) {
    displacement <- (samp_points[i] - noisy_centroids[M[i,]]) * W[i,]
    total <- 0
    for (j in 1:k) {
      total <- displacement[j] + total
    }
    v[i] <- total
  }

  # calculate new boundary points
  new_boundary <- samp_points
  R <- length(noisy_centroids)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  for (i in 1:length(samp_points)) {
    weighted_centroids <- new_centroids[M[i,]] * W[i,]
    total <- 0
    for (j in 1:k) {
      total <- weighted_centroids[j] + total
    }
    new_boundary[i] <- total + v[i] * sqrt(s / norm(st_coordinates(v[i]), type = "f"))
  }

  # convert new boundary points to multilinestring
  new_boundary_coords <- data.frame(st_coordinates(new_boundary))
  new_boundary_coords$L1 <- samp_coords$L1
  group <- unique(samp_coords$L1)
  coords_list <- list()
  for (i in 1:length(group)) {
    points <- new_boundary_coords %>%
      filter(L1 == group[i]) %>%
      select("X","Y")
    coords_list[[i]] <- as.matrix(points)
    }
  new_ls <- st_multilinestring(coords_list)
  new_boundary <- original_boundary
  new_boundary[[1]] <- new_ls

  new_boundary
}
