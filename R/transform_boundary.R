# STEP 2 - Transform boundary polygon

transform_boundary <- function(data, noisy_centroids, new_centroids) {
  # take sample of original boundary points
  original_poly <- sf::st_union(data)
  original_boundary <- sf::st_boundary(original_poly)
  if (sum(class(original_boundary) == "sfc_MULTILINESTRING") == 0) {
    original_boundary <- sf::st_cast(original_boundary, "MULTILINESTRING")
  }
  boundary_points <- sf::st_cast(original_boundary, "MULTIPOINT")
  boundary_coords <- data.frame(sf::st_coordinates(boundary_points))

  prop <- lengths(original_boundary[[1]]) / sum(lengths(original_boundary[[1]]))
  sample_size <- ceiling(prop * 1000)
  sample_groups <- which(sample_size > 3)

  subset <- boundary_coords[which(boundary_coords$L1 == sample_groups[1]),]
  if (sample_size[sample_groups[1]] > nrow(subset)) {
    index <- 1:nrow(subset)
  } else {
    index <- c(1, sort(sample(2:(nrow(subset)-1), sample_size[sample_groups[1]]-2)),
               nrow(subset))
  }

  sample <- subset[index,]
  if (length(sample_groups) > 1) {
    for (i in 2:length(sample_groups)) {
      subset <- boundary_coords[which(boundary_coords$L1 == sample_groups[i]),]
      if (sample_size[sample_groups[i]] > nrow(subset)) {
        index <- 1:nrow(subset)
      } else {
        index <- c(1, sort(sample(2:(nrow(subset)-1), sample_size[sample_groups[i]]-2)),
                   nrow(subset))
      }
      sample <- rbind(sample, subset[index,])
    }
  }

  samp_points <- sf::st_sfc(sf::st_multipoint(as.matrix(sample[,1:2])), crs = sf::st_crs(data))
  samp_points <- sf::st_cast(samp_points, "POINT")

  # find k nearest noisy_centroids to each boundary point
  # M is set of idices of the k nearest noisy centroids to original boundary points
  k <- 3
  dist_matrix <- matrix(as.numeric(sf::st_distance(samp_points, noisy_centroids)),
                        ncol = length(noisy_centroids))
  M <- matrix(rep(0,nrow(sample)*k), ncol = k)
  for (i in 1:nrow(sample)) {
    M[i,] <- order(dist_matrix[i,])[1:k]
  }

  # calculate weights for centroids in M
  W <- matrix(rep(0,nrow(sample)*k), ncol = k)

  for (i in 1:nrow(sample)) {
    W[i,] <- exp(-dist_matrix[i,M[i,]]^2 / (2*min(dist_matrix[i,M[i,]]^2)))
  }

  # normalize weights
  W <- W / rowSums(W)

  # calculate weighted mean of displacement vectors
  noisy_coords <- data.frame(sf::st_coordinates(noisy_centroids))
  v <- matrix(rep(0, 2*nrow(sample)), ncol = 2)
  for (i in 1:nrow(v)) {
    x <- sum((sample[i,1] - noisy_coords[M[i,],1]) * W[i,])
    y <- sum((sample[i,2] - noisy_coords[M[i,],2]) * W[i,])
    v[i,] <- c(x,y)
  }

  # calculate new boundary points
  new_coords <- data.frame(sf::st_coordinates(new_centroids))
  new_boundary <- matrix(rep(0, 2*nrow(sample)), ncol = 2)
  R <- length(noisy_centroids)
  A <- sum(sf::st_area(data))
  s <- as.numeric(sqrt(A/R))

  for (i in 1:nrow(sample)) {
    weighted_centroids <- new_coords[M[i,],] * W[i,]
    x <- sum(weighted_centroids$X)
    y <- sum(weighted_centroids$Y)
    new_boundary[i,] <- v[i,] * sqrt(s / sqrt(sum(v[i,]^2))) + c(x,y)
  }

  # convert new boundary points to polygon
  new_boundary_coords <- data.frame(new_boundary)
  colnames(new_boundary_coords) <- c("X","Y")
  new_boundary_coords$L1 <- sample$L1

  coords_list <- list()
  for (i in 1:length(sample_groups)) {
    points <- new_boundary_coords[which(new_boundary_coords$L1 == sample_groups[i]),1:2]
    coords_list[[i]] <- as.matrix(points)
  }

  new_boundary <- sf::st_sfc(sf::st_polygon(coords_list), crs = sf::st_crs(data))

  if (!sf::st_is_valid(new_boundary)) {
    new_boundary <- sf::st_make_valid(new_boundary)
    if ("sfc_GEOMETRYCOLLECTION" %in% class(new_boundary)) {
      new_boundary <- sf::st_collection_extract(new_boundary, "POLYGON")
    }
  }

  new_boundary
}



