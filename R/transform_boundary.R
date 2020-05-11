# STEP 2 - Transform boundary polygon

transform_boundary <- function(data, noisy_centroids, new_centroids) {
  # take sample of original boundary points
  original_poly <- st_union(data)
  original_boundary <- st_boundary(original_poly)
  if (sum(class(original_boundary) == "sfc_MULTILINESTRING") == 0) {
    original_boundary <- st_cast(original_boundary, "MULTILINESTRING")
  }
  boundary_points <- st_cast(original_boundary, "MULTIPOINT")
  boundary_coords <- data.frame(st_coordinates(boundary_points))

  prop <- lengths(original_boundary[[1]]) / sum(lengths(original_boundary[[1]]))
  sample_size <- ceiling(prop * 1000)
  sample_groups <- which(sample_size > 3)

  subset <- boundary_coords %>%
    filter(L1 == sample_groups[1])
  if (sample_size[1] > nrow(subset)) {
    index <- 1:nrow(subset)
  } else {
    index <- c(1, 1:(sample_size[sample_groups[1]]-2) * floor(nrow(subset)/sample_size[sample_groups[1]]),
               nrow(subset))
  }

  sample <- subset[index,]
  if (length(sample_groups) > 1) {
    for (i in 2:length(sample_groups)) {
      subset <- boundary_coords %>%
        filter(L1 == sample_groups[i])
      if (sample_size[i] > nrow(subset)) {
        index <- 1:nrow(subset)
      } else {
        index <- c(1, 1:(sample_size[sample_groups[i]]-2) * floor(nrow(subset)/sample_size[sample_groups[i]]),
                   nrow(subset))
      }
      sample <- rbind(sample, subset[index,])
    }
  }

  samp_points <- st_sfc(st_multipoint(as.matrix(sample[,1:2])), crs = st_crs(data))
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
    W[i,] <- exp(as.numeric(-dist_matrix[i,M[i,]]^2 / (2*min(dist_matrix[i,M[i,]]^2))))
  }

  # normalize weights
  W <- W / rowSums(W)

  # calculate weighted mean of displacement vectors
  v <- samp_points
  for (i in 1:length(v)) {
    displacement <- unlist((samp_points[i] - noisy_centroids[M[i,]]) * W[i,])
    x <- sum(displacement[2*1:k - 1])
    y <- sum(displacement[2*1:k])
    v[i] <- st_point(c(x,y))
  }

  # calculate new boundary points
  new_boundary <- samp_points
  R <- length(noisy_centroids)
  A <- sum(st_area(data))
  s <- as.numeric(sqrt(A/R))

  for (i in 1:length(samp_points)) {
    weighted_centroids <- unlist(new_centroids[M[i,]] * W[i,])
    x <- sum(weighted_centroids[2*1:k - 1])
    y <- sum(weighted_centroids[2*1:k])
    new_boundary[i] <- v[i] * sqrt(s / norm(st_coordinates(v[i]), type = "f")) + c(x,y)
  }

  # convert new boundary points to polygon
  new_boundary_coords <- data.frame(st_coordinates(new_boundary))
  new_boundary_coords$L1 <- sample$L1

  coords_list <- list()
  for (i in 1:length(sample_groups)) {
    points <- new_boundary_coords %>%
      filter(L1 == sample_groups[i]) %>%
      select("X","Y")
    coords_list[[i]] <- as.matrix(points)
  }

  new_boundary <- st_sfc(st_polygon(coords_list), crs = st_crs(data))

  if (!st_is_valid(new_boundary)) {
    new_boundary <- st_make_valid(new_boundary)
    if ("sfc_GEOMETRYCOLLECTION" %in% class(new_boundary)) {
      new_boundary <- st_collection_extract(new_boundary, "POLYGON")
    }
  }

  new_boundary
}



