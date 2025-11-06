#' Generate a Single Tile Map
#'
#' Generate a single square or hexagon tile map.
#'
#' Implements an algorithm for generating tile maps proposed in
#' \emph{"Generating Tile Maps"} (McNeill and Hale 2017). The regions of the
#' map must be contiguous. Coordinates cannot be in terms of latitude and
#' longitude. Instead the coordinate reference system must be an appropriate
#' planar projection.
#'
#' @param data An object of class \code{sfc_MULTIPOLYGON} or
#'   \code{sfc_POLYGON}, which contains the regions that make up the original
#'   map.
#' @param square logical. If \code{TRUE}, generates a square tile map. If
#'   \code{FALSE}, generates a hexagon tile map.
#' @param flat_topped logical. If \code{TRUE}, hexagons are flat-topped. If
#'   \code{FALSE}, hexagons are pointy-topped.
#' @param prop A proportion used in specifying the standard deviation of
#'   the Gaussian noise added to original region centroids. The standard
#'   deviation of the Gaussian noise is calculated as the mean distance between
#'   a region centroid and its neighboring regions' centroids multiplied by the
#'   value provided for the \code{prop} argument.
#' @param interpolate A number between 0 and 1 controlling the linear
#'   interpolation between the noisy region centroids and fully-transformed
#'   region centroids. If 0, noisy region centroids are used. If 1,
#'   fully-transformed centroids are used.
#' @param smoothness numeric. Controls the bandwidth of the Gaussian kernel
#'   used for smoothing the transformed boundary polygon. The bandwidth is
#'   calculated as the mean distance between adjacent boundary points
#'   multiplied by the value provided for the \code{smoothness} argument.
#' @param shift A numeric vector of length two specifying the number of grid
#'   steps to shift the candidate tile map in the x and y directions before
#'   counting the number of tile centroids that lie within the transformed
#'   boundary.
#'
#' @examples
#' library(sf)
#' northeast <- governors[c(6,7,17,18,19,27,28,30,36,37,43),]
#' northeast$tile_map <- generate_map(northeast$geometry, square = FALSE,
#'                                    flat_topped = TRUE)
#'
#' @return Returns an object of class \code{sfc_POLYGON}, containing the tiles of
#'   the tile map in the same order as the original regions given to the
#'   function.
#'
#' @references McNeill, Graham, and Scott A Hale. 2017. “Generating Tile Maps.”
#'  In \emph{Computer Graphics Forum}, 36:435–45. 3. Wiley Online Library.
#'
#' @export

generate_map <- function(data, square = TRUE, flat_topped = FALSE, prop = 0,
                         interpolate = 1, smoothness = 0, shift = c(0,0)) {
  # get crs
  crs <- sf::st_crs(data)

  # estimate grid step size
  R <- length(data)
  A <- sum(sf::st_area(data))
  s <- as.numeric(sqrt(A/R))

  # find set of neighbors
  neighbors <- sf::st_touches(data)

  # check if regions are contiguous
  neighbor_matrix <- as.matrix(neighbors)
  neighbor_graph <- igraph::graph_from_adjacency_matrix(neighbor_matrix)
  neighbor_search <- igraph::bfs(neighbor_graph, 1, mode = "all",
                                 unreachable = FALSE)$order
  if (length(neighbor_search) != R) {
    stop("regions are not contiguous")
  }

  # STEP 1 - transform centroids
  centroids <- transform_centroids(data, neighbors, crs, s, prop)
  noisy_centroids <- centroids$noisy_centroids
  transformed_centroids <- centroids$transformed_centroids
  transformed_centroids <- interpolate_centroids(noisy_centroids,
                                                 transformed_centroids, crs,
                                                 interpolate)

  # STEP 2 - transform boundary
  transformed_boundary <- transform_boundary(data, noisy_centroids,
                                             transformed_centroids)
  if (smoothness != 0) {
    transformed_boundary <- smoothr::smooth(transformed_boundary,
                                            method = "ksmooth",
                                            smoothness = smoothness)
  }

  # STEP 3 - fit tiles to boundary
  grid <- fit_tiles(transformed_boundary, R, s, square, flat_topped, shift)

  # STEP 4 - assign regions to tiles
  tile_centroids <- sf::st_centroid(grid)
  perm <- assign_regions(transformed_centroids, tile_centroids)
  grid <- grid[order(perm)]

  # output tile map in order of original data
  grid

}
