#' Generate Many Tile Maps
#'
#' Generate, plot, and compare many tile maps.
#'
#' Generates many candidate tile maps using an algorithm proposed in
#' \emph{"Generating Tile Maps"} (McNeill and Hale 2017). The regions of the
#' map must be contiguous. Coordinates cannot be in terms of latitude and
#' longitude. Instead the coordinate reference system must be an appropriate
#' planar projection. The number of maps generated is equal to the product of
#' the lengths of the \code{prop}, \code{interpolate}, \code{smoothness}, and
#' \code{shift} arguments.
#'
#' @param data An object of class \code{sfc_MULTIPOLYGON} or
#'   \code{sfc_POLYGON}, which contains the regions that make up the original
#'   map.
#' @param labels A character vector with the labels of the regions. Labels must
#'   be in the same order as regions given for \code{data} argument.
#' @param square logical. If \code{TRUE}, generates a square tile map. If
#'   \code{FALSE}, generates a hexagon tile map.
#' @param flat_topped logical. If \code{TRUE}, hexagons are flat-topped. If
#'   \code{FALSE}, hexagons are pointy-topped.
#' @param prop A numeric vector of proportions used in specifying the standard
#'   deviation of the Gaussian noise added to original region centroids. The
#'   standard deviation of the Gaussian noise is calculated as the mean
#'   distance between a region centroid and its neighboring regions' centroids
#'   multiplied by the value provided for the \code{prop} argument. A different
#'   set of noisy region centroids is created for each given value.
#' @param interpolate A numeric vector of values between 0 and 1 controlling
#'   the linear interpolation between the noisy region centroids and
#'   fully-transformed region centroids. If 0, noisy region centroids are used.
#'   If 1, fully-transformed centroids are used. A different set of
#'   interpolated centroids is created for each given value.
#' @param smoothness numeric vector. Controls the bandwidth of the Gaussian
#'   kernel used for smoothing the transformed boundary polygon. The bandwidth
#'   is calculated as the mean distance between adjacent boundary points
#'   multiplied by the value provided for the \code{smoothness} argument. A
#'   different transformed boundary is created for each given value.
#' @param shift A list of numeric vectors of length two specifying the number
#'   of grid steps to shift the candidate tile map in the x and y directions
#'   before counting the number of tile centroids that lie within the
#'   transformed boundary. A different final tile map is created for each given
#'   value.
#' @param weights A numeric vector of length 4 specifying the weights used for
#'   calculating the total cost. The first, second, third, and fourth weights
#'   are applied to the location, adjacency, angle, and roughness costs,
#'   respectively.
#' @param plot logical. If \code{TRUE}, prints plot of generated tile maps.
#' @param size numeric. Controls size of labels in plot.
#'
#' @examples
#' library(sf)
#' northeast <- governors[c(6,7,17,18,19,27,28,30,36,37,43),]
#' ne_maps <- many_maps(northeast$geometry, northeast$abbreviation,
#'                      prop = 0, interpolate = 1, smoothness = c(0,20),
#'                      shift = list(c(0,0), c(0,0.5)))
#'
#' @return Returns a \code{data.frame} in which each row corresponds to one map
#'  and the columns contain the generated maps, the parameters used for
#'  creating the maps, and the costs associated with each map. The
#'  \code{data.frame} is ordered by the total cost.
#'
#' @references McNeill, Graham, and Scott A Hale. 2017. “Generating Tile Maps.”
#'  In \emph{Computer Graphics Forum}, 36:435–45. 3. Wiley Online Library.
#'
#' @export

many_maps <- function(data, labels, square = TRUE, flat_topped = FALSE,
                      prop = c(0, 0.05), interpolate = c(0.5, 1),
                      smoothness = c(0, 5),
                      shift = list(c(0,0), c(0.5,0), c(0,0.5)),
                      weights = c(1,1,1,1), plot = TRUE, size = 2) {

  num_maps <- length(prop) * length(interpolate) * length(smoothness) * length(shift)

  maps <- list()
  shift_param <- list()
  df <- data.frame(matrix(rep(0, num_maps*8), nrow = num_maps))
  colnames(df) <- c("prop", "interpolate", "smoothness", "location_cost",
                    "adjacency_cost", "angle_cost", "roughness_cost", "total_cost")
  index <- 1

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

  # get original centroids
  original_centroids <- sf::st_centroid(data)

  for (i in 1:length(prop)) {
    # STEP 1 - transform centroids
    centroids <- transform_centroids(data, neighbors, crs, s, prop[i])
    noisy_centroids <- centroids$noisy_centroids
    transformed_centroids <- centroids$transformed_centroids

    for (j in 1:length(interpolate)) {
      # interpolate centroids
      interpolated_centroids <- interpolate_centroids(noisy_centroids,
                                                      transformed_centroids,
                                                      crs, interpolate[j])

      # STEP 2 - transform boundary
      transformed_boundary <- transform_boundary(data, noisy_centroids,
                                                 interpolated_centroids)

      for (k in 1:length(smoothness)) {
        # smooth boundary
        if (smoothness[k] != 0) {
          smoothed_boundary <- smoothr::smooth(transformed_boundary,
                                               method = "ksmooth",
                                               smoothness = smoothness[k])
        } else {
          smoothed_boundary <- transformed_boundary
        }

        for (l in 1:length(shift)) {
          # STEP 3 - fit tiles to boundary
          grid <- fit_tiles(smoothed_boundary, R, s, square, flat_topped,
                            shift[[l]])

          # STEP 4 - assign regions to tiles
          tile_centroids <- sf::st_centroid(grid)
          perm <- assign_regions(interpolated_centroids, tile_centroids)
          grid <- grid[order(perm)]

          # calculate costs
          tile_neighbors <- sf::st_touches(grid)
          loc <- location_cost(interpolated_centroids, tile_centroids, s)
          adj <- adjacency_cost(neighbors, tile_neighbors)
          angle <- angle_cost(original_centroids, tile_centroids, neighbors)
          rough <- roughness_cost(square, grid)
          total_cost <- sum(c(loc,adj,angle,rough) * weights)

          maps[[index]] <- grid
          shift_param[[index]] <- shift[[l]]
          df[index, ] <- c(prop[i], interpolate[j], smoothness[k], loc, adj,
                           angle, rough, total_cost)
          index <- index + 1
        }
      }
    }
  }

  df$map <- maps
  df$shift <- shift_param
  df <- df[order(df$total_cost),c(9, 8, 1:3, 10, 4:7)]
  rownames(df) <- 1:nrow(df)

  if (plot) {
    print(plot_many_maps(df$map, labels, size))
  }

  df

}
