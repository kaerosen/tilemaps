#' Create a Tile for an Island
#'
#' Create a tile for an island that can be added to an existing tile map
#' layout.
#'
#' Creates a single tile of the same shape and size as the tiles in the given
#' tile map. This tile can be added to the layout of the given tile map to
#' represent an island or region that is not connected to the region
#' represented by the given tile map. The location of the new tile is
#' determined by the \code{position} argument. Setting the \code{position}
#' argument equal to "upper left", "lower left", "upper right", or "lower
#' right" will generate a tile which is located in the specified corner of the
#' given tile map. Setting the \code{position} argument to a numeric vector of
#' length 2 will generate a tile whose centroid is located at the coordinates
#' given in the vector.
#'
#' @param tile_map An \code{sfc_POLYGON} object representing the layout of a
#'   tile map.
#' @param position Either a numeric vector of length 2 giving the coordinates
#'   for the centroid of the new tile, or a string equal to "upper left",
#'   "lower left", "upper right", or "lower right" indicating in which corner
#'   of the original tile map the new tile should be located.
#'
#' @examples
#' library(sf)
#' northeast <- governors[c(6,7,17,18,19,27,28,30,36,37,43),]
#' tile_map <- generate_map(northeast$geometry, square = FALSE)
#' tile_map <- append(tile_map, create_island(tile_map, "lower right"))
#'
#' @return Returns an object of class \code{sfc_POLYGON} representing a single
#'   tile of the same shape and size as the tiles in the original tile map.
#'
#' @export

create_island <- function(tile_map, position) {

  # check if input is a tile map
  min_area <- min(sf::st_area(tile_map))
  max_area <- max(sf::st_area(tile_map))
  if (as.numeric(max_area - min_area) > 1) {
    stop("tiles are not the same size")
  }

  # determine tile shape
  shape <- "unknown"
  if (lengths(tile_map[[1]]) == 10) {
    shape <- "square"
  } else if (lengths(tile_map[[1]]) == 14) {
    if (length(unique(sf::st_coordinates(tile_map[[1]])[,1])) == 4) {
      shape <- "flat"
    } else if (length(unique(sf::st_coordinates(tile_map[[1]])[,1])) == 3) {
      shape <- "pointy"
    }
  }
  if (shape == "unknown") {
    stop("tile shape not recognized")
  }

  # find tile side length
  area <- as.numeric(sf::st_area(tile_map)[1])
  if (shape == "square") {
    side_length <- sqrt(area)
  } else {
    side_length <- sqrt(2*area / (3*sqrt(3)))
  }

  # determine centroid of island tile
  if (inherits(position, "character")) {
    bounds <- sf::st_bbox(tile_map)
    xmin <- bounds[1]
    ymin <- bounds[2]
    xmax <- bounds[3]
    ymax <- bounds[4]
    if (position == "upper left") {
      centroid <- c(xmin + .5*side_length, ymax - .5*side_length)
    } else if (position == "lower left") {
      centroid <- c(xmin, ymin) + .5*side_length
    } else if (position == "upper right") {
      centroid <- c(xmax, ymax) - .5*(side_length)
    } else if (position == "lower right") {
      centroid <- c(xmax - .5*side_length, ymin + .5*side_length)
    } else {
      stop('position argument must be numeric vector of length 2 or a string
           equal to "upper left", "lower left", "upper right", "lower right"')
    }
  } else if (inherits(position, "numeric") & length(position) == 2) {
    centroid <- position
  } else {
    stop('position argument must be numeric vector of length 2 or a string
         equal to "upper left", "lower left", "upper right", "lower right"')
  }

  # create new tile
  if (shape == "square") {
    coords <- matrix(rep(0, 10), ncol = 2)
    coords[1,] <- centroid - .5*side_length
    coords[2,] <- centroid + .5*c(side_length, -side_length)
    coords[3,] <- centroid + .5*side_length
    coords[4,] <- centroid + .5*c(-side_length, side_length)
    coords[5,] <- centroid - .5*side_length
  } else if (shape == "flat") {
    coords <- matrix(rep(0, 14), ncol = 2)
    coords[1,] <- centroid - c(side_length,0)
    coords[2,] <- centroid - c(.5*side_length, sqrt(3)/2*side_length)
    coords[3,] <- centroid + c(.5*side_length, -sqrt(3)/2*side_length)
    coords[4,] <- centroid + c(side_length,0)
    coords[5,] <- centroid + c(.5*side_length, sqrt(3)/2*side_length)
    coords[6,] <- centroid + c(-.5*side_length, sqrt(3)/2*side_length)
    coords[7,] <- centroid - c(side_length,0)
  } else {
    coords <- matrix(rep(0, 14), ncol = 2)
    coords[1,] <- centroid - c(0,side_length)
    coords[2,] <- centroid - c(sqrt(3)/2*side_length, .5*side_length)
    coords[3,] <- centroid + c(-sqrt(3)/2*side_length, .5*side_length)
    coords[4,] <- centroid + c(0,side_length)
    coords[5,] <- centroid + c(sqrt(3)/2*side_length, .5*side_length)
    coords[6,] <- centroid + c(sqrt(3)/2*side_length, -.5*side_length)
    coords[7,] <- centroid - c(0,side_length)
  }

  island_tile <- sf::st_sfc(sf::st_polygon(list(coords)),
                            crs = sf::st_crs(tile_map))

  island_tile

}
