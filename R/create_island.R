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
  if (class(position) == "character") {
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
  } else if (class(position) == "numeric" & length(position) == 2) {
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
