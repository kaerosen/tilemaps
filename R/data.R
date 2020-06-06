#' Party Affiliation of US Governors
#'
#' A dataset containing the political party affiliation of the governors of the
#' contiguous United States (as of May 2020), as well as an \code{sfc} object
#' representing the states.
#'
#' @format \code{sf} data frame with 48 observations and 3 variables:
#' \describe{
#'   \item{geometry}{\code{sfc_MULTIPOLYGON} object representing states}
#'   \item{abbreviation}{state abbreviations}
#'   \item{party}{political party affiliation of state governor}
#' }
#'
#' @source \code{spData::us_states}
#' (\url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html})
#'
#' \url{https://www.nga.org/governors/}
#'
"governors"
