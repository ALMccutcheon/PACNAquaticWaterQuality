#' Coordinates to Columns
#'
#' Extracts coordinates from the sf object and put it in separate columns for X and Y
#'
#' @param x the sf table that needs to be separated
#' @param names the column names that will be used for the coordinates
#' @return returns a table with two columns for x and y coordinates
#' @export

sfc_as_cols <-
function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- stats::setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
