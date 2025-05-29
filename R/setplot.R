#' Generate a Rectangular sf Plot Grid
#'
#' Creates a simple rectangular spatial plot of specified `length` and `width` in meters,
#' and returns a 1m x 1m `sf` grid overlaid on that area. CRS is EPSG:3857 by default.
#'
#' @param length Numeric. Length of the plot in meters (X-dimension).
#' @param width Numeric. Width of the plot in meters (Y-dimension).
#' @param crs Integer. Coordinate Reference System (default = 3857).
#'
#' @return An `sf` object representing a grid of 1m x 1m cells covering the defined plot.
#'
#' @examples
#' grid <- setplot(10, 5)
#' plot(grid)
#'
#' @export

setplot <- function(length, width, crs = 3857, quiet = TRUE) {
  coords <- matrix(c(0, 0, length, 0, length, width, 0, width, 0, 0),
                   ncol = 2, byrow = TRUE)

  sf_plot <- sf::st_polygon(list(coords)) |>
    sf::st_sfc(crs = crs) |>
    sf::st_sf() |>
    dplyr::rename(plot = 1)

  grid <- sf::st_make_grid(sf_plot, cellsize = c(1, 1), crs = crs)



  if(isFALSE(quiet)){
    cat(paste0(length, " x ", width, " m plot\n"))
    print(sum(sf::st_area(grid)))
  }

  return(grid)
}


