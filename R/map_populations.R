#' Map Coral Populations with Species Coloring
#'
#' Plots a spatial map of coral populations over a setplot base layer using `tmap`.
#' Species are color-coded with a customizable palette, and output can be either
#' interactive (web) or static. Optionally enables or disables WebGL for 3D rendering
#' in interactive view.
#'
#' @param setplot An `sf` or `SpatVector` object representing the background plot layer (e.g. reef tiles).
#' @param populations An `sf` object with point geometries and a `species` column to map.
#' @param interactive Logical; if `TRUE`, the map is shown in an interactive web view. If `FALSE`, it renders as a static plot.
#' @param webgl Logical; if `interactive = TRUE`, sets WebGL rendering mode in `tm_view()`.
#'
#' @return A `tmap` object
#' @export
#'
#' @examples
#' \dontrun{
#' grid <- setplot(10, 5)
#' map_populations(setplot = grid, populations = coral_points, interactive = TRUE, webgl = FALSE)
#' }

map_populations <- function(setplot, populations, interactive = TRUE, webgl = TRUE) {

  sp_pal <- c(
    "Acropora hyacinthus" = "#50676c", "Acropora cytherea" = "#3a6c8e",
    "Acropora intermedia" = "#2c3687", "Acropora robusta" = "#7b8ca8",
    "Acropora spathulata" = "#98a062", "Acropora millepora" = "#665a43",
    "Acropora nasuta" = "#48642f", "Acropora humilis" = "#824b3b",
    "Acropora cf. digitifera" = "#a47c73", "Goniastrea pectinata" = "#999999",
    "Goniastrea retiformis" = "#bfbfbf"
  )

  make_paler <- function(color) {
    grDevices::adjustcolor(color, alpha.f = 1, red.f = 1.2, green.f = 1.2, blue.f = 1.2)
  }

  sp_pal <- sapply(sp_pal, make_paler)

  if (isTRUE(interactive)) {
    tmap::tmap_mode("view")

    tmapoutput <-
      tmap::tm_shape(setplot) +
      tmap::tm_borders("black", lwd = 0.1) +
      tmap::tm_shape(populations, is.master = TRUE) +
      tmap::tm_fill("species", tmap::tm_scale_categorical(values = sp_pal), fill_alpha = 0.9) +
      tmap::tm_borders(col = "black", lwd = 0.5) +
      tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "top") +
      tmap::tm_view(set_zoom_limits = c(22, 30), view.set.up = list(webgl = webgl))

    return(tmapoutput)

  } else {
    tmap::tmap_mode("plot")

    tmapoutput <-
      tmap::tm_shape(setplot) +
      tmap::tm_borders("black", lwd = 0.1) +
      tmap::tm_shape(populations, is.master = TRUE) +
      tmap::tm_fill("species",
                    tmap::tm_scale_categorical(values = sp_pal),
                    fill.legend = tmap::tm_legend(position = tmap::tm_pos_out("right", "center")),
                    fill_alpha = 0.9) +
      tmap::tm_borders(col = "black", lwd = 0.5) #+
     #tmap::tm_layout(legend.position = c("center", "bottom"))
#      tmap::tm_layout(legend.outside = TRUE, legend.outside.position = c("center", "bottom"))

    return(tmapoutput)
  }
}
