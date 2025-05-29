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

map_populations <- function(setplot, populations, interactive = TRUE, zoom=c(22,32), webgl = TRUE) {


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


  make_even_paler <- function(color) {
    grDevices::adjustcolor(color, alpha.f = 1, red.f = 5, green.f = 5, blue.f = 5)
  }
  sp_pal_base <- sp_pal
  sp_pal_bleached <- sp_pal
  sp_pal_bleached <- sapply(sp_pal_bleached, make_even_paler)

  populations <- populations |>
    dplyr::mutate(
      species = as.character(species),
      color = dplyr::recode(species, !!!sp_pal_base),
      color = ifelse(status == "bleached", dplyr::recode(species, !!!sp_pal_bleached), color)
    ) |> mutate(species = factor(species, levels=c("Acropora hyacinthus",
                                                  "Acropora cytherea",
                                                  "Acropora intermedia",
                                                  "Acropora robusta",
                                                  "Acropora millepora",
                                                  "Acropora nasuta",
                                                  "Acropora spathulata",
                                                  "Acropora humilis",
                                                  "Acropora cf. digitifera",
                                                  "Goniastrea pectinata",
                                                  "Goniastrea retiformis"))
                ) |>
    dplyr::mutate(growthform=forcats::fct_recode(species, "Tabular"= "Acropora cytherea", "Tabular"= "Acropora hyacinthus",
                                                 "Branching"= "Acropora intermedia", "Branching"= "Acropora robusta",
                                                 "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
                                                 "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora", "Corymbose" = "Acropora spathulata",
                                                 "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis")) |>
    dplyr::mutate(growthform = factor(growthform, levels=c("Tabular", "Branching", "Corymbose", "Digitate", "Massive"))) |>
    dplyr::arrange(rev(growthform), rev(width))


  # Map
  if (isTRUE(interactive)) {
    # suppressMessages(tmap::tmap_mode("view"))
    # tmapoutput <-
    #   tmap::tm_shape(setplot) +
    #   tmap::tm_borders("black", lwd = 0.1) +
    #   tmap::tm_shape(populations) +
    #   tmap::tm_polygons(fill = "species",
    #                     fill.scale = tmap::tm_scale_categorical(values=sp_pal),
    #                     col="black", fill_alpha = 0.7) +
    #   #tmap::tm_borders(col = "black", lwd = 0.5) +
    #   tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "top") +
    #   tmap::tm_view(set_zoom_limits = c(22, 30))
    #   #tmap::tm_view(set_zoom_limits = c(22, 30), view.set.up = list(webgl = webgl))
    #
    # tmapoutput <- tmap::tmap_leaflet(tmapoutput)

    setplot_wgs <- sf::st_transform(setplot, 4326)
    populations_wgs <- sf::st_transform(populations, 4326)

    # Make sure species and color are character/factor
    species_levels <- unique(populations_wgs$species)
    species_colors <- unique(populations_wgs$color)

    # Build named palette
    pal <- leaflet::colorFactor(
      palette = sp_pal,
      domain = populations_wgs$species
    )

    leafletoutput <- leaflet::leaflet(
      options = leaflet::leafletOptions(
        crs = leaflet::leafletCRS(crsClass = "L.CRS.EPSG3857"),
        minZoom = 22,
        maxZoom = 30,
        zoomControl = TRUE
      )
    ) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        data = setplot_wgs,
        color = "black",
        weight = 0.1,
        fill = FALSE
      ) |>
      leaflet::addPolygons(
        data = populations_wgs,
        fillColor = ~color,
        color = "black",
        weight = 0.5,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Species:</b> <i>", species, "</i><br/>",
          "<b>Width (cm):</b> ", round(width, 1), "<br/>",
          "<b>Area (m²):</b> ", round(area, 3),
          if ("output" %in% names(populations_wgs)) paste0("<br/><b>Output:</b> ", round(output, 1)) else ""
         )
        ) |>
      leaflet::addLegend(
        "topright",
        pal = pal,
        values = populations_wgs$species,
        title = "Taxa"
      ) |>
      leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)


    return(leafletoutput)

  } else if (isFALSE(interactive)) {
    suppressMessages(tmap::tmap_mode("plot"))
    tmapoutput <- tmap::tm_shape(setplot) +
      tmap::tm_borders("black", lwd = 0.1) +
      tmap::tm_shape(populations, is.master = TRUE) +
      tmap::tm_fill(fill="color",
                    col="black",
                    col_alpha=0.5,
                    fill_alpha = 0.9) +
      tmap::tm_borders(col = "black", lwd = 0.5) +
      tmap::tm_add_legend(
        title = "Taxa",
        fill = as.character(sp_pal),
        shape = 22,
        size = 1,
        position=c("right", "top"),
        labels = names(sp_pal)
      ) +
      tmap::tm_options(component.autoscale = FALSE)
    return(tmapoutput)
  }


}
