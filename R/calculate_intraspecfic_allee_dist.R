#' Calculate Intraspecific Pairwise Colony Distances as sf LINESTRINGs
#'
#' Computes intraspecific pairwise distances between coral colony centroids and derives
#' edge-to-edge and maximum distances. Results are returned as `sf` LINESTRINGs connecting colony centroids.
#'
#' @param subset An `sf` object with polygon geometries, and columns `species`, `id`, and `width` (diameter in cm).
#' @param label A character label (e.g. "bleached", "unbleached") to annotate each line.
#'
#' @return An `sf` object of LINESTRINGs with columns:
#' \describe{
#'   \item{species}{Species name.}
#'   \item{id_from}{ID of origin colony.}
#'   \item{id_to}{ID of target colony.}
#'   \item{centroid_distance}{Distance between colony centroids (in CRS units).}
#'   \item{colony_distance}{Edge-to-edge distance (centroid distance minus radii).}
#'   \item{max_distance}{Maximum distance including both radii.}
#'   \item{status}{Label passed to `label` argument.}
#' }
#'
#' @importFrom sf st_centroid st_coordinates st_linestring st_crs st_sfc st_sf st_distance
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map2 map_dfr
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_intraspecific_allee_dist(coral_sf, label = "unbleached")
#' }

calculate_intraspecific_allee_dist <- function(subset, label, ...) {
  split_by_species <- split(subset, subset$species)

  purrr::map_dfr(split_by_species, function(sp) {
    if (nrow(sp) < 2) return(NULL)

    centroids <- suppressWarnings(sf::st_centroid(sp))
    coords <- sf::st_coordinates(centroids)
    dist_matrix <- sf::st_distance(centroids)

    dist_df <- as.data.frame(as.table(as.matrix(dist_matrix)))
    colnames(dist_df) <- c("from", "to", "centroid_distance")

    dist_df <- dist_df |>
      dplyr::filter(from != to) |>
      dplyr::mutate(
        from = as.integer(from),
        to = as.integer(to),
        species = sp$species[1],
        id_from = sp$id[from],
        id_to = sp$id[to],
        centroid_distance = as.numeric(centroid_distance),
        radius_from = (sp$width[from] / 2) / 100,
        radius_to = (sp$width[to] / 2) / 100,
        min_distance = pmax(centroid_distance - (radius_from - radius_to), 0),
        max_distance = pmax(centroid_distance + (radius_from + radius_to), 0),
        status = label,
        geometry = purrr::map2(from, to, ~sf::st_linestring(rbind(coords[.x, ], coords[.y, ])))
      )

    sf::st_sf(
      dist_df |> dplyr::select(species, id_from, id_to, centroid_distance, min_distance, max_distance, status),
      geometry = sf::st_sfc(dist_df$geometry, crs = sf::st_crs(sp))
    )
  })
}
