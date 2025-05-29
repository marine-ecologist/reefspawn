#' Calculate Within-Species Pairwise Colony Distances with Status Label
#'
#' Computes centroid-to-centroid, edge-to-edge, and max-distance metrics
#' for colonies within each species, and returns a single combined data frame
#' with a `status` column indicating the source subset.
#'
#' @param populations An `sf` object with columns: `id`, `species`, `status`, `width`, and geometry.
#' @return A single `tibble` with columns:
#'         species, id_from, id_to, centroid_distance, colony_distance, max_distance, status.
#' @export
#'
calculate_allee_effects <- function(populations, metric = "centroid_distance", label, dist = Inf, ...) {
  require(sf)
  require(dplyr)
  require(purrr)


  all_effects <- calculate_intraspecific_allee_dist(populations, label) |>
    filter(!!sym(metric) <= dist)

  return(all_effects)


}
