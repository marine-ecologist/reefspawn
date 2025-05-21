#' Simulate Coral Colonies in a Polygon
#'
#' This function simulates coral cover and colony distribution within a specified polygon.
#' It generates species-specific ratios, predicts coral colony sizes, and uses spatial algorithms
#' to distribute colonies without overlap.
#'
#' @param input An `sf` polygon object defining the plot area.
#' @param set_overlap A percentage overlap setting for colonies (randomized).
#' @param raw Logical. If `TRUE`, returns the raw data instead of spatial polygons.
#' @param seed An optional seed value for reproducibility.
#' @param crs The coordinate reference system (default is EPSG:3857).
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return An `sf` object containing non-overlapping coral colony polygons, or raw data if `raw = TRUE`.
#' @importFrom dplyr group_by mutate ungroup select arrange summarise left_join filter_if bind_cols
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom forcats fct_recode fct_relevel
#' @importFrom sf st_area st_sample st_as_sf st_coordinates st_combine st_cast
#' @importFrom magrittr %>%
#' @importFrom brms posterior_predict
#' @importFrom stats runif rnorm
#' @importFrom packcircles circleRepelLayout circleLayoutVertices
#' @export
#'
coralsim <- function(input, set_overlap, raw = FALSE, seed = NULL, crs = 3857, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate random ratios with noise
  ratios <- c(Tabular = 0.7, Digitate = 0.075, Corymbose = 0.125, Branching = 0.08, Massive = 0.02)
  ratio_var <- 0.2
  ran_ratios <- sapply(ratios, function(x) x + stats::runif(1, -x * ratio_var, x * ratio_var))
  ran_ratios <- ran_ratios / sum(ran_ratios)
  ratios_df <- data.frame(growthform = names(ran_ratios), ratio = ran_ratios)

  # Define species and assign growth forms
  species_cover <- tibble::tibble(
    species = c("Acropora hyacinthus", "Acropora cytherea", "Acropora cf. digitifera",
                "Acropora humilis", "Acropora nasuta", "Acropora spathulata",
                "Acropora millepora", "Acropora intermedia", "Acropora robusta",
                "Goniastrea pectinata", "Goniastrea retiformis")
  ) %>%
    dplyr::mutate(
      growthform = forcats::fct_recode(
        species,
        "Tabular" = "Acropora cytherea", "Tabular" = "Acropora hyacinthus",
        "Branching" = "Acropora intermedia", "Branching" = "Acropora robusta",
        "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
        "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora",
        "Corymbose" = "Acropora spathulata", "Digitate" = "Acropora cf. digitifera",
        "Digitate" = "Acropora humilis"
      )
    ) %>%
    dplyr::left_join(ratios_df, by = "growthform") %>%
    dplyr::group_by(growthform) %>%
    dplyr::mutate(assigned_ratio = stats::runif(dplyr::n(), 0, 1)) %>%
    dplyr::mutate(assigned_ratio = assigned_ratio / sum(assigned_ratio) * ratio) %>%
    dplyr::ungroup()


  # Generate coral cover with beta distribution
  rbeta_coral <- stats::rbeta(1, 12, 4)



  species_cover_random <- species_cover %>%
    dplyr::group_by(growthform) %>%
    dplyr::mutate(random_ratio = stats::runif(dplyr::n())) %>%
    dplyr::mutate(ratio = random_ratio / sum(random_ratio) * ratios[growthform]) %>%
    dplyr::select(-random_ratio) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalcover = rbeta_coral * 100) %>%
    dplyr::mutate(cover = totalcover * ratio) %>%
    dplyr::mutate(aerial = as.numeric(sum(sf::st_area(input))) * (cover / 100))

  # Get size distribution data
  brm_sizedistribution <- readRDS("/Users/rof011/coraldynamics/data/coralfecundity/brm_sizedistribution.RDS")

  # Generate predictions for coral sizes
  preds <- brms::posterior_predict(brm_sizedistribution, ndraws = 40000, newdata = data.frame(species = unique(species_cover$species))) %>%
    as.data.frame() %>%
    dplyr::filter_if(is.numeric, dplyr::all_vars(. >= 0)) %>%
    magrittr::set_colnames(unique(species_cover$species)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "species", values_to = "width") %>%
    dplyr::arrange(species) %>%
    dplyr::mutate(area = pi * ((width / 2)^2), cumulative_area = cumsum(area))

  aerial_coverage <- species_cover_random %>% dplyr::select(species, aerial)
  coral_list_initial <- dplyr::left_join(preds, aerial_coverage, by = "species")

  coral_list <- coral_list_initial %>%
    dplyr::mutate(aerial = aerial * 10000) %>%
    dplyr::mutate(drop = ifelse(cumulative_area < aerial, 1, NA)) %>%
    tidyr::drop_na() %>%
    dplyr::select(-drop) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(id = seq_len(dplyr::n()))

  if (raw) {
    return(coral_list)
  }

  # Generate spatial population and layout
  colony.xy <- input %>%
    sf::st_sample(nrow(coral_list)) %>%
    sf::st_as_sf() #%>%
    #dplyr::rename(coords = geometry)



  spatial_population <- dplyr::bind_cols(colony.xy, coral_list) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(coords = sf::st_coordinates(.)) %>%   # Extract coordinates
    as.data.frame()


  # Run layout algorithm and return final polygons
  non_overlap <- packcircles::circleRepelLayout(spatial_population %>% dplyr::select(coords.X, coords.Y, area), xysizecols = 1:3)
  non_overlap_plot <- packcircles::circleLayoutVertices(non_overlap$layout) %>%
    sf::st_as_sf(coords = c("coords.X", "coords.Y"), crs = crs) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON")

  return(non_overlap_plot)
}
