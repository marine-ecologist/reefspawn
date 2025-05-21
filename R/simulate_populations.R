#' Simulate Coral Population Layout from Posterior Predictions
#'
#' Generates a spatial coral population layout by drawing from a `brms`-fit model of colony sizes,
#' filtering to match aerial coverage constraints, and laying out colonies with optional circle packing.
#'
#' @param setplot An `sf` polygon representing the area in which colonies will be positioned.
#' @param size A data frame containing `species` and size distribution (`area` or `width`).
#' @param community A data frame containing `species` and `aerial` (m²) coverage values.
#' @param brms A `brmsfit` object used to generate posterior predictions of colony size by species.
#' @param return Character. `"df"` returns the raw colony list; `"sf"` returns spatial polygons. Default: `"df"`.
#' @param algorithm Character. `"repel"` (default) or `"progressive"` — determines the circle packing layout method.
#' @param ndraws Integer. Number of posterior predictive draws to simulate. Default: 50000.
#' @param seed Optional integer for reproducibility of sampling and layout.
#' @param plot Logical. If `TRUE`, renders a `tmap` plot of the resulting layout. Default: `FALSE`.
#' @param interactive Logical. If `TRUE`, renders a `tmap` interactive plot of the resulting layout. Default: `FALSE`.
#' @param quiet verbose
#'
#' @return Either a data frame of simulated colonies (`df`) or an `sf` object of spatial polygons (`sf`) depending on `return`.
#'
#' @details
#' - Species names are harmonized using `fct_recode`.
#' - Aerial coverage is enforced using cumulative area filtering.
#' - `circleRepelLayout()` uses an iterative physics-based algorithm, while `circleProgressiveLayout()` is faster and approximate.
#' - Coordinates are assigned using `sf::st_sample()` and radii are scaled to match overlap assumptions.
#'
#' @examples
#' \dontrun{
#' simulate_populations(setplot = myreef_sf, size = colony_data, community = community_df,
#'                      brms = size_model, return = "sf", algorithm = "repel", seed = 42, plot = TRUE)
#' }
#'
#' @export

simulate_populations <- function(setplot, size, community, brms=posterior_coeffs_sizedistribution, return = "df",
                                 algorithm = "repel", ndraws = 20000, seed = NULL,
                                 quiet = TRUE, plot = FALSE, interactive=FALSE, ...) {

  t0 <- tictoc::tic(quiet = TRUE)

  if (!is.null(seed)) {
    set.seed(seed)
  }


  posteriordraws <- posterior_predict(brm_polyp_density, ndraws=ndraws, newdata = data.frame(species=unique(coralsize$species)))

  # generate predictions in long_format
  preds <- posteriordraws |>
    as.data.frame() |>
    dplyr::filter_if(is.numeric, dplyr::all_vars(. >= 0)) |>
    magrittr::set_colnames(unique(coralsize$species)) |>
    tidyr::pivot_longer(everything(), names_to="species", values_to="width") |>
    dplyr::arrange(species) |>
    dplyr::mutate(species=as.factor(species)) |>
    mutate(species = fct_recode(species,  "Acropora digitifera" = "Acropora cf. digitifera")) |>
    dplyr::mutate(area=pi*((width/2)^2)) |>
    dplyr::group_by(species) |>
    dplyr::mutate(cumulative_area = cumsum(area))


  aerial_coverage <- community |>
    dplyr::select(species, aerial) |>
    mutate(aerial = as.numeric(aerial)*10000) # convert from m2 units to cm2

  coral_list_initial <- dplyr::left_join(preds, aerial_coverage, by="species")



  coral_list <- coral_list_initial %>%
    dplyr::mutate(drop=ifelse(as.numeric(cumulative_area) < (as.numeric(aerial)), 1, NA)) %>%
    tidyr::drop_na() |>
    dplyr::select(-drop) |>
    dplyr::group_by(species) %>%
    dplyr::mutate(id=seq(1:dplyr::n()))

  if (return=="df"){
    if (isFALSE(quiet)){
      elapsed <- tictoc::toc(quiet = TRUE)
      print(paste0(elapsed$callback_msg, " - simulate_populations()"))
    }
    return(coral_list)
  }

  # species heirarchy
  species_hierarchy <- rev(c("Acropora hyacinthus", "Acropora cytherea",
                             "Acropora intermedia", "Acropora robusta",
                             "Acropora millepora", "Acropora nasuta", "Acropora spathulata",
                             "Acropora humilis", "Acropora cf. digitifera",
                             "Goniastrea pectinata", "Goniastrea retiformis"))

  colony.xy <- setplot |> sf::st_sample(nrow(coral_list)) |> sf::st_as_sf() |> dplyr::rename(coords=1)

  spatial.population <- coral_list |>
    dplyr::bind_cols(colony.xy) |>
    dplyr::mutate(species=as.factor(species)) |>
    dplyr::mutate(species=forcats::fct_relevel(species, species_hierarchy[species_hierarchy %in% levels(species)])) |> # here add a fct_relevel hack where present
    dplyr::mutate(zindex_id=id) |>
    dplyr::arrange(dplyr::desc(species))

  # set overlap between colonies at random using a normal distribution
  set_overlap <- rnorm(nrow(spatial.population),90,5) #%
  set_overlap <- ifelse(set_overlap > 100, 100, set_overlap)

  ### shuffle df
  spatial.population.xyz <- spatial.population %>%
    dplyr::ungroup() %>%
    dplyr::mutate(species.id=paste0(species,"-",id)) %>%
    dplyr::mutate(x = sf::st_coordinates(coords)[, 'X'], y = sf::st_coordinates(coords)[, 'Y']) |>
    dplyr::mutate(area=area/10000) |>
    dplyr::mutate(overlaparea=(set_overlap/100)*area) |>
    dplyr::mutate(species = factor(species, levels = rev(levels(species)))) |>
    dplyr::arrange(species) |>
    as.data.frame()

  ### run algorithm

  if (algorithm=="repel"){
  coral_output <- packcircles::circleRepelLayout(spatial.population.xyz |> dplyr::select(x, y, overlaparea),
                                                 xlim = c(0, sf::st_bbox(sf_plot)[3]),
                                                 ylim = c(0, sf::st_bbox(sf_plot)[4]),
                                                 xysizecols = 1:3,
                                                 wrap=FALSE)

  ### convert algorithm output to polygon
  ### note above we reduced radius to 0.8 so now increase it to 1.25

  coral_output <- coral_output$layout %>%
    as.data.frame() |>
    dplyr::mutate(radius=((1/set_overlap)*100) * radius)

  # get vertices and make into sf polygone
  coral_output <- packcircles::circleLayoutVertices(coral_output) %>%
    sf::st_as_sf(coords=c("x", "y"), crs=3857)  %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::bind_cols(spatial.population.xyz |> dplyr::select(-id, aerial, zindex_id, cumulative_area))

  coral_output <- coral_output |>
    dplyr::arrange((as.factor(species)))

  } else if (algorithm=="progressive") {


    # Step 1: Generate layout using progressive method

    spatial.population.xyz <- spatial.population.xyz[sample(nrow(spatial.population.xyz)), ] # randomise rows

    coral_layout <- packcircles::circleProgressiveLayout(spatial.population.xyz$overlaparea)

    # Step 2: Normalize x/y into bounding box
    xlim <- c(0, sf::st_bbox(sf_plot)[3])
    ylim <- c(0, sf::st_bbox(sf_plot)[4])

    coral_layout$x <- scales::rescale(coral_layout$x, to = xlim)
    coral_layout$y <- scales::rescale(coral_layout$y, to = ylim)

    # Step 3: Adjust radius scale (if you previously scaled it by 0.8, now invert with 1.25)
    # You must define `set_overlap` (e.g., if radius was originally reduced by 0.8, then set_overlap = 0.8)
    set_overlap <- 0.8
    coral_layout$radius <- coral_layout$radius * (1 / set_overlap)

    # Step 4: Convert layout to circle polygons
    layout_df <- coral_layout |>
      dplyr::mutate(id = row_number()) # add ID to track

    circle_polygons <- packcircles::circleLayoutVertices(layout_df)

    # Step 5: Convert to sf polygon and attach attributes
    coral_output <- circle_polygons |>
      sf::st_as_sf(coords = c("x", "y"), crs = 3857) |>
      dplyr::group_by(id) |>
      dplyr::summarise(geometry = sf::st_combine(geometry), .groups = "drop") |>
      sf::st_cast("POLYGON") |>
      dplyr::bind_cols(
        spatial.population.xyz |>
          dplyr::slice(layout_df$id) |>
          dplyr::select(-id, aerial, zindex_id, cumulative_area)
      ) |>
      dplyr::arrange(as.factor(species))

  }

  if (return=="sf"){
    if (isFALSE(quiet)){
      elapsed <- tictoc::toc(quiet = TRUE)
      print(paste0(elapsed$callback_msg, " - simulate_populations()"))
    }
    return(coral_output)
  }

}
