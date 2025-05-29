#' Simulate Coral Community Composition
#'
#' Generates a simulated coral community composition by allocating aerial cover to species
#' based on user-defined morphological ratios and a total coral cover estimate, optionally drawn
#' from a beta distribution. Optionally plots species size distributions and assigned ratios.
#'
#' @param setplot An `sf` polygon object representing the plot area (used to calculate total area).
#' @param size A data frame with columns `species` and `area_cm2` containing size distributions.
#' @param ratio A named numeric vector with proportions assigned to each growth form
#'        (names must be: "Tabular", "Branching", "Corymbose", "Digitate", "Massive").
#' @param ratiovar Numeric. Proportional noise added to each growthform's ratio. Default is 0.2.
#' @param coralcover Numeric. Mean coral cover percentage (0–100). If `distribution = "beta"`,
#'        this is the mean used to sample actual coral cover.
#' @param distribution Character. If `"beta"`, coral cover is sampled from a beta distribution.
#'        Set to `NULL` to use `coralcover` directly.
#' @param alpha Shape parameter α for the beta distribution (default = 12).
#' @param beta Shape parameter β for the beta distribution (default = 4).
#' @param seed Optional. An integer seed for reproducible randomization.
#' @param plot Logical. If `TRUE`, generates diagnostic plots of the simulation and distributions.
#' @param quiet verbose
#' @param ... Additional arguments (currently ignored).
#'
#' @return A data frame (`tibble`) with columns `species`, `growthform`, `ratio`, `cover`, and `aerial` area per species in cm².
#'
#' @examples
#' \dontrun{
#' ratios <- c(Tabular = 0.2, Branching = 0.2, Corymbose = 0.2, Digitate = 0.2, Massive = 0.2)
#' simulate_community(setplot = my_sf_plot, size = coral_size_data, ratio = ratios, coralcover = 30)
#' }
#'
#' @export

simulate_community <- function(setplot, size, ratio, ratiovar=0.2, coralcover=NULL, distribution="beta",
                               alpha=12, beta=4, seed=NULL, quiet=TRUE, plot=FALSE, ...){

  t0 <- tictoc::tic(quiet = TRUE)

  if (!is.null(seed)) {
    set.seed(seed)
  }


  # load size data
  species_size <- size |>
    dplyr::mutate(width=sqrt(area_cm2/pi)*2) |>
    dplyr::mutate(growthform=forcats::fct_recode(species, "Tabular"= "Acropora cytherea", "Tabular"= "Acropora hyacinthus",
                                 "Branching"= "Acropora intermedia", "Branching"= "Acropora robusta",
                                 "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
                                 "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora", "Corymbose" = "Acropora spathulata",
                                 "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis"))

  ### set ratios across morphology

  ### step 1: add noise proportional to the size of each ratio and normalize to 0
  #ratiovar <- 0.2  # Adjust this value to control the amount of noise
   ratio = c(Tabular = 0.7,
             Digitate = 0.075,
             Corymbose = 0.125,
             Branching = 0.08,
             Massive = 0.02)


  ran_ratios <- sapply(ratio, function(x) x + runif(1, -x * ratiovar, x * ratiovar))
  ran_ratios <-   ran_ratios / sum(ran_ratios)


  ### step 2: randomly assign ratios to species within growthforms
  ratios_df <- data.frame(growthform = names(ran_ratios), ratio = ran_ratios)


  species_cover <- tibble::tibble(
    species = c("Acropora hyacinthus", "Acropora cytherea", "Acropora cf. digitifera",
                "Acropora humilis", "Acropora nasuta", "Acropora spathulata",
                "Acropora millepora","Acropora intermedia","Acropora robusta",
                "Goniastrea pectinata", "Goniastrea retiformis"),
  ) |>
    dplyr::mutate(growthform=forcats::fct_recode(species, "Tabular"= "Acropora cytherea", "Tabular"= "Acropora hyacinthus",
                                 "Branching"= "Acropora intermedia", "Branching"= "Acropora robusta",
                                 "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
                                 "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora",
                                 "Corymbose" = "Acropora spathulata",
                                 "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis")) %>% dplyr::left_join(ratios_df, by = "growthform") |>

    dplyr::group_by(growthform) %>%
    dplyr::mutate(assigned_ratio = runif(dplyr::n(), 0, 1)) %>%
    dplyr::mutate(assigned_ratio = assigned_ratio / sum(assigned_ratio) * ratio) %>%
    dplyr::ungroup()

  ### step 3: generate coral cover using beta distribution
  if (!is.null(distribution)){

    thatcoralcover <- beta_dist(mean = coralcover/100, variance = 0.05, variance_factor = 0.05, samples = 10000) |> arrange(y) #|> dplyr::pull(
    thiscoralcover <- dplyr::slice_sample(thatcoralcover, n=1) |> dplyr::pull(x)


  } else {
    thiscoralcover=coralcover
  }

  plotarea <- sum(sf::st_area(setplot))

  # Redistributing the assigned cover within each growthform group randomly, such that:
  #  •	The sum of new ratio values per group still equals the sum of original assigned_ratio for that group.
  #  •	But individual species within the group get random weights.

  # Function to split ratios randomly among growth forms
  species_cover_random <- species_cover %>%
    dplyr::mutate(
      totalcover = thiscoralcover * 100,
      cover = totalcover * assigned_ratio,
      aerial = plotarea * (cover / 100)
    ) |>
    dplyr::select(-ratio) |>
    dplyr::rename(ratio = assigned_ratio)

  if (isTRUE(plot)){
    sp_pal <- c("Acropora hyacinthus" = "#50676c", "Acropora cytherea" = "#3a6c8e",
                "Acropora intermedia" = "#2c3687", "Acropora robusta" = "#7b8ca8",
                "Acropora millepora" = "#a2885d", "Acropora nasuta" = "#665a43", "Acropora spathulata" = "#98a062",
                "Acropora humilis" = "#824b3b", "Acropora cf. digitifera" = "#a47c73",
                "Goniastrea pectinata" = "#999999", "Goniastrea retiformis"= "#bfbfbf")


    # gf_pal <- c("Tabular" = "#ed7d80", "Branching" = "#a8c5d1", "Corymbose" = "#eebe41", "Digitate" = "#3e8089", "Massive" = "#555a7b")

    gf_pal <- c("Tabular" = "#50676c", "Branching" = "#2c3687", "Corymbose" = "#a2885d", "Digitate" = "#824b3b", "Massive" = "#bfbfbf")



    a1 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::ylab("") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.key.size = grid::unit(0.5, 'cm'),
                    legend.text = ggplot2::element_text(size = 6),
                    legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Tabular"),
                           ggplot2::aes((width), color = species),
                           alpha = 0.8, linewidth = 0.6, show.legend = TRUE) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Tabular"),
                           ggplot2::aes((width), fill = species),
                           alpha = 0.3, linewidth = 0, show.legend = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

    a2 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::ylab("") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.key.size = grid::unit(0.5, 'cm'),
                    legend.text = ggplot2::element_text(size = 6),
                    legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Corymbose"),
                           ggplot2::aes((width), color = species),
                           alpha = 0.8, linewidth = 0.6, show.legend = TRUE) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Corymbose"),
                           ggplot2::aes((width), fill = species),
                           alpha = 0.3, linewidth = 0, show.legend = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

    a3 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::ylab("") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.key.size = grid::unit(0.5, 'cm'),
                    legend.text = ggplot2::element_text(size = 6),
                    legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Digitate"),
                           ggplot2::aes((width), color = species),
                           alpha = 0.8, linewidth = 0.6, show.legend = TRUE) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Digitate"),
                           ggplot2::aes((width), fill = species),
                           alpha = 0.3, linewidth = 0, show.legend = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

    a4 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::ylab("") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.key.size = grid::unit(0.5, 'cm'),
                    legend.text = ggplot2::element_text(size = 6),
                    legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Branching"),
                           ggplot2::aes((width), color = species),
                           alpha = 0.8, linewidth = 0.6, show.legend = TRUE) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Branching"),
                           ggplot2::aes((width), fill = species),
                           alpha = 0.3, linewidth = 0, show.legend = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

    a5 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::ylab("") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.key.size = grid::unit(0.5, 'cm'),
                    legend.text = ggplot2::element_text(size = 6),
                    legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Massive"),
                           ggplot2::aes((width), color = species),
                           alpha = 0.8, linewidth = 0.6, show.legend = TRUE) +
      ggplot2::geom_density(data = species_size |> dplyr::filter(growthform == "Massive"),
                           ggplot2::aes((width), fill = species),
                           alpha = 0.3, linewidth = 0, show.legend = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

    b <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::geom_line(data = thatcoralcover, ggplot2::aes(x*100, y), color = "black", linewidth = 0.6, alpha = 0.8) +
      ggplot2::geom_area(data = thatcoralcover, ggplot2::aes(x*100, y), fill = "turquoise4", linewidth = 1, alpha = 0.2) +
      ggplot2::geom_vline(xintercept = thiscoralcover*100, color = "darkred") +
      #ggplot2::geom_text(aes(x = median(thatcoralcover*100), y=max(coralcover$y), label=""), color = "darkred") +
      ggplot2::ggtitle("Beta Distribution for coral cover predictions (alpha= 12, beta = 4)") +
      ggplot2::xlab("Value") + ggplot2::ylab("Density") + ggplot2::xlim(0,100)


    c <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::xlab("") +
      ggplot2::scale_fill_manual(values = gf_pal) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::geom_col(data = species_cover_random |>
                         dplyr::group_by(growthform) |>
                         dplyr::summarise(ratio = sum(ratio)),
                       ggplot2::aes(forcats::fct_reorder(growthform, dplyr::desc(ratio)), ratio, fill = growthform),
                       color = "black", linewidth = 0.4, alpha = 0.8, show.legend = FALSE)


    d <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::geom_col(
        data = species_cover_random |>
          dplyr::group_by(species) |>
          dplyr::summarise(ratio = sum(ratio), .groups = "drop"),
        ggplot2::aes(
          x = forcats::fct_rev(forcats::fct_reorder(species, ratio)),
          y = ratio,
          fill = species
        ),
        color = "black", linewidth = 0.4, alpha = 0.8, show.legend = FALSE
      )

    combined_plot <- gridExtra::grid.arrange(
      gridExtra::arrangeGrob(a1, a2, a3, a4, a5, nrow = 1),
      b,
      gridExtra::arrangeGrob(c, d, nrow = 1),
      ncol = 1
    )

    grid::grid.newpage()
    grid::grid.draw(combined_plot)

  }


  if (isFALSE(quiet)){
    elapsed <- tictoc::toc(quiet = TRUE)
    print(paste0(elapsed$callback_msg, " - simulate_community()"))
  }


  return(species_cover_random)

}
