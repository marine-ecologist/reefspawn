#' Simulate Spawning Output per Coral Colony
#'
#' Combines multiple Bayesian models to estimate reproductive output (number of oocytes)
#' for each colony in a population. Uses polyp density, reproductive probability,
#' sterile zone proportion, and oocytes per polyp.
#'
#' @param populations An `sf` object containing colony data with `species`, `width`, and `area` columns.
#' @param setplot setplot area
#' @param seed setseed.
#' @param plot plot output?
#' @param quiet verbose
#'
#'
#' @return A modified `populations` `sf` object with added columns:
#' - `polypdensity`: Predicted polyps per cm²
#' - `repro_prob`: Probability of being reproductive
#' - `sterile_proportion`: Proportion outside sterile zone
#' - `oocytes`: Oocytes per reproductive polyp
#' - `colony_polyps`: Total number of polyps per colony
#' - `reproductive_polyps`: Number of reproductive polyps
#' - `sterile_polyps`: Number of sterile polyps
#' - `output`: Total reproductive output (reproductive polyps × oocytes)
#'
#' @export
#'
simulate_spawning <- function(populations, setplot, seed, quiet=TRUE, plot = FALSE, ...) {

  t0 <- tictoc::tic(quiet = TRUE)

  if (!is.null(seed)) {
    set.seed(seed)
  }


  # 1. Polyp density per colony
  populations$polypdensity <- posterior_predict(
    brm_polyp_density,
    newdata = dplyr::select(as.data.frame(populations), species),
    draw_ids = sample(1:brms::ndraws(brm_polyp_density), 1, replace=TRUE)
  ) |> as.numeric()

  # 2. Reproductive probability per colony
  populations$reproductive_probability <- posterior_predict(
    brm_probability,
    newdata = dplyr::select(as.data.frame(populations), species, width),
    draw_ids = sample(1:brms::ndraws(brm_probability), 1, replace=TRUE)
  ) |> as.numeric()

  # 3. Sterile zone proportion per colony (Table S5)
  populations$sterile_proportion <- sample_sterile_zone(
    species_vec = populations$species,
    draw = "random"
  )

  # 4. Oocytes per reproductive polyp
  populations$oocytes <- posterior_predict(
    brm_fecundity,
    newdata = dplyr::select(as.data.frame(populations), species, width),
    draw_ids = sample(1:brms::ndraws(brm_fecundity), 1, replace=TRUE)
  ) |> as.numeric()

  # 5. Total reproductive output per colony
  populations <- populations |>
    dplyr::mutate(
      colony_polyps = polypdensity * (as.numeric(area) * 10000),
      reproductive_polyps = colony_polyps * sterile_proportion,
      sterile_polyps = colony_polyps * (1 - sterile_proportion),
      output = reproductive_polyps * oocytes * reproductive_probability
    )

  if (isFALSE(quiet)){
    elapsed <- tictoc::toc(quiet = TRUE)
    print(paste0(elapsed$callback_msg, " - simulate_populations()"))
    cat(paste0(""))

    plotarea <-  as.numeric(sum(sf::st_area(setplot)))
    totaloutput <- scales::comma(as.numeric(sum(populations$output)), accuracy = 1)
    areaoutput <-  scales::comma(as.numeric(sum(populations$output) / plotarea), accuracy = 1)

    print(paste0(totaloutput, " - total reproductive output"))
    print(paste0(areaoutput, " - reproductive output per m2"))
  }

  #print(paste0(scales::comma(sum(populations$output), accuracy = 1) ), " - total reproductive output")# 56,950,900
  #print(paste0(scales::comma(sum(populations$output)/as.numeric(sum(sf::st_area(setplot))),  accuracy = 1)), " - reproductive output per m2") # 444,929 [1/m^2]

  if (plot==TRUE){

    sp_pal <- c("Acropora hyacinthus" = "#50676c", "Acropora cytherea" = "#3a6c8e",
                "Acropora intermedia" = "#2c3687", "Acropora robusta" = "#7b8ca8",
                "Acropora millepora" = "#a2885d", "Acropora nasuta" = "#665a43", "Acropora spathulata" = "#98a062",
                "Acropora humilis" = "#824b3b", "Acropora cf. digitifera" = "#a47c73",
                "Goniastrea pectinata" = "#999999", "Goniastrea retiformis"= "#bfbfbf")


    # gf_pal <- c("Tabular" = "#ed7d80", "Branching" = "#a8c5d1", "Corymbose" = "#eebe41", "Digitate" = "#3e8089", "Massive" = "#555a7b")

    gf_pal <- c("Tabular" = "#50676c", "Branching" = "#2c3687", "Corymbose" = "#a2885d", "Digitate" = "#824b3b", "Massive" = "#bfbfbf")

    reproductiveoutput <- populations |>
      dplyr::mutate(growthform=forcats::fct_recode(species, "Tabular"= "Acropora cytherea", "Tabular"= "Acropora hyacinthus",
                                                   "Branching"= "Acropora intermedia", "Branching"= "Acropora robusta",
                                                   "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
                                                   "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora",
                                                   "Corymbose" = "Acropora spathulata",
                                                   "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis"))

    # Violin plot with points: output by species
    p1 <- ggplot2::ggplot(data = reproductiveoutput) +
      ggplot2::geom_boxplot(
        ggplot2::aes(x = species, y = output, fill = species),
        linewidth = 0.3,
        alpha = 0.8,
        width = 0.6,
        outliers=TRUE
      ) +
      ggplot2::theme_bw() +
      ggplot2::xlab("") +
      ggplot2::ylab("Reproductive Output") +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::scale_y_log10(labels = scales::label_comma())

    # Scatter plot with lm fit: output by colony size
    p2 <- ggplot2::ggplot(data = reproductiveoutput) +
      ggplot2::facet_wrap(~growthform, ncol=2, scales="free") +
      ggplot2::geom_point(ggplot2::aes(x = width, y = output, fill = species), alpha = 0.5, shape = 21, fill = "black", size = 0.5) +
      ggplot2::stat_smooth(ggplot2::aes(x = width, y = output, group = species, color = species, fill = species), method = "lm", se = TRUE) +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values = sp_pal) +
      ggplot2::scale_fill_manual(values = sp_pal) +
      ggplot2::xlab("Colony Width (cm)") +
      ggplot2::ylab("Reproductive Output") +
      ggplot2::scale_y_continuous(labels = scales::label_comma())

    combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 1)

    print(combined_plot)

  }
  return(populations)
}
