#' Simulate Coral Colony Sizes Using Skewed Distributions
#'
#' This function simulates coral colony sizes per species using either a log-normal
#' (`rlnorm`) or gamma (`rgamma`) distribution, based on the observed mean and standard
#' deviation of colony widths in the input data. Each species will be sampled `ndraws` times.
#'
#' @param input A data frame with columns `species` and `width`. Default is `coralsize`.
#' @param distribution Character string. Distribution type to simulate from.
#'   Supported options are `"rlnorm"` and `"gamma"`.
#' @param ndraws Integer. Number of simulated coral sizes per species.
#' @param plot Logical. If `TRUE`, a histogram of simulated sizes is plotted by species.
#' @param seed Optional integer. If provided, sets the random seed for reproducibility.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{species}{Factor indicating coral species}
#'     \item{width}{Simulated colony width values}
#'     \item{area_cm2}{Simulated colony area values (π × (width/2)²)}
#'   }
#'
#' @export
#'
simulate_coralsize <- function(input = coralsize, distribution = "rlnorm", ndraws, plot = FALSE, seed = NULL) {


  if (!is.null(seed)) set.seed(seed)

  coralstats <- input |>
    group_by(species) |>
    summarise(
      mean = mean(width, na.rm = TRUE),
      sd = sd(width, na.rm = TRUE),
      .groups = "drop"
    )

  if (distribution == "rlnorm") {
    simulated <- coralstats |>
      group_by(species) |>
      reframe(
        width = rlnorm(
          ndraws,
          meanlog = log(mean^2 / sqrt(sd^2 + mean^2)),
          sdlog = sqrt(log(1 + (sd^2 / mean^2)))
        )
      ) |>
      mutate(area_cm2 = pi * ((width / 2)^2))

  } else if (distribution == "gamma") {
    simulated <- coralstats |>
      group_by(species) |>
      reframe(
        width = rgamma(
          ndraws,
          shape = (mean^2) / (sd^2),
          scale = (sd^2) / mean
        )
      ) |>
      mutate(area_cm2 = pi * ((width / 2)^2))

  } else {
    stop("Only 'rlnorm' and 'gamma' distributions are supported.")
  }

  if (isTRUE(plot)) {
    print(
      ggplot(simulated, aes(x = width)) +
        geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
        facet_wrap(~species, scales = "free") +
        theme_bw() +
        labs(x = "Simulated Coral Size", y = "Frequency")
    )
  }

  return(simulated)
}
