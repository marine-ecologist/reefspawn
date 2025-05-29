#' Assign Bleaching Status to Coral Population
#'
#' Randomly assigns bleaching status ("bleached" or "unbleached") to individuals
#' in a population dataset based on species-specific mortality rates.
#'
#' @param population An `sf` object with a `species` column.
#' @param mortality A data frame with columns `species` and `mortality`
#'   (values between 0 and 1).
#'
#' @return An `sf` object with an additional column `status`.
#'
#' @importFrom dplyr left_join group_by mutate ungroup row_number n filter if_else select
#' @importFrom tibble tibble
#'
#' @export
#'

simulate_bleaching <- function(populations, mortality=NULL, seed, silent=FALSE, plot=FALSE){


  if (is_null(mortality)){
  mortality = data.frame(species = c("Acropora hyacinthus", "Acropora cytherea",
                                     "Acropora intermedia", "Acropora robusta",
                                     "Acropora millepora", "Acropora nasuta", "Acropora spathulata",
                                     "Acropora humilis", "Acropora cf. digitifera",
                                     "Goniastrea pectinata", "Goniastrea retiformis"),
                          mortality = c(0.9, 0.9, 0.7, 0.7, 0.8, 0.8, 0.8, 0.8, 0.8, 0.5, 0.5)
                        )
  }

  # Join populations with mortality rates
  populations_mort <- populations |>
    dplyr::left_join(mortality, by = "species") |>
    dplyr::group_by(species) |>
    dplyr::mutate(
      status = ifelse(
        dplyr::row_number(sample(n())) <= round(n() * unique(mortality)),
        "bleached", "unbleached"
      )
    ) |>
    dplyr::ungroup()

  if (isTRUE(plot)){
  ggplot2::ggplot() + ggplot2::theme_bw() + ggplot2::facet_wrap(~species, nrow = 2) +
      ggplot2::geom_density(data=populations_mort, ggplot2::aes(width, color=status, fill=status), alpha=0.4) +
      ggplot2::scale_x_log10()
  }


  if (silent == FALSE){
    populations_postbleaching <- populations_mort |>
      as.data.frame() |>
      dplyr::group_by(species, status) |>
      dplyr::summarise(n = n(), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = status,
        values_from = n,
        values_fill = list(bleached = 0, unbleached = 0)
      ) |>
      dplyr::mutate(bleaching_proportion = unbleached / (unbleached + bleached) * 100)

  }

  return(populations_mort)


}
