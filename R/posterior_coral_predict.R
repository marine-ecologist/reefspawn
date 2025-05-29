#' Generate Posterior Predictions of Coral Colony Area
#'
#' This function uses a fitted `brms` model to generate posterior predictions of colony size (width),
#' transforms width to area, and returns cumulative area predictions per species in long format.
#'
#' @param brms A fitted brms model predicting colony width.
#' @param ndraws Integer. Number of posterior draws to sample.
#' @param newdata A data frame containing the predictor variables for prediction.
#' @param correct_species Logical. Whether to rename "Acropora cf. digitifera" to "Acropora digitifera" in predictions.
#'
#' @return A tibble with posterior draws in long format, including species, width, area, and cumulative area.
#' @importFrom brms posterior_predict
#' @importFrom dplyr filter_if all_vars arrange mutate group_by
#' @importFrom magrittr set_colnames
#' @importFrom forcats fct_recode
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @export
#'
posterior_coral_predict <- function(brms, ndraws = 10000, seed = NULL, newdata, correct_species = FALSE) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Predict posterior draws
  posteriordraws <- brms::posterior_predict(brms, ndraws = ndraws, newdata = data.frame(species =  unique(as.character(brms$data$species))))

  # Extract unique species list
  species_list <- unique(newdata$species)

  # Convert to long format and process
  preds <- posteriordraws |>
    as.data.frame() |>
    dplyr::filter_if(is.numeric, dplyr::all_vars(. >= 0)) |>
    magrittr::set_colnames(species_list) |>
    tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "species", values_to = "width") |>
    dplyr::arrange(species) |>
    dplyr::mutate(species = as.factor(species))

  # Rename species if needed
  if (correct_species) {
    preds <- preds |>
      dplyr::mutate(species = forcats::fct_recode(species, "Acropora digitifera" = "Acropora cf. digitifera"))
  }

  # Compute area and cumulative area
  preds <- preds |>
    dplyr::mutate(area = pi * ((width / 2) ^ 2)) |>
    dplyr::group_by(species) |>
    dplyr::mutate(cumulative_area = cumsum(area))

  return(preds)
}
