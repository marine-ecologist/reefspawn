#' Sample Sterile Zone Proportions by Species
#'
#' Returns a vector of sterile zone proportions (i.e., the proportion of a coral colony
#' that lies outside the sterile zone), sampled from lower, median, or upper estimates
#' per species.
#'
#' @param species_vec A character vector of species names.
#' @param width_vec Optional numeric vector of colony widths (ignored; reserved for future use).
#' @param draw Character, one of `"random"` (default), `"median"`, `"lower"`, or `"upper"`.
#'        Determines how to select the sterile proportion value.
#'
#' @return A numeric vector of sampled sterile zone proportions, same length as `species_vec`.
#'
#' @examples
#' \dontrun{
#' species <- c("Acropora hyacinthus", "Acropora millepora", "Goniastrea retiformis")
#' sample_sterile_zone(species, draw = "random")
#' }

#' @export

sample_sterile_zone <- function(species_vec, width_vec = NULL, draw = c("random", "median", "lower", "upper")) {
  sterile_zone <- tibble::tibble(
    species = c("Acropora hyacinthus", "Acropora cytherea", "Acropora cf. digitifera",
                "Acropora humilis", "Acropora nasuta", "Acropora spathulata",
                "Acropora millepora","Acropora intermedia","Acropora robusta"),
    lower__ = c(0.760, 0.999, 0.773, 0.995, 0.970, 0.885, 0.885, 0.760, 0.760),
    median  = c(0.771, 1.000, 0.868, 1.000, 0.999, 0.913, 0.913, 0.771, 0.771),
    upper__ = c(0.781, 1.000, 0.936, 1.000, 1.000, 0.941, 0.941, 0.781, 0.781)
  ) |>
    dplyr::mutate(growthform = forcats::fct_recode(species,
                                                   "Tabular" = "Acropora cytherea", "Tabular" = "Acropora hyacinthus",
                                                   "Branching" = "Acropora intermedia", "Branching" = "Acropora robusta",
                                                   "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora",
                                                   "Corymbose" = "Acropora spathulata",
                                                   "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis"
    )) |>
    dplyr::bind_rows(tibble::tibble(
      species = c("Goniastrea retiformis", "Goniastrea pectinata"),
      lower__ = c(1, 1),
      median  = c(1, 1),
      upper__ = c(1, 1),
      growthform = c("Massive", "Massive")
    ))

  draw <- match.arg(draw)

  lookup <- dplyr::tibble(species = species_vec)
  sterile_match <- dplyr::left_join(lookup, sterile_zone, by = "species")

  if (draw == "median") {
    return(sterile_match$median)
  } else if (draw == "lower") {
    return(sterile_match$lower__)
  } else if (draw == "upper") {
    return(sterile_match$upper__)
  } else {
    return(purrr::pmap_dbl(
      list(sterile_match$lower__, sterile_match$median, sterile_match$upper__),
      ~ sample(c(..1, ..2, ..3), size = 1)
    ))
  }
}


#' Predict colony width using posterior predict (lognormal model)
#'
#' @param newdata A data.frame with a `species` column.
#' @param draws Posterior draws from `posterior_coeffs_sizedistribution`.
#' @param draw_id Integer index of the draw to use.
#' @return Numeric vector of predicted widths.
#' @export
#'
#' @examples
#' \dontrun{
#' newdata <- data.frame(species = c("Acropora", "Porites"))
#' predict_colony_width(newdata, posterior_coeffs_sizedistribution, draw_id = 10)
#' }

predict_colony_width <- function(newdata, draws, draw_id = 1) {
  X <- model.matrix(~ species, data = newdata)
  beta <- as.numeric(draws[draw_id, colnames(X)])
  mu <- as.vector(X %*% beta)
  sigma <- draws[draw_id, "sigma"]
  rlnorm(length(mu), meanlog = mu, sdlog = sigma)
}

#' Predict polyp density per colony using posterior predict (gaussian model)
#'
#' @param newdata A data.frame with `species` column.
#' @param draws Posterior draws from `posterior_coeffs_polypdensity`.
#' @param draw_id Integer index of the draw to use.
#' @return Numeric vector of predicted polyp densities.
#' @export
#'
#' @examples
#' \dontrun{
#' newdata <- data.frame(species = c("Acropora", "Porites"))
#' predict_polyp_density(newdata, posterior_coeffs_polypdensity, draw_id = 5)
#' }

predict_polyp_density <- function(newdata, draws, draw_id = 1) {
  X <- model.matrix(~ species, data = newdata)
  beta <- as.numeric(draws[draw_id, colnames(X)])
  mu <- as.vector(X %*% beta)
  sigma <- draws[draw_id, "sigma"]
  rnorm(length(mu), mean = mu, sd = sigma)
}


#' Predict reproductive probability using posterior predict (bernoulli model)
#'
#' @param newdata A data.frame with `species` and `width` columns.
#' @param draws Posterior draws from `posterior_coeffs_reproductive_probability`.
#' @param draw_id Integer index of the draw to use.
#' @return Numeric vector of probabilities.
#' @export
#'
#' @examples
#' \dontrun{
#' newdata <- expand.grid(species = levels(coralsize$species), width = seq(0,50,5))
#' newdata$reproductive_probability <- predict_repro_prob(newdata, posterior_coeffs_reproductive_probability, draw_id = 20)
#' }

predict_repro_prob <- function(newdata, draws, draw_id = 1) {
  # Ensure species are valid names
  newdata$species <- factor(newdata$species)
  levels(newdata$species) <- make.names(levels(newdata$species))

  X <- model.matrix(~ log(width) * species, data = newdata)

  # Match to column names in draws
  colnames(X) <- gsub(":", ".", colnames(X))                      # interaction separator
  colnames(X) <- gsub("^\\(Intercept\\)$", "b_Intercept", colnames(X))
  colnames(X) <- gsub("^log\\(width\\)$", "b_logwidth", colnames(X))
  colnames(X) <- paste0("b_", colnames(X)[!colnames(X) %in% c("b_Intercept", "b_logwidth")])
  colnames(X)[colnames(X) == "b_log(width)"] <- "b_logwidth"

  beta <- as.numeric(draws[draw_id, colnames(X)])
  eta <- as.vector(X %*% beta)
  p <- 1 / (1 + exp(-eta))
  rbinom(length(p), 1, p)
}

#' Predict oocytes per reproductive polyp using posterior predict (ZIP model)
#'
#' @param newdata A data.frame with `species` and `width` columns.
#' @param draws Posterior draws from `posterior_coeffs_oocyte_output`.
#' @param draw_id Integer index of the draw to use.
#' @return Numeric vector of expected counts.
#' @export
#'
#' @examples
#' \dontrun{
#' newdata <- expand.grid(species = levels(coralsize$species), width = seq(0,50,5))
#' newdata$oocytes <- predict_oocytes(newdata, posterior_coeffs_oocyte_output, draw_id = 100)
#' }

predict_oocytes <- function(newdata, draws, draw_id = 1) {
  X <- model.matrix(~ log(width) * species, data = newdata)
  beta <- as.numeric(draws[draw_id, grep("^b_", colnames(draws), value = TRUE)])
  mu <- exp(as.vector(X %*% beta))
  zi <- draws[draw_id, "zi"]
  rpois(length(mu), mu) * rbinom(length(mu), 1, 1 - zi)
}
