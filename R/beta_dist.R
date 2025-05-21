#' Sample Beta Distribution and Return Density
#'
#' Generates a beta distribution based on a specified mean and variance,
#' and returns a density estimate as a data frame.
#'
#' The shape (i.e., skew and peak) of a Beta distribution is driven by the ratio alpha / (alpha + beta) (which controls the mean), and the sum alpha + beta (which controls the concentration/peakedness).
#'
#' @param mean Numeric scalar. Desired mean of the beta distribution. Must be between 0 and 1 (exclusive).
#' @param variance Numeric scalar. Desired variance of the beta distribution. Will be constrained by `variance_factor`.
#' @param samples Integer. Number of samples to draw from the beta distribution. Default is 1000.
#' @param variance_factor Numeric scalar. Limits the allowed variance as a fraction of the theoretical maximum variance. Default is 0.1.
#'
#' @return A tibble with columns `x` and `y` representing the smoothed density estimate of the sampled beta distribution.
#'
#' @examples
#' beta_dist(mean = 0.4, variance = 0.01)
#'
#' @export
beta_dist <- function(mean, variance, samples = 1000, variance_factor = 0.1) {
  if (mean <= 0 || mean >= 1) {
    stop("Mean must be between 0 and 1.")
  }

  max_variance <- mean * (1 - mean)
  constrained_variance <- min(variance, variance_factor * max_variance)
  if (constrained_variance <= 0) {
    stop("Constrained variance must be greater than 0.")
  }

  shape_factor <- mean * (1 - mean) / constrained_variance - 1
  alpha <- mean * shape_factor
  beta <- (1 - mean) * shape_factor

  samples <- rbeta(samples, alpha, beta)
  d <- density(samples)
  tibble::tibble(x = d$x, y = d$y)
}
