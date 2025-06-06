% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_allee_effects.R
\name{calculate_allee_effects}
\alias{calculate_allee_effects}
\title{Calculate Within-Species Pairwise Colony Distances with Status Label}
\usage{
calculate_allee_effects(
  populations,
  metric = "centroid_distance",
  label,
  dist = Inf,
  ...
)
}
\arguments{
\item{populations}{An \code{sf} object with columns: \code{id}, \code{species}, \code{status}, \code{width}, and geometry.}
}
\value{
A single \code{tibble} with columns:
species, id_from, id_to, centroid_distance, colony_distance, max_distance, status.
}
\description{
Computes centroid-to-centroid, edge-to-edge, and max-distance metrics
for colonies within each species, and returns a single combined data frame
with a \code{status} column indicating the source subset.
}
