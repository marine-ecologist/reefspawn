% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_populations.R
\name{map_populations}
\alias{map_populations}
\title{Map Coral Populations with Species Coloring}
\usage{
map_populations(
  setplot,
  populations,
  interactive = TRUE,
  zoom = c(22, 32),
  webgl = TRUE
)
}
\arguments{
\item{setplot}{An \code{sf} or \code{SpatVector} object representing the background plot layer (e.g. reef tiles).}

\item{populations}{An \code{sf} object with point geometries and a \code{species} column to map.}

\item{interactive}{Logical; if \code{TRUE}, the map is shown in an interactive web view. If \code{FALSE}, it renders as a static plot.}

\item{webgl}{Logical; if \code{interactive = TRUE}, sets WebGL rendering mode in \code{tm_view()}.}
}
\value{
A \code{tmap} object
}
\description{
Plots a spatial map of coral populations over a setplot base layer using \code{tmap}.
Species are color-coded with a customizable palette, and output can be either
interactive (web) or static. Optionally enables or disables WebGL for 3D rendering
in interactive view.
}
\examples{
\dontrun{
grid <- setplot(10, 5)
map_populations(setplot = grid, populations = coral_points, interactive = TRUE, webgl = FALSE)
}
}
