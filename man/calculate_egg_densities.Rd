% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_egg_densities.R
\name{calculate_egg_densities}
\alias{calculate_egg_densities}
\title{Calculate volumetric density of spheres with percent surface cover}
\usage{
calculate_egg_densities(
  diameter_um,
  surface_percent_cover,
  depth_mm,
  cover_area = 67.92909,
  packing_efficiency = 0.74048,
  method = c("3d_packing", "layered")
)
}
\arguments{
\item{diameter_um}{Numeric. Sphere diameter in microns (µm).}

\item{surface_percent_cover}{Numeric. Percent cover (0–100).}

\item{depth_mm}{Numeric. Depth of the packed layer (in mm).}

\item{packing_efficiency}{Numeric. Packing efficiency (default: 0.74048).}

\item{method}{Character. One of \code{"3d_packing"} (default) or \code{"layered"}.}

\item{cover_area_cm2}{Numeric. Total substrate area (in cm²).}
}
\value{
Numeric. Sphere density in spheres per m³.
}
\description{
Computes the number of spheres per cubic meter from percent cover,
a total area (in cm²), sphere diameter (in µm), and depth (in mm).
Converts all units to SI. Supports idealized layered or 3D packing.
}
\examples{
calculate_egg_densities(500, 100, 2, method = "3d_packing")
calculate_egg_densities(500, 100, 2, method = "layered")
}
