#' Calculate volumetric density of spheres with percent surface cover
#'
#' Computes the number of spheres per cubic meter from percent cover,
#' a total area (in cm²), sphere diameter (in µm), and depth (in mm).
#' Converts all units to SI. Supports idealized layered or 3D packing.
#'
#' @param diameter_um Numeric. Sphere diameter in microns (µm).
#' @param surface_percent_cover Numeric. Percent cover (0–100).
#' @param cover_area_cm2 Numeric. Total substrate area (in cm²).
#' @param depth_mm Numeric. Depth of the packed layer (in mm).
#' @param packing_efficiency Numeric. Packing efficiency (default: 0.74048).
#' @param method Character. One of `"3d_packing"` (default) or `"layered"`.
#'
#' @return Numeric. Sphere density in spheres per m³.
#' @export
#'
#' @examples
#' calculate_egg_densities(500, 100, 2, method = "3d_packing")
#' calculate_egg_densities(500, 100, 2, method = "layered")
calculate_egg_densities <- function(diameter_um,
                                    surface_percent_cover,
                                    depth_mm,
                                    cover_area=67.92909,
                                    packing_efficiency = 0.74048,
                                    method = c("3d_packing", "layered")) {
  method <- match.arg(method)

  # Convert inputs to SI units
  diameter <- diameter_um / 1e6              # µm → m
  surface_area <- (surface_percent_cover / 100) * (cover_area / 1e4)  # % of cm² → m²
  depth <- depth_mm / 1e3                    # mm → m

  radius <- diameter / 2
  sphere_volume <- (4 / 3) * pi * radius^3
  total_volume <- surface_area * depth

  if (method == "3d_packing") {
    total_sphere_volume <- packing_efficiency * total_volume
    total_spheres <- total_sphere_volume / sphere_volume
    density <- total_spheres / total_volume
  } else {
    area_per_sphere <- pi * radius^2
    layers <- floor(depth / diameter)
    spheres_per_layer <- surface_area / area_per_sphere
    total_spheres <- layers * spheres_per_layer
    density <- total_spheres / total_volume
  }

  return(density)
}
