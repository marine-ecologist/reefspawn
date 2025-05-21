
#' Load and preprocess coral fecundity and size data
#'
#' Loads `fecundity.csv` and `size_structure.csv` from the `inst/extdata` directory,
#' computes derived width from area, and assigns species-level growthforms.
#'
#' @details
#' - `fecundity` includes colony-level reproductive data.
#' - `coralsize` contains colony size structure by species, with growthform classification.
#' - Width is computed as `sqrt(area_cm2 / pi) * 2`.
#' - Growthforms are assigned using `fct_recode()` based on species identity.
#'
#' @return
#' Two data frames:
#' - `fecundity`: colony area, width, and reproductive status
#' - `coralsize`: species, width, and assigned growthform
#'
#' @examples
#' \dontrun{
#' head(fecundity)
#' head(coralsize)
#' }
#' @export




sp_order <- c("Acropora hyacinthus", "Acropora cytherea",
              "Acropora intermedia", "Acropora robusta",
              "Acropora millepora", "Acropora nasuta", "Acropora spathulata",
              "Acropora humilis", "Acropora cf. digitifera",
              "Goniastrea pectinata", "Goniastrea retiformis")


sp_pal <- c("Acropora hyacinthus" = "#50676c", "Acropora cytherea" = "#3a6c8e",
            "Acropora intermedia" = "#2c3687", "Acropora robusta" = "#7b8ca8",
            "Acropora spathulata" = "#98a062", "Acropora millepora" = "#665a43", "Acropora nasuta" =  "#48642f",
            "Acropora humilis" = "#824b3b", "Acropora cf. digitifera" = "#a47c73",
            "Goniastrea pectinata" = "#999999", "Goniastrea retiformis"= "#bfbfbf")


fecundity <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/fecundity.csv") |>
  dplyr::mutate(area=area_cm2/10000) |>
  dplyr::mutate(width=sqrt(area_cm2/pi)*2)



coralsize <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/size_structure.csv") |>
  mutate(width=sqrt(area_cm2/pi)*2) |>
  mutate(species=as.factor(species)) |>
  mutate(species=fct_relevel(species,sp_order)) |>
  mutate(growthform=fct_recode(species, "Tabular"= "Acropora cytherea", "Tabular"= "Acropora hyacinthus",
                               "Branching"= "Acropora intermedia", "Branching"= "Acropora robusta",
                               "Massive" = "Goniastrea pectinata", "Massive" = "Goniastrea retiformis",
                               "Corymbose" = "Acropora nasuta", "Corymbose" = "Acropora millepora", "Corymbose" = "Acropora spathulata",
                               "Digitate" = "Acropora cf. digitifera", "Digitate" = "Acropora humilis"))



#' Load and augment coral polyp density data
#'
#' Loads `polyp_density.csv`, adds an estimated entry for *Acropora millepora*
#' based on similar species (*A. nasuta* and *A. spathulata*), and assigns growthform classifications.
#'
#' @details
#' - `Acropora millepora` is not present in the original dataset.
#' - To estimate it, data for *A. nasuta* and *A. spathulata* are duplicated and relabeled.
#' - A new `spp` code "AM" is assigned to the added entries.
#' - Growthforms are recoded using `fct_recode()` based on species name.
#'
#' @return
#' A `polyp_density_data` data frame with all species and assigned growthforms.
#'
#' @examples
#' head(polyp_density_data)
#'
#' @export

polyp_density_data <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/polyp_density.csv")

# Add a hack to estimate millepora from nasuta and spathulata
millepora <- polyp_density_data |>
  dplyr::filter(species %in% c("Acropora nasuta", "Acropora spathulata")) |>
  dplyr::mutate(species = "Acropora millepora", spp = "AM")

# Combine and recode growthforms
polyp_density_data <- polyp_density_data |>
  dplyr::bind_rows(millepora) |>
  dplyr::mutate(growthform = forcats::fct_recode(species,
                                                 "Tabular"   = "Acropora cytherea",
                                                 "Tabular"   = "Acropora hyacinthus",
                                                 "Branching" = "Acropora intermedia",
                                                 "Branching" = "Acropora robusta",
                                                 "Massive"   = "Goniastrea pectinata",
                                                 "Massive"   = "Goniastrea retiformis",
                                                 "Corymbose" = "Acropora nasuta",
                                                 "Corymbose" = "Acropora millepora",
                                                 "Corymbose" = "Acropora spathulata",
                                                 "Digitate"  = "Acropora cf. digitifera",
                                                 "Digitate"  = "Acropora humilis"
  ))
