# build_posterior_coeffs.R

# Load required libraries with explicit :: usage in code
library(tidyverse)
library(brms)
library(usethis)

# -------------------------------------------------------------------------------------
# 1. Size distribution ~ species
# -------------------------------------------------------------------------------------

coralsize <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/size_structure.csv") %>%
  dplyr::mutate(width = sqrt(area_cm2 / pi) * 2) %>%
  dplyr::mutate(species = forcats::fct_relevel(as.factor(species), sp_order)) %>%
  dplyr::mutate(
    growthform = forcats::fct_recode(
      species,
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
    )
  )

brm_sizedistribution <- brms::brm(
  width ~ species,
  data = coralsize,
  family =  brms::lognormal(),
  iter = 10000,
  chains = 4,
  cores = 10
)

usethis::use_data(coralsize, compress = "xz", overwrite = TRUE)
usethis::use_data(brm_sizedistribution, compress = "xz", overwrite = TRUE)


# -------------------------------------------------------------------------------------
# 2. Polyp density per colony
# -------------------------------------------------------------------------------------

polyp_density_data <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/polyp_density.csv")

millepora <- polyp_density_data %>%
  dplyr::filter(species %in% c("Acropora nasuta", "Acropora spathulata")) %>%
  dplyr::mutate(species = "Acropora millepora", spp = "AM")

polyp_density_data <- dplyr::bind_rows(polyp_density_data, millepora) %>%
  dplyr::mutate(
    growthform = forcats::fct_recode(
      species,
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
    )
  )

brm_polyp_density <- brms::brm(
  cm2 ~ species,
  data = polyp_density_data,
  family = gaussian(),
  iter = 10000,
  chains = 4,
  cores = 10
)


usethis::use_data(brm_polyp_density, compress = "xz", overwrite = TRUE)

# -------------------------------------------------------------------------------------
# 3. Reproductive probability per colony
# -------------------------------------------------------------------------------------

fecundity <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/fecundity.csv") %>%
  dplyr::mutate(
    area = area_cm2 / 10000,
    width = sqrt(area_cm2 / pi) * 2
  )

brm_probability <- brms::brm(
  reproductive ~ log(width) * species,
  data = fecundity,
  family = brms::bernoulli(),
  iter = 10000,
  chains = 4,
  cores = 10
)


usethis::use_data(brm_probability, compress = "xz", overwrite = TRUE)

# -------------------------------------------------------------------------------------
# 4. Oocytes per reproductive polyp
# -------------------------------------------------------------------------------------

fecundity <- read.csv("inst/extdata/ecy4017-sup-0001-datas1/Data S1/fecundity.csv") %>%
  dplyr::mutate(
    area = area_cm2 / 10000,
    width = sqrt(area_cm2 / pi) * 2
  )

brm_fecundity <- brms::brm(
  eggs ~ log(width) * species,
  data = fecundity,
  family = brms::zero_inflated_poisson(),
  prior = c(
    brms::prior(normal(1, 2), class = "Intercept"),
    brms::prior(beta(2, 2), class = "zi")
  ),
  iter = 2000,
  warmup=100,
  chains = 4,
  cores = 10
)


usethis::use_data(brm_fecundity, compress = "xz", overwrite = TRUE)
