


#simulate_community
#model_distributions (?)
#simulate_populations



### 1  set plot
sf_plot <- setplot(16,8)

### 2. simulate communities
coral_sizes <- read.csv("/Users/rof011/coraldynamics/data/ecy4017-sup-0001-datas1/Data S1/size_structure.csv")
ratios <- c(Tabular = 0.7, Digitate = 0.075, Corymbose = 0.125, Branching = 0.08, Massive = 0.02)

simulated_community <- simulate_community(setplot = sf_plot,
                                coralcover=40,
                                size = coral_sizes,
                                ratio = ratios,
                                ratiovar = 0.2,
                                alpha = 24,
                                beta = 8,
                                seed = 1001,
                                plot = TRUE,
                                quiet=FALSE)



### 2. simulate populations
populations <- simulate_populations(setplot = sf_plot,
                                    size = coral_sizes,
                                    community = simulated_community,
                                    return="sf",
                                    brms = brm_sizedistribution,
                                    ndraws = 50000,
                                    seed = 1001,
                                    plot=TRUE,
                                    quiet=FALSE)



### 3. simulate reproductive output
reproductiveoutput <- simulate_spawning(populations = populations,
                                        setplot = sf_plot,
                                        seed = 1001,
                                        quiet=FALSE)

### 4. bleaching impacts

# scenario a) healthy no bleaching:


bleaching_prop <- species_cover |>  mutate(mortality = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.5, 0.5)*100)

set.seed(42)

# 1. remove colonies
postbleachingoutput <- reproductiveoutput |>
  dplyr::left_join(bleaching_prop, by = "species") |>
  dplyr::group_by(species) |>
  dplyr::mutate(
    kill = sample(c(TRUE, FALSE), size = dplyr::n(), replace = TRUE,
                  prob = c(unique(mortality)/100, 1 - unique(mortality)/100))
  ) #|>
  # dplyr::ungroup() |>
  # dplyr::filter(!kill) |>
  # dplyr::select(-mortality, -kill)

# 2. adjust ooctyes


# 3. partial mortality?


