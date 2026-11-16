# plantpress

Figures of forests simulated with [`plant`](https://github.com/traitecoevo/plant): a stand growing through time, drawn as individual trees in an interactive page in the browser, and 2D figures of a run.

`plant` simulates a stand as a density of plants, not as individual trees. `plantpress` draws explicit trees from it with `plant::sample_size_distribution()`, which follows the same trees from frame to frame, so a stand can be animated: each tree grows with its cohort, the number of trees follows the model's density, and dead trees never come back. Every size comes from the strategy's own allometry (`plant::expand_state()`), and crowns are shaped by the model's vertical leaf distribution. Positions are illustrative, because `plant` is spatially implicit.

## Example

```r
library(plant)
library(plantpress)

p <- default_parameters("FF16", quiet = TRUE)
p$max_patch_lifetime <- 100
p <- add_strategies(p, trait_matrix(c(0.0825, 0.2), "lma"), birth_rate = c(20, 20))
results <- tidy(run(p, control = control(node_density_coordinate = "birth_date"),
                    record = "steps"))

stand <- realise_stand(results, side = 20)
write_viewer(stand, "stand.html", title = "Pioneer to successor")
```

`stand.html` is self-contained and opens from disk; three.js loads from a CDN. It shows the stand from disturbance onwards, with a timeline, live stem density and basal area per hectare, and the model's light profile through the canopy. Trees can be coloured by species, the light reaching the crown, height, or any other state variable (growth and leaf area by default; see `colour_by` in `write_viewer()`).

`stand_long(stand)` gives one row per living tree per frame, with every state variable plant reports or derives, for other renderers or analyses.

FF16, TF24 and K93 runs can all be realised and drawn. K93's size is stem diameter and it has no height or crown, so its stand is drawn as a map of stems from above. `realise_stand()` takes a `plant_run` or its tidied list.

## 2D figures

Each figure takes a run (a `plant_run` or its tidied list) and returns a ggplot, so it can be restyled with `+ theme(...)`:

- `plot_size_distribution()`: each cohort's size through time, shaded by density.
- `plot_stand_totals()`: density, leaf area index, basal area and biomass by tissue through time.
- `plot_by_size()`: growth, mortality and fecundity against size at chosen patch ages.
- `plot_light_profile()`: light through the canopy at chosen ages.
- `plot_water()`: rainfall, soil water by layer, transpiration, and each cohort's water potentials, conductance, profit and leaf temperature, for a run that records soil water (the TF24 models).

`stand_figures(results)` returns every figure that applies to a run. A figure draws only what the run carries, and labels sizes in the model's own units, so K93 has no leaf area or biomass panels and its sizes are stem diameters in cm.

See [PLAN.md](PLAN.md) for what comes next: stand reports, cinematic renders in Blender, metapopulation landscapes.

## Install

```r
remotes::install_github("traitecoevo/plantpress")
```

This also installs the `plant` commit that `DESCRIPTION` pins, replacing any other `plant` in that library.

The package was rebuilt in 2026. The 2016 `trees3D` code (`rgl` crowns, adapted from Remko Duursma's Maeswrap) remains in the git history.

## Plant family

`plantpress` is part of the **plant family** of packages in the [`traitecoevo`](https://github.com/traitecoevo) org, built around the [`plant`](https://github.com/traitecoevo/plant) forest model. Docs hub: <https://traitecoevo.github.io/overstorey/>.

**Contributing:** please skim the family [issue guide](https://github.com/traitecoevo/plant-meta/blob/main/governance/issue-guide.md) before filing. Issues across the family are triaged on [board #5](https://github.com/orgs/traitecoevo/projects/5), and cross-package context lives in [`plant-meta`](https://github.com/traitecoevo/plant-meta).
