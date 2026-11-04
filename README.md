# standviz

3D views of forests simulated with [`plant`](https://github.com/traitecoevo/plant): a stand growing through time, drawn as individual trees in an interactive page in the browser.

> `standviz` is a working name; the package will be renamed.

`plant` simulates a stand as a density of plants over height, not as individual trees. `standviz` draws explicit trees from that density that persist from frame to frame, so a stand can be animated: each tree grows with its cohort, the number of trees follows the model's size distribution, and trees die in a fixed order and never come back. Tree geometry comes from the strategy's own allometry, and crowns are shaped by the model's vertical leaf distribution. Positions are illustrative, because `plant` is spatially implicit.

## Example

```r
library(plant)
library(standviz)

p <- scm_base_parameters("FF16")
p$max_patch_lifetime <- 100
p <- add_strategies(p, trait_matrix(c(0.0825, 0.2), "lma"), birth_rate = c(20, 20))
results <- run_scm(p, collect = TRUE)

stand <- realise_stand(results, side = 20, min_height = 1)
write_viewer(stand, "stand.html", title = "Pioneer to successor")
```

`stand.html` is self-contained and opens from disk; three.js loads from a CDN. It shows the stand from disturbance onwards, with a timeline, live stem density and basal area per hectare, and the model's light profile through the canopy. Trees can be coloured by species, by the light reaching the crown, or by height.

`stand_long(stand)` gives one row per living tree per frame, for other renderers or analyses.

Only FF16 runs are supported so far. See [PLAN.md](PLAN.md) for what comes next: cinematic renders in Blender, metapopulation landscapes, TF24.

## Install

```r
remotes::install_github("traitecoevo/standViz")
```

The package was rebuilt in 2026. The 2016 `trees3D` code (`rgl` crowns, adapted from Remko Duursma's Maeswrap) remains in the git history.

## Plant family

`standviz` is part of the **plant family** of packages in the [`traitecoevo`](https://github.com/traitecoevo) org, built around the [`plant`](https://github.com/traitecoevo/plant) forest model. Docs hub: <https://traitecoevo.github.io/overstorey/>.

**Contributing:** please skim the family [issue guide](https://github.com/traitecoevo/plant-meta/blob/main/governance/issue-guide.md) before filing. Issues across the family are triaged on [board #5](https://github.com/orgs/traitecoevo/projects/5), and cross-package context lives in [`plant-meta`](https://github.com/traitecoevo/plant-meta).
