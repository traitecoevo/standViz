# standviz plan

3D visualisation of `plant` output, from one individual to a whole stand to a metapopulation of patches. This file holds the decisions that constrain the work; keep it current.

## Decided (2026-11-04)

- **The repo is reused and the old code retired.** The 2016 `trees3D` package (`rgl` crowns) is deleted; git history keeps it. The package is now called `standviz`.
- **One realisation layer, renderers plug in.** R turns a `plant` run into explicit, persistent individuals (`realise_stand()`). Renderers read only its output, so a new renderer never needs to know about `plant`.
- **First renderer: three.js in the browser**, written as a single self-contained HTML page (`write_viewer()`). First scale: one stand through time.
- **Later, in order:** Blender scripted headless for cinematic films; then a metapopulation landscape view (patches with ages drawn from the patch-age distribution, which reuses one realised stand). An `rgl`/rayrender quick look is not pursued.
- Game engines and Manim are not pursued: game engines are too heavy to keep reproducible next to a model, and Manim is 2D.

## Invariants (a future edit could break these without noticing)

- **Geometry is the model, not an artist's guess.** Leaf area, stem diameter and crown shape come from the strategy's own allometry: FF16 `area_leaf = (h / a_l1)^(1 / a_l2)`, stem area = sapwood + bark + heartwood, and crown cross-section area at height `z` proportional to the Yokozawa leaf density `q(z, H)`. A test checks the R allometry against `plant`'s own `Individual`. The only free visual parameter is leaf area per crown volume (`lad`).
- **Trees are persistent.** Individual `k` of cohort interval `i` sits at a fixed fraction `v` between nodes `i` and `i + 1`, so it grows smoothly. It is alive while its rank is within the interval's current count, so it never comes back after dying.
- **Counts follow the model.** The interval count at time `t` is its count at closure times `exp(-(m̄(t) - m̄(t_close)))`, with `m̄` the mean cumulative mortality of its two bounding nodes. That makes it non-increasing by construction. Rounding is stochastic, with one fixed draw per interval, so it is unbiased and stable over time.
- **Density is log-linear in height within an interval**, for both the closure count and the draw of `v`. Just after an interval forms, density can fall several hundredfold from its seedling end; a trapezium plus an even spread put about 45% too many trees above 1 m at 42 years in the demo. A test checks realised counts above a height threshold against the model's at several times.
- **Crowns are drawn down to the height holding 95% of the leaf area.** Below that the Yokozawa profile is a long, near-empty tail that reads as a funnel.
- **Positions are a seeded visual device.** `plant` is spatially implicit. Positions come from best-candidate sampling on a torus, spacing the larger trees by crown size, so stands tile seamlessly. Docs and the viewer must not imply they come from the model.
- **The viewer bundle is the renderer contract**: frames, light profile, per-species node trajectories, and static tree records (species, interval, `v`, `x`, `y`, birth and death frame). `stand_long()` expands it into a long per-tree-per-frame table for renderers that want that.

## Open, in order

0. **New name.** `standviz` is a placeholder. The package and the GitHub repo (`traitecoevo/standViz`) will be renamed to something that evokes trees and visualisation; the name is undecided. The 2013–14 issues about the old code were closed on 2026-11-04.

1. ~~First PR: package scaffold, `realise_stand()`, `stand_long()`, `write_viewer()`, the three.js stand viewer, and a demo FF16 two-species run (`inst/examples/make_demo.R`).~~ Done.
2. Blender script reading `stand_long()` output (`inst/blender/`).
3. `realise_landscape()` and a landscape view with LOD (impostors far away).
4. TF24 support: allometry plus colour by water potential or leaf temperature.
5. Stochastic `plant` runs mapped directly to individuals, skipping the sampling.
