# standviz plan

3D visualisation of `plant` output, from one individual to a whole stand to a metapopulation of patches. This file holds the decisions that constrain the work; keep it current.

## Decided (2026-11-04)

- **The repo is reused and the old code retired.** The 2016 `trees3D` package (`rgl` crowns) is deleted; git history keeps it. The package is now called `standviz`.
- **One realisation layer, renderers plug in.** R turns a `plant` run into explicit, persistent individuals (`realise_stand()`). Renderers read only its output, so a new renderer never needs to know about `plant`.
- **First renderer: three.js in the browser**, written as a single self-contained HTML page (`write_viewer()`). First scale: one stand through time.
- **Later, in order:** Blender scripted headless for cinematic films; then a metapopulation landscape view (patches with ages drawn from the patch-age distribution, which reuses one realised stand). An `rgl`/rayrender quick look is not pursued.
- Game engines and Manim are not pursued: game engines are too heavy to keep reproducible next to a model, and Manim is 2D.

## Invariants (a future edit could break these without noticing)

- **Sampling is plant's.** `realise_stand()` draws trees with `plant::sample_size_distribution()` (traitecoevo/plant#722, fixed in #724), which needs a run carried in birth date. Trees persist through frames and never revive, and at every frame their number is Poisson about the model's density. standviz does no sampling of its own. A test checks counts above `min_height` against `plant::integrate_over_size_distribution(lower = min_height)`.
- **Geometry is the model, not an artist's guess.** Leaf area, stem diameter and every other size come from the strategy's own allometry through `plant::expand_state()`; standviz keeps all of its columns. Crown cross-section area at height `z` is proportional to the Yokozawa leaf density `q(z, H)`. The only free visual parameter is leaf area per crown volume (`lad`).
- **Drawn sizes never shrink.** Height, crown scale and stem diameter take their running maximum per tree, because interpolation across a changing set of nodes can lower a followed height by under a millimetre. Every other column is plant's value.
- **Crowns are drawn down to the height holding 95% of the leaf area.** Below that the Yokozawa profile is a long, near-empty tail that reads as a funnel.
- **Positions are a seeded visual device.** `plant` is spatially implicit. Positions come from best-candidate sampling on a torus, spacing the larger trees by crown size, so stands tile seamlessly. Docs and the viewer must not imply they come from the model.
- **The viewer bundle is the renderer contract**: frames, light profile, static tree records (species, birth date, `x`, `y`, first frame, frame count, offset), and each tree's height, crown scale and stem diameter at every visible frame as 16-bit integers spanning each variable's range, plus any `colour_by` variables. The page grows with tree-frames, by six bytes for the sizes and two for each `colour_by` variable, so `min_height` defaults to 1.5 m: the 100-year demo, with the two default colour variables, is then about 10 MB. At 1 m it would pass the 16 MB limit of a claude.ai page. `stand_long()` gives the same rows at full precision.

## Open, in order

0. **New name.** `standviz` is a placeholder. The package and the GitHub repo (`traitecoevo/standViz`) will be renamed to something that evokes trees and visualisation; the name is undecided. The 2013–14 issues about the old code were closed on 2026-11-04.

1. ~~First PR: package scaffold, `realise_stand()`, `stand_long()`, `write_viewer()`, the three.js stand viewer, and a demo FF16 two-species run (`inst/examples/make_demo.R`).~~ Done.
2. ~~Use plant's sampler and allometry.~~ Done: `plant::sample_size_distribution()` and `plant::expand_state()`.
3. Blender script reading `stand_long()` output (`inst/blender/`).
4. `realise_landscape()` and a landscape view with LOD (impostors far away).
5. TF24 support: crown shape (`expand_state()` already covers its allometry) plus colour by water potential or leaf temperature.
6. Stochastic `plant` runs mapped directly to individuals, skipping the sampling.
