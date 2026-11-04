# `standviz` — developer guide for agents

`standviz` turns `plant` output into 3D views: it draws explicit, persistent trees from a run's cohort size distributions (`realise_stand()`), and writes an interactive three.js page of the stand growing through time (`write_viewer()`). `standviz` is a working name; a rename is planned.

**Read [`PLAN.md`](PLAN.md) first.** It holds the decisions and the invariants a change could break without noticing: geometry comes from the model's allometry, trees persist, counts follow the model, and positions are a visual device, not model output.

## Layout

- `R/realise.R`: `realise_stand()`. It samples trees with `plant::sample_size_distribution()`, takes their sizes from `plant::expand_state()`, places them, and builds the light profile.
- `R/allometry.R`: crown drawing only (`crown_scale()`, `crown_profile()`); every other size comes from plant.
- `R/output.R`: `stand_long()`, `write_viewer()` and the viewer's JSON contract (`stand_json()`, `pack_u16()`).
- `inst/viewer/viewer.html`: the viewer template. It is a page fragment (no `<html>`/`<head>`); `write_viewer(standalone = TRUE)` wraps it. Tokens `__STAND_TITLE__` and `__STAND_DATA__` are filled in.
- `inst/examples/make_demo.R`: the two-species FF16 demo.

## Build & test

- `make install` (runs roxygen first), `make test`, `make check`, `make demo`.
- standviz needs a plant with `sample_size_distribution()` (traitecoevo/plant#722 and its fix #724). Build plant from `develop`, because the site-library copy can lag.
- Runs must carry density in birth date: `plant::control(node_density_coordinate = "birth_date")`.
- To look at the viewer, write a page and screenshot it with headless Chrome: `--headless=new --use-angle=swiftshader --enable-unsafe-swiftshader --screenshot`. Headless Chrome will not go narrower than about 500 px, so a narrower screenshot is cropped rather than reflowed.

## Gotchas

- `plant::expand_state()` pairs species with strategies by position after a `split()`, so `tree_geometry()` makes `species` a factor carrying every level, or a species with no trees would shift the pairing.
- Test counts against plant's integral at collected steps, or the nearest one: a frame between steps compares with a total from a different time.
- Tree placement is quadratic in the trees it spaces, so only large crowns are spaced from one another (see `place_trees()`).

## Plant family

`standviz` is part of the **plant family** in the [`traitecoevo`](https://github.com/traitecoevo)
org — a hub-and-spoke set of packages built around the
[`plant`](https://github.com/traitecoevo/plant) size- and trait-structured forest model.

- **Docs hub** — family user guides & theory: <https://traitecoevo.github.io/overstorey/>
- **Cross-package orientation** — how the family fits together (who depends on whom,
  source-of-truth rules, cross-repo gotchas) lives in
  [`plant-meta`](https://github.com/traitecoevo/plant-meta); start with its
  [`AGENTS.md`](https://github.com/traitecoevo/plant-meta/blob/main/AGENTS.md). Keep
  family-wide concerns there, not here.
- **Issues & board** — follow the
  [issue guide](https://github.com/traitecoevo/plant-meta/blob/main/governance/issue-guide.md);
  work is tracked on [board #5](https://github.com/orgs/traitecoevo/projects/5) (new issues
  auto-add with no Status = the triage queue). Labels: `bug` / `task` / `epic` plus `blocked`,
  `needs-info`, `cross-package`, `breaking`, `question`.
- **Commit messages** — the repo squash-merges, so a PR's title and body are copied verbatim into
  permanent history. Keep them short and durable, and put the working detail (measurements,
  alternatives rejected, what you tried first) in the first PR comment instead — see
  [`commit-messages.md`](https://github.com/traitecoevo/plant-meta/blob/main/governance/commit-messages.md).
