# `plantpress` — developer guide for agents

`plantpress` turns `plant` output into 3D views: it draws explicit, persistent trees from a run's cohort size distributions (`realise_stand()`), and writes an interactive three.js page of the stand growing through time (`write_viewer()`). It is also plant's home for 2D figures of a run and for the stand report (`stand_figures()`, `stand_report()`).

**Read [`PLAN.md`](PLAN.md) first.** It holds the decisions and the invariants a change could break without noticing: geometry comes from the model's allometry, trees persist, counts follow the model, and positions are a visual device, not model output.

## Layout

- `R/run.R`: what a run is here (`tidied_run()`: a `plant_run` or its tidied list) and what its size measures (`run_size()`, from `plant::model_size()`). A model whose size is height has crowns; one sized otherwise (K93) is drawn by its stems. Nothing here branches on a model's name.
- `R/realise.R`: `realise_stand()` and `stand_long()`. It samples trees with `plant::sample_size_distribution()`, takes their sizes from `plant::expand_state()`, places them, and builds the light profile over the model's size.
- `R/crown.R`: a species' crown from `plant::canopy_profile()` (radius `sqrt(q)`, cut at 95% of the leaf area) and its size at the leaf area density `lad`; the shape is never computed here.
- `R/figures.R`: the 2D figures, each a ggplot (or a patchwork) read from plant's tidy outputs, needing no realisation. A figure with model-specific panels attaches each by the columns the run carries (`water_panels()`), never by the model's name.
- `R/report.R`: `stand_report()`. Everything the report shows is computed here (`report_contents()`: the input tables, `stand_figures()` with a heading and account for each, the viewer page), so it is tested without Quarto; the template only lays it out.
- `inst/report/stand-report.qmd`: the report's Quarto template. It reads the saved contents; the saved figures keep plant and plantpress objects, so the Quarto process needs both on its library path, which `stand_report()` gives it.
- `R/viewer.R`: `write_viewer()` and the viewer's JSON contract (`stand_json()`, `pack_u16()`).
- `inst/viewer/viewer.html`: the viewer template. It is a page fragment (no `<html>`/`<head>`); `write_viewer(standalone = TRUE)` wraps it. Tokens `__STAND_TITLE__` and `__STAND_DATA__` are filled in.
- `inst/examples/make_demo.R`: the two-species FF16 demo.
- `tests/testthat/test-models.R`: one realise-and-draw test per model (FF16, TF24, K93).

## Build & test

- `make install` (runs roxygen first), `make test`, `make check`, `make demo`.
- plantpress is built against the plant commit pinned in `DESCRIPTION`'s `Remotes`, which `.plant-interface-version` also records with the breaking changes absorbed so far. Build that commit, because the site-library copy can lag.
- Runs must carry density in birth date: `plant::control(node_density_coordinate = "birth_date")`.
- To look at the viewer, write a page and screenshot it with headless Chrome: `--headless=new --use-angle=swiftshader --enable-unsafe-swiftshader --screenshot`. Headless Chrome will not go narrower than about 500 px, so a narrower screenshot is cropped rather than reflowed.

## Gotchas

- `plant::expand_state()` pairs species with strategies by number since plant#800; before it, splitting on the character `species` paired them wrongly from the tenth species on. Keep the pin past #800.
- Test counts against plant's integral at collected steps, or the nearest one: a frame between steps compares with a total from a different time.
- Tree placement is quadratic in the trees it spaces, so only large crowns (or, with no crowns, the widest stems) are spaced from one another (see `place_trees()`).
- A tidied run's strategies carry an empty `shading_model`; the run's own is `tidy(res)$control`, which `species_crown()` passes to plant.
- The report embeds the viewer as an iframe's `srcdoc`, so its page-wide styles stay out of the report. Quarto rewrites any raw HTML table it finds, even one escaped inside that attribute, so the template sets `html-table-processing: none`; the render test checks that the viewer arrives whole.
- A figure's labels use plotmath (`yr^{-1}`), not the superscript minus character, which some devices' fonts lack.

## Plant family

`plantpress` is part of the **plant family** in the [`traitecoevo`](https://github.com/traitecoevo)
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
