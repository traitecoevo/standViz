# `standviz` — developer guide for agents

`standviz` (R package name **`trees3D`**) makes **3D plots of individual trees and stands**
in R using `rgl` (openGL). Given crown shapes, tree dimensions, and locations it renders a
stand in 3D; the stand-plotting algorithm was adapted from the
[Maeswrap](https://bitbucket.org/remkoduursma/maeswrap) package. It can be used to visualise
output from the [`plant`](https://github.com/traitecoevo/plant) model.

> **Note:** repo is `standviz`; the package is `trees3D`. It is old (circa 2016,
> `RoxygenNote 5.0.1`) and largely dormant — modernise build tooling if you do substantial work.

## Layout

- `R/` — package functions; `man/` — generated docs; `tests/` — tests; `examples.R` — usage.
- Imports `rgl`; Suggests `testthat`, `devtools`.

## Build & test (Makefile)

- `make install` (the default `all`), `make build`, `make check` — standard R package targets.
- `make roxygen` / `make autodoc` — regenerate docs (don't hand-edit `man/` or `NAMESPACE`).
- `make test` — run tests; `make vignettes` — build `vignettes/introduction.Rmd`.

## Gotchas

- `rgl`/openGL needs a working graphics backend; headless CI may need a virtual framebuffer.

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
