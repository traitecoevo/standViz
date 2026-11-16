## A run, for every function here: a plant_run is tidied, a tidied
## deterministic run is taken as it is, and anything else is refused by name.
tidied_run <- function(x) {
  if (inherits(x, "plant_run")) {
    x <- plant::tidy(x)
  }
  ## A run's $solver and a tidied stochastic run carry no $p.
  if (!inherits(x$p, "Parameters") || !is.data.frame(x$species)) {
    stop("Give a plant_run, or a tidied deterministic run: ",
         "plant::tidy(plant::run(p, ..., record = \"steps\"))", call. = FALSE)
  }
  x
}

## What the run's size state (named `height` in every plant output) measures,
## from plant's model registry. A model whose size is height has a crown to
## draw; one sized otherwise (K93: stem diameter) is drawn by its stems alone.
run_size <- function(results) {
  size <- plant::model_size(results)
  size$has_crown <- identical(size$size, "height")
  size
}

## The smallest tree drawn, in the model's own size units, by the inventory
## conventions: taller than 1.5 m, or wider than 10 cm at the stem. The page
## grows with every tree-frame it carries; see PLAN.md.
default_min_size <- function(size) {
  defaults <- c(m = 1.5, cm = 10)
  if (!size$size_units %in% names(defaults)) {
    stop("No default min_size for a size in ", size$size_units,
         "; give min_size", call. = FALSE)
  }
  defaults[[size$size_units]]
}
