#' Turn a plant run into persistent individual trees
#'
#' `plant` simulates a stand as a density of plants over height and birth date.
#' This draws explicit trees from it with [plant::sample_size_distribution()],
#' which follows the same individuals through every frame: each grows with its
#' cohort and, once dead, stays dead, and at every frame the number of trees is
#' Poisson about the model's density. Leaf area and stem diameter come from the
#' strategy's own allometry through [plant::expand_state()].
#'
#' Positions are **not** from the model, which is spatially implicit. They are a
#' seeded visual device: best-candidate sampling on a torus, spacing larger
#' trees by crown size, so the patch tiles seamlessly.
#'
#' @param results Output of `plant::run_scm(p, ctrl, collect = TRUE)`, run with
#'   density in birth date: `plant::control(node_density_coordinate =
#'   "birth_date")`.
#' @param side Side of the square patch drawn (m).
#' @param frames_per_year Frame rate in model time.
#' @param min_height Trees appear once taller than this (m). Seedlings far
#'   outnumber trees and are too small to see, and the viewer page grows with
#'   every tree-frame it carries.
#' @param lad Leaf area per crown volume (m^2 m^-3); see [crown_scale()].
#' @param seed Random seed for the sample and the positions. The caller's random
#'   state is left untouched.
#' @return A `standviz_stand`: frame times, the light profile through time, one
#'   row per tree, and one row per tree per frame it is visible carrying every
#'   state variable from the run and from [plant::expand_state()] (leaf, sapwood,
#'   bark and root areas and masses, fecundity, net production, ...). Expand it
#'   with [stand_long()] or draw it with [write_viewer()].
#' @export
realise_stand <- function(results, side = 20, frames_per_year = 2,
                          min_height = 1.5, lad = 1, seed = 1) {
  model <- stand_model(results$p)
  if (model != "FF16") {
    stop("Only FF16 runs can be realised so far, not ", model, call. = FALSE)
  }
  if (!"sample_size_distribution" %in% getNamespaceExports("plant")) {
    stop("realise_stand() needs a plant with sample_size_distribution() ",
         "(traitecoevo/plant#722)", call. = FALSE)
  }
  with_seed(seed, {
    sp <- results$species
    t_end <- max(sp$time)
    times <- unique(c(seq(0, t_end, by = 1 / frames_per_year), t_end))
    rows <- plant::sample_size_distribution(results, area = side^2, times = times)
    rows <- tree_geometry(rows[rows$height >= min_height, ], results, lad)

    ## Height never falls, so a tree's visible frames are one unbroken run.
    rows$frame <- match(rows$time, times)
    rows <- rows[order(rows$frame, rows$species, rows$individual), ]
    key <- paste(rows$species, rows$individual)
    rows$tree <- match(key, unique(key))
    rows <- rows[order(rows$tree, rows$frame), ]
    first <- !duplicated(rows$tree)
    last <- !duplicated(rows$tree, fromLast = TRUE)
    trees <- data.frame(species = as.integer(rows$species[first]),
                        birth_time = rows$birth_time[first],
                        birth_frame = rows$frame[first],
                        death_frame = rows$frame[last] + 1L,
                        r_max = rows$crown_scale[last])
    pars <- lapply(results$p$strategies, `[[`, "pars")
    ids <- sort(unique(as.integer(sp$species)))
    eta <- vapply(pars[ids], `[[`, 0, "eta")
    trees$r_max <- trees$r_max * crown_widest(eta)[match(trees$species, ids)]
    xy <- place_trees(trees, side)
    trees$x <- xy$x
    trees$y <- xy$y

    stand <- list(
      meta = list(model = model, side = side, lad = lad,
                  min_height = min_height, seed = seed,
                  plant_version = as.character(utils::packageVersion("plant")),
                  standviz_version =
                    as.character(utils::packageVersion("standviz"))),
      times = times,
      species = data.frame(id = ids, label = species_labels(pars[ids]),
                           eta = eta, eta_c = crown_centre(eta)),
      trees = trees[c("species", "birth_time", "x", "y", "birth_frame",
                      "death_frame")],
      rows = tree_rows(rows),
      light = light_profile(results$env$light_availability, times,
                            max(sp$height)))
    class(stand) <- "standviz_stand"
    stand
  })
}

#' @export
print.standviz_stand <- function(x, ...) {
  cat(sprintf("<standviz_stand> %s, %d species, %g x %g m patch\n",
              x$meta$model, nrow(x$species), x$meta$side, x$meta$side))
  cat(sprintf("  %d frames over %g years; %d trees ever, up to %d at once\n",
              length(x$times), max(x$times), nrow(x$trees),
              max(c(0L, tabulate(x$rows$frame)))))
  invisible(x)
}

## One row per tree per visible frame: what the viewer draws first, then every
## other state variable plant reports or expand_state() derives.
tree_rows <- function(rows) {
  rows$stem_diameter <- rows$diameter_stem
  ## Interpolation across a changing set of nodes can lower a followed height by
  ## a fraction of a millimetre between frames. A drawn tree never shrinks, so
  ## the sizes the viewer draws take their running maximum; every other column
  ## is plant's value.
  for (v in c("height", "crown_scale", "stem_diameter")) {
    rows[[v]] <- stats::ave(rows[[v]], rows$tree, FUN = cummax)
  }
  first <- c("tree", "frame", "height", "crown_scale", "stem_diameter")
  drop <- c("species", "individual", "birth_time", "time", "row",
            "diameter_stem")
  rest <- setdiff(names(rows)[vapply(rows, is.numeric, TRUE)], c(first, drop))
  as.data.frame(rows[c(first, rest)])
}

## Leaf area and stem diameter from plant's own allometry, and the crown size
## they give at the chosen leaf area density.
tree_geometry <- function(rows, results, lad) {
  spp <- as.character(sort(unique(results$species$species)))
  rows$row <- seq_len(nrow(rows))
  ## A factor keeps a species with no trees in expand_state()'s split, which
  ## pairs the pieces with strategies by position.
  rows$species <- factor(rows$species, levels = spp)
  expanded <- plant::expand_state(list(species = rows, n_spp = length(spp),
                                       p = results$p))$species
  expanded <- expanded[order(expanded$row), ]
  expanded$species <- as.character(expanded$species)
  expanded$crown_scale <- crown_scale(expanded$area_leaf, expanded$height, lad)
  expanded
}

## Best-candidate sampling on a torus. Trees are placed largest first, each at
## whichever of `k` candidate spots is furthest, relative to the two crowns'
## sizes, from the large trees already placed that share some of its lifetime.
## Only large trees (crown radius >= r_big) are spaced from each other; medium
## trees are spaced from the large ones only, and small trees, the great
## majority, are scattered uniformly. Spacing every pair is quadratic in a
## count that runs to tens of thousands.
place_trees <- function(trees, side, k = 12, r_big = 0.5, r_medium = 0.25) {
  n <- nrow(trees)
  x <- stats::runif(n, 0, side)
  y <- stats::runif(n, 0, side)
  r <- trees$r_max
  birth <- trees$birth_frame
  death <- trees$death_frame
  todo <- order(-r)
  todo <- todo[r[todo] >= r_medium]
  anchors <- integer(0)
  for (j in todo) {
    p <- anchors[birth[anchors] < death[j] & birth[j] < death[anchors]]
    if (length(p) > 0) {
      cx <- stats::runif(k, 0, side)
      cy <- stats::runif(k, 0, side)
      dx <- abs(outer(cx, x[p], `-`))
      dy <- abs(outer(cy, y[p], `-`))
      dx <- pmin(dx, side - dx)
      dy <- pmin(dy, side - dy)
      gap <- sqrt(dx^2 + dy^2) / outer(rep(r[j], k), r[p], `+`)
      best <- which.max(apply(gap, 1, min))
      x[j] <- cx[best]
      y[j] <- cy[best]
    }
    if (r[j] >= r_big) anchors <- c(anchors, j)
  }
  list(x = x, y = y)
}

## Canopy openness on a fixed height grid at each frame: the light the model
## computed, for shading crowns and the ground.
light_profile <- function(light, times, h_top, n = 48) {
  grid <- seq(0, h_top * 1.05, length.out = n)
  steps <- split(light, light$time)
  step_times <- as.numeric(names(steps))
  on_grid <- t(vapply(steps, function(s) {
    stats::approx(s$height, s$light_availability, grid, rule = 2,
                  ties = mean)$y
  }, numeric(n)))
  value <- apply(on_grid, 2, function(col) {
    stats::approx(step_times, col, times, rule = 2, ties = mean)$y
  })
  list(height = grid, value = value)
}

stand_model <- function(p) {
  sub("^Parameters<([^,>]+).*$", "\\1", class(p)[1])
}

## Label species by the traits that differ between them. Strategies carry
## derived parameters too (FF16 derives r_l and k_l from lma), so known traits
## are preferred when any of them vary.
species_labels <- function(pars) {
  if (length(pars) == 1) return("species 1")
  vals <- do.call(rbind, lapply(pars, function(p) unlist(p[vapply(p, is.numeric, TRUE)])))
  varies <- colnames(vals)[apply(vals, 2, function(z) length(unique(z)) > 1)]
  traits <- intersect(c("lma", "rho", "hmat", "narea", "a_f3", "eta", "omega"), varies)
  if (length(traits) > 0) varies <- traits
  if (length(varies) == 0) return(paste("species", seq_along(pars)))
  apply(vals[, varies, drop = FALSE], 1, function(z) {
    paste(varies, signif(z, 3), sep = " = ", collapse = ", ")
  })
}

with_seed <- function(seed, code) {
  if (exists(".Random.seed", globalenv())) {
    old <- get(".Random.seed", globalenv())
    on.exit(assign(".Random.seed", old, globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}
