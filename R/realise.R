#' Turn a plant run into persistent individual trees
#'
#' `plant` simulates a stand as a density of plants over size and birth date.
#' This draws explicit trees from it with [plant::sample_size_distribution()],
#' which follows the same individuals through every frame: each grows with its
#' cohort and, once dead, stays dead, and at every frame the number of trees is
#' Poisson about the model's density. Every size comes from the model's own
#' allometry through [plant::expand_state()], and each species' crown shape
#' from [plant::canopy_profile()].
#'
#' Any plant model can be realised. A model whose size is height (FF16, the
#' TF24 family) has crowns. One sized otherwise has none: K93's size is stem
#' diameter and it has no height, leaf area or crown, so its trees are stems
#' alone, and none is invented. [plant::model_size()] says which a model is.
#'
#' Positions are **not** from the model, which is spatially implicit. They are a
#' seeded visual device: best-candidate sampling on a torus, spacing larger
#' trees by crown (or stem) size, so the patch tiles seamlessly.
#'
#' @param x A `plant_run` recorded with `record = "steps"`, or the tidied run
#'   `plant::tidy()` gives, run with density in birth date:
#'   `plant::control(node_density_coordinate = "birth_date")`.
#' @param side Side of the square patch drawn (m).
#' @param frames_per_year Frame rate in model time.
#' @param min_size Trees appear once larger than this, in the model's own size
#'   units ([plant::model_size()]). Seedlings far outnumber trees and are too
#'   small to see, and the viewer page grows with every tree-frame it carries.
#'   The default follows the inventory conventions: 1.5 m tall, or 10 cm of
#'   stem diameter.
#' @param lad Leaf area per crown volume (m^2 m^-3), the one visual parameter of
#'   a crown: its size holds the plant's leaf area at this density.
#' @param seed Random seed for the sample and the positions. The caller's random
#'   state is left untouched.
#' @return A `plantpress_stand`: frame times, the light profile through time over
#'   the model's size, one row per species (with its crown, if it has one), one
#'   row per tree, and one row per tree per frame it is visible carrying every
#'   state variable from the run and from [plant::expand_state()]. Expand it
#'   with [stand_long()] or draw it with [write_viewer()].
#' @export
realise_stand <- function(x, side = 20, frames_per_year = 2, min_size = NULL,
                          lad = 1, seed = 1) {
  results <- tidied_run(x)
  size <- run_size(results)
  if (is.null(min_size)) {
    min_size <- default_min_size(size)
  }
  with_seed(seed, {
    sp <- results$species
    t_end <- max(sp$time)
    times <- unique(c(seq(0, t_end, by = 1 / frames_per_year), t_end))
    rows <- plant::sample_size_distribution(results, area = side^2, times = times)
    rows <- tree_geometry(rows[rows$height >= min_size, ], results, lad,
                          size$has_crown)

    ## Size never falls, so a tree's visible frames are one unbroken run.
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
                        death_frame = rows$frame[last] + 1L)

    strategies <- results$p$strategies
    ids <- seq_along(strategies)
    species <- data.frame(id = ids, label = species_labels(results$p))
    crowns <- NULL
    if (size$has_crown) {
      crowns <- lapply(strategies, species_crown, control = results$control)
      species$eta_c <- vapply(crowns, `[[`, 0, "eta_c")
      widest <- vapply(crowns, `[[`, 0, "widest")
      trees$r_max <- rows$crown_scale[last] * widest[trees$species]
    } else {
      trees$r_max <- rows$diameter_stem[last] / 2
    }
    xy <- place_trees(trees, side)
    trees$x <- xy$x
    trees$y <- xy$y

    stand <- list(
      meta = list(model = size$model, size = size$size,
                  size_units = size$size_units, has_crown = size$has_crown,
                  side = side, lad = lad, min_size = min_size, seed = seed,
                  plant_version = as.character(utils::packageVersion("plant")),
                  plantpress_version =
                    as.character(utils::packageVersion("plantpress"))),
      times = times,
      species = species,
      crowns = crowns,
      trees = trees[c("species", "birth_time", "x", "y", "birth_frame",
                      "death_frame")],
      rows = tree_rows(rows, size$has_crown),
      light = light_profile(results$env$light_availability, times,
                            max(sp$height, na.rm = TRUE)))
    class(stand) <- "plantpress_stand"
    stand
  })
}

#' @export
print.plantpress_stand <- function(x, ...) {
  cat(sprintf("<plantpress_stand> %s, %d species, %g x %g m patch\n",
              x$meta$model, nrow(x$species), x$meta$side, x$meta$side))
  cat(sprintf("  %d frames over %g years; %d trees ever, up to %d at once\n",
              length(x$times), max(x$times), nrow(x$trees),
              max(c(0L, tabulate(x$rows$frame)))))
  if (!x$meta$has_crown) {
    cat(sprintf("  sized by %s (%s): stems only, no crowns\n",
                x$meta$size, x$meta$size_units))
  }
  invisible(x)
}

#' One row per living tree per frame
#'
#' Expands a realised stand into a long table, for renderers and analyses that
#' want every tree's size at every frame (a Blender script, a check against
#' plant's size distribution).
#'
#' @param stand A `plantpress_stand` from [realise_stand()].
#' @return A data frame with `frame`, `time`, `tree`, `species`, `x`, `y`,
#'   `height` (the model's size, in its own units: stem diameter in cm for K93),
#'   `crown_scale` and `crown_radius` (the widest point; `NA` for a model with
#'   no crown), `stem_diameter` (m), `light_crown` (the light at the crown's
#'   leaf-weighted centre, or at the tree's own size for a model with no
#'   crown), and every other state variable the stand carries, such as
#'   `mortality`, `fecundity` and, where the model has them, `area_leaf` and
#'   `net_mass_production_dt`.
#' @export
stand_long <- function(stand) {
  r <- stand$rows
  tr <- stand$trees
  k <- match(tr$species[r$tree], stand$species$id)
  crowned <- isTRUE(stand$meta$has_crown)
  none <- rep(NA_real_, nrow(r))
  crown_scale <- if (crowned) r$crown_scale else none
  widest <- if (crowned) vapply(stand$crowns, `[[`, 0, "widest")[k] else none
  z <- if (crowned) r$height * stand$species$eta_c[k] else r$height
  out <- data.frame(frame = r$frame, time = stand$times[r$frame], tree = r$tree,
                    species = tr$species[r$tree], x = tr$x[r$tree], y = tr$y[r$tree],
                    height = r$height, crown_scale = crown_scale,
                    crown_radius = crown_scale * widest,
                    stem_diameter = r$stem_diameter,
                    light_crown = light_at(stand$light, r$frame, z))
  cbind(out, r[setdiff(names(r), names(out))])
}

## Canopy openness at size z in frame f, linear on the light grid.
light_at <- function(light, frame, z) {
  g <- light$height
  pos <- pmin(pmax((z - g[1]) / (g[2] - g[1]), 0), length(g) - 1 - 1e-9)
  lo <- floor(pos) + 1
  w <- pos - (lo - 1)
  light$value[cbind(frame, lo)] * (1 - w) + light$value[cbind(frame, lo + 1)] * w
}

## One row per tree per visible frame: what the viewer draws first, then every
## other state variable plant reports or expand_state() derives.
tree_rows <- function(rows, crowned) {
  rows$stem_diameter <- rows$diameter_stem
  ## Interpolation across a changing set of nodes can lower a followed size by
  ## a fraction of a millimetre between frames. A drawn tree never shrinks, so
  ## the sizes the viewer draws take their running maximum; every other column
  ## is plant's value.
  drawn <- c("height", if (crowned) "crown_scale", "stem_diameter")
  for (v in drawn) {
    rows[[v]] <- stats::ave(rows[[v]], rows$tree, FUN = cummax)
  }
  first <- c("tree", "frame", drawn)
  drop <- c("species", "individual", "birth_time", "time", "row",
            "diameter_stem")
  rest <- setdiff(names(rows)[vapply(rows, is.numeric, TRUE)], c(first, drop))
  as.data.frame(rows[c(first, rest)])
}

## Every size from plant's own allometry, and, for a crowned model, the crown
## size its leaf area gives at the chosen leaf area density.
tree_geometry <- function(rows, results, lad, crowned) {
  rows$row <- seq_len(nrow(rows))
  expanded <- plant::expand_state(list(species = rows, n_spp = results$n_spp,
                                       p = results$p))$species
  expanded <- expanded[order(expanded$row), ]
  expanded$species <- as.integer(as.character(expanded$species))
  if (crowned) {
    expanded$crown_scale <- crown_scale(expanded$area_leaf, expanded$height, lad)
  }
  expanded
}

## Best-candidate sampling on a torus. Trees are placed largest first, each at
## whichever of `k` candidate spots is furthest, relative to the two trees'
## sizes, from the large trees already placed that share some of its lifetime.
## Only large trees (radius >= r_big) are spaced from each other; medium trees
## are spaced from the large ones only, and small trees, the great majority,
## are scattered uniformly. Spacing every pair is quadratic in a count that
## runs to tens of thousands. `r_max` is a crown's widest radius, or a stem's.
## Stems (K93) are far narrower than any crown, so when every tree is narrower
## than a medium crown (stems, or a stand of seedlings) the widest tenth are
## spaced from each other and the rest scattered: with a single medium class,
## every stem would be pushed away from the same few anchors and they would
## bunch on the far side of the torus.
place_trees <- function(trees, side, k = 12, r_big = 0.5, r_medium = 0.25) {
  n <- nrow(trees)
  x <- stats::runif(n, 0, side)
  y <- stats::runif(n, 0, side)
  r <- trees$r_max
  if (n > 0 && max(r) < r_medium) {
    r_big <- r_medium <- unname(stats::quantile(r, 0.9))
  }
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

## The light the model computed on a fixed grid of its size at each frame, for
## shading crowns and the ground.
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

## Label species by the parameters that differ between them: their traits if
## any trait differs, else any parameter, never a derived one or a control,
## each as plant's parameter table classes it.
species_labels <- function(p) {
  pars <- lapply(p$strategies, `[[`, "pars")
  if (length(pars) == 1) return("species 1")
  vals <- do.call(rbind, lapply(pars, function(s) unlist(s[vapply(s, is.numeric, TRUE)])))
  varies <- colnames(vals)[apply(vals, 2, function(z) length(unique(z)) > 1)]
  model <- plant::model_size(p)$model
  table <- plant::parameter_table(model)
  traits <- intersect(varies, table$name[table$group == "trait"])
  given <- intersect(varies, table$name[table$group == "parameter"])
  varies <- if (length(traits) > 0) traits else given
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
