#' Turn a plant run into persistent individual trees
#'
#' `plant` simulates a stand as a density of plants over height, carried along
#' nodes (cohort boundaries). This draws explicit trees from that density that
#' persist through time, so they can be drawn and animated:
#'
#' * Each tree belongs to the interval between two adjacent nodes, and sits at a
#'   fixed fraction `v` of the way from the older node to the younger, so it
#'   grows smoothly with them.
#' * The interval holds its count at the moment it forms (both nodes exist),
#'   thinned by the mean cumulative mortality of its two nodes. That count never
#'   increases, and is rounded with one fixed draw per interval, so it is
#'   unbiased and stable over time. A tree is alive while its rank in the
#'   interval is within the count, so trees die in a fixed order and never come
#'   back.
#' * Positions are **not** from the model, which is spatially implicit. They
#'   are a seeded visual device: best-candidate sampling on a torus, spacing
#'   larger trees by crown size, so the patch tiles seamlessly.
#'
#' @param results Output of `plant::run_scm(p, collect = TRUE)`.
#' @param side Side of the square patch drawn (m).
#' @param frames_per_year Frame rate in model time.
#' @param min_height Trees appear once taller than this (m). Seedlings far
#'   outnumber trees, and are too small to see anyway.
#' @param lad Leaf area per crown volume (m^2 m^-3); see [ff16_geometry()].
#' @param seed Random seed for ranks, positions and rounding. The caller's
#'   random state is left untouched.
#' @return A `standviz_stand`: frame times, the light profile through time,
#'   per-species node trajectories, and one row per tree. Expand it with
#'   [stand_long()] or draw it with [write_viewer()].
#' @export
realise_stand <- function(results, side = 20, frames_per_year = 2,
                          min_height = 1, lad = 1, seed = 1) {
  model <- stand_model(results$p)
  if (model != "FF16") {
    stop("Only FF16 runs can be realised so far, not ", model, call. = FALSE)
  }
  with_seed(seed, {
    sp <- results$species
    t_end <- max(sp$time)
    times <- unique(c(seq(0, t_end, by = 1 / frames_per_year), t_end))
    pars <- lapply(results$p$strategies, `[[`, "pars")
    ids <- sort(unique(as.integer(sp$species)))

    per_species <- lapply(ids, function(s) {
      realise_species(sp[as.integer(sp$species) == s, ], pars[[s]], times,
                      side^2, min_height, lad)
    })
    trees <- do.call(rbind, Map(function(s, x) {
      if (nrow(x$trees) > 0) x$trees$species <- s
      x$trees
    }, ids, per_species))
    trees <- trees[order(trees$birth_frame, trees$species), ]
    xy <- place_trees(trees, side)
    trees$x <- xy$x
    trees$y <- xy$y
    rownames(trees) <- NULL

    eta <- vapply(pars[ids], `[[`, 0, "eta")
    stand <- list(
      meta = list(model = model, side = side, lad = lad,
                  min_height = min_height, seed = seed,
                  plant_version = as.character(utils::packageVersion("plant")),
                  standviz_version =
                    as.character(utils::packageVersion("standviz"))),
      times = times,
      species = data.frame(id = ids, label = species_labels(pars[ids]),
                           eta = eta, eta_c = crown_centre(eta)),
      nodes = lapply(per_species, `[[`, "nodes"),
      trees = trees[c("species", "interval", "v", "x", "y", "birth_frame",
                      "death_frame", "h_max")],
      light = light_profile(results$env$light_availability, times,
                            max(sp$height)))
    class(stand) <- "standviz_stand"
    stand
  })
}

#' @export
print.standviz_stand <- function(x, ...) {
  alive <- vapply(seq_along(x$times), function(f) {
    sum(x$trees$birth_frame <= f & x$trees$death_frame > f)
  }, 0)
  cat(sprintf("<standviz_stand> %s, %d species, %g x %g m patch\n",
              x$meta$model, nrow(x$species), x$meta$side, x$meta$side))
  cat(sprintf("  %d frames over %g years; %d trees ever, up to %d at once\n",
              length(x$times), max(x$times), nrow(x$trees), max(alive)))
  invisible(x)
}

## Node trajectories at the frame times, and the trees of one species.
realise_species <- function(d, pars, times, area, min_height, lad) {
  d <- d[order(d$node, d$time), ]
  ## A node with no survivors carries infinite cumulative mortality, which a
  ## spline cannot pass through; exp(-700) is as dead.
  d$mortality <- pmin(d$mortality, 700)
  by_node <- lapply(split(d, d$node), function(x) x[!duplicated(x$time), ])
  birth <- vapply(by_node, function(x) min(x$time), 0)
  n_frames <- length(times)

  ## Height, cumulative mortality and heartwood only ever rise, so a monotone
  ## spline keeps them rising between the solver's (irregular) steps.
  at_frames <- function(col) {
    vapply(by_node, function(x) {
      out <- rep(NA_real_, n_frames)
      ok <- times >= min(x$time) & times <= max(x$time)
      if (nrow(x) == 1) {
        out[ok] <- x[[col]]
      } else {
        out[ok] <- stats::splinefun(x$time, x[[col]], method = "monoH.FC")(times[ok])
      }
      out
    }, numeric(n_frames))
  }
  height <- at_frames("height")
  mortality <- at_frames("mortality")
  geom <- ff16_geometry(height, at_frames("area_heartwood"), pars, lad)
  dim(geom$crown_scale) <- dim(geom$stem_diameter) <- dim(height)

  trees <- lapply(seq_len(length(by_node) - 1), function(i) {
    realise_interval(i, by_node, height, mortality, geom$crown_scale, times,
                     area, min_height)
  })
  trees <- do.call(rbind, trees)
  if (!is.null(trees)) {
    trees$r_max <- trees$crown_max *
      max(crown_profile(seq(0, 1, by = 0.001), pars$eta))
  }
  list(nodes = list(birth = unname(birth), height = height,
                    crown_scale = geom$crown_scale,
                    stem_diameter = geom$stem_diameter),
       trees = trees)
}

## The trees of the interval between node i (older, taller) and node i + 1.
realise_interval <- function(i, by_node, height, mortality, crown_scale, times,
                             area, min_height) {
  young <- by_node[[i + 1]]
  t_close <- young$time[1]
  old <- by_node[[i]][by_node[[i]]$time == t_close, ]
  ## Number when the interval forms. Density is taken as log-linear in height
  ## between the nodes: just after forming it can fall several hundredfold from
  ## the seedling end, which a trapezium (and an even spread) badly overcounts.
  slope <- log(max(young$density[1], 1e-300) / max(old$density, 1e-300))
  n_close <- area * (old$height - young$height[1]) *
    old$density * exp_mean(slope)
  frames <- which(times >= t_close & !is.na(height[, i + 1]))
  if (length(frames) == 0 || !(n_close > 0)) return(NULL)

  m_bar <- (mortality[frames, i] + mortality[frames, i + 1]) / 2
  m_close <- (old$mortality + young$mortality[1]) / 2
  expected <- cummin(pmin(n_close * exp(-(m_bar - m_close)), n_close))
  count <- floor(expected + stats::runif(1))
  n_max <- count[1]
  if (n_max == 0) return(NULL)

  ## Rank r is alive while count >= r; count never rises, so that is a run of
  ## frames starting at the interval's first.
  rank <- sample.int(n_max)
  death <- frames[1] + vapply(rank, function(r) sum(count >= r), 0)
  ## Fraction of the way to the young node, drawn from the same log-linear
  ## density.
  u <- stats::runif(n_max)
  v <- if (abs(slope) < 1e-8) u else log1p(u * expm1(slope)) / slope
  h_tree <- outer(height[frames, i], 1 - v) + outer(height[frames, i + 1], v)
  first_visible <- apply(h_tree >= min_height, 2, function(z) match(TRUE, z))
  birth <- frames[1] - 1 + first_visible
  keep <- !is.na(birth) & birth < death
  if (!any(keep)) return(NULL)

  last <- pmin(death[keep] - 1, length(times))
  lerp <- function(m) m[cbind(last, i)] * (1 - v[keep]) + m[cbind(last, i + 1)] * v[keep]
  data.frame(interval = i, v = v[keep], birth_frame = birth[keep],
             death_frame = death[keep], h_max = lerp(height),
             crown_max = lerp(crown_scale))
}

## Mean of exp(slope * v) over v in [0, 1].
exp_mean <- function(slope) {
  if (abs(slope) < 1e-8) 1 else expm1(slope) / slope
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
