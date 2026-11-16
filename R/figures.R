## 2D figures of a run. Each reads plant's tidy outputs and returns a ggplot
## (or a patchwork of them), so a caller can add `+ ggplot2::theme(...)`. A
## figure that needs a model-specific output states the columns it reads and
## refuses a run without them; nothing branches on a model's name.

#' @importFrom ggplot2 .data
NULL

#' Size distribution through time
#'
#' One line per node (cohort) of each species, its size against time, shaded by
#' the node's density relative to the densest: sparse nodes appear faint.
#'
#' @param data_species A tidied run's `species` table, or the run itself (a
#'   `plant_run` or a tidied deterministic run).
#' @param model The model, to label the size axis when `data_species` is a bare
#'   table, which does not record it. A run labels its own, and `model` is
#'   then ignored. With neither the axis reads "Height (m)", as in every model
#'   sized by height.
#' @return A ggplot.
#' @export
#' @examples
#' \dontrun{
#' res <- plant::run(p, record = "steps")
#' plot_size_distribution(res)
#' plot_size_distribution(plant::tidy(res)$species)
#' }
plot_size_distribution <- function(data_species, model = NULL) {
  if (!is.data.frame(data_species)) {
    run <- tidied_run(data_species)
    data_species <- run$species
    model <- run
  }
  d <- data_species[!is.na(data_species$density), ]
  d$relative_log_density <- relative_log_density(d$log_density)
  d$species <- as.character(d$species)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$time, y = .data$height, colour = .data$species,
      group = interaction(.data$node, .data$species),
      alpha = .data$relative_log_density)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_alpha(range = c(0.01, 1)) +
    ggplot2::labs(x = "Time (years)", y = size_label(model), colour = "Species",
                  alpha = "Relative log density") +
    ggplot2::theme_classic()
}

#' Soil water and plant water use through time
#'
#' A panel for each water variable the run carries: rainfall, soil moisture
#' and soil water potential by layer, transpiration in total and per unit leaf
#' area, and, per node, leaf and root-collar water potential, stomatal
#' conductance, profit and leaf temperature. Rainfall and transpiration are
#' rates over each interval between collected steps, from the run's own
#' cumulative fluxes. Leaf temperature stays at the prescribed driver unless
#' the strategy solved its energy balance.
#'
#' @param x A `plant_run` or a tidied deterministic run whose environment
#'   records soil water (`env$soil_moist`), as every TF24 model's does.
#' @return A patchwork of ggplots.
#' @export
plot_water <- function(x) {
  patchwork::wrap_plots(water_panels(tidied_run(x)), ncol = 2)
}

## plot_water()'s panels, named by the column each draws, leaving out any the
## run does not carry.
water_panels <- function(res) {
  env <- res$env
  if (is.null(env$soil_moist)) {
    stop("plot_water() needs a run that records soil water (env$soil_moist); ",
         "this ", plant::model_size(res)$model, " run does not", call. = FALSE)
  }
  size <- size_label(res)
  leaf_area <- patch_leaf_area(res)
  panels <- list(
    rainfall = flux_panel(env, "sum_rainfall",
                          expression(Rainfall ~ (m ~ yr^{-1}))),
    soil_moist = layer_panel(env, "soil_moist",
                             expression(Soil ~ moisture ~ (m^3 ~ m^{-3}))),
    soil_psi = layer_panel(env, "soil_psi",
                           expression(psi[soil] ~ (-MPa))),
    transpiration = flux_panel(env, "sum_resource_depletion",
                               expression(Transpiration ~ (m ~ yr^{-1}))),
    transpiration_per_leaf_area = if (!is.null(leaf_area)) flux_panel(
      env, "sum_resource_depletion",
      expression(Transpiration ~ per ~ leaf ~ area ~ (m ~ yr^{-1})),
      per = leaf_area),
    opt_psi_leaf = node_panel(res$species, "opt_psi_leaf", size,
                              expression(psi[leaf] ~ (-MPa))),
    opt_root_psi = node_panel(res$species, "opt_root_psi", size,
                              expression(psi[root] ~ (-MPa))),
    stom_cond_CO2 = node_panel(res$species, "stom_cond_CO2", size,
                               expression(g[s] ~ (mol ~ m^{-2} ~ s^{-1}))),
    profit = node_panel(res$species, "profit", size,
                        expression(Profit ~ (mu * mol ~ m^{-2} ~ s^{-1}))),
    Tleaf = node_panel(res$species, "Tleaf", size,
                       expression(T[leaf] ~ (degree * C))))
  panels[!vapply(panels, is.null, logical(1))]
}

## The axis label for the size state, from plant's model registry: "Height
## (m)" for FF16 and TF24, "Stem diameter (cm)" for K93. `model` is anything
## plant::model_size() reads, or NULL for a table that does not say.
size_label <- function(model) {
  if (is.null(model)) return("Height (m)")
  size <- plant::model_size(model)
  sprintf("%s%s (%s)", toupper(substr(size$size, 1, 1)),
          substring(size$size, 2), size$size_units)
}

## log density mapped onto [0, 1], with everything below e^-4 at 0. plant's
## own figure did the same.
relative_log_density <- function(x, xmin = -4) {
  x[x < xmin] <- xmin
  (x - xmin) / (max(x, na.rm = TRUE) - xmin)
}

## The patch's leaf area per unit ground at each collected time, summed over
## species, as plant integrates it over the size distribution.
patch_leaf_area <- function(res) {
  totals <- plant::integrate_over_size_distribution(
    plant::expand_state(res)$species)
  if (is.null(totals$area_leaf)) return(NULL)
  stats::aggregate(area_leaf ~ time, totals, sum)
}

## A rate from a cumulative flux, over each interval between collected steps,
## drawn at the interval's end. With `per` (time, value) the rate is divided by
## that value at the same time.
flux_panel <- function(env, column, ylab, per = NULL) {
  flux <- env$soil_moist_cumulative_flux
  if (is.null(flux[[column]])) return(NULL)
  rate <- diff(flux[[column]]) / diff(flux$time)
  d <- data.frame(time = flux$time[-1], rate = rate)
  if (!is.null(per)) {
    d$rate <- d$rate / per[[2]][match(d$time, per$time)]
  }
  d <- d[is.finite(d$rate), ]
  ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$rate)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time (years)", y = ylab) +
    ggplot2::theme_classic()
}

## A soil variable, one line per layer, coloured by the layer's depth. Each
## environment table holds one row per layer per step, in layer order.
layer_panel <- function(env, column, ylab) {
  d <- env[[column]]
  if (is.null(d) || is.null(env$soil_depth)) return(NULL)
  d <- data.frame(time = d$time, value = d[[column]],
                  depth = env$soil_depth$soil_depth)
  ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$value, group = .data$depth,
                                  colour = .data$depth)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = "Time (years)", y = ylab, colour = "Soil depth (m)") +
    ggplot2::theme_classic()
}

## A per-node variable, one line per node, coloured by the node's size.
node_panel <- function(species, column, size, ylab) {
  if (is.null(species[[column]])) return(NULL)
  d <- species[!is.na(species$density), ]
  ggplot2::ggplot(d, ggplot2::aes(.data$time, .data[[column]],
                                  group = interaction(.data$node, .data$species),
                                  colour = .data$height)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = "Time (years)", y = ylab, colour = size) +
    ggplot2::theme_classic()
}

#' Stand totals through time
#'
#' Totals per unit ground area, integrated over the size distribution by
#' plant: plant density, leaf area index and basal area by species, and
#' biomass by tissue over all species. Each panel is drawn where the run
#' carries its sizes; a K93 run, which has no leaf area or tissue masses,
#' draws density and basal area.
#'
#' @inheritParams plot_water
#' @return A patchwork of ggplots.
#' @export
plot_stand_totals <- function(x) {
  patchwork::wrap_plots(stand_panels(tidied_run(x)), ncol = 2)
}

#' Growth, mortality and fecundity by size
#'
#' Each node's rates at chosen patch ages, against its size: growth in size,
#' the mortality rate and the fecundity rate, by species. Each rate is over
#' the interval between collected steps that ends at the age drawn, along the
#' node's trajectory: of its size, and of plant's cumulative mortality and
#' fecundity.
#'
#' @inheritParams plot_water
#' @param ages Patch ages (years) to draw; each is drawn at the nearest
#'   collected time. The default spans the run on a roughly doubling scale.
#' @return A ggplot.
#' @export
plot_by_size <- function(x, ages = NULL) {
  res <- tidied_run(x)
  size <- plant::model_size(res)
  d <- res$species[!is.na(res$species$density), ]
  d <- d[order(d$species, d$node, d$time), ]
  node <- interaction(d$species, d$node, drop = TRUE)
  backward <- function(v) {
    stats::ave(v, node, FUN = function(y) c(NA, diff(y)))
  }
  dt <- backward(d$time)
  rates <- list(growth = backward(d$height) / dt,
                mortality = backward(d$mortality) / dt,
                fecundity = backward(d$fecundity) / dt)
  ## Plotmath, parsed by the strip labeller: a superscript minus character
  ## is missing from some devices' fonts, as in other panels' axis labels.
  ## The unit is quoted, so any unit parses.
  names(rates) <- c(sprintf("Growth~(\"%s\"~yr^{-1})", size$size_units),
                    "Mortality~(yr^{-1})",
                    "Fecundity~(offspring~yr^{-1})")
  keep <- d$time %in% closest_times(d$time, ages)
  long <- do.call(rbind, lapply(names(rates), function(v) {
    data.frame(time = d$time[keep], species = as.character(d$species[keep]),
               node = d$node[keep], size = d$height[keep], rate = v,
               value = rates[[v]][keep])
  }))
  long$rate <- factor(long$rate, levels = names(rates))
  long$age <- age_factor(long$time)
  long <- long[is.finite(long$value), ]
  if (nrow(long) == 0) {
    stop("No rates at those ages: a rate needs a collected step before it",
         call. = FALSE)
  }
  ggplot2::ggplot(long, ggplot2::aes(.data$size, .data$value,
                                     colour = .data$age, group = .data$age)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(ggplot2::vars(.data$rate),
                        ggplot2::vars(.data$species),
                        scales = "free", labeller = ggplot2::labeller(
                          .rows = ggplot2::label_parsed,
                          .cols = function(s) paste("Species", s))) +
    ggplot2::labs(x = size_label(res), y = NULL, colour = "Patch age (yr)") +
    ggplot2::theme_classic()
}

#' Light through the canopy
#'
#' The fraction of full light reaching each size, as the run's environment
#' records it, at chosen patch ages. For a model sized by height this is light
#' by height; for K93 it is light by stem diameter.
#'
#' @inheritParams plot_by_size
#' @return A ggplot.
#' @export
plot_light_profile <- function(x, ages = NULL) {
  res <- tidied_run(x)
  light <- res$env$light_availability
  if (is.null(light$light_availability)) {
    stop("plot_light_profile() needs a run that records light ",
         "(env$light_availability)", call. = FALSE)
  }
  d <- light[light$time %in% closest_times(light$time, ages), ]
  d$age <- age_factor(d$time)
  ggplot2::ggplot(d, ggplot2::aes(.data$light_availability, .data$height,
                                  colour = .data$age, group = .data$age)) +
    ggplot2::geom_path() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Light (fraction of full sun)", y = size_label(res),
                  colour = "Patch age (yr)") +
    ggplot2::theme_classic()
}

#' Every figure that applies to a run
#'
#' The 2D figures as a named list, each drawn only where the run carries what
#' it reads: the water figure for a run that records soil water, for example.
#' The stand report is built from this list.
#'
#' @inheritParams plot_by_size
#' @return A named list of ggplots and patchworks.
#' @export
stand_figures <- function(x, ages = NULL) {
  res <- tidied_run(x)
  figures <- list(
    size_distribution = plot_size_distribution(res),
    stand_totals = plot_stand_totals(res),
    by_size = plot_by_size(res, ages),
    light_profile = if (!is.null(res$env$light_availability$light_availability))
      plot_light_profile(res, ages),
    water = if (!is.null(res$env$soil_moist)) plot_water(res))
  figures[!vapply(figures, is.null, logical(1))]
}

## The collected times nearest to each requested age, or by default to a
## roughly doubling scale of ages up to the run's end.
closest_times <- function(times, ages = NULL) {
  times <- sort(unique(times))
  if (!is.null(ages) && (!is.numeric(ages) || length(ages) == 0 ||
                         anyNA(ages))) {
    stop("`ages` must be patch ages in years", call. = FALSE)
  }
  if (is.null(ages)) {
    ages <- c(0.1, 1, 2.5, 5, 10, 20, 40, 80, 160)
    ages <- c(ages[ages < max(times)], max(times))
  }
  unique(times[vapply(ages, function(a) which.min(abs(times - a)), 1L)])
}

## Patch ages as a legend: each collected time its own level, in order,
## labelled with the fewest significant digits (three at least) that keep the
## labels distinct.
age_factor <- function(time) {
  u <- sort(unique(time))
  for (digits in 3:15) {
    labels <- as.character(signif(u, digits))
    if (!anyDuplicated(labels)) break
  }
  factor(time, levels = u, labels = labels)
}

## A column summed over species at each collected time, grouping on the times
## themselves rather than on their printed names.
sum_by_time <- function(value, time) {
  u <- sort(unique(time))
  data.frame(time = u, value = vapply(split(value, match(time, u)), sum, 0))
}

## plot_stand_totals()'s panels, named by what each draws, leaving out any
## whose sizes the run does not carry.
stand_panels <- function(res) {
  totals <- plant::integrate_over_size_distribution(
    plant::expand_state(res)$species)
  totals$species <- as.character(totals$species)
  by_species <- function(column, ylab, scale = 1) {
    if (is.null(totals[[column]])) return(NULL)
    d <- data.frame(time = totals$time, species = totals$species,
                    value = totals[[column]] * scale)
    ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$value,
                                    colour = .data$species)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::labs(x = "Time (years)", y = ylab, colour = "Species") +
      ggplot2::theme_classic()
  }
  panels <- list(
    density = by_species("density", expression(Density ~ (plants ~ m^{-2}))),
    area_leaf = by_species("area_leaf",
                           expression(Leaf ~ area ~ index ~ (m^2 ~ m^{-2}))),
    basal_area = by_species("area_stem",
                            expression(Basal ~ area ~ (m^2 ~ ha^{-1})),
                            scale = 1e4),
    biomass = biomass_panel(totals))
  panels[!vapply(panels, is.null, logical(1))]
}

## The tissues that make up a plant's mass_total in plant's expand_state().
biomass_tissues <- c(Leaf = "mass_leaf", Sapwood = "mass_sapwood",
                     Bark = "mass_bark", Heartwood = "mass_heartwood",
                     Root = "mass_root", "Coarse root" = "mass_coarse_root")

## Biomass by tissue, summed over species, stacked. Each plant's tissues add
## to its mass_total exactly; their stand totals add to the total of mass_total
## only to about 0.2%, as plant integrates each over birth date through a
## monotone spline, which is not linear in the values.
biomass_panel <- function(totals) {
  tissues <- biomass_tissues[biomass_tissues %in% names(totals)]
  if (length(tissues) == 0) return(NULL)
  d <- do.call(rbind, lapply(names(tissues), function(k) {
    cbind(sum_by_time(totals[[tissues[[k]]]], totals$time), tissue = k)
  }))
  d$tissue <- factor(d$tissue, levels = rev(names(tissues)))
  ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$value,
                                  fill = .data$tissue)) +
    ggplot2::geom_area() +
    ggplot2::labs(x = "Time (years)", y = expression(Biomass ~ (kg ~ m^{-2})),
                  fill = "Tissue") +
    ggplot2::theme_classic()
}
