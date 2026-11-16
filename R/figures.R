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
