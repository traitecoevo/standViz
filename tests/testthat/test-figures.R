## The 2D figures moved in from plant (plot_size_distribution(), and
## TF24_plot_diagnostics() as plot_water()), with plant's tests for them.
## ggplot evaluates aesthetics lazily, so each figure is built, not only made.

built <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  p
}

test_that("the size distribution draws from a table or a run", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  p <- built(plot_size_distribution(res$species))
  expect_identical(p$labels$y, "Height (m)")
  expect_identical(built(plot_size_distribution(res))$labels$y, "Height (m)")
  ## Every node of every species is its own line.
  d <- ggplot2::ggplot_build(p)$data[[1]]
  live <- res$species[!is.na(res$species$density), ]
  expect_equal(length(unique(d$group)),
               nrow(unique(live[c("node", "species")])))
})

test_that("the size distribution labels a model's own size", {
  skip_if_not_installed("plant")
  run <- k93_run()
  expect_identical(built(plot_size_distribution(run))$labels$y,
                   "Stem diameter (cm)")
  species <- plant::tidy(run)$species
  expect_identical(plot_size_distribution(species, model = "K93")$labels$y,
                   "Stem diameter (cm)")
})

test_that("a seeded run's tidy table draws as a size distribution", {
  skip_if_not_installed("plant")
  p <- plant::default_parameters("FF16", quiet = TRUE)
  p$max_patch_lifetime <- 5
  p <- plant::add_strategies(p, plant::trait_matrix(0.08, "lma"), birth_rate = 1)
  st <- plant::make_initial_state(p, heights = seq(1, 8, length.out = 10),
                                  densities = rep(0.2, 10))
  res <- plant::tidy(plant::run(plant::set_initial_state(p, st),
                                record = "steps"))
  expect_true(any(res$species$time == 0 & !is.na(res$species$density)))
  built(plot_size_distribution(res$species))
})

## plant's TF24 diagnostic run: 15 soil layers and seasonal rainfall.
seasonal_tf24 <- cached(function() {
  lifetime <- 2
  p <- plant::default_parameters("TF24", quiet = TRUE)
  p$max_patch_lifetime <- lifetime
  p <- plant::add_strategies(p, plant::trait_matrix(c(0.07, 0.15), "lma"),
                             birth_rate = c(20, 20))
  env <- plant::Environment("TF24")
  env$set_soil_number_of_depths(15)
  env$set_soil_water_state(rep(0.2, 15))
  x <- seq(0, lifetime, length.out = 100)
  env$extrinsic_drivers_set_variable("rainfall", x = x,
                                     y = 0.25 * sin(2 * pi * x) + 1)
  plant::tidy(plant::run(p, env = env, record = "steps"))
})

test_that("a TF24 run's water figure has every panel, and each draws", {
  skip_if_not_installed("plant")
  res <- seasonal_tf24()
  panels <- water_panels(res)
  expect_named(panels, c("rainfall", "soil_moist", "soil_psi", "transpiration",
                         "transpiration_per_leaf_area", "opt_psi_leaf",
                         "opt_root_psi", "stom_cond_CO2", "profit", "Tleaf"))
  for (p in panels) built(p)
  expect_s3_class(plot_water(res), "patchwork")
  ## One line per soil layer.
  d <- ggplot2::ggplot_build(panels$soil_psi)$data[[1]]
  expect_equal(length(unique(d$group)), 15)
  ## One line per node of each species: node numbers repeat across species.
  d <- ggplot2::ggplot_build(panels$opt_psi_leaf)$data[[1]]
  live <- res$species[!is.na(res$species$density), ]
  expect_equal(length(unique(d$group)),
               nrow(unique(live[c("node", "species")])))
})

test_that("rainfall is read from the run's own fluxes", {
  skip_if_not_installed("plant")
  res <- seasonal_tf24()
  d <- ggplot2::ggplot_build(water_panels(res)$rainfall)$data[[1]]
  ## The driver's exact mean over each interval between collected steps.
  t <- res$env$soil_moist_cumulative_flux$time
  t0 <- t[-length(t)]
  t1 <- t[-1]
  mean_rain <- 1 + 0.25 * (cos(2 * pi * t0) - cos(2 * pi * t1)) /
    (2 * pi * (t1 - t0))
  expect_equal(d$x, t1)
  expect_equal(d$y, mean_rain, tolerance = 1e-3)
})

test_that("transpiration per leaf area divides by the patch's leaf area", {
  skip_if_not_installed("plant")
  res <- seasonal_tf24()
  panels <- water_panels(res)
  total <- ggplot2::ggplot_build(panels$transpiration)$data[[1]]
  per <- ggplot2::ggplot_build(panels$transpiration_per_leaf_area)$data[[1]]
  ## Leaf area summed over both species, at the same times.
  totals <- plant::integrate_over_size_distribution(
    plant::expand_state(res)$species)
  area <- tapply(totals$area_leaf, totals$time, sum)
  i <- match(per$x, total$x)
  expect_equal(per$y * area[as.character(per$x)], total$y[i],
               ignore_attr = TRUE)
})

test_that("water panels attach by the columns a run carries", {
  skip_if_not_installed("plant")
  res <- seasonal_tf24()
  res$species$Tleaf <- NULL
  expect_false("Tleaf" %in% names(water_panels(res)))
  ## Without leaf area there is nothing to divide transpiration by.
  local_mocked_bindings(patch_leaf_area = function(res) NULL)
  panels <- water_panels(res)
  expect_true("transpiration" %in% names(panels))
  expect_false("transpiration_per_leaf_area" %in% names(panels))
  expect_error(plot_water(ff16_run()), "needs a run that records soil water")
})
