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

test_that("every model gets each figure it has data for, and each draws", {
  skip_if_not_installed("plant")
  ff16 <- stand_figures(ff16_run())
  expect_named(ff16, c("size_distribution", "stand_totals", "by_size",
                       "light_profile"))
  k93 <- stand_figures(k93_run())
  expect_named(k93, names(ff16))
  tf24 <- stand_figures(tf24_run())
  expect_named(tf24, c(names(ff16), "water"))
  for (f in c(ff16, k93, tf24)) {
    if (inherits(f, "patchwork")) {
      for (i in seq_along(f$patches$plots)) built(f[[i]])
      built(f[[length(f$patches$plots) + 1]])
    } else {
      built(f)
    }
  }
})

test_that("stand totals draw the sizes a run carries", {
  skip_if_not_installed("plant")
  expect_named(stand_panels(ff16_run()),
               c("density", "area_leaf", "basal_area", "biomass"))
  ## K93 has stem sizes, but no leaf area or tissue masses.
  expect_named(stand_panels(plant::tidy(k93_run())), c("density", "basal_area"))
})

test_that("the tissues stacked make up plant's total biomass", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  ## Every plant's tissues add to its mass_total.
  s <- plant::expand_state(res)$species
  live <- s[!is.na(s$density), ]
  expect_equal(rowSums(as.data.frame(live)[biomass_tissues]), live$mass_total,
               tolerance = 1e-12)
  ## The panel stacks plant's stand total of each tissue, over both species.
  p <- built(stand_panels(res)$biomass)
  totals <- plant::integrate_over_size_distribution(s)
  leaf <- tapply(totals$mass_leaf, totals$time, sum)
  d <- p$data[p$data$tissue == "Leaf", ]
  expect_equal(d$value, as.numeric(leaf))
  expect_equal(d$time, as.numeric(names(leaf)))
})

test_that("basal area is plant's stem area per hectare", {
  skip_if_not_installed("plant")
  res <- plant::tidy(k93_run())
  d <- ggplot2::ggplot_build(stand_panels(res)$basal_area)$data[[1]]
  totals <- plant::integrate_over_size_distribution(
    plant::expand_state(res)$species)
  expect_equal(sort(d$y), sort(totals$area_stem * 1e4))
})

test_that("rates by size are differences along each node's trajectory", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  p <- built(plot_by_size(res, ages = 20))
  d <- p$data[p$data$rate == levels(p$data$rate)[1], ]
  t1 <- unique(d$time)
  expect_length(t1, 1)
  ## Each node's growth over the step that ends at t1.
  s <- res$species[!is.na(res$species$density), ]
  t0 <- max(s$time[s$time < t1])
  a <- s[s$time == t0, ]
  b <- s[s$time == t1, ]
  m <- merge(a, b, by = c("species", "node"))
  growth <- (m$height.y - m$height.x) / (t1 - t0)
  got <- d$value[match(paste(m$species, m$node), paste(d$species, d$node))]
  expect_equal(got, growth)
  ## The default ages reach the run's end.
  expect_equal(max(plot_by_size(res)$data$time), max(s$time))
})

test_that("rates and light are drawn against the model's own size", {
  skip_if_not_installed("plant")
  run <- k93_run()
  p <- built(plot_by_size(run))
  expect_identical(p$labels$x, "Stem diameter (cm)")
  expect_match(levels(p$data$rate)[1], "cm yr", fixed = TRUE)
  expect_identical(built(plot_light_profile(run))$labels$y,
                   "Stem diameter (cm)")
})

test_that("the light profile is the run's own, at the ages asked for", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  p <- built(plot_light_profile(res, ages = c(5, 20)))
  light <- res$env$light_availability
  times <- closest_times(light$time, c(5, 20))
  expect_length(times, 2)
  expect_equal(p$data$light_availability,
               light$light_availability[light$time %in% times])
})

test_that("ages are drawn at the nearest collected time", {
  times <- c(0, 0.5, 1.2, 3, 7.9)
  expect_equal(closest_times(times, c(1, 8)), c(1.2, 7.9))
  expect_equal(closest_times(times, c(1, 1.1)), 1.2)
  expect_equal(closest_times(times), c(0, 1.2, 3, 7.9))
})
