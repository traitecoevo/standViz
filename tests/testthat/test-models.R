## Every model realises and draws from the one core: FF16 and TF24 with their
## crowns, K93, whose size is stem diameter, with its stems alone.

page_data <- function(stand) {
  file <- withr::local_tempfile(fileext = ".html")
  write_viewer(stand, file)
  html <- paste(readLines(file), collapse = "\n")
  json <- regmatches(html, regexpr('(?s)id="stand-data">.*?</script>', html, perl = TRUE))
  jsonlite::fromJSON(sub("</script>$", "", sub('^id="stand-data">', "", json)),
                     simplifyVector = FALSE)
}

test_that("an FF16 run realises with crowns and draws", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 10)
  expect_true(stand$meta$has_crown)
  expect_identical(stand$meta$size_units, "m")
  expect_identical(stand$meta$min_size, 1.5)
  expect_gt(nrow(stand$trees), 0)
  data <- page_data(stand)
  expect_named(data$rows, c("size", "crown_scale", "stem_diameter"))
  expect_true(all(c("profile", "eta_c") %in% names(data$species[[1]])))
  expect_identical(vapply(data$colour, `[[`, "", "column"),
                   c("net_mass_production_dt", "area_leaf"))
})

test_that("a TF24 plant_run realises with crowns from its own shape and draws", {
  skip_if_not_installed("plant")
  run <- tf24_run()
  stand <- realise_stand(run, side = 10, min_size = 1)
  expect_identical(stand$meta$model, "TF24")
  expect_true(stand$meta$has_crown)
  expect_gt(nrow(stand$trees), 0)
  ## TF24's own crown shape.
  out <- plant::tidy(run)
  expect_equal(stand$crowns[[1]], species_crown(out$p$strategies[[1]], out$control))
  expect_true(all(c("opt_psi_leaf", "Tleaf", "area_leaf") %in% names(stand$rows)))
  long <- stand_long(stand)
  expect_true(all(is.finite(long$light_crown)))
  data <- page_data(stand)
  expect_named(data$rows, c("size", "crown_scale", "stem_diameter"))
})

test_that("a run's own shading model shapes the crowns drawn", {
  skip_if_not_installed("plant")
  ## A tidied run's strategies carry no shading model, so the crowns must be
  ## read under the run's control: a soft-box run draws soft-box crowns.
  soft <- plant::control(shading_model = "flat-top-soft-box",
                         node_density_coordinate = "birth_date")
  p <- plant::default_parameters("FF16", quiet = TRUE)
  p$max_patch_lifetime <- 10
  p <- plant::add_strategies(p, plant::trait_matrix(0.0825, "lma"), birth_rate = 20)
  run <- plant::run(p, control = soft, record = "steps")
  stand <- realise_stand(run, side = 10, min_size = 0.5)
  s <- p$strategies[[1]]
  expect_equal(stand$crowns[[1]], species_crown(s, soft))
  expect_false(isTRUE(all.equal(stand$crowns[[1]], species_crown(s))))
})

test_that("a K93 plant_run realises as stems in its own units and draws", {
  skip_if_not_installed("plant")
  run <- k93_run()
  stand <- realise_stand(run, side = 20, min_size = 3)
  expect_identical(stand$meta$model, "K93")
  expect_false(stand$meta$has_crown)
  expect_identical(stand$meta$size, "stem diameter")
  expect_identical(stand$meta$size_units, "cm")
  expect_null(stand$crowns)
  expect_gt(nrow(stand$trees), 0)
  expect_false("crown_scale" %in% names(stand$rows))
  ## The size state is diameter in cm, the drawn stem in m.
  expect_equal(stand$rows$stem_diameter, stand$rows$height / 100)
  expect_true(all(stand$rows$height >= 3))
  long <- stand_long(stand)
  expect_true(all(is.na(long$crown_radius)))
  expect_true(all(long$light_crown > 0 & long$light_crown <= 1))
  ## The page carries no crowns and offers no colour K93 lacks.
  data <- page_data(stand)
  expect_named(data$rows, c("size", "stem_diameter"))
  expect_false("profile" %in% names(data$species[[1]]))
  expect_length(data$colour, 0)
  expect_identical(data$meta$size_units, "cm")
  ## The default threshold is 10 cm of stem diameter.
  expect_identical(realise_stand(run, side = 5)$meta$min_size, 10)
})

test_that("K93 stems spread over the patch", {
  skip_if_not_installed("plant")
  stand <- realise_stand(k93_run(), side = 20)
  long <- stand_long(stand)
  last <- long[long$frame == max(long$frame), ]
  expect_gt(nrow(last), 200)
  ## No 5 x 5 m quadrat holds much more than its 1/16 share of the stems.
  q <- table(cut(last$x, seq(0, 20, 5)), cut(last$y, seq(0, 20, 5)))
  expect_lt(max(q) / sum(q), 2 / 16)
})

test_that("a K93 stand with no stems large enough still expands and draws", {
  skip_if_not_installed("plant")
  stand <- realise_stand(k93_run(), side = 5, min_size = 1e4)
  expect_equal(nrow(stand$trees), 0)
  expect_equal(nrow(stand_long(stand)), 0)
  data <- page_data(stand)
  expect_length(data$trees$frames, 0)
})

test_that("K93 counts follow plant's integral above the threshold", {
  skip_if_not_installed("plant")
  res <- plant::tidy(k93_run())
  stand <- realise_stand(res, side = 30, min_size = 3, seed = 3)
  totals <- plant::integrate_over_size_distribution(res$species, lower = 3)
  f <- length(stand$times)
  for (s in 1:2) {
    ts <- totals[totals$species == s, ]
    expected <- ts$density[which.min(abs(ts$time - stand$times[f]))] * 900
    got <- sum(stand$rows$frame == f & stand$trees$species[stand$rows$tree] == s)
    expect_lte(abs(got - expected), max(0.1 * expected, 4 * sqrt(expected) + 2))
  }
})
