test_that("the crown holds the plant's leaf area at the given density", {
  area_leaf <- 7.3
  h <- 12
  lad <- 1.7
  s <- crown_scale(area_leaf, h, lad)
  ## Crown volume times leaf area density is the leaf area.
  volume <- stats::integrate(function(z) {
    pi * (s * crown_profile(z / h, eta = 12))^2
  }, 0, h)$value
  expect_equal(volume * lad, area_leaf, tolerance = 1e-6)
})

test_that("tree sizes come from plant's own allometry", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  stand <- realise_stand(res, side = 10, min_height = 2)
  r <- stand$rows[1:50, ]
  ind <- plant::FF16_Individual(res$p$strategies[[stand$trees$species[r$tree[1]]]])
  ind$set_state("height", r$height[1])
  expect_equal(r$area_leaf[1], ind$aux("competition_effect"), tolerance = 1e-10)
  expect_equal(r$crown_scale, crown_scale(r$area_leaf, r$height, 1))
  expect_true(all(c("mass_live", "fecundity", "net_mass_production_dt") %in%
                    names(stand$rows)))
})
