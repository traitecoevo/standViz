test_that("leaf area matches plant's own FF16 individual", {
  skip_if_not_installed("plant")
  s <- plant::FF16_Strategy()
  ind <- plant::FF16_Individual(s)
  for (h in c(0.5, 2, 10, 25)) {
    ind$set_state("height", h)
    geom <- ff16_geometry(h, 0, s$pars)
    expect_equal(geom$area_leaf, ind$aux("competition_effect"), tolerance = 1e-12)
  }
})

test_that("the crown holds the plant's leaf area at the given density", {
  pars <- list(a_l1 = 5.44, a_l2 = 0.306, theta = 2.14e-4, a_b1 = 0.17)
  h <- 12
  lad <- 1.7
  geom <- ff16_geometry(h, 0, pars, lad = lad)
  ## Crown volume times leaf area density is the leaf area.
  volume <- stats::integrate(function(z) {
    pi * (geom$crown_scale * crown_profile(z / h, eta = 12))^2
  }, 0, h)$value
  expect_equal(volume * lad, geom$area_leaf, tolerance = 1e-6)
})

test_that("stem diameter adds sapwood, bark and heartwood", {
  pars <- list(a_l1 = 5.44, a_l2 = 0.306, theta = 2.14e-4, a_b1 = 0.17)
  g0 <- ff16_geometry(10, 0, pars)
  g1 <- ff16_geometry(10, 0.01, pars)
  expect_equal(pi * g1$stem_diameter^2 / 4 - pi * g0$stem_diameter^2 / 4, 0.01)
})
