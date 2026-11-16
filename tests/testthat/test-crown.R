test_that("a crown is plant's profile and holds the plant's leaf area", {
  skip_if_not_installed("plant")
  s <- plant::FF16_Model()
  crown <- species_crown(s)
  ## The outline's radius is sqrt(q), q being plant's leaf density in u.
  u <- crown$profile$u[-1]
  q <- plant::canopy_profile(s, u)$profile$q
  expect_equal(crown$profile$r[-1], sqrt(q))
  expect_equal(crown$eta_c, plant::canopy_profile(s, 1)$eta_c)
  ## Drawn down to the height above which 95% of the leaf area lies.
  Q_base <- plant::canopy_profile(s, crown$profile$u[1])$profile$Q
  expect_equal(Q_base, 0.95, tolerance = 1e-3)
  ## Crown volume times leaf area density is the leaf area.
  area_leaf <- 7.3
  h <- 12
  lad <- 1.7
  scale <- crown_scale(area_leaf, h, lad)
  volume <- stats::integrate(function(z) {
    pi * scale^2 * plant::canopy_profile(s, z / h)$profile$q
  }, 0, h)$value
  expect_equal(volume * lad, area_leaf, tolerance = 1e-6)
})

test_that("a run's shading model shapes its crowns", {
  skip_if_not_installed("plant")
  s <- plant::FF16_Model()
  soft <- species_crown(s, plant::control(shading_model = "flat-top-soft-box"))
  deep <- species_crown(s)
  expect_false(isTRUE(all.equal(soft$profile, deep$profile)))
})

test_that("tree sizes come from plant's own allometry", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  stand <- realise_stand(res, side = 10, min_size = 2)
  r <- stand$rows[1:50, ]
  ind <- plant::FF16_Individual(res$p$strategies[[stand$trees$species[r$tree[1]]]])
  ind$set_state("height", r$height[1])
  expect_equal(r$area_leaf[1], ind$aux("competition_effect"), tolerance = 1e-10)
  expect_equal(r$crown_scale, crown_scale(r$area_leaf, r$height, 1))
  expect_true(all(c("mass_live", "fecundity", "net_mass_production_dt") %in%
                    names(stand$rows)))
})
