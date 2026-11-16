test_that("realised tree counts follow plant's own integral above the threshold", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  stand <- realise_stand(res, side = 30, min_size = 2, seed = 3)
  totals <- plant::integrate_over_size_distribution(res$species, lower = 2)
  for (t in c(10, 20, 40)) {
    f <- which.min(abs(stand$times - t))
    for (s in 1:2) {
      ## Frames fall between collected steps; take plant's total at the nearest.
      ts <- totals[totals$species == s, ]
      expected <- ts$density[which.min(abs(ts$time - stand$times[f]))] * 900
      got <- sum(stand$rows$frame == f & stand$trees$species[stand$rows$tree] == s)
      expect_lte(abs(got - expected), max(0.1 * expected, 4 * sqrt(expected) + 2))
    }
  }
})

test_that("trees persist: they only grow, and never come back", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 10, min_size = 2)
  tr <- stand$trees
  expect_true(all(tr$birth_frame < tr$death_frame))
  long <- stand_long(stand)
  long <- long[order(long$tree, long$frame), ]
  same <- diff(long$tree) == 0
  ## Each tree's frames are consecutive (no gaps, so no resurrection) ...
  expect_true(all(diff(long$frame)[same] == 1))
  ## ... and its height never falls.
  expect_true(all(diff(long$height)[same] >= -1e-9))
  expect_true(all(long$height >= stand$meta$min_size))
  expect_equal(tabulate(long$tree, nrow(tr)), tr$death_frame - tr$birth_frame)
})

test_that("positions are seeded and leave the caller's random state alone", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  a <- realise_stand(res, side = 10, min_size = 2, seed = 7)
  expect_equal(stats::runif(1), before)
  b <- realise_stand(res, side = 10, min_size = 2, seed = 7)
  expect_identical(a$trees, b$trees)
  expect_true(all(a$trees$x >= 0 & a$trees$x <= 10))
})

test_that("anything but a run or a tidied deterministic run is refused", {
  ## A run's solver and a tidied stochastic run carry no $p.
  expect_error(realise_stand(list(time = 1, species = list(), env = list())),
               "tidied deterministic run")
  expect_error(realise_stand(list(p = 1, species = data.frame())),
               "tidied deterministic run")
})
