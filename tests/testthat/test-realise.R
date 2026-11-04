test_that("realised tree counts follow the model's size distribution", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  stand <- realise_stand(res, side = 20, min_height = 2, seed = 3)
  long <- stand_long(stand)
  for (t in c(10, 22, 40)) {
    f <- which.min(abs(stand$times - t))
    for (s in 1:2) {
      expected <- model_count_above(res, as.character(s), stand$times[f], 2) * 400
      got <- sum(long$frame == f & long$species == s)
      ## Within 15%, or Poisson noise when the count is small.
      expect_lte(abs(got - expected), max(0.15 * expected, 3 * sqrt(expected) + 1))
    }
  }
})

test_that("trees persist: they only grow, and never come back", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 10, min_height = 2)
  tr <- stand$trees
  expect_true(all(tr$birth_frame < tr$death_frame))
  long <- stand_long(stand)
  long <- long[order(long$tree, long$frame), ]
  same <- diff(long$tree) == 0
  ## Each tree's frames are consecutive (no gaps, so no resurrection) ...
  expect_true(all(diff(long$frame)[same] == 1))
  ## ... and its height never falls.
  expect_true(all(diff(long$height)[same] >= -1e-9))
  expect_true(all(long$height >= stand$meta$min_height - 1e-9))
})

test_that("positions are seeded and leave the caller's random state alone", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  a <- realise_stand(res, side = 10, min_height = 2, seed = 7)
  expect_equal(stats::runif(1), before)
  b <- realise_stand(res, side = 10, min_height = 2, seed = 7)
  expect_identical(a$trees, b$trees)
  expect_true(all(a$trees$x >= 0 & a$trees$x <= 10))
})

test_that("only FF16 runs are accepted", {
  expect_error(realise_stand(list(p = structure(list(), class = c("Parameters<TF24,TF24_Env>", "Parameters")))),
               "Only FF16")
})
