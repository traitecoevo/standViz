context("Construction")

test_that("stand3d_dataframe", {
  expect_null(stand3d_dataframe(NULL))
  trees <- data.frame(height=c())
  expect_equal(stand3d_dataframe(trees), trees)

  trees <- data.frame(height=1)
  expect_error(stand3d_dataframe(trees))

  v_essential  <-  c("x", "y", "height", "height_crown_base",
                                            "crown_width", "dbh")
  v_extra <- c("crown_shape", "eta", "crown_color", "stem_color")
  trees <- data.frame(height=1, x=1, y=1, height_crown_base=0.5,
                            crown_width=0.3, dbh=0.1)

  expect_not_error(stand3d_dataframe(trees))
  trees_complete <- stand3d_dataframe(trees)
  expect_is(trees_complete, "data.frame")
  expect_isin(c(v_essential, v_extra), names(trees_complete))

  expect_equal(nrow(trees), nrow(trees_complete))
  expect_equal(trees, trees_complete[,names(trees)])
})


test_that("tree_shape3d", {
  expect_not_error(tree_shape3d())
  s <- tree_shape3d()
  expect_is(s, "matrix")
  expect_equal(ncol(s), 3)
  expect_equal(nrow(s), 24*24*2*3)
  s <- tree_shape3d(nz=20)
  expect_equal(nrow(s), 19*24*2*3)
  s <- tree_shape3d()
  expect_equal(range(s[,1]), 0.5*c(-1,1))
  expect_equal(range(s[,2]), 0.5*c(-1,1))
  expect_equal(range(s[,3]), c(0,1))
  expect_not_error(tree_shape3d(zvals=seq(0,2,length.out=10)))
  s <- tree_shape3d(zvals=seq(0,2,length.out=10))
  expect_equal(nrow(s), 9*24*2*3)
  expect_equal(range(s[,1]), 0.5*c(-1,1))
  expect_equal(range(s[,2]), 0.5*c(-1,1))
  expect_equal(range(s[,3]), c(0,2))

  shapes <- c("cone", "ellipsoid", "halfellipsoid",
                                  "paraboloid", "cylinder", "yokozawa")
  for(sh in shapes) {
    expect_not_error(tree_shape3d(sh))
  }
  s <- tree_shape3d()
  for(sh in shapes[-c(1)]) {
    s2 <- tree_shape3d(sh)
    expect_not_identical(s, s2)
    expect_equal(range(s2[,1]), 0.5*c(-1,1))
    expect_equal(range(s2[,2]), 0.5*c(-1,1))
    expect_equal(range(s2[,3]), c(0,1))
  }
})
