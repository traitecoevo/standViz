context("Construction")

test_that("make_stand_dataframe", {
  expect_null(make_stand_dataframe(NULL))
  trees <- data.frame(height=c())
  expect_equal(make_stand_dataframe(trees), trees)

  trees <- data.frame(height=1)
  expect_error(make_stand_dataframe(trees))

  v_essential  <-  c("x", "y", "height", "height_crown_base",
                                            "crown_width", "dbh")
  v_extra <- c("crown_shape", "eta", "crown_color", "stem_color")
  trees <- data.frame(height=1, x=1, y=1, height_crown_base=0.5,
                            crown_width=0.3, dbh=0.1)

  expect_not_error(make_stand_dataframe(trees))
  trees_complete <- make_stand_dataframe(trees)
  expect_is(trees_complete, "data.frame")
  expect_isin(c(v_essential, v_extra), names(trees_complete))

  expect_equal(nrow(trees), nrow(trees_complete))
  expect_equal(trees, trees_complete[,names(trees)])
})


test_that("make_shape", {
  expect_not_error(make_shape())
  s <- make_shape()
  expect_is(s, "matrix")
  expect_equal(ncol(s), 3)
  expect_equal(nrow(s), 24*24*2*3)
  s <- make_shape(nz=20)
  expect_equal(nrow(s), 19*24*2*3)
  s <- make_shape()
  expect_equal(range(s[,1]), 0.5*c(-1,1))
  expect_equal(range(s[,2]), 0.5*c(-1,1))
  expect_equal(range(s[,3]), c(0,1))
  expect_not_error(make_shape(zvals=seq(0,2,length.out=10)))
  s <- make_shape(zvals=seq(0,2,length.out=10))
  expect_equal(nrow(s), 9*24*2*3)
  expect_equal(range(s[,1]), 0.5*c(-1,1))
  expect_equal(range(s[,2]), 0.5*c(-1,1))
  expect_equal(range(s[,3]), c(0,2))

  shapes <- c("cone", "ellipsoid", "halfellipsoid",
                                  "paraboloid", "cylinder", "yokozawa")
  for(sh in shapes) {
    expect_not_error(make_shape(sh))
  }
  s <- make_shape()
  for(sh in shapes[-c(1)]) {
    s2 <- make_shape(sh)
    expect_not_identical(s, s2)
    expect_equal(range(s2[,1]), 0.5*c(-1,1))
    expect_equal(range(s2[,2]), 0.5*c(-1,1))
    expect_equal(range(s2[,3]), c(0,1))
  }
})

test_that("resize_shape", {
  s <- make_shape()
  expect_not_error(resize_shape(s))
  s2 <- resize_shape(s)
  expect_identical(s, s2)
  s2 <- resize_shape(s, height=2)
  expect_not_identical(s, s2)
  expect_equal(range(s2[,1]), 0.5*c(-1,1))
  expect_equal(range(s2[,2]), 0.5*c(-1,1))
  expect_equal(range(s2[,3]), c(0,2))

})