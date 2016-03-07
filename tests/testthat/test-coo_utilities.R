context("coo_ utilities")

# coo_check -----
test_that("coo_check works fine", {
  expect_true(is.shp(coo_check(bot[1])))
  expect_true(is.shp(coo_check(olea[1])))
  expect_true(is.shp(coo_check(wings[1])))
  expect_false(is.shp(coo_check(flower[1])))
  expect_false(is.shp("plop"))
  expect_true(is.Out(coo_check(bot)))
  bot[1] <- NA
  expect_error(is.Out(coo_check(bot)))
  expect_true(is.Opn(coo_check(olea)))
  data(bot)
  olea[1] <- NA
  expect_error(is.Opn(coo_check(olea)))
  data(olea)
  expect_true(is.Ldk(coo_check(wings)))
  wings[1] <- NA
  expect_error(is.Ldk(coo_check(wings)))
  data(wings)
  expect_error(coo_check(flower))
})

# coo_nb -----
test_that("coo_nb works fine", {
  expect_equal(coo_nb(coo_sample(bot[1],  45)), 45)
  expect_equal(coo_nb(coo_sample(bot[1],  0)), 0)
  expect_identical(sapply(bot$coo, function(x) nrow(x)), coo_nb(bot))
})

# coo_center -----
test_that("coo_center works fine", {
  expect_equal(apply(coo_center(bot[1]), 2, mean), rep(0, 2))
  expect_equal(sum(sapply(coo_center(bot)$coo, function(x) apply(x, 2, mean))), 0)
})

# coo_centsize ----
test_that("coo_centsize works fine", {
  expect_equal(coo_centsize(bot[1]), coo_centsize(coo_trans(bot[1], runif(1, -1e3, 1e3), runif(1, -1e3, 1e3))))
  expect_equal(sum(coo_centsize(olea) -
                     coo_centsize(coo_trans(olea, runif(1, -1e3, 1e3), runif(1, -1e3, 1e3)))), 0)
})

# coo_area -----
test_that("coo_area returns a scalar", {
  expect_equal(coo_area(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), nc=2, byrow=TRUE)), 1)
})

