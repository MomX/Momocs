context("coo_ utilities")

# coo_check -----
test_that("coo_check works fine", {
  expect_true(is_shp(coo_check(bot[1])))
  expect_true(is_shp(coo_check(olea[1])))
  expect_true(is_shp(coo_check(wings[1])))
  expect_error(coo_check(flower[1]))
  expect_false(is_shp("plop"))
  expect_true(is_Out(coo_check(bot)))
  bot[1] <- NA
  expect_error(is_Out(coo_check(bot)))
  expect_true(is_Opn(coo_check(olea)))
  data(bot)
  olea[1] <- NA
  expect_error(is_Opn(coo_check(olea)))
  data(olea)
  expect_true(is_Ldk(coo_check(wings)))
  wings[1] <- NA
  expect_error(is_Ldk(coo_check(wings)))
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

# coo_scale ----
test_that("coo_scale works fine", {
  expect_equal(coo_centsize(coo_scale(bot[1])), 1)
  expect_equal(sum(coo_centsize(coo_scale(bot))), length(bot))

})

# coo_scalex -----
test_that("coo_scalex works fine", {
  expect_equal(diff(range(coo_scalex(coo_template(bot[1]), 1.5)[, 1])), diff(range(coo_template(bot[1])[, 1]))*1.5)
  expect_equal(diff(range(coo_scalex(coo_template(olea[1]), 1/3)[, 1])), diff(range(coo_template(olea[1])[, 1]))*1/3)
})

# coo_scalex -----
test_that("coo_scaley works fine", {
  expect_equal(diff(range(coo_scaley(coo_template(bot[1]), 1.5)[, 2])), diff(range(coo_template(bot[1])[, 2]))*1.5)
  expect_equal(diff(range(coo_scaley(coo_template(olea[1]), 1/3)[, 2])), diff(range(coo_template(olea[1])[, 2]))*1/3)
})

# coo_template -----
test_that("coo_template works fine", {
  expect_equal(max(coo_template(bot[1])), 0.5)
  expect_equal(max(coo_template(olea[1])), 0.5)
  expect_equal(max(coo_template(wings[1])), 0.5)
  expect_equal(max(coo_template(wings[1], 5)), 5/2)
  expect_equal(max(coo_template(wings[1], 0.01)), 0.01/2)
  expect_equal(max(sapply(coo_template(bot)$coo, max)), 0.5)
  expect_equal(max(sapply(coo_template(bot)$coo, min)), -0.5)
})

# coo_rotate ----
test_that("coo_rotate works fine", {
  shp <- bot[1]
  shp.rot <- coo_rotate(shp, pi/7)
  nr <- nrow(shp)
  expect_equal(.coo_angle_edge1(rbind(shp[24,], c(0, 0), c(1e3, 0))) - pi/7,
               .coo_angle_edge1(rbind(shp.rot[24,], c(0, 0), c(1e3, 0))))
})

# coo_translate -----
test_that("coo_translate works fine", {
  cp <- coo_centpos(bot[1])
  cpt <- coo_centpos(coo_trans(bot[1], 123, -321))
  expect_equal(cpt[1] - cp[1], 123)
  expect_equal(cpt[2] - cp[2], -321)
})

# coo_centsize ----
test_that("coo_centsize works fine", {
  expect_equal(coo_centsize(bot[1]), coo_centsize(coo_trans(bot[1], runif(1, -1e3, 1e3), runif(1, -1e3, 1e3))))
  expect_equal(sum(coo_centsize(olea) -
                     coo_centsize(coo_trans(olea, runif(1, -1e3, 1e3), runif(1, -1e3, 1e3)))), 0)
})


# coo_area -----
test_that("coo_area works fine", {
  expect_equal(coo_area(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), nc=2, byrow=TRUE)), 1)
})

