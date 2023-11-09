context("coo_ utilities")

# coo_angle_tangent -----
test_that("coo_angle_tangent works fine", {

  expect_is(coo_angle_tangent(bot[1]),
            "numeric")
  expect_identical(length(coo_angle_tangent(bot[1])),
                   nrow(bot[1]))

  expect_is(coo_angle_tangent(olea[1]),
            "numeric")
  expect_identical(length(coo_angle_tangent(olea[1])),
                   nrow(olea[1]))

  expect_is(coo_angle_tangent(wings[1]),
            "numeric")
  expect_identical(length(coo_angle_tangent(wings[1])),
                   nrow(wings[1]))

})

# coo_angle_edge1 -----
test_that(".coo_angle_edge1 works fine", {
  sq_ang <- matrix(c(0, 1, 0, 0, 1, 0), ncol=2, byrow=TRUE)
  expect_is(.coo_angle_edge1(sq_ang),
            "numeric")
  expect_true(.coo_angle_edge1(sq_ang)<0)
  expect_true(.coo_angle_edge1(sq_ang, "acos")>0)
  expect_error(.coo_angle_edge1(bot))
  expect_error(.coo_angle_edge1(bot[1][1:4, ]))
  expect_error(.coo_angle_edge1(bot[1][1:3, 1]))
  })

# coo_angle_edge1 -----
test_that("coo_angle_edges works fine", {
  expect_is(coo_angle_edges(bot[1]),
            "numeric")
  expect_length(coo_angle_edges(bot[1]), nrow(bot[1]))
  expect_warning(coo_angle_edges(bot[1], "acos"))

  expect_is(coo_angle_edges(olea[1]),
            "numeric")
  expect_length(coo_angle_edges(olea[1]), nrow(olea[1]))

  expect_is(coo_angle_edges(wings[1]),
            "numeric")
  expect_length(coo_angle_edges(wings[1]), nrow(wings[1]))
})
