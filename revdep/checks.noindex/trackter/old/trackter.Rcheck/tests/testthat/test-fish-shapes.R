context("fish shapes")

test_that("fishshapes data are OK", {
  
 data(fishshapes)
  expect_type(fishshapes,"list")
  expect_is(fishshapes,"Out")
  expect_is(fishshapes,"Coo")
  expect_type(fishshapes$coo,"list")

})

