context("classes")
x <- Out(a2l(replicate(50, matrix(1:4, 2, 2))),
         fac=data.frame(a=rep(letters[1:5], 10),
                        b=rep(LETTERS[1:5], each=10)))

test_that("Coo works fine", {
  # Coo constructor doesnt exist alone
  expect_message(Coo())
  expect_message(Coo(bot$coo))
  expect_true(x %>% is_Coo())
  expect_true(x %>% Opn() %>% verify %>% is_Coo())
  expect_true(x %>% Ldk() %>% verify %>% is_Coo())
})

test_that("Out works fine", {
  expect_true(x %>% verify %>% is_Out())
  expect_true(x[1] %>% is_shp())
  expect_true(x[1] %>% is.matrix)
  expect_true(x[1:2] %>% is.list)
})

test_that("Opn works fine", {
  x <- Opn(x)
  expect_true(x %>% verify %>% is_Opn())
  expect_true(x[1] %>% is_shp())
  expect_true(x[1] %>% is.matrix)
  expect_true(x[1:2] %>% is.list)
})

test_that("Ldk works fine", {
  x <- Ldk(x)
  expect_true(x %>% verify %>% is_Ldk())
  expect_true(x[1] %>% is_shp())
  expect_true(x[1] %>% is.matrix)
  expect_true(x[1:2] %>% is.list)
})

