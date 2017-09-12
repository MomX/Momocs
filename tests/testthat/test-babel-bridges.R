context("babel-bridges")

test_that("unary bridges work fine", {
  expect_true(bot[1] %>% m2l %>% is.list())
  expect_true(bot[1] %>% m2l %>% l2m() %>% is.matrix())
  expect_equal(wings$coo %>% l2a %>%  dim %>% length, 3)
  expect_true(wings$coo %>% l2a %>%  a2l %>% Ldk() %>% validate() %>% is.Ldk())
})


test_that("as_df and as.data.frame work fine",{
  idf <- is.data.frame
  #Coo
  expect_true(bot %>% as_df %>% idf)
  expect_true(bot %>% as.data.frame %>% idf)
  #Coe
  bf <- bot %>% efourier
  expect_true(bf %>% as_df %>% idf)
  expect_true(bf %>% as.data.frame %>% idf)
  # TraCoe
  expect_true(flower %>% as_df %>% idf)
  expect_true(flower %>% as.data.frame %>% idf)
  #PCA
  bp <- bf %>% PCA
  expect_true(bp %>% as_df %>% idf)
  expect_true(bp %>% as.data.frame %>% idf)
})

test_that("cpx2coo and coo2cpx work fine",{
  x <- coo2cpx(shapes[4])
  expect_true(is.complex(x))
  expect_true(is_shp(x %>% cpx2coo))
})

test_that("m2d and d2m work fine",{
  x <- bot[5] %>% m2d
  expect_true(is.data.frame(x))
  expect_true(is.matrix(x %>% d2m))
})

test_that("a2m and m2a work fine", {
  m <- array(1:24, dim = c(3, 2, 4)) %>% a2m
  expect_true(is.matrix(m))
  expect_true(length(dim(m2a(m)))==3)
})

test_that("as_df converts all classes to data.frames", {
  expect_true(is.data.frame(bot %>% as_df))
  x <- bot %>% efourier(3)
  expect_true(is.data.frame(x %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% LDA(1) %>% as_df))

  x <- olea %>% npoly(3, nb.pts=40)
  expect_true(is.data.frame(x %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% LDA(1) %>% as_df))

  x <- wings %>% fgProcrustes(10)
  expect_true(is.data.frame(x %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% as_df))
  expect_true(is.data.frame(x %>% PCA %>% LDA(1) %>% as_df))

  expect_true(is.data.frame(flower %>% as_df))
  expect_true(is.data.frame(flower %>% PCA %>% as_df))
  expect_true(is.data.frame(flower %>% PCA %>% LDA(~sp) %>% as_df))
  flower$fac <- data.frame()
  expect_true(is.data.frame(flower %>% as_df))
})

test_that("m2ll works fine", {
  x <- m2ll(wings[1], c(6, 4, 3, 5))
  expect_true(is.list(x))
  expect_length(x, 4)
})
