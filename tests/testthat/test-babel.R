context("babel")

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
