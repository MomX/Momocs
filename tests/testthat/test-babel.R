context("babel")

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
