context("babel-export")

test_that("export writes files", {
  x <- bot %>% efourier(3)
  fn <- "plop.txt"
  expect_message(export(x, file=fn))
  expect_true(fn %in% list.files())
  shut_up <- file.remove("plop.txt")

  x %<>% PCA
  fn <- "plop.txt"
  expect_message(export(x, file=fn))
  expect_true(fn %in% list.files())
  shut_up <- file.remove("plop.txt")

  x <- shapes[4]
  fn <- "plop.txt"
  expect_message(export(x, file=fn))
  expect_true(fn %in% list.files())
  shut_up <- file.remove("plop.txt")
})
