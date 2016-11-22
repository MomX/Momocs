context("nse")

# prepare_fac -----
test_that("prepare_fac works fine", {
  olea$fac$fake <-  rnorm(length(olea))
  # Valid ways below ------
  # nothing
  expect_null(prepare_fac(olea))
  # column id
  expect_identical(prepare_fac(olea, 2), olea$fac[, 2])
  # column name
  expect_identical(prepare_fac(olea, "domes"), olea$fac[, "domes"])
  # column name NSE, unquoted
  expect_identical(prepare_fac(olea, domes), olea$fac$domes)
  # formula style
  expect_identical(prepare_fac(olea, ~domes), olea$fac$domes)
  # formula + interactions
  expect_identical(prepare_fac(olea, ~domes+var), with(olea$fac, interaction(domes, var)))
  # factor on the fly
  f <- factor(rep(letters[1:7], each=30))
  expect_identical(prepare_fac(olea, f), f)
  # numeric on the fly
  n <- rnorm(length(olea))
  expect_identical(prepare_fac(olea, n), n)
  # non-valid ways
  data(olea)
  # non-existing column
  expect_error(prepare_fac(olea, 84))
  # non existing column name
  expect_error(prepare_fac(olea, "rock_and_roll"))
  # also, formula style
  expect_error(prepare_fac(olea, ~rock_and_roll))
  # passing a factor of the wrong length
  expect_error(prepare_fac(olea, factor(rep(letters[1:7], each=10))))
  # passing a numeric of the wrong length
  expect_error(prepare_fac(olea, rnorm(70)))
})

# dplyr verbs -------
test_that("select works fine",{
  nc <- function(x) ncol(x$fac)
  expect_equal(select(olea, 1) %>% nc, 1)
  expect_equal(select(olea, 1:2) %>% nc, 2)
  expect_equal(select(olea, var) %>% nc, 1)
  expect_equal(select(olea, var, domes) %>% nc, 2)
  expect_equal(select(olea, -(1:2)) %>% nc, nc(olea)-2)
})


test_that("filter works fine",{
  x <- Out(a2l(replicate(50, matrix(1:4, 2, 2))),
           fac=data.frame(a=rep(letters[1:5], 10),
                          b=rep(LETTERS[1:5], each=10)))

  expect_equal(length(filter(x, a=="a")) , 10)
  expect_equal(length(filter(x, a %in% c("a", "b"))) , 20)
  expect_equal(length(filter(x, !(a %in% c("a", "b")))) , 30)
  # onley two rows in the fac, both with {a; B}
  x_lite <- filter(x, a=="a", b=="B")
  expect_equal(length(x_lite) , 2)
  # test for the dropping of factor levels
  expect_true(x_lite$fac %>% sapply(nlevels) %>% `==`(1) %>% all)
})

# rename
# mutate
#  trnsmute
# arrange
# slice
# sample_n
# sample_frac
# chop
# combine
# dissolve
# subset(?)
# rw
# at_least



