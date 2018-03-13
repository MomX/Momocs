context("nse")

x <- Out(a2l(replicate(50, matrix(1:4, 2, 2))),
         fac=data.frame(a=rep(letters[1:5], 10),
                        b=rep(LETTERS[1:5], each=10)))

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
  expect_equal(length(filter(x, a=="a")) , 10)
  expect_equal(length(filter(x, a %in% c("a", "b"))) , 20)
  expect_equal(length(filter(x, !(a %in% c("a", "b")))) , 30)
  # only two rows in the fac, both with {a; B}
  x_lite <-filter(x, a=="a", b=="B")
  expect_equal(length(x_lite), 2)
  # test for the dropping of factor levels
  expect_true(all(sapply(x_lite$fac, nlevels)==1))
})

# rename
# mutate
#  trnsmute
# arrange
# slice

test_that("slice works fine",{
  expect_true(x %>% slice(0) %>% validate %>% is_Out())
  expect_true(x %>% slice(1) %>% validate %>% is_Out())
  expect_true(x %>% slice(-1) %>% validate %>% is_Out())
  expect_true(x %>% slice(1:5) %>% validate %>% is_Out())
  expect_true(x %>% slice(-(1:5)) %>% validate %>% is_Out())
  expect_true(x %>% slice(sample(c(TRUE, FALSE), length(.), rep=T)) %>% is_Out())
})

# sample_n
test_that("sample_n works fine", {
  expect_equal(sample_n(x, 0) %>% length(), 0)
  expect_equal(sample_n(x, 5) %>% length(), 5)
})

# sample_frac
test_that("sample_frac works fine", {
  expect_equal(sample_frac(x, 0) %>% length(), 0)
  expect_equal(sample_frac(x, 0.5) %>% length(), length(x)/2)
  expect_equal(sample_frac(x, 1) %>% length(), length(x))
})


# chop
test_that("chop works fine",{
  expect_true(chop(x, a) %>% is.list())
  expect_equal(chop(x, a) %>% length, 5)
  expect_true(chop(x, a) %>% sapply(is_Out) %>% all)
})

# combine
test_that("combine works fine", {
  expect_true(chop(x, a) %>% combine %>% is_Out())
  expect_equal(chop(x, a) %>% combine %>% length(), length(x))
})

# dissolve
test_that("dissolve works fine", {
  bw <- bot %>% chop(type) %>% lapply(efourier, 10) %>% combine
  expect_true(bw %>% is_OutCoe())
  expect_equal(ncol(bw$coe), 80)
  expect_true(bw %>% dissolve(1) %>% is_OutCoe())
  expect_equal(bw %>% dissolve(1) %$% ncol(coe), 40)
})

# subsetize
test_that("subsetize works fine", {
  expect_equal(subsetize(bot, type=="whisky") %>% length, 20)
  expect_equal(subsetize(bot, type!="whisky") %>% length, 20)
  expect_equal(subsetize(bot, 1:5) %>% length, 5)
  expect_equal(subsetize(bot, -(1:5)) %>% length, 35)
})

# rw
test_that("rw_rule works fine", {
  expect_equal(rw_fac(x, "b", "C", "foo")$b %>% levels(), c("A", "B", "D", "E", "foo"))
})

# at_least
xx <- Out(a2l(replicate(50, matrix(1:4, 2, 2))),
         fac=data.frame(a=rep(letters[1:5], 10),
                        b=c(rep(LETTERS[1], 5),
                            rep(LETTERS[2], 10),
                            rep(LETTERS[3], 15),
                            rep(LETTERS[4], 20))))
test_that("at_least works fine", {
  expect_equal(at_least(xx, "b", 5)$b %>% nlevels(), 4)
  expect_equal(at_least(xx, "b", 6)$b %>% nlevels(), 3)
  expect_equal(at_least(xx, "b", 20)$b %>% nlevels(), 1)
  expect_equal(at_least(xx, "b", 50)$b %>% nlevels(), 0)
  expect_message(at_least(xx, "b", 50)$b %>% nlevels())
})

