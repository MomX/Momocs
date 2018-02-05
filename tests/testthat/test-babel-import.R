context("babel-import")

test_that("import .txt", {
  fns <- c("butterfly.txt", "cat.txt")
  export(shapes[3], file=fns[1])
  export(shapes[4], file=fns[2])
  expect_true(is_shp(import_txt(fns[1], header=TRUE, verbose = FALSE)$butterfly))
  expect_s3_class(import_txt(rep(fns, 6), header=TRUE, verbose = FALSE) %>% Out(), "Out")
  shut_up <- file.remove(fns)
})

test_that("import_Conte works fine",{
  expect_true(
    system.file("extdata/beer_chimay.jpg", package="Momocs") %>%
    jpeg::readJPEG()  %>%
    import_Conte(round(dim(.)/2)) %>% is_shp()
  )
})

test_that("import_jpg1 works fine",{
  path <- system.file("extdata/beer_chimay.jpg", package="Momocs")
  expect_true(path %>% import_jpg1 %>% is_shp())
  expect_true("rvb.jpg" %>% import_jpg1() %>% is_shp)
  expect_true("borders.jpg" %>% import_jpg1() %>% is_shp)
})

test_that("import_jpg works fine",{
  paths <- c(system.file("extdata/beer_chimay.jpg", package="Momocs"),
             system.file("extdata/whisky_jb.jpg", package="Momocs"))
  expect_true(paths %>% import_jpg(verbose = FALSE) %>% Out %>% is_Out)
})

test_that("import_tps works fine",{
  x <- "lm.tps" %>% import_tps
  expect_true(x$coo %>% is.list)
  expect_true(x$coo %>% sapply(is_shp) %>% all())
  expect_true(x$scale %>% is.list)
  expect_true(x$scale %>% sapply(is.numeric) %>% all)

  x <- "outlines.tps" %>% import_tps()
  expect_true(x$coo %>% is.list)
  expect_true(x$coo %>% sapply(is_shp) %>% all())
  expect_true(x$scale %>% is.list)
  expect_true(x$scale %>% sapply(is.numeric) %>% all)
})
