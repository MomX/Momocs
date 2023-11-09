context("kinematics functions")

test_that("half.wave works fine", {
   x <- seq(0,pi,0.01)
    y <- sin(x^2*pi)
    
   w <-  halfwave(x,y,method="zeros",fit=TRUE,smoothing="spline")
   
   expect_is(w,"list")
   expect_named(w,c("method", "names", "dat"))
   expect_type(w$names$x,type= "double")
   expect_type(w$dat$wave.begin,type = "double")
   expect_true(w$method[1]== "zeros")
   expect_true(is.na(w$dat$amp2[1]) & is.na(w$dat$pos2[1]))
   expect_error(halfwave(x,y,method="foo",fit=TRUE,smoothing="spline"),"method must be set to")
   expect_error(halfwave(x,y,method="zeros",fit=TRUE,smoothing="foo"), "should be set to")
   
   expect_error(halfwave(x,y+1.1,method="zeros",fit=TRUE,smoothing="spline"),"1 or fewer zero crossings found") #test no zero crossings
   
   w2 <- halfwave(x,y+1.1,method="p2t",fit=TRUE,smoothing="spline")
   
   expect_is(w2,"list")
   expect_named(w2,c("method", "names", "dat"))
   expect_type(w2$names$x,type= "double")
   expect_type(w2$dat$wave.begin,type = "double")
   expect_true(w2$method[1]== "p2t")
   expect_true(is.na(w2$dat$zeros[1]))
   
})

test_that("wave works fine", {
  x <- seq(0,pi,0.01)
  y <- sin(x^2*pi)
  
  w <-  wave(x,y,method="zeros",fit=TRUE,smoothing="spline")
  
  expect_is(w,"list")
  expect_named(w,c("method", "names", "dat"))
  expect_type(w$names$x,type= "double")
  expect_type(w$dat$wave.begin,type = "double")
  expect_true(w$method[1]== "zeros")
  expect_true(!is.na(w$dat$amp2[1]) & !is.na(w$dat$pos2[1]))
  
  expect_error(wave(x,y,method="foo",fit=TRUE,smoothing="spline"),"method must be set to")
  expect_error(wave(x,y,method="zeros",fit=TRUE,smoothing="foo"), "should be set to")
  expect_error(wave(x,y+1.1,method="zeros",fit=TRUE,smoothing="spline")) #test no zero crossings
  
  w2 <- wave(x,y+1.1,method="p2p",fit=TRUE,smoothing="spline")
  expect_is(w2,"list")
  expect_named(w2,c("method", "names", "dat"))
  expect_type(w2$names$x,type= "double")
  expect_type(w2$dat$wave.begin,type = "double")
  expect_true(w2$method[1]== "p2p")
  expect_true(is.na(w2$dat$zeros[1]))
  
  w3 <- wave(x,y+1.1,method="t2t",fit=F,smoothing="spline")
  expect_is(w3,"list")
  expect_named(w3,c("method", "names", "dat"))
  expect_type(w3$names$x,type= "double")
  expect_type(w3$dat$wave.begin,type = "double")
  expect_true(w3$method[1]== "t2t")
  expect_true(is.na(w3$dat$zeros[1]))
  
})

test_that("amp.freq works fine", {
x <- seq(0,pi,0.1)
 y <- sin(x^1.3*pi)
 af <- amp.freq(x=x,y=y)
 expect_is(af,"list")
 expect_equivalent(round(af$f,1), 69.4)
 expect_equivalent(af$snr,38.76)
 expect_named(af,c("a", "f", "a.f","snr"))
 
 
 x <- seq(0.2,pi,0.1)
 y <- sin(x^0.3*pi)
 af2 <- amp.freq(x,y)
 expect_is(af2,"list")
 expect_identical(af2$a,numeric(0))
 expect_identical(af2$f,numeric(0))
 expect_identical(af2$a.f,numeric(0))
})
