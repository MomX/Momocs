context("trig functions")

test_that("dist.2d works fine", {
   x <- c(0,3,2)
   y <- c(0,3,0)
   hyp <- dist.2d(x[1],x[2],y[1],y[2])
   s1 <- dist.2d(x[1],x[3],y[1],y[3])
   s2 <- dist.2d(x[2],x[3],y[2],y[3])

   
   expect_is(dist.2d(x[1],x[2],y[1],y[2]),"numeric")
   expect_true(hyp>s1)
   expect_true(hyp>s2)
   
   
})

test_that("cosine.ang works fine", {
#a right triangle
L=3
R=3
O=sqrt(L^2+R^2)

A <- acos((O^2-L^2-R^2)/(-2*L*R))

expect_is(cosine.ang(L,R,O),"numeric")
expect_identical(cosine.ang(L,R,O),A)

expect_equal(cosine.ang(3,3,3),pi*1/3)
expect_equal(cosine.ang(3,3,6),pi)


})

test_that("deg works fine", {
   
   expect_identical(deg(pi),180)
   
})


test_that("rad works fine", {
   
   expect_identical(rad(180),pi)
   
})

test_that("bearing works fine", {
   
    A <- c(0,0)
    B <- c(-5,5)
   expect_identical(bearing.xy(A[1],B[1],A[2],B[2]),atan((B[1]-A[1])/(A[2]-B[2])))
 
 
})



