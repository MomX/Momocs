rm(list = ls())
setwd("/Users/vincent/Momocs/")
require(MASS) ; require(rgl) ; require(ape) ; require(orthopolynom)
source("R/domestic.R")
source("R/coo.R")
source("R/graphics.R")
source("R/morpho.R")
source("R/Out.R")
source("R/Opn.R")
load("data/bot.rda")
load("data/trilo.rda")
load("data/mosquito.rda")
load("data/hearts.rda")
load("~/Dropbox/MG-out.rda")

bot <- coo.sample(bot, 64)
b <- bot[5]
bup <- coo.up(coo.align.xax(b))
botE <- eFourier(bot, 6, norm=TRUE)
botP <- pca(botE)
ms <- meanshapes(botE, 1)

# coo.plot(bup)
# bup.p <- polynomials(bup, n=5, ortho=FALSE)
# bup.pi <- polynomials.i(bup.p, bup[, 1])
# lines(bup.pi, col="red", type="b", pch=20)

b <- coo.scale(coo.center(coo.sample(bot[5], 12)))

splines <- function(coo, method="natural", deriv=2){
  coo <- coo.check(coo)
  z <- coo.perim.cum(coo)
  fx <- splinefun(z, coo[, 1], method=method)
  fy <- splinefun(z, coo[, 2], method=method)
  xcoe <- fy(z, deriv=2)
  ycoe <- fy(z, deriv=2)
  return(list(xcoe=xcoe, ycoe=ycoe))}


splines2 <- function(coo, nb.pts=100){
  z <- coo.perim.cum(coo)
  x.i <- spline(z, coo[, 1], method="natural", n=100)$y
  y.i <- spline(z, coo[, 2], method="natural", n=100)$y
  return(cbind(x.i, y.i))}

# fx.i <- spline(seq(1, 6.3647907, length=length(z)), b[, 1], method="natural", n=100)$y
# fy.i <- spline(seq(1, 6.3647907, length=length(z)), b[, 2], method="natural", n=100)$y
# lines(fx.i, fy.i, col="green")



# 
# coo.plot(b)
# 
# fx.i <- spline(seq(1, 6.3647907, length=length(z)), b[, 1], method="natural", n=100)$y
# fy.i <- spline(seq(1, 6.3647907, length=length(z)), b[, 2], method="natural", n=100)$y
# lines(fx.i, fy.i, col="green")

require(jpeg)
require(grid)

x <- readJPEG("~/Desktop/M_lite.jpg")
x <- apply(x, 1:2, mean)
class(x)

# dev.off()
# system.time(image(x, asp=1))
# dev.off()
# system.time(image(x, asp=1, useRaster=TRUE))
# dev.off()
# system.time(grid.raster(x))
# dev.off()
# system.time(grid.raster(as.raster(x)))

click.bez <- function(x, n=10){
  x <- as.raster(x)
  plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
  grid.raster(x)
  ldk <- matrix(NA, n, 2)
  bez <- NA
  ldk[1, ] <- l2m(locator(1))
  for (i in 2:n){
    grid.raster(x)
    lines(bez, col="red")
    ldk[i, ] <- l2m(locator(1))
    cat(ldk)
    bez <- bezier.i(bezier(ldk[1:i,])$B)
  }}
click.splines <- function(x, n=20){
  x <- as.raster(x)
  plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
  grid.raster(x)
  ldk <- matrix(NA, n, 2)
  spl <- NA
  ldk[1, ] <- l2m(locator(1))
  for (i in 2:n){
    grid.raster(x)
    points(ldk[1:i,], pch=20, col="black")
    lines(spl, col="red")
    ldk[i, ] <- l2m(locator(1))
    cat(ldk)
    spl <- splines2(ldk[1:i,])
  }}

#click.splines(x)

