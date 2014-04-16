rm(list = ls())
setwd("/Users/vincent/Momocs/")
require(MASS) ; require(rgl) ; require(ape) ; require(orthopolynom)
source("R/domestic.R")
source("R/import.R")
source("R/babel.R")
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

# x <- readJPEG("~/Desktop/M_lite.jpg")
# x <- apply(x, 1:2, mean)
# class(x)

# dev.off()
# system.time(image(x, asp=1))
# dev.off()
# system.time(image(x, asp=1, useRaster=TRUE))
# dev.off()
# system.time(grid.raster(x))
# dev.off()
# system.time(grid.raster(as.raster(x)))

#click.splines(x)

# # deprecate (but #todo) import.multi1.jpg

auto.notcentered = TRUE
threshold   = 0.5
imgs <- list.files("/Users/vincent/Research/Momocs/Datasets/test-import/", full=TRUE)
coo <- import.jpg(imgs)
coo.plot(coo[[1]])










