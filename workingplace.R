rm(list = ls())
#library(Momocs2)
source("R/domestic.R")
source("R/import.R")
source("R/babel.R")
source("R/coo.R")
source("R/graphics.R")
source("R/morpho.R")
source("R/Out.R")
source("R/Opn.R")
load("data/olea.rda")
load("data/bot.rda")
bot <- coo.sample(bot, 200)
#todo
#panel / fac (colors / pile)

olea <- coo.align.calliper(olea)
olea$coo <- lapply(olea$coo, coo.up)
olea <- Opn(olea$coo, fac=olea$fac)
olea <- coo.sample(olea, 100)
olea$coo <- lapply(olea$coo, coo.baseline, 
              ldk1=1, ldk2=100, t1=c(-1, 0), t2=c(1, 0))
ol <- subset(olea, view=="VL")
od <- subset(olea, view=="VD")

ol.pol <- Polynomials(ol, ortho=TRUE)
od.pol <- Polynomials(od, ortho=FALSE)

ol.pca <- pca(ol.pol)
od.pca <- pca(od.pol)

plot(ol.pca)
plot(od.pca)


bot.f <- eFourier(bot, 24)
bot.p <- pca(bot.f)
plot(bot.p, 1)
plot(bot.p, 1, pos.shp="xy")
