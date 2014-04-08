rm(list = ls())
setwd("/Users/vincent/Momocs/")
require(MASS)
require(rgl)
source("R/global.R")
source("R/Out.R")
source("R/FourierCore.R")
# source("R/Landmarks.R")
# source("R/Multivariate.R")
# source("R/Open.R")
load("data/bot.rda")
load("data/trilo.rda")
load("data/mosquito.rda")
load("data/hearts.rda")

bot <- coo.sample(bot, 64)
b <- bot[5]
botE <- eFourier(bot, 6, norm=TRUE)
botP <- pca(botE)

load("~/Dropbox/MG-out.rda")
# we arrange sp. wise
MG <- subset(MG, order(c(4, 1, 3, 5, 2, 6)[MG$fac$sp]))
MGd   <- subset(MG, view == "dorsal")
MGd.E <- eFourier(MGd, 12)

