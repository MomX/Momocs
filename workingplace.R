rm(list = ls())
source("R/global.R")
source("R/Babel.R")
source("R/Out.R")
source("R/OutCoe.R")
source("R/OutMethods.R")
source("R/FourierCore.R")
source("R/domestic.R")
# source("R/Landmarks.R")
# source("R/Multivariate.R")
# source("R/Open.R")
load("data/bot.rda")
load("data/trilo.rda")
load("data/mosquito.rda")
load("data/hearts.rda")

# http://cran.r-project.org/web/packages/tiff/index.html
# http://cran.r-project.org/web/packages/png/index.html

b <- bot[5]
bot <- coo.sample(bot, 64)
botE <- eFourier(bot, 6, norm=TRUE)
botP <- pca(botE)

plot(botP)
subset(bot, type=="whisky")

ldk.polar <- list()
for (i in 1:length(MGpolar)){
  coo.i <- MGpolar$coo[[i]]
  ldk.polar[[i]] <- coo.ldk(coo.i, 1)}

res <- numeric(length(MGpolar))
for (i in 1:length(MGpolar)) {
  coo.i <- MGpolar$coo[[i]]
  coo.ldk <- t(as.matrix(MGpolar$coo[[i]][ldk.polar[[i]][1], ]))
  d <- edm(coo.i, coo.ldk)
  ldk.polar[[i]][2] <- which.max(d)
}

coo.sample(MGpolar.al, 64)
MGpolar.E <- eFourier(MGpolar.al, 12, norm=FALSE)
MGpolar.P <- pca(MGpolar.E)
plot(MGpolar.P, "sp")
