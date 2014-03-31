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


pca.OutCoe <- function(OutCoe){
  PCA <- prcomp(OutCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  class(PCA) <- c("OutPCA", class(PCA))
  return(PCA)}

b <- bot[5]
bot <- coo.sample(bot, 64)
botE <- eFourier(bot, 6, norm=TRUE)
botP <- pca(botE)

plot(botP)

