rm(list = ls())
#library(Momocs2)
source("R/domestic.R")
source("R/import.R")
source("R/babel.R")
source("R/coo.R")
source("R/graphics.R")
source("R/morpho.R")
source("R/multivariate.R")
source("R/Out.R")
source("R/Opn.R")
load("data/olea.rda")
load("data/bot.rda")
load("data/trilo.rda")
load("data/mosquito.rda")
load("data/hearts.rda")

bot.f <- eFourier(bot, 24)
ol.p <- rawPolynomials(olea)

#' @param coo either a matrix of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a matrix of \eqn{(x; y)} coordinates, or a \link{Coo} object.