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

#todo
#panel / fac (colors / pile)

olea <- coo.align.calliper(olea)
olea$coo <- lapply(olea$coo, coo.up)
olea <- Opn(olea$coo, fac=olea$fac)

pol <- Polynomials(olea)
x <- pca(pol)
