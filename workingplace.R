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


# 2. Out calibration -----------------------------------------------------------
nqual <- function(Opn, ...){UseMethod("nqual")}
nqual.Opn <-
  function(Opn, method=c("rawPolynomials", "orthoPolynomials"),
           id, 
           n.range = c(2, 3, 4, 6, 8, 10),
           smooth.it=0,
           baseline1=c(-1, 0), baseline2=c(1, 0),
           plot.method=c("panel", "stack")[1],
           legend = TRUE,
           legend.title = "Degree",
           palette = col.india,
           shp.border="#1A1A1A",
           ...){
    if (missing(id)) id <- sample(length(Opn$coo), 1)
    if (missing(method)) {
      cat(" * Method not provided. orthoPolynomials is used.\n")
      orthogonal   <- TRUE
    } else {
      p <- pmatch(tolower(method), c("rawpolynomials", "orthopolynomials"))
      if (is.na(p)) { warning(" * Unvalid method. orthoPolynomials is used.\n")
      } else {
        orthogonal <- switch(p, TRUE,   FALSE)}}
    
    # check for too ambitious harm.range
    if (max(n.range) > (min(sapply(Opn$coo, nrow))- 1)) {
      n.range <- (min(sapply(Opn$coo, nrow))- 1)
      cat(" * n.range was too high and set to: ", n.range, ".\n")}
    coo <- Opn$coo[[id]]
    if (smooth.it  != 0) coo <- coo.smooth(coo, smooth.it)
    coo <- coo.baseline(coo, ldk1=1, ldk2=nrow(coo), t1=baseline1, t2=baseline2)
    res <- list()
    for (i in seq(along=n.range)) {
      res[[i]] <- polynomials.i(polynomials(coo, n=n.range[i], orthogonal=orthogonal))}
    # plotting
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    cols <- paste0(palette(length(n.range)), "EE")
    if (plot.method=="stack") {
      #to initiate the plot but stack may be a better option for that part
      coo.plot(coo, border=shp.border, lwd=1)
      for (i in seq(along=n.range)){
        lines(res[[i]], col=cols[i])}
      if (legend) {
        legend("topright", legend = as.character(n.range), bty="n",
               col = cols, lty = 1, lwd=1, cex=0.7,
               title = legend.title)}
    } else {
      if (plot.method=="panel") {
        #par(oma=c(1, 1, 3, 0))
        pos <- coo.list.panel(res, borders=cols, cols=par("bg"), poly=FALSE)
        if (legend) {text(x=pos[, 1], y=pos[, 2],
                          as.character(n.range))}
        title(names(Opn)[id], cex=1.3)
      }}}
