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
olea <- coo.sample(olea, 100)
olea$coo <- lapply(olea$coo, coo.baseline, 
              ldk1=1, ldk2=100, t1=c(-1, 0), t2=c(1, 0))
ol <- subset(olea, view=="VL")
od <- subset(olea, view=="VD")

ol.pol <- Polynomials(ol)
od.pol <- Polynomials(od)

ol.pca <- pca(ol.pol)
od.pca <- pca(od.pol)

# plot(ol.pca, "cep")
# plot(od.pca, "cep")

x <- ol[1][, 1]
y <- ol[1][, 2]
coo.plot(cbind(x, y), points=FALSE)

pca2shp.polynomials <- function (pos, rot, mshape, amp=1, nb.pts=60, trans=TRUE, mod) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  # stupid function
  mprod <- function(m, s){
    res <- m
    for (i in 1:ncol(m)) { res[, i] <- m[, i]*s[i] }
    return(res)}
  degree <- length(mshape)
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- mprod(rot, pos[i, ])*amp
    mod$coefficients        <- mshape + apply(ax.contrib, 1, sum)
    coo        <- polynomials.i(mod)
    if (trans) {coo <- coo.trans(coo, x=pos[i, 1], y=pos[i, 2]-
                                   mod$coefficients[1])}
    res[[i]] <- coo}
  return(res)}

.morphospace2 <- 
function(xy, pos.shp, rot, mshape, amp.shp=1,
         size.shp=15, border.shp="#00000055", mod,..){
  pos <- pos.shapes(xy, pos.shp=pos.shp)
  shp <- pca2shp.polynomials(pos=pos, rot=rot,
                             mshape=mshape, amp=amp.shp, trans=TRUE, mod=mod)
  width <- (par("usr")[4] - par("usr")[3]) / size.shp
  shp <- lapply(shp, coo.scale, 1/width)
  burp <- lapply(shp, lines, col=border.shp)}


ol.pol <- Polynomials(ol, degree=5, ortho=FALSE)
ol.pca <- pca(ol.pol)

pos <- ol.pca$x[, 1:2]
rot <- ol.pca$rotation[,1:2]
mshape <- ol.pca$mshape
pos.shp <- pos.shapes(pos, pos.shp="full")


modi <- polynomials(ol[1], n=5, orthogonal=FALSE)
plot(NA, xlim=range(pos[, 1]), ylim=range(pos[, 2]), asp=1)
points(pos, pch=20, cex=0.5)
.morphospace2(pos, pos.shp=pos, rot=rot, mshape=mshape,
             size.shp=20, border.shp="black", mod=modi)



