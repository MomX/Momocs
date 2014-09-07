## ------------------------------------------------------------------------
data(bot)
bot.f <- eFourier(bot, 12)
bot.p <- PCA(bot.f)
class(bot.p)

## ----, fig.show='hold'---------------------------------------------------
plot(bot.p)

## ----, fig.show='hold'---------------------------------------------------
plot(bot.p, "type") # equivalent to plot(bot.p, 1)

## ----, fig.show='hold'---------------------------------------------------
# PC2 and 3 with 0.75 bivariate gaussian confidence ellipses but not their axes
plot(bot.p, "type", xax = 2, yax=3, ellipses=TRUE, conf.ellipses=0.5, 
     ellipsesax.conf = c(0.25, 0.5, 0.75, 0.9), cex=0.3)
# color + title
plot(bot.p, "type", pch=c("w", "b"), col=c("darkslateblue", "hotpink"), 
     rug=FALSE, eigen=FALSE, ellipsesax=FALSE, title="A nice title", cex=1)
# another color example
plot(bot.p, col=col.summer(40), cex=1.5)
# a fac passed on the fly
plot(bot.p, fac=factor(rep(letters[1:4], each=10)))
# delaunay triangulation
plot(bot.p, 1, zoom=1.5, delaunay=TRUE, labelsgroups = FALSE, ellipsesax=FALSE)
# convex hulls
plot(bot.p, 1, chull=TRUE, ellipsesax=FALSE, stars=TRUE, chull.lty = 1, 
     palette=col.india, abbreviate.labelsgroups = TRUE, 
     cex.labelsgroups = 1.25, rect.labelsgroups=FALSE)

## ----, fig.show='hold'---------------------------------------------------
plot(bot.p, 1, pos.shp="circle", nb.shp=16, col.shp="gold")
plot(bot.p, 1, pos.shp="range", nr.shp=20, nc.shp=15, 
     size.shp = 1/3, amp.shp=2, border.shp="grey20")
plot(bot.p, 1, pos.shp="xy", lwd.shp=3)
plot(bot.p, 1, morphospace=FALSE) # no morphospace

## ----, fig.show='hold'---------------------------------------------------
plot(bot.p, density=TRUE, contour=FALSE) # density only
plot(bot.p, 1, contour=TRUE, lev.contour=5, palette=col.autumn) # contour only

## ----, fig.show='hold'---------------------------------------------------
data(olea)
olea.pol <- orthoPolynomials(olea, 5, nb.pts = 60)
olea.p <- PCA(olea.pol)
plot(olea.p, "cep", lwd.shp=2)

## ----, fig.show='hold'---------------------------------------------------
data(wings)
wings.al <- fgProcrustes(wings, verbose=FALSE, tol = 1e-4) # to speed up
wings.p <- PCA(wings.al)
plot(wings.p)

## ----, fig.show='hold', eval=FALSE---------------------------------------
#  plot3(bot.p, 1, contour=TRUE, lev.contour=5,
#        pos.shp="circle", border.shp="grey20")

## ----, fig.show='hold'---------------------------------------------------
# a pca
mat <- matrix(rnorm(100), 20)
mat.p <- prcomp(mat)
class(mat.p) <- "PCA"
plot(mat.p)

# a kmeans-helped scatter-plot
set.seed(123)
x <- replicate(2, rnorm(500))
fac <- factor(kmeans(x, centers = 8)$cluster)
fake <- list(x=x)
class(fake) <- c("PCA", class(PCA))
op <- par(bg="grey20", fg="white")
plot(fake, fac, density=TRUE, palette=col.solarized, eigen=FALSE)
par(op)

## ----, fig.show='hold'---------------------------------------------------
Manova(bot.f, "type") #bottles - type
Manova(olea.pol, "cep") #olea - cepages
# not yet on Ldk.

## ----, fig.show='hold'---------------------------------------------------
ManovaPW(olea.pol, "cep")

## ----, fig.show='hold'---------------------------------------------------
bot.l <- LDA(bot.p, "type") 
plot(bot.l)
plotCV2(bot.l$CV.tab)
bot.l

## ----, fig.show='hold'---------------------------------------------------
olea.l <- LDA(olea.p, "cep")
plot(olea.l)
plotCV(olea.l$CV.tab)

## ----, fig.show='hold'---------------------------------------------------
# todo: add shapes on hclust plots
clust(bot.f, "type", type = "fan")
clust(bot.f, "type", type = "cladogram")

