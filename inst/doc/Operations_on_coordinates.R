## ------------------------------------------------------------------------
library(Momocs)
data(shapes)
shp <- shapes[4] #try shapes[20] if you like Monty Pythons
coo.plot(shp, main = "A cat")

## ----, fig.show='hold'---------------------------------------------------
shp <- coo.sample(shp, 128)
coo.plot(coo.align(shp), main = "coo.align")
# Calliper length, we also add the corresponding length
coo.plot(feret <- coo.aligncalliper(shp), main="coo.aligncalliper")
feretxy <- coo.calliper(shp, arr.ind = TRUE)$arr.ind
segments(feret[feretxy[1], 1], feret[feretxy[1], 2],
         feret[feretxy[2], 1], feret[feretxy[2], 2], col="red")
coo.plot(coo.alignxax(shp), main="coo.alignxax")

## ------------------------------------------------------------------------
coo.plot(coo.center(shp), main="coo.center")

## ------------------------------------------------------------------------
# centroid position
centxy <- coo.centpos(shp) 
centxy
# the centroid size
coo.centsize(shp) 
# distance of the (first) points to the centroid
head(coo.centdist(shp)) 

## ----, fig.show='hold'---------------------------------------------------
# Note the two baseline points
par(xpd=NA)
coo.plot(coo.baseline(shp, 17, 6, t1=c(-50, 0), t2=c(50, 0)), main="coo.baseline")
points(c(-50, 50), c(0, 0), col="red", pch=3)
coo.plot(coo.bookstein(shp, 17, 6), main="coo.bookstein")
points(c(-0.5, 0.5), c(0, 0), col="red", pch=3)

## ----, fig.show='hold'---------------------------------------------------
coo.plot(coo.rotate(shp, pi), main="coo.rotate")
coo.plot(coo.rotatecenter(shp, pi, center=c(25, 25)), main="coo.rotatecenter")

## ----, fig.show='hold'---------------------------------------------------
# equidistant sampling along curvilinear abscissa
coo.plot(coo.sample(shp, 36), points=TRUE, main="coo.sample")
# regular angle
shp.rr <- coo.samplerr(shp, 36)
centxy <- coo.centpos(shp.rr)
coo.plot(shp.rr, main="coo.samplerr")
segments(centxy[1], centxy[2], shp.rr[, 1], shp.rr[, 2])
# interpolate
shp12 <- coo.sample(shp, 12)
coo.plot(shp12, pch=20, cex=2, main="coo.interpolate")
points(coo.interpolate(shp12, 36), pch=20, col="red")

## ----, fig.show='hold'---------------------------------------------------
coo.plot(shp)
coo.plot(coo.scale(shp))

## ----, fig.show='hold'---------------------------------------------------
shp.c <- coo.center(shp)
coo.plot(shp.c)
coo.draw(coo.scalex(shp.c, 1/3), main="coo.scalex")
coo.plot(shp.c)
coo.draw(coo.scaley(shp.c, 1/3), main="coo.scaley")

## ----, fig.show='hold'---------------------------------------------------
coo.plot(coo.shearx(shp.c, 0.5), main="coo.shearx")
coo.plot(coo.sheary(shp.c, 1.2), main="coo.sheary")

## ----, fig.show='hold'---------------------------------------------------
coo.plot(shp)
coo.plot(coo.slide(shp, 94)) # note the nose
coo.plot(coo.slidedirection(shp, direction = "N"), main="N")
coo.plot(coo.slidedirection(shp, direction = "W"), main="W")
coo.plot(coo.slidedirection(shp, direction = "S"), main="S")
coo.plot(coo.slidedirection(shp, direction = "E"), main="E")

## ----, fig.show='hide'---------------------------------------------------
coo.plot(shp)
for (i in c(10, 50, 200)) coo.plot(coo.smooth(shp, i))

## ------------------------------------------------------------------------
coo.plot(shp)
for (i in seq(0, 100, 20)) coo.draw(coo.trans(shp, i))

## ----, fig.show='hide'---------------------------------------------------
coo.plot(shp)
coo.draw(coo.close(coo.chull(shp)), border="red")
shp.c <- coo.center(shp)
coo.plot(shp.up <- coo.up(shp.c), points=TRUE)
coo.plot(coo.down(shp.c), points=TRUE) # you may also need coo.slide
coo.plot(coo.rev(shp.up)) # now counter-clockwise

## ------------------------------------------------------------------------
coo.plot(coo.center(coo.scale(shp)))

## ----, eval=FALSE--------------------------------------------------------
#  coo.cs <- function(coo){
#    return(coo.center(coo.scale(coo)))}
#  coo.plot(coo.cs) # try that

## ----, eval=FALSE--------------------------------------------------------
#  library(magrittr)
#  coo.cs2 <- function(coo) coo %>% coo.scale %>% coo.center
#  coo.plot(coo.cs2(shp))

## ----, fig.show='hold'---------------------------------------------------
data(bot)
stack(bot)
bot.c <- coo.center(bot)
stack(bot.c)

## ------------------------------------------------------------------------
bot.cs <- bot.c # we make a 'copy'
bot.cs$coo <- lapply(bot.cs$coo, function(x) coo.center(coo.scale(x)))
stack(bot.cs)

