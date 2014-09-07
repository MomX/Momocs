## ----, fig.show='hold'---------------------------------------------------
library(Momocs)
data(shapes)
shp <- coo.center(shapes[22])
coo.plot(shp)
title(1)
coo.plot(shp, col = "grey80", border = "grey50", lwd = 2,
         zoom = 1.5, )
title(2)
coo.plot(shp, ylim=c(-130, 150),
         xy.axis = FALSE, first.point = FALSE, centroid=FALSE)
title(3)
coo.plot(coo.sample(shp, 64), lty=2, pch=3, points = TRUE, zoom=1.5, main="4")

## ----, fig.show='hold'---------------------------------------------------
data(wings)
w1 <- wings[1]
w2 <- wings[2]
coo.plot(w1, border = "#FF0000", lwd = 0.5, lty = 3, main="Two mosquito wings")
coo.draw(w2, border="#0000FF", col=.transp("000FF", 0.9), poly=FALSE, pch=20)

## ----, fig.show='hold'---------------------------------------------------
coo.lolli(w1, w2, main="coo.lolli")
coo.arrows(w1, w2, main="coo.arrows")
ldk.labels(w1)

## ----, fig.show='hold'---------------------------------------------------
data(bot)
bot.f <- eFourier(bot, 12)
ms <- mshapes(bot.f, "type") #mshapes 'type'-wise, see bot.f$fac
whisky <- ms$shp$whisky
beer   <- ms$shp$beer 
tps.grid(beer, whisky, grid.size = 30, amp=2)
tps.arr(beer, whisky, amp=2, palette=col.summer2)
tps.iso(beer, whisky, amp=2, palette=col.spring)

## ------------------------------------------------------------------------
data(bot)

## ----, eval=FALSE--------------------------------------------------------
#  plot(bot) # random inspection (not shown)

## ------------------------------------------------------------------------
plot(bot, 9) # let's go for a stout

## ----, fig.show='hold'---------------------------------------------------
panel(bot)
panel(bot, fac = "type", palette=col.spring, names=TRUE)

## ----, fig.show='hold'---------------------------------------------------
stack(bot)
bot <- coo.slidedirection(coo.scale(coo.center(bot)), "W")
stack(bot)

## ----, fig.show='hold'---------------------------------------------------
data(olea) # Opn 
class(olea)
panel(olea)
stack(olea)

data(wings) # Ldk
class(wings)
panel(wings)
stack(wings)

## ----, fig.show='hold'---------------------------------------------------
bot.f <- eFourier(bot, 12)
hist(bot.f)
boxplot(bot.f)

## ----, fig.show='hold'---------------------------------------------------
panel(bot.f)
stack(bot.f)

## ----, fig.show='hold'---------------------------------------------------
olea.p <- orthoPolynomials(olea, degree = 5, nb.pts = 50)
hist(olea.p)
boxplot(olea.p)
#todo
#panel(olea.p)
#stack(olea.p)

wings.fg <- fgProcrustes(wings, verbose = FALSE)
#todo
#hist(wings.fg)
#boxplot(wings.fg)
panel(wings.fg)
stack(wings.fg)

