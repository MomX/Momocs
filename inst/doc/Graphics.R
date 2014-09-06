## ------------------------------------------------------------------------
library(Momocs)
data(wings)
w1 <- wings[1]
w2 <- wings[2]
coo.plot(w1, border = "#FF0000", lwd = 0.5, lty = 3, main="Two mosquito wings")
coo.draw(w2, border="#0000FF", col=.transp("000FF", 0.9), poly=FALSE, pch=20)

## ------------------------------------------------------------------------
data(bot)
bot.f <- eFourier(bot, 12)
ms <- mshapes(bot.f, "type") #mshapes 'type'-wise, see bot.f$fac
whisky <- ms$shp$whisky
beer   <- ms$shp$beer 
tps.grid(beer, whisky, grid.size = 30, amp=2)
tps.arr(beer, whisky, amp=2, palette=col.summer2)
tps.iso(beer, whisky, amp=2, palette=col.spring)

