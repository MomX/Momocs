#' @demoTitle eft
data(bot)
bot
panel(bot, fac="type")
bot.f <- eFourier(bot, 24)
bot.f
bot.p <- pca(bot.f)
plot(bot.p, "type")
Manova(bot.f, "type")
ms <- mshapes(bot.f, "type")
tps.iso(ms$beer, ms$whisky)