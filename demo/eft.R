#' @demoTitle eft
# Outline analysis using R
data(bot)
bot

# How EFT works
coo.oscillo(bot[1])

# a family picture
panel(bot, fac="type", palette=col.india, names=TRUE)

# Let's calibrate the number of harmonics
hpow(bot)
hquant(bot, id=1:5)
hqual(bot, harm.range = 1:12)

# Let's go for 12 
bot.f <- eFourier(bot, 9)
bot.f

# A PCA
bot.p <- PCA(bot.f)
plot(bot.p, "type")
# some variation around PCA
plot(bot.p, "type", xax=2, yax=3, pos="xy")

# Let's test for differences
Manova(bot.f, "type")

# Let's try and LDA now
bot.l <- LDA(bot.f, "type")
bot.l
plot(bot.l)

# Hierachical clustering
clust(bot.f, "type", palette=col.gallus)

# Mean shapes and Thin Plate Splines
ms <- mshapes(bot.f, "type")
tps.iso(ms$beer, ms$whisky)
tps.arr(ms$beer, ms$whisky)
tps.grid(ms$beer, ms$whisky)
