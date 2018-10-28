## ---- echo=FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(eval = TRUE, message=FALSE,
               warnings=FALSE, results="hold")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("MomX/Momocs")

## ---- eval=TRUE, echo=TRUE, message=FALSE--------------------------------
library(Momocs)

## ---- echo=FALSE, message=FALSE------------------------------------------
shapes[18] %>% coo_sample(60) %>% coo_plot(points=TRUE)
olea[11] %>% coo_sample(32) %>% coo_plot(points=TRUE)
wings[1] %>% coo_plot(points=TRUE)

## ------------------------------------------------------------------------
shapes[18] %>% head()

## ------------------------------------------------------------------------
shapes                    # prints a brief summary
panel(shapes, names=TRUE) # base graphics

## ------------------------------------------------------------------------
shp <- shapes[4]
coo_plot(shp)
# coo_plot is the base plotter for shapes
# but it can be finely customized, see ?coo_plot
coo_plot(shp, col="grey80", border=NA, centroid=FALSE, main="Meow")

## ------------------------------------------------------------------------
coo_plot(coo_center(shp), main="centered Meow")
coo_plot(coo_sample(shp, 64), points=TRUE, pch=20, main="64-pts Meow")

## ------------------------------------------------------------------------
shapes[4] %>% coo_smooth(5) %>% coo_sample(64) %>% coo_scale() %>% coo_plot()
# pipes can be turned into custom function
cs64 <- function(x) x %>% coo_sample(64) %>% coo_scale() %>% coo_center()
shapes[4] %>% cs64 %>% coo_plot() # note the axes

## ------------------------------------------------------------------------
bot %>%
coo_center %>% coo_scale %>%
coo_alignxax() %>% coo_slidedirection("up") %T>%
print() %>% stack()

## ------------------------------------------------------------------------
data(bot)
bot
panel(bot, fac="type", names=TRUE)
stack(bot)

## ------------------------------------------------------------------------
coo_oscillo(bot[1], "efourier")

## ---- eval=FALSE---------------------------------------------------------
#  Ptolemy(bot[1])

## ---- echo=FALSE, eval=FALSE, results='hide'-----------------------------
#  calibrate_harmonicpower_efourier(bot)
#  calibrate_deviations_efourier(bot)
#  calibrate_reconstructions_efourier(bot)

## ------------------------------------------------------------------------
bot.f <- efourier(bot, nb.h=10)
bot.f

## ------------------------------------------------------------------------
boxplot(bot.f, drop=1)

## ------------------------------------------------------------------------
bot.p <- PCA(bot.f)
class(bot.p)        # a PCA object, let's plot it
plot(bot.p)

## ------------------------------------------------------------------------
olea
pile(olea, ~view)    # a family picture colored by a factor

## ------------------------------------------------------------------------
op <- opoly(olea)           # orthogonal polynomials
class(op)                   # an OpnCoe, but also a Coe
op.p <- PCA(op)             # we calculate a PCA on it
class(op.p)                 # a PCA object
op %>% PCA %>% plot(~domes+var)   # notice the formula interface to combine factors

## ---- message=FALSE------------------------------------------------------
with(olea$fac, table(view, var))
# we drop 'Cypre' since there is no VL for 'Cypre' var
olea %>% filter(var != "Cypre") %>%
# split, do morphometrics, combine
chop(~view) %>% opoly %>% combine() %T>%
# we print the OpnCoe object, then resume to the pipe
print() %>%
# note the two views in the morphospace
PCA() %>% plot_PCA(~var)

## ---- message=FALSE, echo=TRUE-------------------------------------------
pile(wings)
options(Momocs_verbose=FALSE) # to silent Momocs
w.al <- fgProcrustes(wings)
pile(w.al)

# PCA
PCA(w.al) %>% plot_PCA(1)

## ---- message=FALSE, echo=TRUE-------------------------------------------
pile(chaff)
chaff.al <- fgsProcrustes(chaff)
pile(chaff.al)
chaff.al %>% PCA() %>% plot_PCA(~taxa, chullfilled = TRUE)

## ------------------------------------------------------------------------
hearts
panel(hearts, fac="aut", names="aut")

## ------------------------------------------------------------------------
ht <- measure(hearts, coo_area, coo_circularity, d(1, 3))
class(ht)
ht$coe
ht %>% PCA() %>% plot_PCA(~aut)

## ---- eval=FALSE---------------------------------------------------------
#  flower
#  flower %>% PCA() %>% plot_PCA(~sps)

## ------------------------------------------------------------------------
bot_sc <- bot %>% coo_scalars %>% TraCoe(fac=bot$fac)
bot_sc %>% PCA %>% plot_PCA(~type)

## ---- message=FALSE------------------------------------------------------
bot.f <- efourier(bot)

## ------------------------------------------------------------------------
bot.p <- PCA(bot.f)
plot_PCA(bot.p)

## ------------------------------------------------------------------------
plot_PCA(bot.p, ~type) # there are many ways to pass the factor, see ?plot_PCA and ?fac_dispatcher

## ---- eval=FALSE---------------------------------------------------------
#  scree(bot.p)
#  scree_plot(bot.p)
#  boxplot(bot.p, 1)
#  PCcontrib(bot.p)

## ------------------------------------------------------------------------
bot.p %>% as_df(3) # The first three PCs 

## ------------------------------------------------------------------------
TraCoe(iris[, -5], fac=data.frame(sp=iris$Species)) %>%
PCA() %>% plot_PCA(~sp)

## ---- eval=FALSE---------------------------------------------------------
#  #LDA(bot.f, 1)
#  # we work on PCA scores
#  bot.l <- LDA(bot.p, 1)
#  # print a summary, along with the leave-one-out cross-validation table.
#  bot.l
#  # a much more detailed summary
#  bot.l %>% summary
#  # plot.LDA works pretty much with the same grammar as plot.PCA
#  # here we only have one LD
#  plot(bot.l)
#  # plot the cross-validation table
#  plot_CV(bot.l)  # tabular version

## ------------------------------------------------------------------------
MANOVA(bot.p,  ~type)

## ------------------------------------------------------------------------
MANOVA_PW(bot.p, ~type)

## ------------------------------------------------------------------------
bot %<>% mutate(cs=coo_centsize(.))
bot %>% efourier %>% PCA %>% MANOVA(~cs)

## ---- eval=FALSE---------------------------------------------------------
#  CLUST(bot.p, ~type)

## ------------------------------------------------------------------------
KMEANS(bot.p, centers = 5)

## ---- eval=TRUE----------------------------------------------------------
# mean shape
bot.f %>% MSHAPES() %>% coo_plot()
# mean shape, per group
bot.ms <- MSHAPES(bot.f, ~type)
# lets rebuild an Out
Out(bot.ms$shp) %>% panel(names=TRUE)
# or individual shapes
beer   <- bot.ms$shp$beer    %>% coo_plot(border="blue")
whisky <- bot.ms$shp$whisky  %>% coo_draw(border="red")

## ---- eval=TRUE----------------------------------------------------------
leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo
leaves %>% plot_MSHAPES()

# or from mshapes directly
bot %>% efourier(6) %>% MSHAPES(~type) %>% plot_MSHAPES()

## ---- results="markup"---------------------------------------------------
data(olea)
olea

## ------------------------------------------------------------------------
mutate(olea, fake=factor(rep(letters[1:2], each=105)))

## ---- results="markup"---------------------------------------------------
slice(olea, 1:5)
slice(olea, -(1:100))

## ---- results="markup"---------------------------------------------------
filter(olea, domes=="cult")
# %in% is useful
filter(olea, var %in% c("Aglan", "Cypre"))
# or its complement
filter(olea, !(var %in% c("Aglan", "Cypre")))
# Also works with more than one condition
filter(olea, domes=="cult", view!="VD")
# or on operation on numeric, here a dummy numeric column
olea %>% mutate(foo=1:210) %>% filter(foo<12)
olea %>% mutate(foo=1:210) %>% filter(foo>median(foo))

## ------------------------------------------------------------------------
# reorder columns
select(olea, view, domes, var, ind)
# drop some and show the use of numeric index
select(olea, 1, Ind=ind)
# drop one
select(olea, -ind)

## ------------------------------------------------------------------------
olea %>%
filter(domes=="cult", view=="VD") %>%
rename(domesticated=domes) %>%
select(-ind)

## ------------------------------------------------------------------------
olea$fac %>% dplyr::group_by(var) %>% 
  dplyr::mutate(n=1:n(), N=n())

## ------------------------------------------------------------------------
with(olea$fac, table(var, view))
# we drop 'Cypre' since there is no VL for 'Cypre' var
olea %>% 
  filter(var != "Cypre") %>%
  # split, do morphometrics, combine
  chop(~view) %>% opoly %>% combine() %>%
  # note the two views in the morphospace
  PCA() %>% plot_PCA(~var)

## ---- eval=FALSE---------------------------------------------------------
#  names(bot)
#  length(bot)
#  bot[1]
#  bot[1:5]
#  bot$brahma
#  bot$type

## ---- eval=FALSE---------------------------------------------------------
#  library(geomorph)
#  data(pupfish)
#  str(pupfish)
#  # so $coords will become $coo, and
#  # all other components will be turned into a data.frame to feed $fac
#  # with a single line
#  Ldk(coo=pupfish$coords %>% a2l,
#    fac=pupfish[-1] %>% as.data.frame())

## ------------------------------------------------------------------------
# we simulate an imported matrix of coordinates, eg from a .csv
coeffs_from_the_wild <- bot %>% efourier(6) %$% coe
coeffs_in_Momocs <- OutCoe(coe=coeffs_from_the_wild, method="efourier", norm=TRUE)
coeffs_in_Momocs %>% PCA %>% plot

## ---- eval=FALSE---------------------------------------------------------
#  save(bot, file="Bottles.rda")
#  # closing R, going to the beach, back at work
#  load("Bottles.rda") # bot is back

## ------------------------------------------------------------------------
bot %>% as_df
bot %>% efourier %>% as_df
bot %>% efourier %>% PCA %>% as_df

## ---- eval=FALSE---------------------------------------------------------
#  bot %>% efourier %>% export
#  bot %>% efourier %>% PCA %>% export

## ---- eval=FALSE---------------------------------------------------------
#  # from Coo objects
#  bot$coo # list of matrices (of xy coordinates)
#  bot$fac # data.frame for covariates
#  
#  # from Coe objects
#  bot.f$coe # matrix of coefficients
#  bot.f$fac # data.frame for covariates
#  
#  # from PCA objects
#  bot.p$x        # scores
#  bot.p$rotation # rotation matrix

## ---- eval=FALSE---------------------------------------------------------
#  coo_lolli(beer, whisky); title("coo_lolli")
#  coo_arrows(beer, whisky); title("coo_arrow")
#  
#  # an example with coo_ruban
#  coo_plot(beer) # to get the first plot
#  coo_ruban(beer, edm(beer, whisky), lwd=8) # we add ruban based from deviations
#  coo_draw(whisky)
#  title("coo_ruban")

