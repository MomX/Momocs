## ---- echo=FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(eval = TRUE, message=FALSE, 
               warnings=FALSE, results="hold")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("vbonhomme/Momocs")

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
panel2(shapes)            # ggplot2 graphics

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
  coo_alignxax() %>% coo_slidedirection("N") %T>% 
  print() %>% stack()

## ------------------------------------------------------------------------
data(bot)
bot
panel(bot, fac="type", names=TRUE)
stack(bot)

## ------------------------------------------------------------------------
coo_oscillo(bot[1], "efourier")

## ------------------------------------------------------------------------
Ptolemy(bot[1])

## ------------------------------------------------------------------------
calibrate_harmonicpower(bot)
calibrate_deviations(bot)
calibrate_reconstructions(bot)

## ------------------------------------------------------------------------
bot.f <- efourier(bot, nb.h=10)
bot.f

## ------------------------------------------------------------------------
hist(bot.f, drop=0)
boxplot(bot.f, drop=1)
stack(bot.f)

## ------------------------------------------------------------------------
bot.p <- PCA(bot.f)
class(bot.p)        # a PCA object, let's plot it
plot(bot.p)

## ---- message=FALSE, error=FALSE, warning=FALSE, results="hide"----------
# raw molars dataset
stack(molars, title = "Non-aligned molars")
# Procrustes-aligned and slided molars
mol.al <- fgProcrustes(molars) %>% coo_slidedirection("W")
stack(mol.al, title="Aligned molars")

# Now compare PCA and morphospace using the 1st harmonic alignment
molars %>% efourier(norm=TRUE) %>% PCA() %>% plot("type")
# and the a priori normalization 
molars %>% efourier(norm=FALSE) %>% PCA() %>% plot("type")

## ------------------------------------------------------------------------
olea
stack(olea, fac="view")     # already aligned \o/
panel(olea, names="ind")    # another family picture

## ------------------------------------------------------------------------
op <- opoly(olea)           # orthogonal polynomials
class(op)                   # an OpnCoe, but also a Coe
op.p <- PCA(op)             # we calculate a PCA on it
class(op.p)                 # a PCA object
plot(PCA(op), ~domes+var)   # notice the formula interface to combine factors

## ---- message=FALSE------------------------------------------------------
table(olea, "view", "var") 
# we drop 'Cypre' since there is no VL for 'Cypre' var
olea %>% filter(var != "Cypre") %>%              
  # split, do morphometrics, combine
  chop(view) %>% lapply(opoly) %>% combine() %T>%
   # we print the OpnCoe object, then resume to the pipe
  print() %>%
  # note the two views in the morphospace
  PCA() %>% plot("var")

## ---- message=FALSE, echo=FALSE------------------------------------------
stack(wings)
w.al <- fgProcrustes(wings)
stack(w.al)
ldk_confell(w.al$coo, col = "red")
ldk_chull(w.al$coo)
ldk_contour(w.al$coo)
ldk_labels(mshapes(w.al$coo))

## ------------------------------------------------------------------------
hearts
panel(hearts, fac="aut", names="aut")

## ------------------------------------------------------------------------
ht <- measure(hearts, coo_area, coo_circularity, d(1, 3))
ht %>% PCA() %>% plot("aut", pch=20, ellipsesax=F, ellipse=T, loadings=T)

## ------------------------------------------------------------------------
flower
flower %>% PCA() %>% plot("sp", loadings=TRUE)

## ---- message=FALSE------------------------------------------------------
bot.f <- efourier(bot)

## ------------------------------------------------------------------------
bot.p <- PCA(bot.f)
plot(bot.p)

## ------------------------------------------------------------------------
plot(bot.p, "type") # equivalent to plot(bot.p, 1)

## ------------------------------------------------------------------------
plot(bot.p, 1, ellipses=TRUE, ellipsesax = FALSE, pch=c(4, 5))
plot(bot.p, 1, chull=TRUE, pos.shp = "full_axes", abbreviate.labelsgroups = TRUE, points=FALSE, labelspoints = TRUE)
plot(bot.p, 1, pos.shp="circle", stars=TRUE, palette=col_qual)

## ------------------------------------------------------------------------
plot2(bot.p, "type")

## ------------------------------------------------------------------------
scree(bot.p)
scree_plot(bot.p)
boxplot(bot.p, 1)
PCcontrib(bot.p)

## ------------------------------------------------------------------------
TraCoe(iris[, -5], fac=data.frame(sp=iris$Species)) %>% 
  PCA() %>% plot("sp", loadings=TRUE)
# or
PCA(iris[, -5], fac=data.frame(sp=iris$Species)) %>%
  plot("sp", chull=TRUE, ellipses=TRUE, conf_ellipses = 0.9)

## ---- eval=FALSE---------------------------------------------------------
#  #LDA(bot.f, 1)
#  # we work on PCA scores
#  bot.l <- LDA(bot.p, 1)
#  # print a summary, along with the leave-one-out cross-validation table.
#  bot.l
#  # plot.LDA works pretty much with the same grammar as plot.PCA
#  # here we only have one LD
#  plot(bot.l)
#  # plot the cross-validation table
#  plot_CV(bot.l)  # tabular version
#  plot_CV2(bot.l) # arrays version

## ------------------------------------------------------------------------
MANOVA(bot.p, "type")

## ------------------------------------------------------------------------
MANOVA_PW(bot.p, "type")

## ---- eval=FALSE---------------------------------------------------------
#  CLUST(bot.p, 1)

## ------------------------------------------------------------------------
KMEANS(bot.p, centers = 5)

## ------------------------------------------------------------------------
# mean shape
bot.f %>% mshapes() %>% coo_plot()
# mean shape, per group
bot.ms <- mshapes(bot.f, 1)
beer   <- bot.ms$shp$beer    %T>% coo_plot(border="blue")
whisky <- bot.ms$shp$whisky  %T>% coo_draw(border="red")
legend("topright", lwd=1,
       col=c("blue", "red"), legend=c("beer", "whisky"))

## ---- results="markup"---------------------------------------------------
data(olea)
olea

# slice: select individuals based on their position
slice(olea, 1:5)
slice(olea, -(1:100))

# filter: select individual based on a logical condition
filter(olea, domes=="cult", view!="VD")

# select: pick, reorder columns from the $fac
select(olea, 1, Ind=ind)

# rename: rename columns (select can also do it)
rename(olea, domesticated=domes)

# mutate: add new columns
mutate(olea, fake=factor(rep(letters[1:2], each=105)))

# transmute: add new columns and drop others
transmute(olea, fake=factor(rep(letters[1:2], each=105)))

## ------------------------------------------------------------------------
olea %>% 
filter(domes=="cult", view=="VD") %>% 
rename(domesticated=domes) %>% 
select(-ind)

## ------------------------------------------------------------------------
table(olea, "view", "var") 
# we drop 'Cypre' since there is no VL for 'Cypre' var
olea %>% filter(var != "Cypre") %>%              
  # split, do morphometrics, combine
  chop(view) %>% lapply(opoly) %>% combine() %>% 
  # note the two views in the morphospace
  PCA() %>% plot("var")

## ---- eval=FALSE---------------------------------------------------------
#  names(bot)
#  length(bot)
#  table(olea, "var", "domes")
#  Ntable(olea, "var", "domes")
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
#  fac=pupfish[-1] %>% as.data.frame())

## ---- eval=FALSE---------------------------------------------------------
#  save(bot, file="Bottles.rda")
#  # closing R, going to the beach, back at work
#  load("Bottles.rda") # bot is back

## ---- eval=FALSE---------------------------------------------------------
#  bot %>% as_df # then %>% write.table
#  bot %>% efourier %>% export
#  bot %>% efourier %>% PCA %>% export

## ---- eval=FALSE---------------------------------------------------------
#  # from Coo objects
#  bot$coo
#  bot$fac
#  
#  # from Coe objects
#  bot.f$coe
#  bot.f$fac
#  as_df(bot.f)
#  
#  # from PCA objects
#  bot.p$x # scores
#  bot.p$rotation # rotation matrix

## ------------------------------------------------------------------------
panel(bot)
panel2(bot)

## ------------------------------------------------------------------------
library(ggplot2)
gg <- panel2(bot)
gg + theme_minimal()

## ------------------------------------------------------------------------
# we build a ggplot object from a shape turned into a data.frame
shapes[4] %>% m2d() %>% ggplot() + 
  aes(x, y) + geom_path() + coord_equal() + 
  labs(title="ggplot2 Meow") + theme_minimal()

## ------------------------------------------------------------------------
bot.p %>% as_df() %>% ggplot() +
  aes(x=PC1, y=PC2, col=type) + coord_equal() + 
  geom_point() + geom_density2d() + theme_light()

