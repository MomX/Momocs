#' @demoTitle landmarks
# The first implementations for landmarks, using Momocs
# let's load 'wings' See ?wings
data(wings)
wings
stack(wings)

# Full Generalized Procrustes alignment
wp <- fgProcrustes(wings)
stack(wp)

# We calculate and add the meanshape (needs a proper 'links' handling)
m <- mshape(wp)
coo_draw(m)
# plus some labels
ldk.labels(m)

# Confidence ellipses
stack(wp)
ldk.confell(wp$coo)
ldk.chull(wp$coo)

# Same thing but with contours
stack(wp)
ldk.contour(wp$coo, col = "red", nlevels = 3)
ldk.chull(wp$coo)

# A PCA and its plot
wpp <- PCA(wp)
wpp
plot(wpp, 1)

# An LDA, a cross-validation and its plot
wpl <- LDA(wp, 1)
wpl

# meanshapes
ms <- mshapes(wp, 1)
names(ms)
panel(Ldk(ms), names=TRUE)
tps.grid(ms$CX, ms$UR, grid.size = 50, grid.col = "#1A1A1A")
