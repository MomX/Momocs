#' @demoTitle polynomials
#' @demoTitle landmarks
# The first implementations for open curves using polynomial fitting
# let's load 'olea' See ?olea for informations and credits
data(olea)
olea
stack(olea)

# Let's reregister them with Booktein's coordinate to normalize them
# (that's already the case, but it's for the example)
olea <- coo.bookstein(olea)

# There are some ventral and dorsal views
table(olea$fac[, 2:3])

# Let's create two subsets
olea.V <- subset(olea, view=="VL")
olea.D <- subset(olea, view=="VD")

# Polynomials fitting
# qualitative inspection (more to come)
nqual(olea, method = "orthoPolynomials", degree.range = 2:10)
# let's choose a degree = 5
opV <- rawPolynomials(olea.V, 5)
opD <- rawPolynomials(olea.D, 5)

# a summary of the fit (V), and an hist (D)
summary(opV$r2)
hist(opD$r2) # really good with a degree5

# A PCA and its plots
Vp <- PCA(opV)
Dp <- PCA(opD)

plot(Vp, "domes")
plot(Dp, "domes")

plot(Vp, "cep")
plot(Dp, "cep")

# some LDA - can we discriminate between cepages and domesticated/wild ?
LDA(Vp, "domes")
LDA(Dp, "domes")

LDA(Vp, "cep")
LDA(Dp, "cep")

# some hierarchical clustering
clust(opV, "domes", palette=col.sari, cex=0.5, type="cladogram")
clust(opV, "cep", palette=col.sari, cex=0.5)

# Manova - let's test it
Manova(opV, "cep")
Manova(opD, "domes")

# mean shapes differences
ms <- mshapes(opV, "cep")
tps.grid(ms$Aglan, ms$MouBo1, grid.col = "grey70")