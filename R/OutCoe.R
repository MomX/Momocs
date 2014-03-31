
OutCoe <- function(coe.matrix, fac=data.frame(), method, norm){
  if (missing(method)) stop("a method must be provided to Coe")
  OutCoe <- list(coe=coe.matrix, fac=fac, method=method, norm=norm)
  class(OutCoe) <- "OutCoe"
  return(OutCoe)}

# The print method for Out objects
print.OutCoe <- function(OutCoe){
  p <- pmatch(OutCoe$method, c("eFourier", "rFourier", "tFourier"))
  met <- switch(p, "elliptical Fourier", "radii variation", "tangent angle")
  ### Header
  cat("An OutCoe object [", met, "analysis ] (see ?OutCoe) \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- nrow(OutCoe$coe) #nrow method ?
  harm.nb <- ncol(OutCoe$coe)/ifelse(p == 1, 4, 2)
  # number of outlines and harmonics
  cat(" -", coo.nb, "outlines described\n")
  cat(" -", coo.nb, "harmonics\n")
  # lets show some of them for a quick inspection
  cat(" - Some harmonic coefficients from random outlines in $coe: \n")
  row.eg <- sort(sample(coo.nb, 5, replace=FALSE))
  col.eg <- coeff.sel(retain=ifelse(harm.nb > 3, 3, harm.nb), drop=0,
                      nb.h=harm.nb, cph=ifelse(p==1, 4, 2))
  print(signif(OutCoe$coe[row.eg, col.eg], 3))
  cat("etc.\n")
  # number of grouping factors
  df <- OutCoe$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      cat("     ", colnames(df)[i], ": ", levels(df[, i]),"\n")}}}

pca <- function(x, ...){UseMethod("pca")}
pca.OutCoe <- function(OutCoe){
  PCA <- prcomp(OutCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  class(PCA) <- c("OutPCA", class(PCA))
  return(PCA)}

plot.OutPCA <- function(#basics
  PCA, fac, xax=1, yax=2, 
  #color choice
  col="black", pch=20, palette=col.summer2,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp=1,
  size.shp=20, border.shp="#00000055", col.shp="#00000011",
  #stars
  stars=TRUE,
  #ellipses
  ellipses=TRUE, conf=0.5,
  #convexhulls
  chull=TRUE,
  #labels
  labels=TRUE,
  #axisnames
  axisnames=TRUE,
  #axisvar
  axisvar=TRUE,
  #eigen
  eigen=TRUE,
  #
  rug=TRUE,
  title=substitute(PCA)
){
  xy <- PCA$x[, c(xax, yax)]
  # we check and prepare
  if (!missing(fac)) {
    if (!is.factor(fac)) { fac <- factor(PCA$fac[, fac]) }
    if (missing(col)) {
      col.groups <- palette(nlevels(fac))
      col <- col.groups[fac]}
    if (!missing(pch)) {
      if (length(pch)==nlevels(fac)) { pch <- pch[fac] }}}
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  .frame(xy, center.origin, zoom=zoom)
  if (grid) .grid(xy)
  .morphospace(xy, pos.shp=pos.shp, rot=PCA$rotation[, c(xax, yax)], mshape=PCA$mshape,
               size.shp=size.shp, border.shp=border.shp, col.shp=col.shp)
  if (!missing(fac)) {
    if (stars)    .stars(xy, fac, col.groups)
    if (ellipses) .ellipses(xy, fac, conf=conf, col.groups) #+conf
    if (chull)    .chull(xy, fac, col.groups)
    if (labels)   .labels(xy, fac, col.groups)
    if (rug)      .rug(xy, fac, col.groups)
  } else {
    if (rug)      .rug(xy, NULL, col)
  }
  points(xy, pch=pch, col=col)
  if (axisnames)  .axisnames(xax, yax)
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax)
  box()}

#c OutCoe
# boxpltoCoe
#hist

#pca
#hcontrib
#deprecate ellipse par
#manova
#meanshape
#clust
#discri

#### eigen barplot en rouge dudi.plot





