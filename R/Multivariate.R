# 1. mshapes on Coe ------------------------------------------------------------
mshapes <- function(Coe, fac, nb.pts){UseMethod("mshapes")}
mshapes.OutCoe <- function(Coe, fac, nb.pts=120){
  OutCoe <- Coe
  nb.h <-  ncol(OutCoe$coe)/4 #todo
  if (missing(fac)) {
    cat("* no 'fac' provided. Returns meanshape.\n")
    coe.mshape <- apply(OutCoe$coe, 2, mean)
    xf <- coeff.split(coe.mshape, nb.h, 4)
    return(efourier.i(xf, nb.pts=nb.pts))}
  
  f <- OutCoe$fac[, fac]
  fl <- levels(f)
  res <- list()
  
  for (i in seq(along=fl)){
    coe.i <- OutCoe$coe[f==fl[i], ]
    if (is.matrix(coe.i)) {
      coe.i <- apply(coe.i, 2, mean)}
    xf <- coeff.split(cs=coe.i, nb.h=nb.h, cph=4)
    res[[i]] <- efourier.i(xf, nb.h=nb.h, nb.pts=nb.pts)}
  names(res) <- fl
  return(res)}

mshapes.OpnCoe <- function(Coe, fac, nb.pts=120){
  OpnCoe <- Coe
  n <-  length(OpnCoe$mshape) #todo
  if (missing(fac)) {
    cat("* no 'fac' provided. Returns meanshape.\n")
    coe.mshape <- apply(OpnCoe$coe, 2, mean)
    mod.mshape <- OpnCoe$mod
    mod.mshape$coefficients <- coe.mshape
    return(polynomials.i(mod.mshape))}
  
  f <- OpnCoe$fac[, fac]
  fl <- levels(f)
  res <- list()
  mod.mshape <- OpnCoe$mod
  for (i in seq(along=fl)){
    coe.i <- OpnCoe$coe[f==fl[i], ]
    if (is.matrix(coe.i)) {
      coe.i <- apply(coe.i, 2, mean)}
    mod.mshape$coefficients <- coe.mshape
    res[[i]] <- polynomials.i(mod.mshape)}
  names(res) <- fl
  return(res)}

# 2. MANOVAs -------------------------------------------------------------------
#todo
manova.default <- manova
manova <- function(...){UseMethod("manova")}
manova.OutCoe <- function(OutCoe, fac, retain, drop, ...){
  if (missing(fac)) stop("'fac' must be provided")
  fac <- OutCoe$fac[, fac]
  x <- OutCoe$coe
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0 }}
  
  nb.h <- ncol(x)/4
  fr   <- floor((ncol(x)-2)/4) #full rank efourier
  
  if (!missing(retain)) {
    if ((retain - drop) > fr) {
      retain <- fr
      if (retain > nb.h) retain <- nb.h
      cat("'retain' was too high and the matrix not of full rank. Analysis done with", retain, "harmonics\n")}
  } else {
    retain <- fr
    if (retain > nb.h) {retain <- nb.h}
    cat("* Analysis done with", retain, "harmonics\n")}
  
  harm.sel <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=4)
  mod <- summary(manova(x[,harm.sel]~fac), test="Hotelling")
  return(mod)}


# 2. PCA on Coe ----------------------------------------------------------------
pca <- function(x, ...){UseMethod("pca")}
pca.OutCoe <- function(x, ...){
  OutCoe <- x
  PCA <- prcomp(OutCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

pca.OpnCoe <- function(x, ...){
  OpnCoe <- x
  PCA <- prcomp(OpnCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OpnCoe$fac
  PCA$mshape <- apply(OpnCoe$coe, 2, mean)
  PCA$method <- OpnCoe$method
  PCA$mod    <- OpnCoe$mod #the only diff so far
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}


clust <- function(...){UseMethod("clust")}
clust.OutCoe <- function(OutCoe, fac,
                         method = "euclidean", type="unrooted", palette=col.summer2, ...){
  if (missing(fac)) {
    cols <- rep("black", nrow(OutCoe$coe))
  } else {
    facs <- OutCoe$fac[, fac]
    cols <- palette(nlevels(facs))[facs]
  }
  dist.mat <- dist(OutCoe$coe, method=method)
  OutCoe.hc <- hclust(dist.mat)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(oma=rep(0, 4), mar=rep(0,4))
  plot(as.phylo.hclust(OutCoe.hc), tip.color=cols, type=type, ...)
  return(list(dist.mat=dist.mat, hclust=OutCoe.hc))}


