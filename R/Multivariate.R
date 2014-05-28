# 1. mshapes on Coe ------------------------------------------------------------
#' Mean shape calculation from matrices of coefficients
#' 
#' Calculates mean shapes on matrices of coefficients by groups (if passed with
#' a "fac") or globally (if not).
#' @aliases mshapes
#' @param Coe a \link{Coe} object
#' @param fac factor from the $fac slot. See examples below.
#' @param nb.pts numeric the number of points for calculated shapes
#' @return a list of matrices of (x,y) coordinates.
#' @keywords multivariate
#' @export
mshapes <- function(Coe, fac, nb.pts){UseMethod("mshapes")}
#' @export
mshapes.default <- function(Coe, fac, nb.pts){}
#' @export
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

#' @export
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
#' Multivariate analysis of variance on matrices of coefficients
#' 
#' Calculates mean shapes on matrices of coefficients by groups (if passed with
#' a "fac") or globally (if not).
#' @aliases Manova
#' @param ... a \link{Coe} object
#' @return a list of matrices of (x,y) coordinates.
#' @keywords multivariate
#' @export
Manova <- function(...){UseMethod("Manova")}
#' @export
Manova.OutCoe <- function(OutCoe, fac, retain, drop, ...){
  if (missing(fac)) stop("'fac' must be provided")
  if (!is.factor(fac)) {fac <- OutCoe$fac[, fac]}
  x <- OutCoe$coe
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0 }}
  cph <- ifelse(OutCoe$method == "eFourier", 4, 2)
  nb.h <- ncol(x)/cph
  fr <- nrow(x) - nlevels(fac)
  max.h <- floor(fr/cph)
  if (missing(retain)) {
    retain <- max.h + drop
    if (retain > nb.h) { retain <- nb.h }
    cat(" * 'retain' was missing. MANOVA done with", retain, "harmonics ")
    if (drop>0) { cat( "and the first", drop, "dropped.\n") } else {cat("./n")}
  } else {
    if ((retain - drop)> max.h) {
      retain <- max.h + drop
      if (retain > nb.h) { retain <- nb.h }
      cat(" * 'retain' was too ambitious. MANOVA done with", retain, "harmonics ")
      if (drop>0) {cat( "and the first", drop, "dropped.\n")} else {cat("./n")}}}

  harm.sel <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=cph)
  #cat(retain, drop, nb.h, cph)  
  cat("\n")
  mod <- summary(manova(x[, harm.sel]~fac), test="Hotelling")
  return(mod)}


# 3. PCA on Coe ----------------------------------------------------------------
#' Principal component analysis on Coe objects
#' 
#' Performs a PCA on OutCoe, OpnCoe objects.
#' @aliases pca
#' @param Coe the \link{Coe} object
#' @return a "PCA" object on which to apply \link{plot.PCA}
#' @export
pca <- function(Coe){UseMethod("pca")}
#' @export
pca.Coe <- function(Coe){
  OutCoe <- Coe
  PCA <- prcomp(OutCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}
#' @export
pca.OpnCoe <- function(Coe){
  OpnCoe <- Coe
  PCA <- prcomp(OpnCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OpnCoe$fac
  PCA$mshape <- apply(OpnCoe$coe, 2, mean)
  PCA$method <- OpnCoe$method
  PCA$mod    <- OpnCoe$mod #the only diff so far
  PCA$baseline1 <- OpnCoe$baseline1
  PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}


# 4 - LDA ----------------------------------------------------------------
#' Linear Discriminant Analysis on Coe objects
#' 
#' Performs a LDA on OutCoe, (OpnCoe no yet) objects. Relies on \link{lda} in MASS.
#' @aliases LDA
#' @param Coe the \link{Coe} object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @return a "LDA" object on which to apply \link{plot.LDA}. #todo
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.l <- LDA(bot.f, "type")
#' bot.l
#' plot(bot.l)
#' 
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, "plop")
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(Coe, fac){UseMethod("LDA")}
#' @export
LDA.OutCoe <- function(Coe, fac){
  if (missing(fac)) stop(" * no fac provided")
  fac    <- Coe$fac[, fac]
  X      <- Coe$coe
  remove <- which(apply(X, 2, sd)<1e-10)
  if (length(remove)!=0) { X <- X[, -remove] } else { remove <- NULL }    
  mod    <- lda(X, grouping=fac)
  mod.pred <- predict(mod, X)
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  LDA <- list(x=X, fac=fac, removed=remove,
              mod=mod, mod.pred=mod.pred,
              CV=CV, correct=sum(diag(CV))/sum(CV))
  LDA$mshape <- apply(Coe$coe, 2, mean)
  LDA$method <- Coe$method
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
print.LDA <- function(x, ...){
  cat("An 'LDA' object. See ?LDA.\n")
  cat("Cross-validation table (", signif(x$correct * 100, 3), "% ): \n")
  print(x$CV)}

# n - Clustering ----------------------------------------------------------

#' Hierarchical clustering #todo
#' 
#' @param OutCoe an OutCoe object
#' @param fac a colum or an id from $fac
#' @param method the method to use
#' @param type the type of plot
#' @param palette a color palette to use
#' @param ... additional arguments to fed plot.phylo
#' @export
clust <- function(OutCoe, fac, method, type, palette, ...){UseMethod("clust")}
#' @export
clust.OutCoe <- function(OutCoe, fac,
                         method = "euclidean", type="unrooted", palette=col.summer, ...){
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
