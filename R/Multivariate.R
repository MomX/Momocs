# 1. mshapes on Coo/Coe ------------------------------------------------------------

#' Mean shape calculation
#' 
#' Returns the mean shape, usually for procrustes/bookstein/baseline - aligned
#' Ldk objects, matrices (just return the matrix) and dim-3 arrays.
#' @param x an array (of dim=3), a list of coordinates or an Ldk object
#' @return a matrix of (x; y) coordinates
#' @examples 
#' data(wings)
#' mshape(wings)
#' mshape(wings$coo)
#' data(bot)
#' mshape(coo.sample(bot, 24)$coo)
#' mshape(wings[1])
#' @export
mshape <- function(x){UseMethod("mshape")}

#' @export
mshape.default <- function(x){cat(" * can only be called on matrices, 3-arrays, lists or Ldk objects")}

#' @export
mshape.Ldk <- function(x){
  Ldk <- x
  A <- ldk.check(Ldk$coo)
  return(apply(A, 1:2, mean, na.rm=TRUE))}

#' @export
mshape.list <- function(x){
  A <- ldk.check(x)
  return(apply(A, 1:2, mean, na.rm=TRUE))}

#' @export
mshape.array <- function(x){
  if (length(dim(x))==3) {
    A <- ldk.check(x)
    return(apply(A, 1:2, mean, na.rm=TRUE))}}

#' @export
mshape.matrix <- function(x){
  x <- coo.check(x)
  return(x)}

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
    mod.mshape$coeff <- coe.i
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
  if (length(OutCoe$method)>1) stop(" * cannot yet be used on combined OutCoe. Do it manually.")
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

#' @export
pca.LdkCoe <- function(Coe){
  LdkCoe <- Coe
  LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(LdkCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- LdkCoe$fac
  PCA$mshape <- apply(LdkCoe$coe, 2, mean)
  PCA$method <- "procrustes"
  #PCA$mod    <- OpnCoe$mod #the only diff so far
  #PCA$baseline1 <- OpnCoe$baseline1
  #PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

#' @export
pca.default <- function(Coe){
  PCA <- prcomp(Coe, scale.=TRUE, center=TRUE)
  PCA$fac <- NULL
  PCA$method <- NULL
  #PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

  #' @export
pca.data.frame <- function(Coe){
  PCA <- prcomp(Coe, scale.=TRUE, center=TRUE)
  PCA$fac <- NULL
  PCA$method <- NULL
  #PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}


# 4 - LDA ----------------------------------------------------------------
#' Linear Discriminant Analysis on Coe objects
#' 
#' Performs a LDA on Coe objects. Relies on \link{lda} in MASS.
#' @aliases LDA
#' @param x a \link{Coe}, or a PCA object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @param retain the number of PC axis to retain for LDA.PCA
#' @return a "LDA" object on which to apply \link{plot.LDA}, which is a list with components:
#' \describe{
#' \item{x}{matrix of coefficients used for computation}
#' \item{fac}{grouping structure used}
#' \item{removed}{ids of columns in the original matrix that have been removed since constant (if any)}
#' \item{mod}{raw lda object returned by \link{lda}}
#' \item{mod.pred}{predicted classification using the model. Probably optimistic, you can try CV leave-one-out or other approaches.}
#' \item{CV}{cross-validation table using mod.pred results}
#' \item{correct}{proportion of well classified individuals}
#' \item{LDs}{unstandardized LD scores see Claude (2008)}
#' \item{mshape}{mean values of coefficients in the original matrix}
#' \item{method}{inherited from the Coe object (if any)}
#' }
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.l <- LDA(bot.f, "type")
#' bot.l
#' plot(bot.l)
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, "plop")
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(x, fac, retain){UseMethod("LDA")}

#' @export
LDA.Coe <- function(x, fac, retain){
  Coe <- x
  if (missing(fac)) stop(" * no fac provided")
  fac    <- Coe$fac[, fac]
  X      <- Coe$coe
  remove <- which(apply(X, 2, sd)<1e-10)
  if (length(remove)!=0) {
    cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
    X <- X[, -remove] 
  } else { remove <- NULL }  
  # now we calculate two linear models with MASS::lda
  # one with
  mod    <- lda(X, grouping=fac)
  mod.pred <- predict(mod, X)
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  # we calculate unstandardized LDs
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we build the list with all the components that may be useful elsewhere
  # including the lda output, the CV prediction and unstandardized LDs
  # for shape reconstruction
  LDA <- list(x        = X,
              fac      = fac,
              removed  = remove,
              mod      = mod,
              mod.pred = mod.pred,
              CV       = CV,
              correct  = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape   = apply(Coe$coe, 2, mean),
              method   = Coe$method)
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
LDA.default <- function(x, fac, retain){
  X <- x
  if (missing(fac)) stop(" * no fac provided")
  # we compute lda and predictions
  mod      <- lda(X, grouping=fac)
  mod.pred <- predict(mod, X)
  # we prepare a nice CV table
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  # we calculate unstandardized LDs
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we build the list to be returned
  LDA <- list(x        = X,
              fac      = fac,
              removed  = remove,
              mod      = mod,
              mod.pred = mod.pred,
              CV       = CV,
              correct  = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape   = NULL,
              method   = NULL)
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
LDA.PCA <- function(x, fac, retain=3){
  PCA <- x
  if (missing(fac)) stop(" * no fac provided")
  fac    <- PCA$fac[, fac]
  if (missing(retain)) { cat(" * the first", retain, "PC axes are used.") } 
  X      <- PCA$x[, 1:retain]
  if (is.matrix(X)){
    remove <- which(apply(X, 2, sd)<1e-10)
    if (length(remove)!=0) {
      cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
      X <- X[, -remove] }
  } else { remove <- NULL } 
  X <- as.matrix(X)
  # we compute lda and predictions
  mod      <- lda(X, grouping=fac, tol=1e-10)
  mod.pred <- predict(mod, X)
  # we calculate unstandardized LDs (wrong here for use in shape reconstruction, 
  # would need one more step (pca2shp?) but not sure how useful it is)
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we prepare a nice CV table
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  LDA <- list(x          = X,
              fac        = fac,
              removed    = remove,
              mod        = mod,
              mod.pred   = mod.pred,
              CV         = CV,
              correct    = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape     = NULL,
              method     = PCA$method) # may be interesting to add LDA on PCA here?
  LDA$method <- PCA$method
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
