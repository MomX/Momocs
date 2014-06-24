
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

