
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
