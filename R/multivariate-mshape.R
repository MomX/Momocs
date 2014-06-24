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
