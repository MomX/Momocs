##### mean shape on raw coordinates.

#' Mean shape calculation
#' 
#' Returns the mean shape, usually for procrustes/bookstein/baseline - aligned
#' \link{Ldk} objects, matrices (just return the matrix) and dim-3 arrays.
#' @param x an array (of dim=3), a list of coordinates or an Ldk object
#' @return a matrix of (x; y) coordinates
#' @rdname mshape
#' @seealso \link{mshapes}
#' @keywords Multivariate
#' @examples 
#' data(wings)
#' mshape(wings)
#' mshape(wings$coo)
#' data(bot)
#' mshape(coo.sample(bot, 24)$coo)
#' mshape(wings[1])
#' stack(wings)
#' coo.draw(mshape(wings))
#' @export
mshape <- function(x){UseMethod("mshape")}

#' @rdname mshape
#' @export
mshape.default <- function(x){cat(" * can only be called on matrices, 3-arrays, lists or Ldk objects")}

#' @rdname mshape
#' @export
mshape.Ldk <- function(x){
  Ldk <- x
  A <- ldk.check(Ldk$coo)
  return(apply(A, 1:2, mean, na.rm=TRUE))}

#' @rdname mshape
#' @export
mshape.list <- function(x){
  A <- ldk.check(x)
  return(apply(A, 1:2, mean, na.rm=TRUE))}

#' @rdname mshape
#' @export
mshape.array <- function(x){
  if (length(dim(x))==3) {
    A <- ldk.check(x)
    return(apply(A, 1:2, mean, na.rm=TRUE))}}

#' @rdname mshape
#' @export
mshape.matrix <- function(x){
  x <- coo.check(x)
  return(x)}
