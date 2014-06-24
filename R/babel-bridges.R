##### Simple bridges between R classes
#' Converts a list of coordinates to a matrix.
#' 
#' \code{l2m} converts a \code{list} with x and y components to a 2-col
#' \code{matrix} of coordinates.
#' 
#' @usage l2m(l)
#' @param l A \code{list} with x and y coordinates as components.
#' @return Returns a matrix of \eqn{(x; y)}coordinates.
#' @seealso \link{m2l}.
#' @keywords Babel
#' @examples
#' data(wings)
#' l <- m2l(wings[1])
#' l
#' m <- l2m(l)
#' m
#' @export
l2m  <- function(l) {
  m <- cbind(l$x, l$y)
  colnames(m) <- c("x", "y")
  return(m)}

#' Convert a matrix of coordinates to a list of coordinates.
#' 
#' \code{m2l} converts a matrix of \eqn{(x; y)}coordinates to a list with
#' \code{x; y} components.
#' 
#' @usage m2l(m)
#' @param m A 2-columns \code{matrix} containing x and y coordinates.
#' @return Returns a \code{list} with \eqn{x; y} components.
#' @seealso \link{l2m}.
#' @keywords Babel
#' @examples
#' data(wings)
#' l <- m2l(wings[1])
#' l
#' m <- l2m(l)
#' m
#' @export
m2l  <- function(m) {return(list(x=m[,1], y=m[,2]))}

#' Converts a list of coordinates to an array.
#' 
#' \code{l2a} converts a list of \code{k} matrices with n-rows and n-col
#' matrices to a \code{m x n x k} array.
#' 
#' @usage l2a(l)
#' @param l A \code{list} of matrices of the same dimension.
#' @return An array of coordinates.
#' @seealso \link{a2l}.
#' @keywords Babel
#' @examples
#' data(wings)
#' l <- wings$coo
#' l
#' a <- l2a(l)
#' a
#' @export
l2a  <- function(l){
  nr <- nrow(l[[1]])
  nc <- 2
  ni <- length(l)
  a  <- array(unlist(l), dim=c(nr, nc, ni),
              dimnames=list(1:nr, c("x", "y"), names(l)))
  return(a)}

#' Converts an array of coordinates to a list.
#' 
#' \code{a2l} converts an array of coordinates into a list of 2-cols matrices.
#' 
#' @usage a2l(a)
#' @param a An \code{array} of coordinates.
#' @return A \code{list} with 2-cols matrices of \eqn{(x; y)} coordinates.
#' @seealso \link{l2a}
#' @keywords Babel
#' @examples
#' data(wings)
#' l <- wings$coo
#' l
#' a <- l2a(l)
#' a
#' @export
a2l <- function(a){
  if (!is.array(a)) stop("An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {l[[i]] <- a[,,i]}
  return(l)}

#' Converts an array of coordinates to a matrix
#' 
#' Converts an array of coordinates into a matrix.
#' All the individuals (the 3rd dimension of the array) becomes rows, and
#' columns are (all) x coordinates and (all) y coordinates, so that we have 
#' x1, x2, ..., xn, y1, y2, ..., yn. Rows and colums are names anyway.
#' 
#' @param a An \code{array} of coordinates.
#' @return A matrix (see above)
#' @seealso \link{m2a} the reverse function
#' @keywords Babel
#' @examples
#' data(wings)
#' a <- l2a(wings$coo)
#' a
#' a2m(a)
#' @export
a2m <- function(a){
  #ugly
  m <- sapply(a, as.numeric)
  nc <- dim(a)[1]
  m <- matrix(m, nrow=dim(a)[3], ncol=nc*2, byrow=TRUE)
  colnames(m) <- paste0(rep(c("x", "y"), each=nc), 1:nc)
  if (!is.null(dimnames(a))) {
    rownames(m) <- dimnames(a)[[3]]}
  return(m)}

#' Converts a "matrix" of coordinates to an array of coordinates
#' 
#' Converts a matrix arranged with the individuals (the 3rd dimension of the array) as rows,
#' and (all) x coordinates and (all) y coordinates as columns, into an array built as follows:
#' nb.of.landmarks x 2 (x; y) x nb.of.individuals.
#' 
#' @param m a matrix (see above)
#' @return an array (see above)
#' @seealso \link{a2m} the reverse function
#' @keywords Babel
#' @examples
#' data(wings)
#' m <- a2m(l2a(wings$coo))
#' m2a(m)
#' @export
m2a <- function(m){
  #ugly
  a <- array(NA, dim=c(ncol(m)/2, 2, nrow(m)),
             dimnames=list(1:(ncol(m)/2), c("x", "y"), rownames(m)))
  for (i in 1:nrow(m)){
   a[,,i] <- matrix(m[i, ], ncol=2)}
  return(a)}
