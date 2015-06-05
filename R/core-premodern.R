#' Truss measurement
#' 
#' A method to calculate on shapes or on Coo truss measurements,
#' which is all pairwise combinations of euclidean distances
#' @param x a shape or an Ldk object
#' @return a named numeric or matrix
#' @note Mainly implemented for historical/didactical reasons.
#' @examples
#' # example on a single shape
#' data(shapes)
#' cat <- coo_sample(shapes[4], 6)
#' truss(cat)
#' 
#' # example on wings dataset
#' data(wings)
#' tx <- truss(wings)
#' dim(tx)
#' # we normalize and plot an heatmap
#' txn <- apply(tx, 2, .normalize)
#' # heatmap(txn)
#' 
#' txp <- PCA(tx, scale. = TRUE, center=TRUE, fac=wings$fac)
#' plot(txp, 1)
#' @export
truss <- function(x){
  UseMethod("truss")
}
#' @export
truss.default <- function(x){
  res <- as.numeric(dist(x))
  names(res) <- apply(combn(1:nrow(x), 2), 2, paste, collapse="-")
  return(res)
}
#' @export
truss.Ldk <- function(x){
  return(t(sapply(x$coo, truss)))
}



