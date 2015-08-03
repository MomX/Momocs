#' Calculates shape descriptors from Coo objects
#' 
#' Extracts descriptors from a list of functions and returns a TraCoe object
#' @param Coo object on which to calculate descriptors
#' @param ... functions that returns a scalar (a single value), see examples
#' @details any function, including custom ones, that return a scalar can be passed.
#' To access the list of 'coo_' functions - not all are suitable, see \code{apropos("coo_")}
#' @examples 
#' data(bot)
#' desc <- shape_descriptors(bot, coo_circularity, coo_rectangularity, coo_area)
#' desc
#' desc$coe
#' # a custom function the product of length*width
#' pseudo_area <- function(x) prod(coo_lw(x))
#' shape_descriptors(bot, pseudo_area)
#' @export
shape_descriptors <- function(Coo, ...){
  desc <- eval(substitute(alist(...)))
  coos <- Coo$coo
  nr <- length(coos)
  nc <- length(desc)
  res <- matrix(NA, nrow=nr, ncol=nc,
                dimnames=list(names(coos),
                              gsub("coo.", "", as.character(desc))))
  for (i in 1:nc){
    res[, i] <- sapply(coos, desc[[i]])
  }
  if (ncol(Coo$fac)!=0){
    fac <- Coo$fac
  } else {
    fac <- data.frame()
  }
  TraCoe <- TraCoe(coe=res, fac=fac)
  return(TraCoe)}


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



