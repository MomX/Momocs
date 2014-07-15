##### Various ldk utilities some could have been coo, but they
##### tend to be dedicated to landmarks

#' Checks 'ldk' shapes
#'
#' A simple utility, used internally, mostly by \link{Ldk} methods,
#' in some graphical functions, and notably in \link{l2a}.
#' Returns an array of landmarks arranged as \code{(nb.ldk) x (x; y) x (nb.shapes)},
#' when passed with either a list, a matrix or an array of coordinates.
#' If a list is provided, checks that the number of landmarks is consistent.
#'
#' @param ldk a \code{matrix} of (x; y) coordinates, a list, or an array.
#' @return an \code{array} of (x; y) coordinates.
#' @seealso \link{coo.check}
#' @keywords ShapeUtilities
#' @examples
#' #coo.check('Not a shape')
#' #coo.check(matrix(1:10, ncol=2))
#' #coo.check(list(x=1:5, y=6:10))
#' @export
ldk.check <- function(ldk) {
    if (is.array(ldk)) {
        if (length(dim(ldk) == 3)) {
            return(ldk)
        }
        if (length(dim(ldk) == 2)) {
            return(array(ldk, dim = c(nrow(ldk), ncol(ldk), 1)))
        }
        stop("A matrix an array (dim=3) must be provided.")
    }
    if (is.list(ldk)) {
        l <- sapply(ldk, length)
        if (length(unique(l)) == 1) {
            return(l2a(ldk))
        }
        stop("A list of matrices with the same number of coordinates must be provided.")
    }
    stop("A list, a matrix or a dim=3 array must be provided.")
}


#' Create links (all pariwise combinations) between landmarks
#' 
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a matrix that can be passed to \link{ldk.links}, etc. The columns
#' are the row ids of the original shape.
#' @examples
#' data(wings)
#' w <- wings[1]
#' coo.plot(w)
#' links <- links.all(w)
#' ldk.links(w, links)
#' @export
links.all <- function(coo) {
    coo <- coo.check(coo)
    links <- t(combn(1:nrow(coo), 2))
    return(links)
}

#' Create links (Delaunay triangulation) between landmarks
#' 
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a matrix that can be passed to \link{ldk.links}, etc. The columns
#' are the row ids of the original shape.
#' @details uses \link{delaunayn} in the \code{geometry} package.
#' @examples
#' data(wings)
#' w <- wings[1]
#' coo.plot(w, poly=FALSE)
#' links <- links.delaunay(w)
#' ldk.links(w, links)
#' @export
links.delaunay <- function(coo) {
    coo <- coo.check(coo)
    links <- delaunayn(coo)
    links <- rbind(links[, -1], links[, -2], links[, -3])
    links <- links[-which(duplicated(links)), ]
    return(links)
} 
