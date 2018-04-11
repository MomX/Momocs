# MDS ------------------
#' (Metric) multidimensional scaling
#'
#' A wrapper around [stats::cmdscale].
#'
#' For Details, see [vegan::metaMDS]
#'
#' @param x any [Coe] object
#' @param method a dissiminarity index to feed `method` in [stats::dist] (default: `euclidean`)
#' @param k `numeric` number of dimensions to feed [stats::cmdscale] (default: 2)
#' @param ... additional parameters to feed [stats::cmdscale]
#' @return what is returned by [stats::dist] plus `$fac`. And prepend `MDS` class to it.
#'
#' @family multivariate
#' @examples
#' x <- bot %>% efourier %>% MDS
#' x
#'
#'
#' @export
MDS <- function(x,
                method="euclidean", k=2, ...){
  UseMethod("MDS")
}

#' @export
MDS.default <- function(x,
                        method="euclidean", k=2, ...){
  message("only defined on Coe")
}

#' @export
MDS.Coe <- function(x,
                    method="euclidean", k=2, ...){
  d <- stats::dist(x$coe, method=method)

  res <- list(x=stats::cmdscale(d, k=k, ...),
              fac=x$fac)
  res <- .prepend_class(res, "MDS")
  return(res)
}

# NMDS ------------------
#' Non metric multidimensional scaling
#'
#' A wrapper around [vegan::metaMDS].
#'
#' For Details, see [vegan::metaMDS]
#'
#' @param x any [Coe] object
#' @param distance a dissiminarity index to feed [vegan::vegdist] (default: `bray`)
#' @param k `numeric` number of dimensions to feed [vegan::metaMDS] (default: 2)
#' @param try `numeric` minimum number of random starts to feed [vegan::metaMDS] (default: 20)
#' @param trymax `numeric` minimum number of random starts to feed [vegan::metaMDS] (default: 20)
#' @param ... additional parameters to feed [vegan::metaMDS]
#' @return what is returned by [vegan::metaMDS] plus `$fac`. And prepend `NMDS` class to it.
#'
#' @family multivariate
#' @examples
#' x <- bot %>% efourier %>% NMDS
#'
#' # Shepard diagram # before a Momocs wrapper
#' # vegan::stressplot(x)
#'
#' @export
NMDS <- function(x,
                 distance="bray", k=2,
                 try=20, trymax=20, ...){
  UseMethod("NMDS")
}

#' @export
NMDS.default <- function(x,
                         distance="bray", k=2,
                         try=20, trymax=20, ...){
  message("only defined on Coe")
}

#' @export
NMDS.Coe <- function(x,
                     distance="bray", k=2,
                     try=20, trymax=20, ...){
  res <- vegan::metaMDS(x$coe, distance=distance, k=k, try=try, trymax=trymax, ...)
  res$fac <- x$fac
  res <- .prepend_class(res, "NMDS")
  return(res)
}
