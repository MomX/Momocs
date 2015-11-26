
# 1. Ldk builder and domestic functions
# -------------------------------------------

#' Builds an Ldk object
#'
#' In Momocs, \code{Ldk} classes objects are
#' lists of configurations of \bold{l}an\bold{d}mar\bold{k}s, with optionnal components,
#' on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. todo-Procrustes can be applied.
#'  \code{Ldk} objects are primarily \code{\link{Coo}} objects.
#'
#' All the shapes in x must have the same number of landmarks. If you are
#' trying to make an Ldk object from an Out or an Opn object, try \link{coo_sample}.
#'
#' @param coo a \code{list} of matrices of (x; y) coordinates,
#' or an array, an Ldk object.
#' @param links a matrix of 'links' between landmarks, mainly for plotting
#' @param cur a \code{list} of \code{list} of matrices of (x; y) coordinates
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @return an \code{Ldk} object
#' @seealso \link{Coo}, \link{Out}, link{Opn}.
#' @note You can pass directly as coo, the list returned by \link{tps_import} when curves are set \code{TRUE}.
#' The \code{$coo} will be passed to \code{coo} and the \code{$cur} to \code{cur}.
#' \code{Ldk} methods must be, so far, considered as experimental in Momocs.
#' @examples
#' methods(class=Ldk)
#' @export
Ldk <- function(coo, links = NULL, cur=NULL, fac = data.frame()) {
  UseMethod("Ldk")
}

#' @export
Ldk.default <- function(coo, links = NULL, cur=NULL, fac = data.frame()) {
  cat(" * an Ldk object can only be build from a list, an array or an Ldk object")
}

#' @export
Ldk.list <- function(coo, links = NULL, cur=NULL, fac = data.frame()) {
  if (missing(cur) & identical(names(coo), c("coo", "cur", "scale"))){
    cur  <- coo$cur
    coo  <- coo$coo
  }
  Ldk <- structure(list(coo = coo, links = links, cur=cur, fac = fac), class=c("Ldk", "Coo"))
  if (!is.null(Ldk$fac))
    Ldk$fac <- as.data.frame(Ldk$fac, stringsAsFactors = FALSE)
  class(Ldk) <- c("Ldk", "Coo")
  if (is.null(names(Ldk))) names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

#' @export
Ldk.array <- function(coo, links = NULL, cur=NULL, fac = data.frame()) {
  x <- a2l(coo)
  Ldk <- Ldk(x, links = links, cur=cur, fac = fac)
  if (is.null(names(Ldk))) names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

#' @export
Ldk.Coo <- function(coo, links = NULL, cur=NULL, fac = data.frame()) {
  nb.ldk <- sapply(x$coo, length)
  if (length(unique(nb.ldk)) > 1)
    stop(" * shapes do not have the same number of landmarks.")
  Ldk <- Ldk(x = x$coo, links=links, cur=cur, fac = x$fac, links = x$links)
  if (is.null(names(Ldk))) names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

# The print method for Ldk objects
#' @export
print.Ldk <- function(x, ...) {
  Ldk <- x
  ### Header
  cat("An Ldk object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- length(Ldk)
  coo_len <- sapply(Ldk$coo, nrow)
  coo_closed <- sapply(Ldk$coo, is_closed)
  #     # number of open outlines
  #     cat(" -", coo_nb, "configurations of landmarks\n")
  #     # one random outline
  #     eg <- sample(length(Ldk$coo), 1)
  #     coo_eg <- Ldk$coo[[eg]]
  #     colnames(coo_eg) <- c("x", "y")
  #     cat(" - One random configuration in $coo: '", names(Ldk$coo)[eg],
  #         "':\n", sep = "")
  #     if (nrow(coo_eg) > 5) {
  #         print(coo_eg[1:5, ], print.gap = 2)
  #         cat("etc.\n")
  #     } else {
  #         print(coo_eg, print.gap = 2)
  #         cat("\n\n")
  #     }
  # number of conf landmarks
  cat(" - $coo:", coo_nb, "configurations of landmarks")
  # number of coordinates
  cat(" (", unique(coo_len), " coordinates)\n", sep="")
  # number of curves, slidings
  if (is.cur(Ldk)){
    cur_nrow <- sapply(Ldk$cur[[1]], nrow)
    if (length(cur_nrow)==1){
      cat(" - $cur: ", length(cur_nrow), " group of sliding landmarks (", cur_nrow, " coordinates)\n", sep="")
    } else {
    cat(" - $cur: ", length(cur_nrow), " group of sliding landmarks (", paste(cur_nrow, collapse=", "), " coordinates)\n", sep="")
    }
  }
  # we print the fac
  .print.fac(Ldk$fac)
}

# The print method for LdkCoe objects
#' @export
print.LdkCoe <- function(x, ...) {
  Ldk <- x
  ### Header
  cat("An LdkCoe [full Generalized Procrustes] object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- length(Ldk)
  coo_len <- sapply(Ldk$coo, nrow)
  coo_closed <- sapply(Ldk$coo, is_closed)
  #     # number of open outlines
  #     cat(" -", coo_nb, "configurations of landmarks\n")
  #     # one random outline
  #     eg <- sample(length(Ldk$coo), 1)
  #     coo_eg <- Ldk$coo[[eg]]
  #     colnames(coo_eg) <- c("x", "y")
  #     cat(" - One random configuration in $coo: '", names(Ldk$coo)[eg],
  #         "':\n", sep = "")
  #     if (nrow(coo_eg) > 5) {
  #         print(coo_eg[1:5, ], print.gap = 2)
  #         cat("etc.\n")
  #     } else {
  #         print(coo_eg, print.gap = 2)
  #         cat("\n\n")
  #     }
  # number of conf landmarks
  cat(" - $coo:", coo_nb, "configuration of landmarks")
  # number of coordinates
  cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates)\n", sep="")
  # we print the fac
  .print.fac(Ldk$fac)
}
