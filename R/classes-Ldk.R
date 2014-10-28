
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
#' @param x a \code{list} of matrices of (x; y) coordinates,
#' or an array, an Ldk object.
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @param links a matrix of 'links' between landmarks, mainly for plotting
#' @return an \code{Ldk} object
#' @seealso \link{Coo}, \link{Out}, link{Opn}.
#' @note \code{Ldk} methods must be, so far, considered as experimental in Momocs.
#' @keywords Ldk
#' @aliases Ldk Ldk.default Ldk.Ldk Ldk.list LdkCoe
#' @examples
#' methods(class=Ldk)
#' @export
Ldk <- function(x, fac = data.frame(), links = NULL) {
  UseMethod("Ldk")
}

#' @export
Ldk.default <- function(x, fac = data.frame(), links = NULL) {
  cat(" * an Ldk object can only be build from a list, an array or an Ldk object")
}

#' @export
Ldk.list <- function(x, fac = data.frame(), links = NULL) {
  Ldk <- structure(list(coo = x, fac = fac, links = links), class=c("Ldk", "Coo"))
  if (!is.null(Ldk$fac)) 
    Ldk$fac <- as.data.frame(Ldk$fac, stringsAsFactors = FALSE)
  class(Ldk) <- c("Ldk", "Coo")
  return(Ldk)
}

#' @export
Ldk.array <- function(x, fac = data.frame(), links = NULL) {
  x <- a2l(x)
  Ldk(x, fac = fac, links = links)
}

#' @export
Ldk.Coo <- function(x, fac = data.frame(), links = NULL) {
  nb.ldk <- sapply(x$coo, length)
  if (length(unique(nb.ldk)) > 1) 
    stop(" * shapes do not have the same number of landmarks.")
  Ldk(x = x$coo, fac = x$fac, links = x$links)
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
  coo_closed <- sapply(Ldk$coo, is.closed)
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

# The print method for LdkCoe objects
#' @export
print.LdkCoe <- function(x, ...) {
  Ldk <- x
  ### Header
  cat("An LdkCoe [full Generalized Procrustes] object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- length(Ldk)
  coo_len <- sapply(Ldk$coo, nrow)
  coo_closed <- sapply(Ldk$coo, is.closed)
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
