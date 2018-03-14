
# Ldk ----------

#' Builds an Ldk object
#'
#' In Momocs, \code{Ldk} classes objects are
#' lists of configurations of \bold{l}an\bold{d}mar\bold{k}s, with optionnal components,
#' on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. [fgProcrustes]).
#'  \code{Ldk} objects are primarily \code{\link{Coo}} objects. In a sense, morphometrics methods
#'  on Ldk objects preserves (x, y) coordinates and `LdkCoe` are also `Ldk` objects.
#'
#' All the shapes in x must have the same number of landmarks. If you are
#' trying to make an Ldk object from an Out or an Opn object, try \link{coo_sample} beforehand
#' to homogeneize the number of coordinates among shapes.
#'
#' @param coo a \code{list} of matrices of (x; y) coordinates,
#' or an array, an Ldk object.
#' @param links (optionnal) a 2-columns \code{matrix} of 'links' between landmarks, mainly for plotting
#' @param slidings (optionnal) a 3-columns \code{matrix} defining (if any) sliding landmarks
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @return an \code{Ldk} object
#' @details implementation of \code{$slidings} is inspired by \code{geomorph}
#' @family classes
#' \code{Ldk} methods must be, so far, considered as experimental in Momocs.
#' @aliases LdkCoe
#' @examples
#' #Methods on Ldk
#' methods(class=Ldk)
#' @export
Ldk <- function(coo, links = NULL, slidings = NULL, fac = dplyr::data_frame()) {
  UseMethod("Ldk")
}

#' @export
Ldk.default <- function(coo, links = NULL, slidings = NULL, fac = dplyr::data_frame()) {
  if (is_shp(x))
    Ldk(list(x))
  else
    message("an Ldk object can only be build from a shape, a list, an array or a Coo object")

}

#' @export
Ldk.list <- function(coo, links = NULL, slidings = NULL, fac = dplyr::data_frame()) {
  if (missing(slidings) & identical(names(coo), c("coo", "slidings", "scale"))) {
    slidings <- coo$slidings
    coo <- coo$coo
  }
  Ldk <- structure(list(coo = coo, links = links, slidings = slidings, fac = fac), class = c("Ldk",
    "Coo"))
  if (!is.null(Ldk$fac))
    Ldk$fac <- as.data.frame(Ldk$fac, stringsAsFactors = FALSE)
  class(Ldk) <- c("Ldk", "Coo")
  if (is.null(names(Ldk)))
    names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

#' @export
Ldk.array <- function(coo, links = NULL, slidings = NULL, fac = dplyr::data_frame()) {
  x <- a2l(coo)
  Ldk <- Ldk(x, links = links, slidings = slidings, fac = fac)
  if (is.null(names(Ldk)))
    names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

#' @export
Ldk.Coo <- function(coo, links = NULL, slidings = NULL, fac = coo$fac) {
  .check((length(unique(coo_nb(coo))) == 1), "shapes do not have the same number of landmarks")
  Ldk <- Ldk(coo = coo$coo, links = links, slidings = slidings, fac = fac)
  if (is.null(names(Ldk)))
    names(Ldk) <- paste0("shp", 1:length(Ldk))
  return(Ldk)
}

# # The print method for Ldk objects
# #' @export
# print.Ldk <- function(x, ...) {
#   Ldk <- validate(x)
#   coo_nb <- length(Ldk)
#   if (coo_nb==0){
#     cat("An empty Ldk object")
#     return()
#   }
#   ### Header
#   cat("An Ldk object with: \n")
#   coo_nb <- length(Ldk)
#   coo_len <- sapply(Ldk$coo, nrow)
#   coo_closed <- sapply(Ldk$coo, is_closed)
#   cat(" - $coo:", coo_nb, "configurations of landmarks")
#
#   # number of coordinates
#   cat(" (", unique(coo_len), " coordinates)\n", sep = "")
#   # slidings partitions
#   if (is_slidings(Ldk)) {
#     scheme <- .slidings_scheme(Ldk$slidings)
#     n <- scheme$n
#     if (scheme$n == 1) {
#       cat(" - $slidings: ", 1, " partition of sliding landmarks (",
#           scheme$id[[1]][1], ":", scheme$id[[1]][2], ")\n", sep = "")
#     } else {
#       cat(" - $slidings: ", scheme$n, " partitions of sliding landmarks (",
#           paste(apply(scheme$id, 1, function(x) paste(x[1], x[2], sep=":")), collapse="; "),
#           ")\n", sep = "")
#     }
#   }
#   # we print the fac
#   .print.fac(Ldk$fac)
# }

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
  # # number of open outlines cat(' -', coo_nb, 'configurations of landmarks\n') #
  # one random outline eg <- sample(length(Ldk$coo), 1) coo_eg <- Ldk$coo[[eg]]
  # colnames(coo_eg) <- c('x', 'y') cat(' - One random configuration in $coo: '',
  # names(Ldk$coo)[eg], '':\n', sep = '') if (nrow(coo_eg) > 5) {
  # print(coo_eg[1:5, ], print.gap = 2) cat('etc.\n') } else { print(coo_eg,
  # print.gap = 2) cat('\n\n') } number of conf landmarks
  cat(" - $coo:", coo_nb, "configuration of landmarks")
  # number of coordinates
  cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates)\n",
    sep = "")
  # we print the fac
  .print.fac(Ldk$fac)
}
