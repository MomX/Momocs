
# Opn -------------------------------------------

#' Builds an Opn object
#'
#' In Momocs, \code{Opn} classes objects are
#' lists of \bold{op}e\bold{n} outlines, with optionnal components,
#' on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. \link{npoly} can be applied.
#'  \code{\link{Opn}} objects are primarily \code{\link{Coo}} objects.
#'
#' @param x \code{list} of matrices of (x; y) coordinates
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @param ldk (optionnal) \code{list} of landmarks as row number indices
#' @return an \code{Opn} object
#' @family classes
#' @aliases Opn
#' @examples
#' #Methods on Opn
#' methods(class=Opn)
#' # we load some open outlines. See ?olea for credits
#' olea
#' panel(olea)
#' # orthogonal polynomials
#' op <- opoly(olea, degree=5)
#' # we print the Coe
#' op
#' # Let's do a PCA on it
#' op.p <- PCA(op)
#' plot(op.p, 'domes')
#' plot(op.p, 'var')
#' # and now an LDA after a PCA
#' olda <- LDA(PCA(op), 'var')
#' # for CV table
#' olda
#' plot(olda)
#' @export
Opn <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  UseMethod("Opn")
}

#' @export
Opn.default <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  if (is_shp(x))
    Opn(list(x))
  else
    message("an Opn object can only be build from a shape, a list, an array or a Coo object")
}

#' @export
Opn.list <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  x <- lapply(x, as.matrix)
  Opn <- structure(list(coo = x, fac = fac, ldk = ldk), class=c("Opn", "Coo"))
  if (!is.null(Opn$fac))
    Opn$fac <- as.data.frame(Opn$fac, stringsAsFactors = FALSE)
  class(Opn) <- c("Opn", "Coo")
  if (is.null(names(Opn))) names(Opn) <- paste0("shp", 1:length(Opn))
  return(Opn)
}

#' @export
Opn.array <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  x <- a2l(x)
  Opn <- Opn(x, fac = fac, ldk = ldk)
  if (is.null(names(Opn))) names(Opn) <- paste0("shp", 1:length(Opn))
  return(Opn)
}

#' @export
Opn.Coo <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  Opn <- Opn(x = x$coo, fac = x$fac, ldk = x$ldk)
  if (is.null(names(Opn))) names(Opn) <- paste0("shp", 1:length(Opn))
  return(Opn)
}

# # The print method for Out objects
# #' @export
# print.Opn <- function(x, ...) {
#   Opn <- validate(x)
#   coo_nb <- length(Opn)
#   if (coo_nb==0){
#     cat("An empty Opn object")
#     return()
#   }
#   ### Header
#   cat("An Opn object with: \n")
#   coo_len <- sapply(Opn$coo, nrow)
#   coo_closed <- sapply(Opn$coo, coo_is_closed)
#   # number of outlines
#   cat(" - $coo:", coo_nb, "open outlines")
#   # number of coordinates
#   cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates)\n", sep="")
#   # number of landmarks
#   if (length(Opn$ldk) != 0) {
#     cat(" - $ldk:", length(Opn$ldk[[1]]), "landmark(s) defined\n")
#   } else {
#     #     cat(" - No landmark defined\n")
#   }
#   # we print the fac
#   .print.fac(Opn$fac)
# }

# OpnCoe ---------------------------------------------------------
#' Builds an OpnCoe object
#'
#' In Momocs, \code{OpnCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot})
#' and specific methods can be applied.
#'  \code{OpnCoe} objects are primarily \code{\link{Coe}} objects.
#'
#' @param coe \code{matrix} of morphometric coefficients
#' @param fac (optionnal) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param baseline1 \eqn{(x; y)} coordinates of the first baseline point
#' @param baseline2 \eqn{(x; y)} coordinates of the second baseline point
#' @param mod an R \link{lm} object, used to reconstruct shapes
#' @param r2 numeric, the r-squared from every model
#' @return an \code{OpnCoe} object
#' @family classes
#' @examples
#' # all OpnCoe classes
#' methods(class='OpnCoe')
#' @export
OpnCoe <- function(coe = matrix(), fac = dplyr::data_frame(), method = character(),
                   baseline1 = numeric(), baseline2 = numeric(), mod = list(),
                   r2 = numeric()) {
  if (missing(method))
    stop("a method must be provided to OpnCoe")
  OpnCoe <- list(coe = coe, fac = fac, method = method, baseline1 = baseline1,
                 baseline2 = baseline2, mod = mod, r2 = r2)
  OpnCoe$coe %<>% as.matrix()
  class(OpnCoe) <- c("OpnCoe", "Coe")
  return(OpnCoe)
}

# The print method for Out objects
#' @export
print.OpnCoe <- function(x, ...) {
  OpnCoe <- x
  if (length(OpnCoe$method)>1) {
    met <- c("combined:", paste0(OpnCoe$method, collapse=" + "))
    met <- c(met, "analyses ]\n")
    combined <- TRUE
  } else {
    p <- pmatch(OpnCoe$method, c("npoly", "opoly", "dfourier"))
    met <- switch(p, "npoly", "opoly", "discrete cosine tansform")
    met <- c(met, "analysis ]\n")
    combined <- FALSE
  }
  ### Header
  cat("An OpnCoe object [", met)
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- nrow(OpnCoe$coe)  #nrow method ?
  cat(" - $coe:", coo_nb, "open outlines described\n")
  if (combined) {
    degree <- ncol(OpnCoe$coe)
    # p==3 is the case for dfourier all along the method
    # if (p==3) degree <- degree/2
    # number of outlines and harmonics
    #     if (p==3){
    #       cat(degree, " harmonics\n", sep="")
    #     } else {
    #       cat(degree, "th degree (+Intercept)\n", sep="")
    #     }
    # we print the baselines
    if (!is.null(c(x$baseline1, x$baseline2))) {
      cat(" - $baseline1: (", paste(x$baseline1, collapse="; "), ")\n", sep="")
      cat(" - $baseline2: (", paste(x$baseline2, collapse="; "), ")\n", sep="")
    }
    # lets show some of the coefficients for a quick inspection
    # boring removed it
    # cat(" - $coe: 1st coefficients from random open outlines: \n")
    # row.eg <- sort(sample(coo_nb, ifelse(coo_nb < 5, coo_nb, 5), replace = FALSE))
    # nc <- ncol(OpnCoe$coe)
    # if (nc > 6) nc <- 6
    # col.eg <- 1:nc
    #
    # print(round(OpnCoe$coe[row.eg, col.eg], 3))
    # cat("etc.\n")
  } else {
    # we print the baselines
    if (!is.null(c(x$baseline1, x$baseline2))) {
      cat(" - $baseline1: (", paste(x$baseline1, collapse="; "), "), ", sep="")
      cat("$baseline2: (", paste(x$baseline2, collapse="; "), ")\n", sep="")
    }
  }
  #   if (p != 3) {
  #     # r2 quick summary
  #     r2  <- OpnCoe$r2
  #     cat(" - $r2: min=", signif(min(r2), 3),
  #         ", median=",    signif(median(r2), 3),
  #         ", mean=",      signif(mean(r2), 3),
  #         ", sd=",        signif(mean(r2), 3),
  #         ", max=",       signif(max(r2), 3), "\n", sep="")}
  # we print the fac
  .print_fac(OpnCoe$fac)
}

###### end Opn
