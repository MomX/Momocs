# Out ---------------

#' Builds an Out object
#'
#' In Momocs, \code{Out}-classes objects are lists of closed \bold{out}lines,
#' with optional components, and on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. \link{efourier} can be applied.
#'  \code{Out} objects are primarily \code{\link{Coo}} objects.
#'
#' @param x a \code{list} of matrices of \eqn{(x; y)} coordinates,
#' or an array or an Out object or an Ldk object
#' @param fac (optional) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @param ldk (optional) \code{list} of landmarks as row number indices
#' @return an \code{Out} object
#' @family classes
#' @aliases Out
#' @examples
#' methods(class=Out)
#' @export
Out <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  UseMethod("Out")
}

#' @export
Out.default <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  if (is_shp(x))
    Out(list(x))
  else
    message("an Out object can only be build from a shape, a list, an array or a Coo object")
}

#' @export
Out.list <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  Out <- structure(list(coo = x, fac = fac, ldk = ldk), class=c("Out", "Coo"))
  if (!is.null(Out$fac))
    Out$fac <- dplyr::as_data_frame(Out$fac, stringsAsFactors = FALSE)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

#' @export
Out.array <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  x <- a2l(x)
  Out <- Out(x, fac = fac, ldk = ldk)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

#' @export
Out.Coo <- function(x, fac = dplyr::data_frame(), ldk = list()) {
  Out <- Out(x = x$coo, fac = x$fac, ldk = x$ldk)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

# #' Convert an OutCoe object into an Out object
# #'
# #' Uses the \code{$method} to do the inverse corresponding function. For instance,
# #' an \link{OutCoe} object obtained with \link{efourier}, will be converted to an \link{Out}
# #' object (outlines from harmonic coefficients), using \link{efourier_i}.
# #'
# #' Note that the 'positionnal' coefficients (\code{ao} and \code{co} if any) are lost, so for a proper
# #' comparison between a raw \code{Out} and a \code{Out} from \code{Out -> OutCoe -> Out},
# #' the raw \code{Out} should be centered.
# #'
# #' This method is useful since it allows a direct inspection at how Fourier-based
# #' methods handle outlines, and in particular how they normalize it (when they do). If you
# #' have bad "reconstruction" using \code{as_Out}, this probably means that you have to think
# #' about alternative alignements on the raw outlines. For instance, it is obvious
# #' that normalization does a good job on the bottle example, yet it -pi/2 turns the "outlines"
# #' yet neutral for further analysis (and that can be manage with the argument \code{rotate.shp} in
# #' functions/methods that use reconstructed outlines, e.g. \link{plot.PCA}).
# #' @param object an OutCoe object
# #' @param OutCoe used by \code{as}, useless for the front user
# #' @param nb.pts number of point for the reconstructed outlines
# #' @return an \link{Out} object.
# #' @examples
# #' bot <- coo_center(bot)
# #' bot.f <- rfourier(bot, 120)
# #' bot.fi <- as_Out(bot.f)
# #' op <- par(mfrow=c(1, 2))
# #' stack(bot, title="raw bot")
# #' stack(bot.fi, title="outlines from bot.f")
# #' par(op)
# #' @export
# as_Out <- function(object, OutCoe, nb.pts=120){
#   # we swith among methods, with a messsage
#   method <- object$method
#   if (is.null(method)) {
#     stop("'$method' is missing. Not a regular Coe object")
#   } else {
#     p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
#     method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)
#     cph      <- switch(p, 4, 2, 2)
#   }
#   coe <- object$coe
#   nb.h <- ncol(coe)/cph
#   coo <- list()
#   for (i in 1:nrow(coe)){
#     ef.i <- coeff_split(coe[i, ], nb.h = nb.h, cph = cph)
#     coo[[i]] <- method.i(ef.i, nb.pts = nb.pts)}
#   names(coo) <- rownames(coe)
#   return(Out(coo, fac=object$fac))}


# # The print method for Out objects
# #' @export
# print.Out <- function(x, ...) {
#   Out <- verify(x)
#   coo_nb <- length(Out)
#   if (coo_nb==0){
#     cat("An empty Out object")
#     return()
#   }
#   ### Header
#   cat("An Out object with: \n")
#   coo_len <- sapply(Out$coo, nrow)
#   coo_closed <- sapply(Out$coo, coo_is_closed)
#   # number of outlines
#   cat(" - $coo:", coo_nb, "outlines")
#
#   # number of coordinates
#   cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates, ", sep="")
#   # outlines closed or not
#   if (all(coo_closed)) {
#     cat("all closed)\n")
#   } else {
#     if (any(!coo_closed)) {
#       cat("all unclosed)\n")
#     } else {
#       cat(sum(coo_closed), " closed\n")
#     }
#   }
#   # number of landmarks
#   if (length(Out$ldk) != 0) {
#     cat(" - $ldk:", length(Out$ldk[[1]]), "landmark(s) defined\n")
#   } else {
#     #cat(" - No landmark defined\n")
#   }
#   # we print the fac
#   .print.fac(Out$fac)
# }



#  OutCoe -----------------------------------------------
#' Builds an OutCoe object
#'
#' In Momocs, \code{OutCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot})
#' and specific methods can be applied.
#'  \code{OutCoe} objects are primarily \code{\link{Coe}} objects.
#'
#' @param coe \code{matrix} of harmonic coefficients
#' @param fac (optional) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param norm the normalisation used to obtain these coefficients
#' @return an \code{OutCoe} object
#' @details These methods can be applied on \code{Out} objects:
#' @family classes
#' @examples
#' # all OutCoe methods
#' methods(class='OutCoe')
#' @export
OutCoe <- function(coe = matrix(), fac = dplyr::data_frame(), method,
                   norm) {
  if (missing(method))
    stop("a method must be provided to OutCoe")
  OutCoe <- structure(list(coe = coe, fac = fac, method = method, norm = norm),
                      class=c("OutCoe", "Coe"))
  OutCoe$coe %<>% as.matrix()
  return(OutCoe)
}


#' @export
print.OutCoe <- function(x, ...) {
  OutCoe <- x
  if (length(OutCoe$method)>1) {
    met <- c("combined:", paste0(OutCoe$method, collapse=" + "))
    met <- c(met, "analyses ]\n")
    combined <- TRUE
  } else {
    p <- pmatch(OutCoe$method[1], c("efourier", "rfourier", "sfourier", "tfourier"))
    met <- switch(p, "elliptical Fourier", "radii variation (equally spaced radii)",
                  "radii variation (equally spaced curvilinear abscissa)", "tangent angle")
    met <- c(met, "analysis ]\n")
    combined <- FALSE}
  ### Header
  cat("An OutCoe object [", met)
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- nrow(OutCoe$coe)  #nrow method ?
  if (!combined){
    harm.nb <- ncol(OutCoe$coe)/ifelse(p == 1, 4, 2)
    # number of outlines and harmonics
    cat(" - $coe:", coo_nb, "outlines described, ")
    cat(harm.nb, "harmonics\n")
    # lets show some of them for a quick inspection.
    # boring, removed it
    # cat(" - $coe: 1st harmonic coefficients from random individuals: \n")
    # row.eg <- sort(sample(coo_nb, ifelse(coo_nb < 5, coo_nb, 5), replace = FALSE))
    # col.eg <- coeff_sel(retain = ifelse(harm.nb > 3, 3, harm.nb), drop = 0, nb.h = harm.nb, cph = ifelse(p == 1, 4, 2))
    # print(round(OutCoe$coe[row.eg, col.eg], 3))
    # cat("etc.\n")
  } else {
    harm.nb <- ncol(OutCoe$coe)
    # number of outlines and harmonics
    cat(" - $coe:", coo_nb, "outlines described, and (total) ")
    cat(harm.nb, "coefficients\n")
    #cat(" - $coe: harmonic coefficients\n")
  }
  # we print the fac
  .print_fac(OutCoe$fac)
}


# Out symmetry methods ---------

#' Calcuates symmetry indices on OutCoe objects
#'
#' For \link{OutCoe} objects obtained with \link{efourier}, calculates several
#' indices on the matrix of coefficients: \code{AD}, the sum of absolute values of
#' harmonic coefficients A and D; \code{BC} same thing for B and C; \code{amp} the
#' sum of the absolute value of all harmonic coefficients and \code{sym} which is the ratio
#' of \code{AD} over \code{amp}. See references below for more details.
#' @param OutCoe [efourier] objects
#' @return a matrix with 4 colums described above.
#' @references Below: the first mention, and two applications.
#' \itemize{
#' #' \item Iwata, H., Niikura, S., Matsuura, S., Takano, Y., & Ukai, Y. (1998).
#' Evaluation of variation of root shape of Japanese radish (Raphanus sativus L.)
#' based on image analysis using elliptic Fourier descriptors. Euphytica, 102, 143-149.
#' \item Iwata, H., Nesumi, H., Ninomiya, S., Takano, Y., & Ukai, Y. (2002).
#' The Evaluation of Genotype x Environment Interactions of Citrus Leaf Morphology
#' Using Image Analysis and Elliptic Fourier Descriptors. Breeding Science, 52(2),
#' 89-94. doi:10.1270/jsbbs.52.89
#' \item Yoshioka, Y., Iwata, H., Ohsawa, R., & Ninomiya, S. (2004).
#' Analysis of petal shape variation of Primula sieboldii by elliptic fourier descriptors
#' and principal component analysis. Annals of Botany, 94(5), 657-64. doi:10.1093/aob/mch190
#' }
#' @note What we call symmetry here is bilateral symmetry.
#' By comparing coefficients resulting from \link{efourier},
#' with AD responsible for amplitude of the Fourier functions,
#'  and BC for their phase, it results in the plane and for
#'  fitted/reconstructed shapes that symmetry. As long as your shapes are
#'   aligned along their bilateral symmetry axis, you can use the approach
#'   coined by Iwata et al., and here implemented in Momocs.
#' @seealso \link{rm_asym} and \link{rm_sym}.
#' @examples
#' bot.f <- efourier(bot, 12)
#' res <- symmetry(bot.f)
#' hist(res[, 'sym'])
#' @export
symmetry <- function(OutCoe) {
  UseMethod("symmetry")
}
#' @export
symmetry.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "efourier")
    stop("can only be applied on OutCoe [efourier] objects")
  x <- OutCoe$coe
  nb.h <- ncol(x)/4
  AD.ids <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
  BC.ids <- (nb.h + 1):(nb.h * 3)
  AD <- apply(abs(x[, AD.ids]), 1, sum)
  BC <- apply(abs(x[, BC.ids]), 1, sum)
  amp <- apply(abs(x), 1, sum)
  sym <- AD/amp
  res <- cbind(AD, BC, amp, sym)
  return(res)
}


#' Removes asymmetric and symmetric variation on OutCoe objects
#'
#' Only for those obtained with \link{efourier}, otherwise a message is returned.
#' \code{rm_asym} sets all B and C coefficients to 0; \code{rm_sym} sets
#' all A and D coefficients to 0.
#' @param OutCoe an OutCoe object
#' @return an OutCoe object
#' @references Below: the first mention, and two applications.
#' \itemize{
#' #' \item Iwata, H., Niikura, S., Matsuura, S., Takano, Y., & Ukai, Y. (1998).
#' Evaluation of variation of root shape of Japanese radish (Raphanus sativus L.)
#' based on image analysis using elliptic Fourier descriptors. Euphytica, 102, 143-149.
#' \item Iwata, H., Nesumi, H., Ninomiya, S., Takano, Y., & Ukai, Y. (2002).
#' The Evaluation of Genotype x Environment Interactions of Citrus Leaf Morphology
#' Using Image Analysis and Elliptic Fourier Descriptors. Breeding Science, 52(2),
#' 89-94. doi:10.1270/jsbbs.52.89
#' \item Yoshioka, Y., Iwata, H., Ohsawa, R., & Ninomiya, S. (2004).
#' Analysis of petal shape variation of Primula sieboldii by elliptic fourier descriptors
#' and principal component analysis. Annals of Botany, 94(5), 657-64. doi:10.1093/aob/mch190
#' }
#' @seealso \link{symmetry} and the note there.
#' @examples
#' botf <- efourier(bot, 12)
#' botSym <- rm_asym(botf)
#' boxplot(botSym)
#' botSymp <- PCA(botSym)
#' plot(botSymp)
#' plot(botSymp, amp.shp=5)
#'
#' # Asymmetric only
#' botAsym <- rm_sym(botf)
#' boxplot(botAsym)
#' botAsymp <- PCA(botAsym)
#' plot(botAsymp)
#' # strange shapes because the original shape was mainly symmetric and would need its
#' # symmetric (eg its average) for a proper reconstruction. Should only be used like that:
#' plot(botAsymp, morpho=FALSE)
#' @rdname rm_asym
#' @aliases rm_sym
#' @export
rm_asym <- function(OutCoe) {
  UseMethod("rm_asym")
}
#' @rdname rm_asym
#' @export
rm_asym.default <- function(OutCoe) {
  cat("can only be applied on OutCoe objects")
}
#' @rdname rm_asym
#' @export
rm_asym.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "efourier")
    stop("can only be applied on OutCoe [efourier] objects")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- (nb.h + 1):(nb.h * 3)
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

#' @rdname rm_asym
#' @export
rm_sym <- function(OutCoe) {
  UseMethod("rm_sym")
}
#' @rdname rm_asym
#' @export
rm_sym.default <- function(OutCoe) {
  stop("can only be applied on OutCoe objects")
}
#' @rdname rm_asym
#' @export
rm_sym.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "efourier")
    stop("can only be applied on OutCoe [efourier] objects")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

