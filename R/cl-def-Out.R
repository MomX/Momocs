# 1. Out builders ---------------------------------------------

#' Builds an Out object
#'
#' In Momocs, \code{Out}-classes objects are lists of closed \bold{out}lines,
#' with optionnal components, and on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. \link{efourier} can be applied.
#'  \code{Out} objects are primarily \code{\link{Coo}} objects.
#'
#' @param x a \code{list} of matrices of \eqn{(x; y)} coordinates,
#' or an array or an Out object or an Ldk object
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @param ldk (optionnal) \code{list} of landmarks as row number indices
#' @return an \code{Out} object
#' @seealso \link{Coo}, \link{Opn}, link{Ldk}.
#' @aliases Out
#' @examples
#' methods(class=Out)
#' @export
Out <- function(x, fac = data.frame, ldk = list()) {
  UseMethod("Out")
}

#' @export
Out.default <- function(x, fac = data.frame(), ldk = list()) {
  cat(" * an Out object can only be built from a list, an array or a Coo object")
}

#' @export
Out.list <- function(x, fac = data.frame(), ldk = list()) {
  Out <- structure(list(coo = x, fac = fac, ldk = ldk), class=c("Out", "Coo"))
  if (!is.null(Out$fac))
    Out$fac <- as.data.frame(Out$fac, stringsAsFactors = FALSE)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

#' @export
Out.array <- function(x, fac = data.frame(), ldk = list()) {
  x <- a2l(x)
  Out <- Out(x, fac = fac, ldk = ldk)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

#' @export
Out.Coo <- function(x, fac = data.frame(), ldk = list()) {
  Out <- Out(x = x$coo, fac = x$fac, ldk = x$ldk)
  if (is.null(names(Out))) names(Out) <- paste0("shp", 1:length(Out))
  return(Out)
}

#' Convert an OutCoe object into an Out object
#'
#' Uses the \code{$method} to do the inverse corresponding function. For instance,
#' an \link{OutCoe} object obtained with \link{efourier}, will be converted to an \link{Out}
#' object (outlines from harmonic coefficients), using \link{efourier_i}.
#'
#' Note that the 'positionnal' coefficients (\code{ao} and \code{co} if any) are lost, so for a proper
#' comparison between a raw \code{Out} and a \code{Out} from \code{Out -> OutCoe -> Out},
#' the raw \code{Out} should be centered.
#'
#' This method is useful since it allows a direct inspection at how Fourier-based
#' methods handle outlines, and in particular how they normalize it (when they do). If you
#' have bad "reconstruction" using \code{as.Out}, this probably means that you have to think
#' about alternative alignements on the raw outlines. For instance, it is obvious
#' that normalization does a good job on the bottle example, yet it -pi/2 turns the "outlines"
#' yet neutral for further analysis (and that can be manage with the argument \code{rotate.shp} in
#' functions/methods that use reconstructed outlines, e.g. \link{plot.PCA}).
#' @param object an OutCoe object
#' @param OutCoe used by \code{as}, useless for the front user
#' @param nb.pts number of point for the reconstructed outlines
#' @return an \link{Out} object.
#' @examples
#' data(bot)
#' bot <- coo_center(bot)
#' bot.f <- rfourier(bot, 120)
#' bot.fi <- as.Out(bot.f)
#' op <- par(mfrow=c(1, 2))
#' stack(bot, title="raw bot")
#' stack(bot.fi, title="outlines from bot.f")
#' par(op)
#' @export
as.Out <- function(object, OutCoe, nb.pts=120){
  # we swith among methods, with a messsage
  method <- object$method
  if (is.null(method)) {
    stop(" * '$method' is missing. Not a regular Coe object.")
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
    method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)
    cph      <- switch(p, 4, 2, 2)
  }
  coe <- object$coe
  nb.h <- ncol(coe)/cph
  coo <- list()
  for (i in 1:nrow(coe)){
    ef.i <- coeff_split(coe[i, ], nb.h = nb.h, cph = cph)
    coo[[i]] <- method.i(ef.i, nb.pts = nb.pts)}
  names(coo) <- rownames(coe)
  return(Out(coo, fac=object$fac))}


# The print method for Out objects
#' @export
print.Out <- function(x, ...) {
  Out <- x
  ### Header
  cat("An Out object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- length(Out)
  coo_len <- sapply(Out$coo, nrow)
  coo_closed <- sapply(Out$coo, is_closed)
  #     # one random outline
  #     eg <- sample(length(Out$coo), 1)
  #     coo_eg <- Out$coo[[eg]]
  #     colnames(coo_eg) <- c("x", "y")
  #     cat(" - One random outline in $coo: '", names(Out$coo)[eg],
  #         "':\n", sep = "")
  #     if (nrow(coo_eg) > 5) {
  #       print(coo_eg[1:5, ], print.gap = 2)
  #       cat("etc.\n")
  #     } else {
  #       print(coo_eg, print.gap = 2)
  #       cat("\n\n")
  #     }
  # number of outlines
  cat(" - $coo:", coo_nb, "outlines")

  # number of coordinates
  cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates, ", sep="")
  # outlines closed or not
  if (all(coo_closed)) {
    cat("all closed)\n")
  } else {
    if (any(!coo_closed)) {
      cat("all unclosed)\n")
    } else {
      cat(sum(coo_closed), " closed\n")
    }
  }
  # number of landmarks
  if (length(Out$ldk) != 0) {
    cat(" - $ldk:", length(Out$ldk[[1]]), "landmark(s) defined\n")
  } else {
    #cat(" - No landmark defined\n")
  }
  # we print the fac
  .print.fac(Out$fac)
}



#  3. OutCoe builders -----------------------------------------------
#' Builds an OutCoe object
#'
#' In Momocs, \code{OutCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot})
#' and specific methods can be applied.
#'  \code{OutCoe} objects are primarily \code{\link{Coe}} objects.
#'
#' @param coe \code{matrix} of harmonic coefficients
#' @param fac (optionnal) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param norm the normalisation used to obtain these coefficients
#' @return an \code{OutCoe} object
#' @details These methods can be applied on \code{Out} objects:
#' @seealso \link{Coe}, \link{OpnCoe}
#' @examples
#' # all OutCoe methods
#' methods(class='OutCoe')
#' @export
OutCoe <- function(coe = matrix(), fac = data.frame(), method,
                   norm) {
  if (missing(method))
    stop("a method must be provided to OutCoe")
  OutCoe <- structure(list(coe = coe, fac = fac, method = method, norm = norm), class=c("OutCoe", "Coe"))
  return(OutCoe)
}

##### TO FIX FOR Combined OutCoe The print method for Out objects
#' @export
print.OutCoe <- function(x, ...) {
  OutCoe <- x
  if (length(OutCoe$method)>1) {
    met <- c("combined:", paste0(OutCoe$method, collapse=" + "))
    met <- c(met, "analyses ]\n")
    combined <- TRUE
  } else {
    p <- pmatch(OutCoe$method[1], c("efourier", "rfourier", "tfourier"))
    met <- switch(p, "elliptical Fourier", "radii variation", "tangent angle")
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
    # lets show some of them for a quick inspection
    cat(" - $coe: 1st harmonic coefficients from random individuals: \n")
    row.eg <- sort(sample(coo_nb, ifelse(coo_nb < 5, coo_nb, 5), replace = FALSE))
    col.eg <- coeff_sel(retain = ifelse(harm.nb > 3, 3, harm.nb), drop = 0, nb.h = harm.nb, cph = ifelse(p == 1, 4, 2))
    print(round(OutCoe$coe[row.eg, col.eg], 3))
    cat("etc.\n")
  } else {
    harm.nb <- ncol(OutCoe$coe)
    # number of outlines and harmonics
    cat(" - $coe:", coo_nb, "outlines described, and (total) ")
    cat(harm.nb, "coefficients\n")
    cat(" - $coe: harmonic coefficients\n")
  }
  # we print the fac
  .print.fac(OutCoe$fac)
}


# 5. Out + landmarks --------------------------------------

#' Define landmarks on Out and Opn objects
#'
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @examples
#' \dontrun{
#' data(bot)
#' bot <- bot[1:5] # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by def_ldk...
#' bot2 <- def_ldk(bot, 3)
#' stack(bot2)
#' bot2$ldk
#' }
#' @export
def_ldk <- function(Coo, nb.ldk) {
  UseMethod("def_ldk")
}
#' @export
def_ldk.Out <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop(" * 'nb.ldk' must be specified.")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    Coo$ldk[[i]] <- coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk)
  }
  return(Coo)
}
#' @export
def_ldk.Opn <- def_ldk.Out

#' Retrieve landmarks coordinates from Opn and Out objects
#'
#' In \link{Out} and \link{Opn} classes, landmarks (if any) are stored as
#' row indices. This methods allows to retrieve the corresponding (x; y) coordinates.
#' @param Coo a Coo object, either Out or Opn
#' @return an array of coordinates X (x; y) coordinates X number of shapes.
#' @seealso \link{def_ldk}, \link{fgProcrustes}
#' @examples
#' data(hearts)
#' ldk.h <- get_ldk(hearts)
#' stack(Ldk(a2l(ldk.h)))
#' ldk.h
#' @export
get_ldk <- function(Coo) {
  UseMethod("get_ldk")
}
#' @export
get_ldk.Out <- function(Coo) {
  coo <- Coo$coo
  ldk <- Coo$ldk
  ref <- array(NA, dim = c(length(ldk[[1]]), ncol(coo[[1]]),
                           length(coo)))
  for (i in seq(along = coo)) {
    ref[, , i] <- coo[[i]][ldk[[i]], ]
  }
  return(ref)
}
#' @export
get_ldk.Opn <- get_ldk.Out


# 6. Out symmetry --------------------------------------------

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
#' @seealso \link{rm_Asym} and \link{rm_Sym}.
#' @examples
#' data(bot)
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
    stop(" * Can only be applied on OutCoe [efourier] objects.")
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
#' \code{rm_Asym} sets all B and C coefficients to 0; \code{rm_Sym} sets
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
#' @seealso \link{symmetry}.
#' @examples
#' data(bot)
#' botf <- efourier(bot, 12)
#' botSym <- rm_Asym(botf)
#' boxplot(botSym)
#' botSymp <- PCA(botSym)
#' plot(botSymp)
#' plot(botSymp, amp.shp=5)
#'
#' # Asymmetric only
#' botAsym <- rm_Sym(botf)
#' boxplot(botAsym)
#' botAsymp <- PCA(botAsym)
#' plot(botAsymp)
#' # strange shapes because the original shape was mainly symmetric and would need its
#' # symmetric (eg its average) for a proper reconstruction. Should only be used like that:
#' plot(botAsymp, morpho=FALSE)
#' @rdname rm_Asym
#' @aliases rm_Sym
#' @export
rm_Asym <- function(OutCoe) {
  UseMethod("rm_Asym")
}
#' @rdname rm_Asym
#' @export
rm_Asym.default <- function(OutCoe) {
  cat(" * Can only be applied on OutCoe objects.")
}
#' @rdname rm_Asym
#' @export
rm_Asym.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "efourier")
    stop(" * Can only be applied on OutCoe [efourier] objects.")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- (nb.h + 1):(nb.h * 3)
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

#' @rdname rm_Asym
#' @export
rm_Sym <- function(OutCoe) {
  UseMethod("rm_Sym")
}
#' @rdname rm_Asym
#' @export
rm_Sym.default <- function(OutCoe) {
  cat(" * Can only be applied on OutCoe objects.")
}
#' @rdname rm_Asym
#' @export
rm_Sym.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "efourier")
    stop(" * Can only be applied on OutCoe [efourier] objects.")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

