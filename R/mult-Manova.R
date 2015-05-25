##### MANOVA methods --------------------------------------

# would need a good review. # todo

#' Multivariate analysis of variance on Coe objects
#'
#' Performs multivariate analysis of variance on \link{PCA} objects.
#' 
#' Performs a MANOVA on PC scores. Just a wrapper around \link{manova}. See examples for multifactorial manova and 
#' \link{summary.manova} for more details and examples.
#'
#' @aliases MANOVA
#' @rdname MANOVA
#' @param x a \link{Coe} object
#' @param fac a name of a colum in the \code{$fac} slot, or its id
#' @param test a test for \link{manova} (\code{'Hotelling'} by default)
#' @param retain how many harmonics (or polynomials) to retain, for PCA 
#' the highest number of PC axis to retain, or the proportion of the variance to capture.
#' @param drop how many harmonics (or polynomials) to drop
#' @param verbose logical whether to print messages
#' @return a list of matrices of (x,y) coordinates.
#' @keywords Multivariate
#' @seealso \link{MANOVA_PW}
#' @note Needs a review and should be considered as experimental.
#' @examples
#' data(bot)
#' bot.p <- PCA(efourier(bot, 12))
#' MANOVA(bot.p, 'type')
#'
#' data(olea)
#' op <- PCA(npoly(olea, 5))
#' MANOVA(op, 'domes')
#' 
#'  m <- manova(op$x[, 1:5] ~  op$fac$domes * op$fac$var)
#'  summary(m)
#'  summary.aov(m)
#' @export
MANOVA <- function(x, fac, test = "Hotelling", retain, drop,
                   verbose) {
  UseMethod("MANOVA")
}

#' @rdname MANOVA
#' @export
MANOVA.OpnCoe <- function(x, fac, test = "Hotelling", retain,
                          drop, verbose = TRUE) {
  OpnCoe <- x
  if (length(OpnCoe$method) > 1)
    stop(" * cannot yet be used on combined OutCoe. Do it manually.")
  if (missing(fac))
    stop(" * 'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- OpnCoe$fac[, fac]
  }
  x <- OpnCoe$coe
  if (missing(drop))
    drop <- 0
  if (missing(retain))
    retain <- ncol(x)
  keep <- (drop + 1):retain
  if (verbose)
    cat(" * MANOVA done on:", colnames(x)[keep], "\n")
  mod <- summary(manova(x[, keep] ~ fac), test = test)
  return(mod)
}

#' @rdname MANOVA
#' @export
MANOVA.OutCoe <- function(x, fac, test = "Hotelling", retain, drop, verbose = TRUE) {
  OutCoe <- x
  if (length(OutCoe$method) > 1)
    stop(" * cannot yet be used on combined OutCoe. Do it manually.")
  if (missing(fac))
    stop("'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- OutCoe$fac[, fac]
  }
  x <- OutCoe$coe
  cph <- NULL
  # we check for (a)symetrization
  if (OutCoe$method == "efourier") {
    nb.h <- ncol(x)/4
    BC <- (nb.h + 1):(nb.h * 3)
    AD <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
    if (sum(x[, BC]) == 0) {
      x <- x[, -BC]
      cph <- 2
      if (verbose)
        cat(" * B and C harmonics removed (because of removeAsymetric)\n")
    } else {
      if (sum(x[, AD]) == 0) {
        x <- x[, -AD]
        cph <- 2
        if (verbose)
          cat(" * A and D harmonics removed (because of removeSymetric)\n")
      }
    }
  }
  # we remove normalized harmonics (if any)
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      if (verbose)
        cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0
    }
  }
  
  if (is.null(cph)) {
    cph <- ifelse(OutCoe$method == "efourier", 4, 2)
  }
  nb.h <- ncol(x)/cph
  fr <- nrow(x) - nlevels(fac)
  max.h <- floor(fr/cph)
  if (missing(retain)) {
    retain <- max.h + drop
    if (retain > nb.h) {
      retain <- nb.h
    }
    if (verbose) {
      cat(" * 'retain' was missing. MANOVA done with",
          retain, "harmonics ")
      if (drop > 0) {
        cat("and the first", drop, "dropped\n")
      } else {
        cat("\n")
      }
    }
  } else {
    if ((retain - drop) > max.h) {
      retain <- max.h + drop
      if (retain > nb.h) {
        retain <- nb.h
      }
      if (verbose)
        cat(" * 'retain' was too ambitious. MANOVA done with",
            retain, "harmonics ")
      if (verbose) {
        if (drop > 0) {
          cat("and the first", drop, "dropped\n")
        } else {
          cat("\n")
        }
      }
    }
  }
  
  harm.sel <- coeff_sel(retain = retain, drop = drop, nb.h = nb.h,
                        cph = cph)
  # cat(retain, drop, nb.h, cph)
  if (verbose)
    cat("\n")
  mod <- summary(manova(x[, harm.sel] ~ fac), test = test)
  return(mod)
}

#' @rdname MANOVA
#' @export
MANOVA.PCA <- function(x, fac, test = "Hotelling", retain=0.99, drop, verbose = TRUE) {
  if (missing(fac))
    stop(" * 'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- x$fac[, fac]
  }
  # we grab the PCs to capture retain% of the variance
  if (retain<1){
    vars <- x$sdev^2
    retain <- min(which((cumsum(vars)/sum(vars))>retain))
  }  
  if (retain>nrow(x$x))
    retain <- nrow(x$x)
  cat(" * PC axes 1 to", retain, "were retained.\n")
  retain <- 1:retain
  x <- x$x[, retain]
  mod <- summary(manova(x ~ fac), test = test)
  return(mod)
}
