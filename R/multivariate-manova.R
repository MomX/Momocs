##### Manova methods --------------------------------------

# would need a good review. # todo

#' Multivariate analysis of variance on Coe objects
#'
#' Performs multivariate analysis of variance on \link{Coe} objects.
#'
#' For outlines, checks if the matrix of coefficients is of full rank, and if not removes the
#' higher order harmonics (for Out objects).
#'
#' If OutCoe objects have been normalized, the first harmonic will be removed with a message.
#'
#' If \link{removeAsymmetric} or \link{removeSymmetric}
#' have been used on OutCoe object, the zero-ed harmonics will be removed with a message.
#' @aliases Manova
#' @rdname Manova
#' @param x a \link{Coe} object
#' @param fac a name of a colum in the \code{$fac} slot, or its id
#' @param test a test for \link{manova} (\code{'Hotelling'} by default)
#' @param retain how many harmonics (or polynomials) to retain
#' @param drop how many harmonics (or polynomials) to drop
#' @param verbose logical whether to print messages
#' @return a list of matrices of (x,y) coordinates.
#' @keywords Multivariate
#' @seealso \link{ManovaPW}
#' @note Needs a review and should be considered as experimental.
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' Manova(bot.f, 'type')
#'
#' data(olea)
#' op <- rawPolynomials(olea, 5)
#' Manova(op, 'domes')
#' @export
Manova <- function(x, fac, test = "Hotelling", retain, drop,
                   verbose) {
  UseMethod("Manova")
}

#' @rdname Manova
#' @export
Manova.OpnCoe <- function(x, fac, test = "Hotelling", retain,
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
    cat(" * Manova done on:", colnames(x)[keep], "\n")
  mod <- summary(manova(x[, keep] ~ fac), test = test)
  return(mod)
}

#' @rdname Manova
#' @export
Manova.OutCoe <- function(x, fac, test = "Hotelling", retain,
                          drop, verbose = TRUE) {
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
  if (OutCoe$method == "eFourier") {
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
    cph <- ifelse(OutCoe$method == "eFourier", 4, 2)
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

  harm.sel <- coeff.sel(retain = retain, drop = drop, nb.h = nb.h,
                        cph = cph)
  # cat(retain, drop, nb.h, cph)
  if (verbose)
    cat("\n")
  mod <- summary(manova(x[, harm.sel] ~ fac), test = test)
  return(mod)
}

# Manova PW -----------------------------------------------

#' Pairwise Multivariate analyses of variance
#'
#' A wrapper for pairwise \link{Manova}s on \link{Coe} objects. Calculates a Manova for every
#' pairwise combination of the factor provided.
#' @param x a \link{Coe} or a \link{PCA} object
#' @param fac a name (or its id) of a grouping factor in \code{$fac} or a factor
#' @param verbose to feed \link{Manova}
#' @param retain the number of PCaxis to retain
#' @param ... more arguments to feed \link{Manova}
#' @note Needs a review and should be considered as experimental.
#' If the fac passed has only two levels, there is only pair and it is
#' equivalent to \link{Manova}. \code{ManovaPW.PCA} works with the regular \link{manova}.
#' @seealso \link{Manova}, \link{manova}.
#' @keywords Multivariate
#' @return a list with the following components is returned (invisibly because $manovas
#' may be very long, see examples):
#' \itemize{
#'  \item manovas a list containing all the raw manovas
#'  \item summary a matrix with all important statists
#'  \item stars.tab a table with 'significance star', discutable but useful:
#'  '***' if Pr(>F) < 0.001; '**' of < 0.01; '*' if < 0.05; '.' if < 0.10 and '-' if above.
#' }
#' @examples
#' data(bot)
#' # we create a fake factor with 4 levels
#' bot$fac$fake <- factor(rep(letters[1:4], each=10))
#' bot.f <- eFourier(bot, 8)
#' ManovaPW(bot.f, 'fake') # or ManovaPW(bot.f, 2)
#'
#' # an example on open outlines
#' data(olea)
#' op <- rawPolynomials(olea)
#' ManovaPW(op, 'domes')
#' # to get the results
#' res <- ManovaPW(op, 'domes')
#' res$manovas
#' res$stars.tab
#' res$summary
#' @rdname ManovaPW
#' @export
ManovaPW <- function(x, fac, verbose, ...) {
  UseMethod("ManovaPW")
}
#' @rdname ManovaPW
#' @export
ManovaPW.Coe <- function(x, fac, verbose = FALSE, ...) {
  # preliminaries
  Coe <- x
  fac0 <- fac
  if (length(Coe$method) > 1)
    stop(" * cannot yet be used on combined OutCoe. Do it manually.")
  if (missing(fac))
    stop("'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- Coe$fac[, fac]
  }
  # we get all combinations, and prepare the loop
  pws <- t(combn(levels(fac), 2))
  n <- nrow(pws)
  cn <- colnames(Manova(Coe, fac, verbose = FALSE)$stats)
  res <- matrix(NA, nrow = n, ncol = 6,
                dimnames = list(paste(pws[, 1], pws[, 2], sep = " - "), cn))
  manovas <- list()
  # we loop and do all the Manovas
  for (i in 1:nrow(pws)) {
    if (verbose)
      cat(pws[i, ], "\n")
    Coe.i <- subset(Coe, fac == pws[i, ])
    m <- Manova(Coe.i, fac0, verbose = verbose, ...)
    manovas[[i]] <- m
    res[i, ] <- m$stats[1, ]
  }
  names(manovas) <- rownames(res)
  # we prepare a 'signifance' table, with 'significant' stars
  # (discutable but helpful) see stats:::print.summary.lm
  stars <- symnum(res[, 6], corr = FALSE, na = FALSE, cutpoints = c(0,
                                                                    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**",
                                                                                                            "*", ".", "-"))
  stars <- as.character(stars)
  nl <- nlevels(fac)
  # bloody ugly
  stars.tab <- matrix(NA, nrow = nl - 1, ncol = nl, dimnames = list(levels(fac)[-nl],
                                                                    levels(fac)))
  stars.tab <- as.table(stars.tab)
  k <- 1
  for (i in 1:(nl - 1)) {
    for (j in (i + 1):nl) {
      stars.tab[i, j] <- stars[k]
      k <- k + 1
    }
  }
  cat("$stars.tab\n")
  print(stars.tab)
  cat("\n$summary (see also $manovas)\n")
  print(res)
  invisible(list(manovas = manovas, summary = res, stars.tab = stars.tab))
}

#' @rdname ManovaPW
#' @export
ManovaPW.PCA <- function(x, fac, verbose = FALSE, retain=1:5,...) {
  # preliminaries
  PCA <- x
  x <- PCA$x[, retain]
  cat(" * PC axis", retain, "were retained\n")
  fac0 <- fac
  if (missing(fac))
    stop("'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- factor(PCA$fac[, fac])
  }
  # we get all combinations, and prepare the loop
  pws <- t(combn(levels(fac), 2))
  n <- nrow(pws)
  cn <- colnames(summary(manova(x~ fac))$stats)
  res <- matrix(NA, nrow = n, ncol = 6,
                dimnames = list(paste(pws[, 1], pws[, 2], sep = " - "), cn))
  manovas <- list()
  # we loop and do all the Manovas
  for (i in 1:nrow(pws)) {
    x.i   <-   x[fac == pws[i, ], ]
    fac.i <- factor(fac[fac == pws[i, ]])
    if (nlevels(fac.i)>1){
      m <- summary(manova(x.i ~ fac.i))
      manovas[[i]] <- m
      res[i, ] <- m$stats[1, ]}
    if (verbose) cat(pws[i, ], "\n")
  }
  names(manovas) <- rownames(res)
  # we prepare a 'signifance' table, with 'significant' stars
  # (discutable but helpful) see stats:::print.summary.lm
  stars <- symnum(res[, 6], corr = FALSE, na = "?",
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", "-"))
  stars <- as.character(stars)
  nl <- nlevels(fac)
  # bloody ugly
  stars.tab <- matrix(NA, nrow = nl - 1, ncol = nl,
                      dimnames = list(levels(fac)[-nl], levels(fac)))
  stars.tab <- as.table(stars.tab)
  k <- 1
  for (i in 1:(nl - 1)) {
    for (j in (i + 1):nl) {
      stars.tab[i, j] <- stars[k]
      k <- k + 1
    }
  }
  cat("$stars.tab\n")
  print(stars.tab)
  cat("\n$summary (see also $manovas)\n")
  print(res)
  invisible(list(manovas = manovas, summary = res, stars.tab = stars.tab))
}


# todo #' @rdname Manova #' @export Manova.LdkCoe <-
# function(x, fac, test='Hotelling', retain, drop){ LdkCoe <-
# x if (length(LdkCoe$method)>1) stop(' * cannot yet be used
# on combined OutCoe. Do it manually.') if (missing(fac))
# stop(' * 'fac' must be provided') if (!is.factor(fac)) {fac
# <- LdkCoe$fac[, fac]} x <- LdkCoe$coe mod <-
# summary(manova(x~fac), test=test) return(mod)}

##### end Manova
