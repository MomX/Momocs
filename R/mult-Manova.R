##### MANOVA methods --------------------------------------

# would need a good review. # todo

#' Multivariate analysis of variance on Coe objects
#'
#' Performs multivariate analysis of variance on \link{PCA} objects and on \link{Coe} objects.
#'
#' For outlines, checks if the matrix of coefficients is of full rank, and if not removes the
#' higher order harmonics (for Out objects). If OutCoe objects have been normalized, the first harmonic will be removed with a message. If \link{rm_Asym} or \link{rm_Sym}
#' have been used on OutCoe object, the zero-ed harmonics will be removed with a message.
#' 
#' Overall it is probably a better idea to work on the PCA scores
#' (and it should ultimately be the same results).
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
#' bot.f <- efourier(bot, 12)
#' MANOVA(bot.f, 'type')
#'
#' data(olea)
#' op <- npoly(olea, 5)
#' MANOVA(op, 'domes')
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

# MANOVA PW -----------------------------------------------

#' Pairwise Multivariate analyses of variance
#'
#' A wrapper for pairwise \link{MANOVA}s on \link{Coe} objects. Calculates a MANOVA for every
#' pairwise combination of the factor provided.
#' @param x a \link{Coe} or a \link{PCA} object
#' @param fac a name (or its id) of a grouping factor in \code{$fac} or a factor
#' @param verbose to feed \link{MANOVA}
#' @param retain the number of PC axis to retain (1:retain) or the proportion of variance to capture (0.99 par default). 
#' @param ... more arguments to feed \link{MANOVA}
#' @note Needs a review and should be considered as experimental.
#' If the fac passed has only two levels, there is only pair and it is
#' equivalent to \link{MANOVA}. \code{MANOVA_PW.PCA} works with the regular \link{manova}.
#' @seealso \link{MANOVA}, \link{manova}.
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
#' bot.f <- efourier(bot, 8)
#' MANOVA_PW(bot.f, 'fake') # or MANOVA_PW(bot.f, 2)
#'
#' # an example on open outlines
#' data(olea)
#' op <- npoly(olea)
#' MANOVA_PW(op, 'domes')
#' # to get the results
#' res <- MANOVA_PW(op, 'domes')
#' res$manovas
#' res$stars.tab
#' res$summary
#' @rdname MANOVA_PW
#' @export
MANOVA_PW <- function(x, fac, verbose, ...) {
  UseMethod("MANOVA_PW")
}
# #' @rdname MANOVA_PW
# #' @export
# MANOVA_PW.Coe <- function(x, fac, verbose = FALSE, ...) {
#   # preliminaries
#   Coe <- x
#   fac0 <- fac
#   if (length(Coe$method) > 1)
#     stop(" * cannot yet be used on combined OutCoe. Do it manually.")
#   if (missing(fac))
#     stop("'fac' must be provided")
#   if (!is.factor(fac)) {
#     fac <- Coe$fac[, fac]
#   }
#   # we get all combinations, and prepare the loop
#   pws <- t(combn(levels(fac), 2))
#   n <- nrow(pws)
#   cn <- colnames(MANOVA(Coe, fac, verbose = FALSE)$stats)
#   res <- matrix(NA, nrow = n, ncol = 6,
#                 dimnames = list(paste(pws[, 1], pws[, 2], sep = " - "), cn))
#   manovas <- list()
#   # we loop and do all the MANOVAs
#   for (i in 1:nrow(pws)) {
#     if (verbose)
#       cat(pws[i, ], "\n")
#     Coe.i <- subset(Coe, fac == pws[i, ])
#     m <- MANOVA(Coe.i, fac0, verbose = verbose, ...)
#     manovas[[i]] <- m
#     res[i, ] <- m$stats[1, ]
#   }
#   names(manovas) <- rownames(res)
#   # we prepare a 'signifance' table, with 'significant' stars
#   # (discutable but helpful) see stats:::print.summary.lm
#   stars <- symnum(res[, 6], corr = FALSE, na = FALSE, cutpoints = c(0,
#                                                                     0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**",
#                                                                                                             "*", ".", "-"))
#   stars <- as.character(stars)
#   nl <- nlevels(fac)
#   # bloody ugly
#   stars.tab <- matrix(NA, nrow = nl - 1, ncol = nl, dimnames = list(levels(fac)[-nl],
#                                                                     levels(fac)))
#   stars.tab <- as.table(stars.tab)
#   k <- 1
#   for (i in 1:(nl - 1)) {
#     for (j in (i + 1):nl) {
#       stars.tab[i, j] <- stars[k]
#       k <- k + 1
#     }
#   }
#   cat("$stars.tab\n")
#   print(stars.tab)
#   cat("\n$summary (see also $manovas)\n")
#   print(res, digits=4)
#   invisible(list(manovas = manovas, summary = res, stars.tab = stars.tab))
# }

#' @rdname MANOVA_PW
#' @export
MANOVA_PW.PCA <- function(x, fac, verbose = FALSE, retain=0.99,...) {
  # preliminaries
  PCA <- x
  
  fac0 <- fac
  if (missing(fac))
    stop("'fac' must be provided")
  if (!is.factor(fac)) {
    fac <- factor(PCA$fac[, fac])
  }

  # we grab the PCs to capture retain% of the variance
  if (retain<1){
    vars <- PCA$sdev^2
    retain <- min(which((cumsum(vars)/sum(vars))>0.99))
  }
  
  # we check all factors to avoid full rank
  tab <- table(fac)
  if (min(tab) < retain ) {
    retain <- min(tab)
    cat(" * '", names(which.min(tab)), "' has ", min(tab), " rows, and 'retain' is set accordingly.\n", sep="")}
  
  cat(" * PC axes 1 to", retain, "were retained\n")
  retain <- 1:retain
  x <- PCA$x[, retain]  
  
  
  

  # we get all combinations, and prepare the loop
  pws <- t(combn(levels(fac), 2))
  n <- nrow(pws)
  cn <- colnames(summary(manova(x~ fac))$stats)
  res <- matrix(NA, nrow = n, ncol = 6,
                dimnames = list(paste(pws[, 1], pws[, 2], sep = " - "), cn))
  manovas <- list()
  # we loop and do all the MANOVAs
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
  print(res, digits=4)
  invisible(list(manovas = manovas, summary = res, stars.tab = stars.tab))
}


# todo #' @rdname MANOVA #' @export MANOVA.LdkCoe <-
# function(x, fac, test='Hotelling', retain, drop){ LdkCoe <-
# x if (length(LdkCoe$method)>1) stop(' * cannot yet be used
# on combined OutCoe. Do it manually.') if (missing(fac))
# stop(' * 'fac' must be provided') if (!is.factor(fac)) {fac
# <- LdkCoe$fac[, fac]} x <- LdkCoe$coe mod <-
# summary(manova(x~fac), test=test) return(mod)}

##### end MANOVA
