##### MANOVA methods --------------------------------------

# would need a good review. # todo

#' Multivariate analysis of (co)variance on Coe objects
#'
#' Performs multivariate analysis of variance on \link{PCA} objects.
#'
#' Performs a MANOVA/MANCOVA on PC scores. Just a wrapper around \link{manova}. See examples for multifactorial manova and
#' \link{summary.manova} for more details and examples.
#'
#' @aliases MANOVA
#' @rdname MANOVA
#' @param x a \link{Coe} object
#' @param fac a name of a colum in the \code{$fac} slot, or its id, or a formula
#' @param test a test for \link{manova} (\code{'Hotelling'} by default)
#' @param retain how many harmonics (or polynomials) to retain, for PCA
#' the highest number of PC axis to retain, or the proportion of the variance to capture.
#' @param drop how many harmonics (or polynomials) to drop
#' @param verbose logical whether to print messages
#' @return a list of matrices of (x,y) coordinates.
#' @family multivariate
#' @note Needs a review and should be considered as experimental.
#' @examples
#' # MANOVA
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
#'
#'  # MANCOVA example
#'  # we create a numeric variable, based on centroid size
#'  bot %<>% mutate(cs=coo_centsize(.))
#'  # same pipe
#'  bot %>% efourier %>% PCA %>% MANOVA("cs")
#'
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
    stop("cannot yet be used on combined OutCoe, do it manually")
  if (missing(fac))
    stop("'fac' must be provided")
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
    message("MANOVA done on:", colnames(x)[keep])
  mod <- summary(manova(x[, keep] ~ fac), test = test)
  return(mod)
}

#' @rdname MANOVA
#' @export
MANOVA.OutCoe <- function(x, fac, test = "Hotelling", retain, drop, verbose = TRUE) {
  OutCoe <- x
  if (length(OutCoe$method) > 1)
    stop("cannot yet be used on combined OutCoe. Do it manually")
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
        message("B and C harmonics removed (because of removeAsymetric)")
    } else {
      if (sum(x[, AD]) == 0) {
        x <- x[, -AD]
        cph <- 2
        if (verbose)
          message("A and D harmonics removed (because of removeSymetric)")
      }
    }
  }
  # we remove normalized harmonics (if any)
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      if (verbose)
        message("1st harmonic removed (because of normalization)")
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
      message("'retain' was missing. MANOVA done with", retain, "harmonics ")
      if (drop > 0) {
        message("and the first", drop, "dropped")
      }
    }
  } else {
    if ((retain - drop) > max.h) {
      retain <- max.h + drop
      if (retain > nb.h) {
        retain <- nb.h
      }
      if (verbose)
        message("'retain' was too ambitious. MANOVA done with ",
            retain, " harmonics ")
      if (verbose) {
        if (drop > 0) {
          message("and the first ", drop, " were dropped")
        } else {
          message("\n")
        }
      }
    }
  }

  harm.sel <- coeff_sel(retain = retain, drop = drop, nb.h = nb.h,
                        cph = cph)
  if (verbose)
    message("\n")
  mod <- summary(manova(x[, harm.sel] ~ fac), test = test)
  return(mod)
}

#' @rdname MANOVA
#' @export
MANOVA.PCA <- function(x, fac, test = "Hotelling", retain=0.99, drop, verbose = TRUE) {
  if (missing(fac))
    stop("'fac' must be provided")

  if (class(fac) == "formula") {
    column_name <- attr(terms(fac), "term.labels")
    # we check for wrong formula
    if (any(is.na(match(column_name, colnames(x$fac)))))
      stop("formula provided must match with $fac column names")
    # otherwise we retrive the column(s)
    fac <- x$fac[, column_name]
    # multicolumn/fac case
    if (is.data.frame(fac))
      fac <- factor(apply(fac, 1, paste, collapse="_"))
  }

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
  message("PC axes 1 to ", retain, " were retained")
  retain <- 1:retain
  if (length(retain)==1)
    stop("needs more than a single response")
  x <- x$x[, retain]
  mod <- summary(manova(x ~ fac), test = test)
  return(mod)
}

# MANOVA PW -----------------------------------------------

#' Pairwise Multivariate analyses of variance
#'
#' A wrapper for pairwise \link{MANOVA}s on \link{Coe} objects. Calculates a MANOVA for every
#' pairwise combination of the factor provided.
#' @param x a \link{PCA} object
#' @param fac a name (or its id) of a grouping factor in \code{$fac} or a factor or a formula.
#' @param verbose to feed \link{MANOVA}
#' @param retain the number of PC axis to retain (1:retain) or the proportion of variance to capture (0.99 par default).
#' @param ... more arguments to feed \link{MANOVA}
#' @note Needs a review and should be considered as experimental.
#' If the fac passed has only two levels, there is only pair and it is
#' equivalent to \link{MANOVA}. \code{MANOVA_PW.PCA} works with the regular \link{manova}.
#' @seealso \link{MANOVA}, \link{manova}.
#' @family multivariate
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
#' bot.p <- PCA(efourier(bot, 8))
#' MANOVA_PW(bot.p, 'fake') # or MANOVA_PW(bot.p, 2)
#'
#' # an example on open outlines
#' data(olea)
#' op <- PCA(npoly(olea))
#' MANOVA_PW(op, 'domes')
#' # to get the results
#' res <- MANOVA_PW(op, 'domes')
#' res$manovas
#' res$stars.tab
#' res$summary
#' @rdname MANOVA_PW
#' @export
MANOVA_PW <- function(x, ...) {
  UseMethod("MANOVA_PW")
}

#' @rdname MANOVA_PW
#' @export
MANOVA_PW.PCA <- function(x,
                          fac,
                          verbose = FALSE,
                          retain=0.99,
                          ...) {
  # preliminaries
  PCA <- x

  fac0 <- fac
  if (missing(fac))
    stop("'fac' must be provided")

  if (class(fac) == "formula") {
    #f0 <- PCA$fac[, attr(terms(fac), "term.labels")]
    #fac <- interaction(f0)
    fac <- x$fac[, attr(terms(fac), "term.labels")]
    if (is.data.frame(fac))
      fac <- factor(apply(fac, 1, paste, collapse="_"))
  }

  if (!is.factor(fac)) {
    fac <- factor(PCA$fac[, fac])
  } else {
    fac <- droplevels.factor(fac)
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
    message("'", names(which.min(tab)), "' has ", min(tab), " rows, and 'retain' is set accordingly")}

  message("PC axes 1 to ", retain, " were retained")
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
    x.i   <-   x[fac %in% pws[i, ], ]
    fac.i <- factor(fac[fac %in% pws[i, ]])
    if (nlevels(fac.i)>1){
      m <- summary(manova(x.i ~ fac.i))
      manovas[[i]] <- m
      res[i, ] <- m$stats[1, ]}
    if (verbose)
      message(pws[i, ])
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
  pvalue.tab <- stars.tab <- matrix(NA, nrow = nl - 1, ncol = nl,
                      dimnames = list(levels(fac)[-nl], levels(fac)))
  stars.tab <- as.table(stars.tab)
  k <- 1
  for (i in 1:(nl - 1)) {
    for (j in (i + 1):nl) {
      stars.tab[i, j] <- stars[k]
      pvalue.tab[i, j] <- res[, 6][k]
      k <- k + 1
    }
  }
  cat("$stars.tab\n")
  print(stars.tab)
  cat("\n$summary (see also $manovas)\n")
  print(res, digits=4)
  invisible(list(manovas = manovas, summary = res,
                 stars.tab = stars.tab, pvalue.tab = pvalue.tab))
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

# todo #' @rdname MANOVA #' @export MANOVA.LdkCoe <-
# function(x, fac, test='Hotelling', retain, drop){ LdkCoe <-
# x if (length(LdkCoe$method)>1) stop(' * cannot yet be used
# on combined OutCoe. Do it manually.') if (missing(fac))
# stop(' * 'fac' must be provided') if (!is.factor(fac)) {fac
# <- LdkCoe$fac[, fac]} x <- LdkCoe$coe mod <-
# summary(manova(x~fac), test=test) return(mod)}

##### end MANOVA

