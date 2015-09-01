
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
    x.i   <-   x[fac %in% pws[i, ], ]
    fac.i <- factor(fac[fac %in% pws[i, ]])
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
