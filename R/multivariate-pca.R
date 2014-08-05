# PCA on Coe objects

# 1. PCA calculation and builder --------------------------
#' Principal component analysis on Coe objects
#'
#' Performs a PCA on \link{Coe} objects, using \link{prcomp}.
#'
#' By default, methods on \link{Coe} object do not scale the input data but center them.
#' There is also a generic method (eg for traditional morphometrics) that centers and scales data.
#' @aliases PCA
#' @rdname PCA
#' @param x a \link{Coe} object or an appropriate object (eg \link{prcomp}) for \code{as.PCA}
#' @param fac any factor or data.frame to be passed to \code{as.PCA} and for use with \link{plot.PCA}
#' @param scale. logical whether to scale the input data
#' @param center logical whether to center the input data
#' @return a 'PCA' object on which to apply \link{plot.PCA}
#' @seealso \link{plot.PCA}
#' @keywords Multivariate
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' bot.p
#' plot(bot.p, morpho=FALSE)
#' plot(bot.p, 'type')
#'
#' data(olea)
#' op <- rawPolynomials(olea, 5)
#' op.p <- PCA(op)
#' op.p
#' plot(op.p, 1, morpho=TRUE)
#'
#' data(wings)
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wpp <- PCA(wp)
#' wpp
#' plot(wpp, 1)
#'
#' # "foreign prcomp"
#' head(iris)
#' iris.p <- prcomp(iris[, 1:4])
#' iris.p <- as.PCA(iris.p, iris[, 5])
#' class(iris.p)
#' plot(iris.p, 1)
#' @export
PCA <- function(x, scale., center) {
  UseMethod("PCA")
}

#' @rdname PCA
#' @export
PCA.OutCoe <- function(x, scale. = FALSE, center = TRUE) {
  OutCoe <- x
  PCA <- prcomp(OutCoe$coe, scale. = scale., center = center)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  PCA$cuts   <- OutCoe$cuts
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.OpnCoe <- function(x, scale. = FALSE, center = TRUE) {
  OpnCoe <- x
  PCA <- prcomp(OpnCoe$coe, scale. = scale., center = center)
  PCA$fac <- OpnCoe$fac
  PCA$mshape <- apply(OpnCoe$coe, 2, mean)
  PCA$method <- OpnCoe$method
  PCA$mod <- OpnCoe$mod  #the only diff so far
  PCA$baseline1 <- OpnCoe$baseline1
  PCA$baseline2 <- OpnCoe$baseline2
  PCA$cuts   <- OpnCoe$cuts
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.LdkCoe <- function(x, scale. = FALSE, center = TRUE) {
  LdkCoe <- x
  # LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(LdkCoe$coe, scale. = scale., center = center)
  PCA$fac <- LdkCoe$fac
  PCA$mshape <- apply(LdkCoe$coe, 2, mean)
  PCA$method <- "procrustes"
  PCA$cuts   <- LdkCoe$cuts
  # PCA$mod <- OpnCoe$mod #the only diff so far PCA$baseline1
  # <- OpnCoe$baseline1 PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.default <- function(x, scale. = TRUE, center = TRUE) {
  PCA <- prcomp(x, scale. = scale., center = center)
  PCA$fac <- NULL
  PCA$method <- NULL
  # PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

# 2. PCA Bridges ------------------------------------------
#' @rdname PCA
#' @export
as.PCA <- function(x, fac){UseMethod("as.PCA")}
#' @rdname PCA
#' @export
as.PCA.default <- function(x, fac){
  if (class(x)[1] != "PCA"){
    class(x) <- c("PCA", class(x))
    if (!missing(fac)) x$fac <- as.data.frame(fac)
    return(x)}}

#' @export
print.PCA <- function(x, ...){
  cat("A PCA object\n")
  cat(rep("-", 20), "\n", sep = "")
  # Method printer
  if (length(x$method)>1) {
    cat(" - $method: [", paste0(x$method, collapse=" + "), "analyses ]\n")
  } else {
    cat(" - $method: [", x$method, "analysis ]\n")}
    # Fac printers
  df <- x$fac
  nf <- ncol(df)
  if (nf == 0) {
    cat(" - $fac: No groups defined\n")
  } else {
    if (nf<2) {
      cat(" - $fac:", nf, "grouping factor:\n")
    } else {
      cat(" - $fac:", nf, "grouping factors:\n")}
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      # cosmectics below
      if (sum(nchar(lev.i))>60){
        maxprint <- which(cumsum(nchar(lev.i))>30)[1]
        cat("     '", colnames(df)[i], "': ", paste(lev.i[1:maxprint], collapse=", "),
            " ... + ", length(lev.i) - maxprint, " more.\n", sep="")
      } else {
        cat("     '", colnames(df)[i], "': ", paste(lev.i, collapse=", "), ".\n", sep="")
      }
    }
  }
  cat(" - All components: ",  paste(names(x), collapse=", "), ".\n", sep="")
}

# 3. PCA methods ------------------------------------------
#' Get paired individual on a Coe, PCA or LDA objects
#'
#' If you have paired individuals, i.e. before and after a treatment or for repeated measures,
#' and if you have coded coded it into \code{$fac}, this methods allows you to retrieve the cooresponding PC/LD scores,
#' or coefficients for \link{Coe} objects.
#' @param x any \link{Coe}, \link{PCA} of \link{LDA} object.
#' @param fac factor or column name or id corresponding to the pairing factor.
#' @param range numeric the range of coefficients for \code{Coe}, or PC (LD) axes on which to return scores.
#' @return a list with components \code{x1} all coefficients/scores corresponding to the
#' first level of the \code{fac} provided; \code{x2} same thing for the second level;
#' \code{fac} the corresponding \code{fac}.
#' @keywords Coe Multivariate
#' @examples
#' data(bot)
#' bot2 <- bot1 <- coo.scale(coo.center(coo.sample(bot, 60)))
#' bot1$fac$session <- factor(rep("session1", 40))
#' bot2$coo <- lapply(bot2$coo, function(x) x + rnorm(nrow(x)*2, sd=2e-3)) # we simulate an measurement error
#' bot2$fac$session <- factor(rep("session2", 40))
#' botc <- combine(bot1, bot2)
#' botcf <- eFourier(botc, 12)
#'
#' # we gonna plot the PCA with the two measurement sessions and the two types
#' botcp <- PCA(botcf)
#' plot(botcp, "type", col=col.summer(2), pch=rep(c(1, 20), each=40), eigen=FALSE)
#' bot.pairs <- getPairs(botcp, fac = "session", range=1:2)
# # with bot.pairs we can add segments between the two sessions
#' segments(bot.pairs$session1[, 1], bot.pairs$session1[, 2],
#'        bot.pairs$session2[, 1], bot.pairs$session2[, 2],
#'        col=col.summer(2)[bot.pairs$fac$type])
#' @export
getPairs <- function(x, fac, range){UseMethod("getPairs")}
#' @export
getPairs.Coe <- function(x, fac, range){
  # we check and prepare
  if (missing(fac)) stop(" * a 'fac' mus be provided.")
  fac <- x$fac[, fac]
  if (nlevels(fac) != 2) stop(" * more than two levels for the 'fac' provided.")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop(" * some mismatches between pairs.")
  # we get paired individuals
  if (missing(range)) range <- 1:ncol(x$coe)
  fl  <- levels(fac)
  x1   <- x$coe[fac==fl[1], range]
  x2   <- x$coe[fac==fl[2], range]
  res <- list(x1, x2, fac=x$fac[fac==fl[1],])
  names(res)[1:2] <- fl
  return(res)
}

#' @export
getPairs.PCA <- function(x, fac, range){
  # we check and prepare
  if (missing(fac)) stop(" * a 'fac' mus be provided.")
  fac <- x$fac[, fac]
  if (nlevels(fac) != 2) stop(" * more than two levels for the 'fac' provided.")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop(" * some mismatches between pairs.")
  # we get paired individuals
  if (missing(range)) range <- 1:ncol(x$x)
  fl  <- levels(fac)
  x1   <- x$x[fac==fl[1], range]
  x2   <- x$x[fac==fl[2], range]
  res <- list(x1, x2, fac=x$fac[fac==fl[1],])
  names(res)[1:2] <- fl
  return(res)
}
#' @export
getPairs.LDA <- getPairs.PCA


##### end PCA
