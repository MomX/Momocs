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
#' @family multivariate
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' bot.p
#' plot(bot.p, morpho=FALSE)
#' plot(bot.p, 'type')
#'
#' data(olea)
#' op <- npoly(olea, 5)
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
PCA <- function(x, scale., center, fac) {
  UseMethod("PCA")
}

#' @rdname PCA
#' @export
PCA.OutCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
  OutCoe <- x
  PCA <- prcomp(OutCoe$coe, scale. = scale., center = center)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  if (!is.null(OutCoe$baseline1)){
    PCA$baseline1 <- OutCoe$baseline1
    PCA$baseline2 <- OutCoe$baseline2}
  PCA$cuts   <- OutCoe$cuts
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.OpnCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
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
PCA.LdkCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
  LdkCoe <- x
  # LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(LdkCoe$coe, scale. = scale., center = center)
  PCA$fac <- LdkCoe$fac
  PCA$mshape <- apply(LdkCoe$coe, 2, mean)
  PCA$method <- "procrustes"
  PCA$cuts   <- LdkCoe$cuts
  PCA$links <- LdkCoe$links
  # PCA$mod <- OpnCoe$mod #the only diff so far PCA$baseline1
  # <- OpnCoe$baseline1 PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.TraCoe <- function(x, scale. = TRUE, center = TRUE, fac) {
  TraCoe <- x
  # LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(TraCoe$coe, scale. = scale., center = center)
  PCA$fac <- TraCoe$fac
  PCA$mshape <- NULL
  PCA$method <- NULL

  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.default <- function(x, scale. = TRUE, center = TRUE, fac=data.frame()) {
  PCA <- prcomp(x, scale. = scale., center = center)
  if (!is.null(fac)) fac <- as.data.frame(fac)
  PCA$fac <- fac
  PCA$method <- NULL
  # PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

# 2. PCA Bridges ------------------------------------------
#' @rdname PCA
#' @export
as.PCA <- function(x, fac){UseMethod("as.PCA")}

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
  cat(" -", ifelse(is.null(nrow(x$x)), 1, nrow(x$x)), "shapes \n")
  # Method printer
  if (length(x$method)>1) {
    cat(" - $method: [", paste0(x$method, collapse=" + "), "analyses ]\n")
  } else {
    cat(" - $method: [", x$method, "analysis ]\n")}
  # we print the fac
  .print.fac(x$fac)
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
#' @examples
#' data(bot)
#' bot2 <- bot1 <- coo_scale(coo_center(coo_sample(bot, 60)))
#' bot1$fac$session <- factor(rep("session1", 40))
#' # we simulate an measurement error
#' bot2 <- coo_jitter(bot1, amount=0.01)
#' bot2$fac$session <- factor(rep("session2", 40))
#' botc <- combine(bot1, bot2)
#' botcf <- efourier(botc, 12)
#'
#' # we gonna plot the PCA with the two measurement sessions and the two types
#' botcp <- PCA(botcf)
#' plot(botcp, "type", col=col_summer(2), pch=rep(c(1, 20), each=40), eigen=FALSE)
#' bot.pairs <- get_pairs(botcp, fac = "session", range=1:2)
# # with bot.pairs we can add segments between the two sessions
#' segments(bot.pairs$session1[, 1], bot.pairs$session1[, 2],
#'        bot.pairs$session2[, 1], bot.pairs$session2[, 2],
#'        col=col_summer(2)[bot.pairs$fac$type])
#'
#'
#' @export
get_pairs <- function(x, fac, range){UseMethod("get_pairs")}
#' @export
get_pairs.Coe <- function(x, fac, range){
  # we check and prepare
  if (missing(fac)) stop("'fac' must be provided")
  fac <- x$fac[, fac]
  if (nlevels(fac) != 2) stop("more than two levels for the 'fac' provided")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop("some mismatches between pairs")
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
get_pairs.PCA <- function(x, fac, range){
  # we check and prepare
  if (missing(fac)) stop("'fac' mus be provided")
  fac <- x$fac[, fac]
  if (nlevels(fac) != 2) stop("more than two levels for the 'fac' provided")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop("some mismatches between pairs")
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
get_pairs.LDA <- get_pairs.PCA


# 4. redo PCA ---------


#' "Redo" a PCA on a new Coe
#'
#' Basically reapply rotation to a new Coe object.
#' @param PCA a \link{PCA} object
#' @param Coe a \link{Coe} object
#' @note Quite experimental. Dimensions of the matrices and methods must match.
#' @examples
#' data(bot)
#' b <- filter(bot, type=="beer")
#' w <- filter(bot, type=="whisky")
#'
#' bf <- efourier(b, 8)
#' bp <- PCA(bf)
#'
#' wf <- efourier(w, 8)
#'
#' # and we use the "beer" PCA on the whisky coefficients
#' wp <- rePCA(bp, wf)
#'
#' plot(wp)
#'
#' plot(bp, eig=FALSE)
#' points(wp$x[, 1:2], col="red", pch=4)
#'
#'@export
rePCA <- function(PCA, Coe){
  UseMethod("rePCA")
}


#'@export
rePCA.default <- function(PCA, Coe){
  stop("method only defined for PCA objetcs")
}


#'@export
rePCA.PCA <- function(PCA, Coe){
  if (Coe$method != PCA$method)
    warning("methods differ between Coe and PCA")
  scores <- PCA$x
  rot <- PCA$rotation
  coe <- Coe$coe
  if (any(colnames(coe) != rownames(rot)))    warning("matrices coefficients must match")
  # we prepare a new PCA object
  PCA2 <- PCA
  PCA2$x <- matrix(NA, nrow(coe), ncol(rot), dimnames = list(rownames(coe), colnames(rot)))
  # we recenter
  coe <- apply(coe, 2, function(x) x - mean(x))
  # learn matrix calculus bitch
  for (PC in 1:ncol(rot)){
    for (ind in 1:nrow(coe)){
      PCA2$x[ind, PC] <- sum(coe[ind, ] * rot[, PC])
    }
  }
  return(PCA2)
}


#' Calculates convex hull area/volume of PCA scores
#'
#' May be useful to compare shape diversity. Expressed in PCA units that should
#' only be compared within the same PCA.
#'
#' @param x a PCA object
#' @param fac (optionnal) column name or ID from the $fac slot.
#' @param xax the first PC axis to use (1 by default)
#' @param yax the second PC axis (2 by default)
#' @param zax the third PC axis (3 by default only for volume)
#'
#' @return
#' If fac is not provided global area/volume is returned; otherwise a named
#' list for every level of fac
#'
#' @details get_chull_area is calculated using \link{coo_chull} followed by \link{coo_area};
#'  get_chull_volume is calculated using geometry::convexhulln
#'
#' @examples
#' data(bot)
#' bp <- PCA(efourier(bot, 12))
#' get_chull_area(bp)
#' get_chull_area(bp, 1)
#'
#' get_chull_volume(bp)
#' get_chull_volume(bp, 1)
#' @export
get_chull_area <- function (x, fac, xax = 1, yax = 2) {
  if (!is.PCA(x)) stop("'x' must be a PCA object")
  # no fac provided
  if (missing(fac)){
    xy <- x$x[, c(xax, yax)]
    return(coo_area(coo_chull(xy)))
  }
  # else... if a fac is provided
  fac <- x$fac[, fac]
  x <- x$x[, c(xax, yax)]
  # we prepare the list
  res <- list()
  # we loop over to extract subset of coordinates
  for (i in seq(along = levels(fac))) {
    xy.i <- x[fac == levels(fac)[i], ]
    # boring but prevents the numeric/2rows matrices
    if (is.matrix(xy.i)) {
      if (nrow(xy.i) > 2) {
        res[[i]] <- coo_chull(xy.i)
      }  else {
        res[[i]] <- NULL
      }
    } else {
      res[[i]] <- NULL
    }
  }
  # we calculate the area and return the results
  names(res) <- levels(fac)
  res <- lapply(res, coo_area)
  return(res)
}

#' @rdname get_chull_area
#' @export
get_chull_volume <- function (x, fac, xax = 1, yax = 2, zax = 3) {
  if (!is.PCA(x)) stop("'x' must be a PCA object")

  # no fac provided
  if (missing(fac)){
    xy <- x$x[, c(xax, yax, zax)]
    res <- geometry::convhulln(xy, options="FA")$vol
    return(res)
  }
  # else...fac provided
  fac <- x$fac[, fac]
  # we prepare the list
  x <- x$x[, c(xax, yax, zax)]
  res <- list()
  # we loop over to extract subset of coordinates
  for (i in seq(along = levels(fac))) {
    xy.i <- x[fac == levels(fac)[i], ]
    # boring but prevents the numeric/2rows matrices
    if (is.matrix(xy.i)) {
      if (nrow(xy.i) >= 4) {
        res[[i]] <- geometry::convhulln(xy.i, options="FA")$vol
      }  else {
        res[[i]] <- NULL
      }
    } else {
      res[[i]] <- NULL
    }
  }
  # we calculate the area and return the results
  names(res) <- levels(fac)
  return(res)
}

#5 . Remove outliers on the fly

#' Remove outliers on Coe
#'
#' First performs a PCA, then searches for outliers using \link{dnorm}
#'
#' @param x object, either Coe or a numeric on which to search for outliers
#' @param conf confidence for dnorm
#' @param nax number of axes to retain (only for Coe),
#' if <1 retain enough axes to retain this proportion of the variance
#' @param ... additional parameters to be passed to PCA (only for Coe)
#' @note experimental. dnorm parameters used are \code{median(x), sd(x)}
#' @examples
#' x <- rnorm(10)
#' x[4] <- 99
#' which_out(x)
#' @export
which_out <- function(x, conf, nax, ...){
  UseMethod("which_out")
}

#' @export
which_out.default <- function(x, conf=1e-3, ...){
  out <- which(dnorm(x, median(x), sd(x)) < conf)
  if(length(out)==0) {
    return(NA)
  } else {
    return(out)}
  }

#' @export
which_out.Coe <- function(x, conf=1e-3, nax=0.99, ...){
  p <- PCA(x, ...)
  if (length(nax)==1)
    if (nax < 1)
      nax <- scree_min(p, nax)
    m <- p$x[, 1:nax]
    m <- matrix(m, ncol=nax)
    outliers <- apply(m, 2, which_out, conf=conf)
    outliers <- unlist(outliers)
    return(unique(outliers))
}

#### end PCA
