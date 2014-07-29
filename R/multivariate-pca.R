##### PCA on Coe objects

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

##### end PCA 
