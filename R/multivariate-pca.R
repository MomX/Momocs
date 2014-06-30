
# 3. PCA on Coe ----------------------------------------------------------------
#' Principal component analysis on Coe objects
#' 
#' Performs a PCA on \link{Coe} objects.
#' 
#' By default, methods on \link{Coe} object do not scale the input data but center them.
#' There is also a generic method (eg for traditional morphometrics) that centers and scales data.
#' @aliases PCA
#' @rdname PCA
#' @param x a \link{Coe} object
#' @param scale. logical whether to scale the input data
#' @param center logical whether to center the input data
#' @return a "PCA" object on which to apply \link{plot.PCA}
#' @seealso \link{plot.PCA}
#' @keywords Multivariate
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' plot(bot.p)
#' @export
PCA <- function(x){UseMethod("PCA")}

#' @rdname PCA
#' @export
PCA.OutCoe <- function(x, scale.=FALSE, center=TRUE){
  OutCoe <- x
  PCA <- prcomp(OutCoe$coe, scale.=sclae., center=center)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

#' @rdname PCA
#' @export
PCA.OpnCoe <- function(x, scale.=FALSE, center=TRUE){
  OpnCoe <- x
  PCA <- prcomp(OpnCoe$coe, scale.=scale., center=center)
  PCA$fac <- OpnCoe$fac
  PCA$mshape <- apply(OpnCoe$coe, 2, mean)
  PCA$method <- OpnCoe$method
  PCA$mod    <- OpnCoe$mod #the only diff so far
  PCA$baseline1 <- OpnCoe$baseline1
  PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

#' @rdname PCA
#' @export
PCA.LdkCoe <- function(x, scale.=FALSE, center=TRUE){
  LdkCoe <- x
  LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(LdkCoe$coe, scale.=scale., center=center)
  PCA$fac <- LdkCoe$fac
  PCA$mshape <- apply(LdkCoe$coe, 2, mean)
  PCA$method <- "procrustes"
  #PCA$mod    <- OpnCoe$mod #the only diff so far
  #PCA$baseline1 <- OpnCoe$baseline1
  #PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

#' @rdname PCA
#' @export
PCA.default <- function(x, scale.=TRUE, center=TRUE){
  PCA <- prcomp(x, scale.=scale., center=center)
  PCA$fac <- NULL
  PCA$method <- NULL
  #PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)}

##### end PCA
