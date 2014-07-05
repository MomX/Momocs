##### Manova methods on Coe

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
#' @param test a test for \link{manova} (\code{"Hotelling"} by default)
#' @param retain how many harmonics (or polynomials) to retain
#' @param drop how many harmonics (or polynomials) to drop
#' @return a list of matrices of (x,y) coordinates.
#' @keywords Multivariate
#' @note Needs a review #todo.
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' Manova(bot.f, "type")
#' 
#' data(olea)
#' op <- rawPolynomials(olea, 5)
#' Manova(op, "domes")
#' @export
Manova <- function(x, fac, test="Hotelling", retain, drop){UseMethod("Manova")}

#' @rdname Manova
#' @export
Manova.OpnCoe <- function(x, fac, test="Hotelling", retain, drop){
  OpnCoe <- x
  if (length(OpnCoe$method)>1) stop(" * cannot yet be used on combined OutCoe. Do it manually.")
  if (missing(fac)) stop(" * 'fac' must be provided")
  if (!is.factor(fac)) {fac <- OpnCoe$fac[, fac]}
  x <- OpnCoe$coe
  if (missing(drop)) drop <- 0
  if (missing(retain)) retain <- ncol(x)
  keep <- (drop+1):retain
  cat(" * Manova done on:", colnames(x)[keep], "\n")
  mod <- summary(manova(x[, keep] ~fac), test=test)
  return(mod)}

#' @rdname Manova
#' @export
Manova.OutCoe <- function(x, fac, test="Hotelling", retain, drop){
  OutCoe <- x
  if (length(OutCoe$method)>1) stop(" * cannot yet be used on combined OutCoe. Do it manually.")
  if (missing(fac)) stop("'fac' must be provided")
  if (!is.factor(fac)) {fac <- OutCoe$fac[, fac]}
  x <- OutCoe$coe
  cph <- NULL
  # we check for (a)symetrization
  if (OutCoe$method == "eFourier"){
    nb.h <- ncol(x)/4
    BC <- (nb.h+1):(nb.h*3)
    AD <- c(1:nb.h, ((nb.h*3 +1):(nb.h*4)))
    if (sum(x[, BC])==0){
      x <- x[, -BC]
      cph <- 2
      cat(" * B and C harmonics removed (because of removeAsymetric)\n")
    } else {
      if (sum(x[, AD])==0){
        x <- x[, -AD]
        cph <- 2
        cat(" * A and D harmonics removed (because of removeSymetric)\n")
      }
    }
  }
  # we remove normalized harmonics (if any)
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0 }}
  
  if (is.null(cph)) { cph <- ifelse(OutCoe$method == "eFourier", 4, 2) }
  nb.h <- ncol(x)/cph
  fr <- nrow(x) - nlevels(fac)
  max.h <- floor(fr/cph)
  if (missing(retain)) {
    retain <- max.h + drop
    if (retain > nb.h) { retain <- nb.h }
    cat(" * 'retain' was missing. MANOVA done with", retain, "harmonics ")
    if (drop>0) { cat( "and the first", drop, "dropped.\n") } else {cat("./n")}
  } else {
    if ((retain - drop)> max.h) {
      retain <- max.h + drop
      if (retain > nb.h) { retain <- nb.h }
      cat(" * 'retain' was too ambitious. MANOVA done with", retain, "harmonics ")
      if (drop>0) {cat( "and the first", drop, "dropped.\n")} else {cat("./n")}}}
  
  harm.sel <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=cph)
  #cat(retain, drop, nb.h, cph)  
  cat("\n")
  mod <- summary(manova(x[, harm.sel]~fac), test=test)
  return(mod)}

#todo
# #' @rdname Manova
# #' @export
# Manova.LdkCoe <- function(x, fac, test="Hotelling", retain, drop){
#   LdkCoe <- x
#   if (length(LdkCoe$method)>1) stop(" * cannot yet be used on combined OutCoe. Do it manually.")
#   if (missing(fac)) stop(" * 'fac' must be provided")
#   if (!is.factor(fac)) {fac <- LdkCoe$fac[, fac]}
#   x <- LdkCoe$coe
#   mod <- summary(manova(x~fac), test=test)
#   return(mod)}

##### end Manova
