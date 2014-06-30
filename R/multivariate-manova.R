##### Manova methods on Coe

#' Multivariate analysis of variance on Coe objects
#' 
#' Performs multivariate analysis of variance on \link{Coe} objects.
#' 
#' For outlines, checks if the matrix of coefficients is of full rank, and if not removes the
#' higher order harmonics (for Out objects.)
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
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0 }}
  cph <- ifelse(OutCoe$method == "eFourier", 4, 2)
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
