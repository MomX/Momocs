
# 2. MANOVAs -------------------------------------------------------------------
#' Multivariate analysis of variance on matrices of coefficients
#' 
#' Calculates mean shapes on matrices of coefficients by groups (if passed with
#' a "fac") or globally (if not).
#' @aliases Manova
#' @param ... a \link{Coe} object
#' @return a list of matrices of (x,y) coordinates.
#' @keywords multivariate
#' @export
Manova <- function(...){UseMethod("Manova")}
#' @export
Manova.OutCoe <- function(OutCoe, fac, retain, drop, ...){
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
  mod <- summary(manova(x[, harm.sel]~fac), test="Hotelling")
  return(mod)}

