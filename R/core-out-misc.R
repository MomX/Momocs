##### Miscellaneous functions for Fourier-based approaches

#' Helps to select a given number of harmonics from a numerical vector.
#' 
#' \code{coeff.sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{$coe} slot of \code{\link{Coe}}-objects in
#' that way: \deqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{eFourier} and
#' \link{efourier}) while \deqn{C_n and D_n} harmonic are absent for radii
#' variation and tangent angle approaches (see \link{rfourier} and
#' \link{tfourier} respectively). . This function is used internally but might
#' be of interest elwewhere.
#' 
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff.sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff.split} returns a named list
#' of coordinates.
#' @keywords miscFourier
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 32)
#' coe <- bot.f$coe # the raw matrix
#' coe
#' # if you want, say the first 8 harmonics but not the first one
#' retain <- coeff.sel(retain=8, drop=1, nb.h=32, cph=4)
#' head(coe[, retain])
#' @export
coeff.sel <- function(retain=8, drop=0, nb.h=32, cph=4){
  cs <- numeric()
  for (i in 1:cph) {
    cs <- c(cs, (1+drop):retain + nb.h*(i-1))}
  return(cs)}

#' Converts a numerical description of harmonic coefficients to a named list.
#' 
#' \code{coeff.split} returns a named list of coordinates from a vector of
#' harmonic coefficients. For instance, harmonic coefficients are arranged in
#' the \code{$coe} slot of \code{Coe}-objects in that way: \deqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{eFourier} and \link{efourier}) while \deqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#' 
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @keywords miscFourier
#' @examples
#' coeff.split(1:128, nb.h=32, cph=4) # efourier
#' coeff.split(1:64, nb.h=32, cph=2)  # t/r fourier
#' @export
coeff.split <- function(cs, nb.h=8, cph=4){
  if (missing(nb.h)) {nb.h <- length(cs)/cph }
  cp <- list()
  for (i in 1:cph) {
    cp[[i]] <- cs[1:nb.h + (i-1)*nb.h]
  }
  names(cp) <- paste(letters[1:cph], "n", sep="")
  return(cp)}

#' Calculates harmonic power given a list from e/t/rfourier
#' 
#' Given a list with \code{an, bn (and eventually cn and dn)}, returns the
#' harmonic power.
#' 
#' @param xf A list with an, bn (and cn, dn) components, typically from a
#' e/r/tfourier passed on coo.
#' @return Returns a \code{vector} of harmonic power
#' @keywords miscFourier
#' @examples
#' 
#' data(bot)
#' ef <- efourier(bot[1], 24)
#' rf <- efourier(bot[1], 24)
#' harm.pow(ef)
#' harm.pow(rf)
#' 
#' plot(cumsum(harm.pow(ef)[-1]), type="o",
#'   main="Cumulated harmonic power without the first harmonic",
#'   ylab="Cumulated harmonic power", xlab="Harmonic rank")
#' 
#' @export
harm.pow <- function(xf){
  if (is.list(xf)) {
    if (all(c("an", "bn", "cn", "dn") %in% names(xf))) {
      return((xf$an^2 + xf$bn^2 + xf$cn^2 + xf$dn^2)/2)
    } else {
      if (all(c("an", "bn") %in% names(xf))) {
        return((xf$an^2 + xf$bn^2)/2)}
    }
  } else {
    stop(" * a list containing 'an', 'bn' ('cn', 'dn') harmonic coefficients must be provided")}}

##### end misc Fourier
