##### Coo and Coe classes declarations, domestic functions and helpers.

# 0. Coo (Opn/Out/Ldk) domestics -----------------------------------------------

#' Coo class
#' 
#' \code{Coo} class is a super class for \link{Opn} and \link{Out} classes,
#' \bold{open outlines} and \bold{closed outlines}, respectively.
#' 
#' It allows to recycle most of the methods, since both of them apply on
#' \eqn{(x; y)} coordinates. In other words, \code{Opn} and \code{Out} classes
#' are all, primarily, \code{Coo} objects on which we define generic \emph{and} 
#' specific methods.
#' 
#' If you used Momocs before version <1.0, or if you have read the JSS paper,
#' you are probably looking for \link{Out}.
#' 
#' More, generally, see \link{Opn} and \link{Out} for documentation on these classes.
#'
#'@param ... anything and, anyway, this function will simply returns a message.
#' @keywords Coo
#' @export
Coo <- function(...){
  cat(" * Coo constructor has been deprecated. See ?Coo")}

#' Coe class
#' 
#' \code{Coe} class is a super class for \link{OutCoe} and \link{OpnCoe} classes,
#' matrices of coefficients, along with other informations, obtained with
#' morphometrics methods on \code{\link{Out}} and \code{\link{Opn}} objects.
#' 
#' It allows to recycle most of the methods, since both of them apply on
#' matrices of coefficients. In other words, \code{OutCoe} and \code{OpnCoe}
#' classes are all, primarily, \code{Coe} objects on which we define generic 
#' \emph{and} specific methods.
#' 
#' More, generally, see \link{Opn} and \link{Out} for documentation on these classes.
#'
#' @param ... anything and, anyway, this function will simply returns a message.
#' @keywords Coe
#' @export
Coe <- function(...){
  cat(" * Coe constructor does not exist alone. See ?Coe")}

# allows to maintain the tradition str() behaviour
# actually useless but dont remember why/where
#' @export
str.Coo <- function(object, ...){
  Coo <- object
  ls.str(Coo)}
#' @export
str.Coe <- function(object, ...){
  Coe <- object
  ls.str(Coe)}

# Coo can be indexing both to [ ] and [[ ]]
# and returns the corresponding coordinate(s)
# We define some getters

#' @export
"[.Coo" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

#' @export
"[[.Coo" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

# length on an Coo return the length of Coo$coo, ie the number of coordinates
#' @export
length.Coo <- function(x) {
  Coo <- x
  return(length(Coo$coo))}
#' @export
dim.Coe <- function(x){
  return(dim(x$coe))}


# names() on a Coo retrieves the names of the Coo$coo
#' @export
names.Coo <- function(x){
  Coo <- x
  return(names(Coo$coo))}
#' @export
names.Coe <- function(x){
  Coe <- x
  return(rownames(Coe$coe))}

# which can in return may be named using names(Coo) <- 
#' @export
"names<-.Coo" <- function(x, value){
  names(x$coo) <- value
  return(x)}
#' @export
"names<-.Coe" <- function(x, value){
  rownames(x$coe) <- value
  return(x)}

#' Create subsets of Coo objects
#' 
#' Pretty useful in morphometrics. Imagine you have a \link{Out} or a \link{Opn}
#' object that combines several different \emph{groups}, 
#' whatever \emph{groups} are : species, views, etc. You may be interested in
#' doing separated analyses (even if you could combine them later), then this
#' function will ease the process. See the examples below.
#' @aliases subset
#' @param x a \code{Coo} object, i.e. \link{Out} or \link{Opn}
#' @param subset logical taken from the $fac slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @keywords Opn Coo
#' @examples
#' data(bot)
#' bot$fac
#' beers <- subset(bot, type=="beer")
#' whisk <- subset(bot, type=="whisky")
#' # or you may prefere indices
#' subset(bot, c(1, 13, 34, 37))
#' # and you can combine them :
#' data(olea)
#' olea$fac
#' subset(olea, domes=="cult" & view=="VL")
#' @export
# bloody dirty #todo
subset.Coo <- function(x, subset, ...){
  Coo <- x
  e <- substitute(subset)
  retain <- eval(e, Coo$fac, parent.frame())
  Coo2 <- Coo
  Coo2$coo <- Coo$coo[retain]
  if (length(Coo$ldk)>0) Coo2$ldk <- Coo$ldk[retain]
  if (ncol(Coo$fac)>0) {
    Coo2$fac <- Coo$fac
    Coo2$fac <- as.data.frame(Coo2$fac[retain, ])
    names(Coo2$fac) <- names(Coo$fac)
    Coo2$fac <- .refactor(Coo2$fac)
  }
  return(Coo2)}

#' @export
subset.OutCoe <- function(x, subset, ...){
  OutCoe <- x
  e <- substitute(subset)
  retain <- eval(e, OutCoe$fac, parent.frame())
  OutCoe2 <- OutCoe
  OutCoe2$coe <- OutCoe$coe[retain, ]
  if (ncol(OutCoe$fac)>0) {
    OutCoe2$fac <- OutCoe$fac
    OutCoe2$fac <- as.data.frame(OutCoe2$fac[retain, ])
    names(OutCoe2$fac) <- names(OutCoe$fac)
    OutCoe2$fac <- .refactor(OutCoe2$fac)
  }
  return(OutCoe2)}
