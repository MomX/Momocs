##### Coo and Coe classes declarations, domestic functions and helpers.

# 0. Coo (Opn/Out/Ldk) domestics -----------------------------------------------

#' Coo class
#' 
#' \code{Coo} class is a "super class" for \link{Out}, \link{Opn} and \link{Ldk} classes,
#' for handling \bold{closed outlines}, \bold{open outlines} and \bold{configuration of landmarks}, respectively.
#' 
#' This class allows to recycle most of the methods, since all of them apply on
#' (x; y) coordinates stored \code{$coo} slot. All of them also have a \code{$fac} component,
#' yet not mandatory, to store grouping informations (also called classifiers). Others slots
#' may be present in certain classes.
#' 
#' In other words, \code{Out}, \code{Out} and \code{Ldk} classes
#' are all, primarily, \code{Coo} objects on which we define generic \emph{and} 
#' specific methods.
#' 
#' More, generally, see \link{Out}, \link{Opn} and \link{Ldk} for documentation on these classes.
#' And Momocs' vignette for more detailed informations on the Momocs' "architecture".
#' 
#' Finally, note that if you used Momocs before version <0.9, and/or if you have read the JSS paper,
#' you are probably looking for \link{Out}.
#' 
#' You can access all the methods available for Coo objects with \code{methods(class=Coo)}.
#' Among them, some are not individually documented but you will find an exhaustive list
#' below, as well as in individual  Momocs' vignettes.
#' 
#' @param ... anything and, anyway, this function will simply returns a message.
#' @examples
#' # to see all methods for Coo objects.
#' methods(class="Coo")
#' # Let's take an Out example. But all methods shown here
#' # work on Ldk (try data(wings) ) and on Opn (try data(olea))
#' data(bot)
#' # Primarily a "Coo" objects, but also an "Out"
#' class(bot)
#' panel(bot)
#' stack(bot)
#' \dontrun{
#' plot(bot)
#' }
#' # Getters (you can also use it to set data)
#' bot[1] # equivalent to bot[[1]]
#' # access the different components
#' # $coo coordinates
#' bot$coo
#' # $fac grouping factors
#' bot$fac
#' # table
#' table(bot$fac)
#' # an internal view of an Out object
#' str(bot)
#' # subsetting
#' w <- subset(bot, type=="whisky") # if you dont like beer
#' b <- subset(bot, type=="beer")   # if you don't like whisky
#' w # an example of Momocs:::print.Out
#' b # same thing for beers
#' combine(b, w) # if, eventually, you want to mix them
#' length(bot) # the number of shapes
#' names(bot) # access all individual names
#' bot2 <- bot
#' names(bot2) <- paste0("newnames", 1:length(bot2)) # define new names
#' @export
Coo <- function(...){
  cat(" * Coo constructor has been deprecated. You may be looking for Out(), Opn)() or Ldk(). See ?Coo")}

#' Coe class
#' 
#' \code{Coe} class is a "super class" for \link{OutCoe} \link{OpnCoe} and \link{LdkCoe} classes,
#' matrices of coefficients (in their \code{$coe} slot), along with other informations,
#' (e.g. an inherited \code{$fac}) obtained with morphometrics methods on 
#' \link{Out}, \link{Opn} and \link{Ldk} objects.
#' 
#' This classes allows to recycle most of the methods, since both of them apply on
#' matrices of coefficients. In other words, \code{OutCoe}, \code{OpnCoe} and \code{LdkCoe}
#' classes are all, primarily, \code{Coe} objects on which we define generic 
#' \emph{and} specific methods.
#' 
#' All these classes contain a \code{$coe} slot where the coefficients are stored,
#' and can be accessed.
#' 
#' More, generally, see \link{OutCoe}, \link{OpnCoe} and \link{LdkCoe}
#' for documentation on these classes.
#'
#' @param ... anything and, anyway, this function will simply returns a message.
#' @examples
#' # to see all methods for Coo objects.
#' methods(class="Coe")
#' 
#' data(bot)
#' bot.f<- eFourier(bot, 12)
#' bot.f
#' class(bot.f)
#' 
#' # if you want to work directly on the matrix of coefficients
#' bot.f$coe
#' 
#' data(olea)
#' op <- orthoPolynomials(olea, 5)
#' op
#' class(op)
#' op$coe # same thing
#' 
#' data(wings)
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wp
#' class(wp) # for Ldk methods, LdkCoe objects can also be considered as Coo objects
#' # so you can apply all Ldk methods available.
#' wp$coe # Procrustes aligned coordinates
#' 
#' @export
Coe <- function(...){
  cat(" * Coe constructor does not exist alone. See ?Coe")}

# allows to maintain the traditionnal str() behaviour
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
length.Coo <- function(x){
  Coo <- x
  return(length(Coo$coo))}
#' @export
dim.Coo <- function(x){
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
