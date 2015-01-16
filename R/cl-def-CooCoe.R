##### Coo and Coe classes declarations, domestic functions and
##### helpers.

# 1. Coo -----------------------------------------------

#' Coo class
#'
#' \code{Coo} class is the 'parent' class of 
#' \code{\link{Out}}, \code{\link{Opn}} and \code{\link{Ldk}} classes.
#' 
#' @param ... anything and, anyway, this function will simply returns a message.
#' 
#'@details
#' \code{Coo} class is the 'parent' class of the following 'child' classes \itemize{
#' \item \code{\link{Out}} for closed \bold{out}lines
#' \item \code{\link{Opn}} for \bold{op}e\bold{n} outlines
#' \item \code{\link{Ldk}} for configuration of \bold{l}an\bold{d}mar\bold{k}s
#' }
#'
#' Since all 'child classes' of them handle $(x; y)$ coordinates among other generic methods,
#' but also all have their specificity, this architecture allow to recycle generic methods and
#' to use specific methods.
#'
#' In other words, \code{\link{Out}}, \code{\link{Opn}} and \code{\link{Ldk}} classes
#' are all, primarily, \code{Coo} objects on which we define generic \emph{and}
#' specific methods. See their respective help pages for more help.
#'
#' You can access all the methods available for \code{Coo} objects 
#' with \code{methods(class=Coo)}.
#'
#' @note
#' If you used Momocs before version <0.9, and/or if you have read the JSS paper,
#' you are probably looking for \link{Out}. If you have "old" \code{Coo} files, e.g. saved as \code{.rda} files,
#' no worry, you can import them:
#' \enumerate{
#' \item load your file: \code{load("foo.rda")}
#' It may produce an error but the \code{.rda} has been loaded (see \code{ls()})
#' \item type: \code{foo2 <- Out(foo@@coo, fac=foo@@fac)}, 
#' same thing for the \code{@@ldk} slot and others, if any.
#'}
#' @examples
#' \dontrun{
#' # to see all methods for Coo objects.
#' methods(class='Coo')
#' # Let's take an Out example. But all methods shown here
#' # work on Ldk (try data(wings) ) and on Opn (try data(olea))
#' data(bot)
#' # Primarily a 'Coo' object, but also an 'Out'
#' class(bot)
#' inherits(bot, "Coo")
#' panel(bot)
#' stack(bot)
#' plot(bot)
#' # Getters (you can also use it to set data)
#' bot[1]
#' # access the different components
#' # $coo coordinates
#' head(bot$coo)
#' # $fac grouping factors
#' head(bot$fac)
#' # table
#' table(bot$fac)
#' # an internal view of an Out object
#' str(bot)
#' # subsetting
#' w <- subset(bot, type=='whisky') # if you dont like beer
#' b <- subset(bot, type=='beer')   # if you don't like whisky
#' w # an example of Momocs:::print.Out
#' b # same thing for beers
#' combine(b, w) # if, eventually, you want to mix them
#' length(bot) # the number of shapes
#' names(bot) # access all individual names
#' bot2 <- bot
#' names(bot2) <- paste0('newnames', 1:length(bot2)) # define new names
#' }
#' @export
Coo <- function(...) {
    cat(" * Coo constructor does not exist alone. See ?Coo")
}

#' Coe class
#'
#'\code{Coe} class is the 'parent' class of 
#' \code{\link{OutCoe}}, \code{\link{OpnCoe}} and \code{\link{LdkCoe}} classes.
#' 
#' @param ... anything and, anyway, this function will simply returns a message.
#' 
#'@details
#' \code{Coe} class is the 'parent' class of the following 'child' classes \itemize{
#' \item \code{\link{OutCoe}} for coefficients from closed \bold{out}lines morphometrics
#' \item \code{\link{OpnCoe}} for coefficients from \bold{op}e\bold{n} outlines morphometrics
#' \item \code{\link{LdkCoe}} for coefficients from configuration of \bold{l}an\bold{d}mar\bold{k}s morphometrics.
#' }
#' 
#' In other words, \code{\link{OutCoe}}, \code{\link{OpnCoe}} and \code{\link{LdkCoe}} classes
#' are all, primarily, \code{Coe} objects on which we define generic \emph{and}
#' specific methods. See their respective help pages for more help.
#'
#' You can access all the methods available for \code{Coe} objects 
#' with \code{methods(class=Coe)}.
#'
#' @examples
#' # to see all methods for Coo objects.
#' methods(class='Coe')
#'
#' data(bot)
#' bot.f<- efourier(bot, 12)
#' bot.f
#' class(bot.f)
#' inherits(bot.f, "Coe")
#'
#' # if you want to work directly on the matrix of coefficients
#' bot.f$coe
#'
#' data(olea)
#' op <- opoly(olea, 5)
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
Coe <- function(...) {
    cat(" * Coe constructor does not exist alone. See ?Coe")
}

# allows to maintain the traditionnal str() behaviour
# actually useless but dont remember why/where
#' @export
str.Coo <- function(object, ...) {
    Coo <- object
    ls.str(Coo)
}
#' @export
str.Coe <- function(object, ...) {
    Coe <- object
    ls.str(Coe)
}

# Coo can be indexing both to [ ] and [[ ]] and returns the
# corresponding coordinate(s) We define some getters

#' @export
"[.Coo" <- function(x, i, ...) {
    if (missing(i)) {
        return(x$coo[])
    }
    if (is.integer(i)) {
        return(x$coo[i])
    }
    if (is.numeric(i)) {
        return(x$coo[[i]])
    }
}


#' @export
length.Coo <- function(x) {
    Coo <- x
    return(length(Coo$coo))}

#' @export
dim.Coo <- function(x) {
    return(length(Coo$coo))
}

#' @export
dim.Coe <- function(x) {
    return(dim(x$coe))
}

# names() on a Coo retrieves the names of the Coo$coo
#' @export
names.Coo <- function(x) {
    Coo <- x
    return(names(Coo$coo))
}
#' @export
names.Coe <- function(x) {
    Coe <- x
    return(rownames(Coe$coe))
}

# which can in return may be named using names(Coo) <-
#' @export
"names<-.Coo" <- function(x, value) {
    names(x$coo) <- value
    return(x)
}
#' @export
"names<-.Coe" <- function(x, value) {
    rownames(x$coe) <- value
    return(x)
}

# utils ######

is.fac <- function(x) length(x$fac) > 0



