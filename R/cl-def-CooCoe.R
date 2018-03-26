##### Coo and Coe classes declarations, domestic functions and
##### helpers.

#' Coo "super" class
#'
#' \code{Coo} class is the 'parent' or 'super' class of
#' \code{\link{Out}}, \code{\link{Opn}} and \code{\link{Ldk}} classes.
#'
#' Useful shortcuts are described below. See \code{browseVignettes("Momocs")} for
#' a detail of the design behind Momocs' classes.
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
#' Since all 'child classes' of them handle \eqn{(x; y)} coordinates among other generic methods,
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
#' @family classes
#' @examples
#' \dontrun{
#' # to see all methods for Coo objects.
#' methods(class='Coo')
#'
#' # to see all methods for Out objects.
#' methods(class='Out') # same for Opn and Ldk
#'
#' # Let's take an Out example. But all methods shown here
#' # work on Ldk (try on 'wings') and on Opn ('olea')
#' bot
#'
#' # Primarily a 'Coo' object, but also an 'Out'
#' class(bot)
#' inherits(bot, "Coo")
#' panel(bot)
#' stack(bot)
#' plot(bot)
#'
#' # Getters (you can also use it to set data)
#' bot[1] %>% coo_plot()
#' bot[1:5] %>% str()
#'
#' # Setters
#' bot[1] <- shapes[4]
#' panel(bot)
#'
#' bot[1:5] <- shapes[4:8]
#' panel(bot)
#'
#' # access the different components
#' # $coo coordinates
#' head(bot$coo)
#' # $fac grouping factors
#' head(bot$fac)
#' # or if you know the name of the column of interest
#' bot$type
#' # table
#' table(bot$fac)
#' # an internal view of an Out object
#' str(bot)
#'
#' # subsetting
#' # see ?filter, ?select, and their 'see also' section for the
#' # complete list of dplyr-like verbs implemented in Momocs
#'
#' length(bot) # the number of shapes
#' names(bot) # access all individual names
#' bot2 <- bot
#' names(bot2) <- paste0('newnames', 1:length(bot2)) # define new names
#' }
#' @export
Coo <- function(...) {
    message("Coo constructor does not exist alone. See ?Coo")
}

#' Coe "super" class
#'
#'\code{Coe} class is the 'parent' or 'super' class of
#' \code{\link{OutCoe}}, \code{\link{OpnCoe}}, \code{LdkCoe} and \code{TraCoe} classes.
#'
#' Useful shortcuts are described below. See \code{browseVignettes("Momocs")} for
#' a detail of the design behind Momocs' classes.
#'
#' @param ... anything and, anyway, this function will simply returns a message.
#'
#'@details
#' \code{Coe} class is the 'parent' class of the following 'child' classes \itemize{
#' \item \code{\link{OutCoe}} for coefficients from closed \bold{out}lines morphometrics
#' \item \code{\link{OpnCoe}} for coefficients from \bold{op}e\bold{n} outlines morphometrics
#' \item \code{LdkCoe} for coefficients from configuration of \bold{l}an\bold{d}mar\bold{k}s morphometrics.
#' }
#'
#' In other words, \code{\link{OutCoe}}, \code{\link{OpnCoe}} and \code{LdkCoe} classes
#' are all, primarily, \code{Coe} objects on which we define generic \emph{and}
#' specific methods. See their respective help pages for more help.
#'
#' You can access all the methods available for \code{Coe} objects
#' with \code{methods(class=Coe)}.
#'
#' @family classes
#' @examples
#' # to see all methods for Coe objects.
#' methods(class='Coe')
#' # to see all methods for OutCoe objects.
#' methods(class='OutCoe') # same for OpnCoe, LdkCoe, TraCoe
#'
#' bot.f<- efourier(bot, 12)
#' bot.f
#' class(bot.f)
#' inherits(bot.f, "Coe")
#'
#' # if you want to work directly on the matrix of coefficients
#' bot.f$coe
#'
#' #getters
#' bot.f[1]
#' bot.f[1:5]
#'
#' #setters
#' bot.f[1] <- 1:48
#' bot.f[1]
#'
#' bot.f[1:5] <- matrix(1:48, nrow=5, ncol=48, byrow=TRUE)
#' bot.f[1:5]
#'
#' # An illustration of Momocs desing. See also browseVignettes("Momocs")
#' op <- opoly(olea, 5)
#' op
#' class(op)
#' op$coe # same thing
#'
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wp
#' class(wp) # for Ldk methods, LdkCoe objects can also be considered as Coo objects
#' # so you can apply all Ldk methods available.
#' wp$coe # Procrustes aligned coordinates
#'
#' @export
Coe <- function(...) {
    message("Coe constructor does not exist alone. See ?Coe")
}

# print ----------

# The print method for Out objects
#' @export
print.Coo <- function(x, n=6, ...) {
  x <- verify(x)
  coo_nb <- length(x)
  if (coo_nb==0){
    cat("empty", class(x)[1])
    return()
  }
  ### Header
  if (is_Out(x)){
    what <- "outlines"
    cat("Out (outlines)\n")
  }
  if (is_Opn(x)){
    what <- "curves"
    cat("Opn (curves)\n")
  }
  if (is_Ldk(x)){
    what <- "landmarks"
    cat("Ldk (landmarks)\n")
  }
  coo_len <- sapply(x$coo, nrow)
  coo_closed <- sapply(x$coo, coo_is_closed)
  # number of outlines
  cat("  - ", coo_nb, " ", what, ", ",
      round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coords (in $coo)\n", sep="")
  # we print the fac
  #.print_fac(x$fac, n)
  paste0("  - ", ncol(x$fac), " classifiers (in $fac): \n") %>% cat
  # summary(x$fac) %>% print
  .print_fac(x$fac, n=n)
  .other_components(x)
  cat("\n")
}

# str.* ----------------------
# allows to maintain the traditionnal str() behaviour
# actually useless but dont remember why/where
#' @export
str.Coo <- function(object, ...) {
    Coo <- object
    utils::ls.str(Coo)
}
#' @export
str.Coe <- function(object, ...) {
    Coe <- object
    utils::ls.str(Coe)
}

# getters ---------------

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
    if (is.character(i)) {
      return(x$coo[[i]])
    }
}

#' @export
"[.Coe" <- function(x, i, ...) {
  if (missing(i)) {
    return(x$coe[])
  }
  if (is.integer(i)) {
    return(x$coe[i, ])
  }
  if (is.numeric(i)) {
    return(x$coe[i, ])
  }
  if (is.character(i)) {
    return(x$coe[i, ])
  }
}

# setters -------------------

#' @export
"[<-.Coo" <- function(x, i, ..., value) {
  if (is.integer(i)) {
    x$coo[i] <- value
    return(x)
  }
  if (is.numeric(i)) {
    x$coo[[i]] <- value
    return(x)
  }
  if (is.character(i)) {
    x$coo[[i]] <- value
    return(x)
  }
}

#' @export
"[<-.Coe" <- function(x, i, ..., value) {
  if (is.integer(i)) {
    x$coe[i, ] <- value
    return(x)
  }
  if (is.numeric(i)) {
    x$coe[i, ] <- value
    return(x)
  }
  if (is.character(i)) {
    x$coo[i, ] <- value
    return(x)
  }
}


# length.* --------------------

#' @export
length.Coo <- function(x) {
    Coo <- x
    return(length(Coo$coo))}

#' @export
length.Coe<- function(x) {
  Coe <- x
  return(nrow(Coe$coe))}

# dim.* ---------------------
#' @export
dim.Coo <- function(x) {
    return(length(Coo$coo))
}

#' @export
dim.Coe <- function(x) {
    return(dim(x$coe))
}

# names.* -------------------
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

# names<-.* -----------------
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

# $.* -----------------------

# $ can directly access to $fac colnames
#' @export
`$.Coo` <-
  function (x, name)
  {
    a <- x[[name]]
    if (!is.null(a))
      return(a)
    a <- x$fac[[name]]
    if (!is.null(a))
      return(a)
    a <- x[[name, exact = FALSE]]
    if (!is.null(a) && getOption("warnPartialMatchDollar", default = FALSE)) {
      names <- names(x)
      warning(gettextf("Partial match of '%s' to '%s' in data frame",
                       name, names[pmatch(name, names)]))
    }
    return(a)
  }

#' @export
`$.Coe` <- `$.Coo`

#' @export
`$.PCA` <- `$.Coo`
