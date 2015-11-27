##### Coo and Coe classes declarations, domestic functions and
##### helpers.

#' Coo class
#'
#' \code{Coo} class is the 'parent' class of
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
    cat(" * Coo constructor does not exist alone. See ?Coo")
}

#' Coe class
#'
#'\code{Coe} class is the 'parent' class of
#' \code{\link{OutCoe}}, \code{\link{OpnCoe}} and \code{LdkCoe} classes.
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

#' Validates Coo objects
#' No validation for S3 objects, so this method is a (very tolerant) attempt at checking
#' \link{Coo} objects, \link{Out}, \link{Opn} and \link{Ldk} objects.
#'
#' This methods checks \enumerate{
#'    \item if coordinates do all pass \link{coo_check}
#'    \item that \code{$fac}, if any, is a {data.frame}, with the same number of rows and coordinates.
#'    Copies the name of \code{$coo} to th rownames
#'    \item that \code{$ldk}, if any, has a the same number of landmarks, for all shapes
#' } and finally returns the \code{Coo}. This method is used in \code{Coo} constructor, and before applying
#' any method on thos object.
#' @param Coo any Coo object
#' @return a Coo object.
#' @examples
#' \dontrun{
#' validate(bot)
#' bot[12] <- NA
#' validate(bot)
#'
#' validate(hearts)
#' hearts$ldk[[4]] <- c(1, 2)
#' validate(hearts)
#' }
#' @export
validate <- function(Coo){
  UseMethod("validate")
}

#' @export
validate.Coo <- function(Coo){
  # checks coo
  Coo <- coo_check(Coo)
  n <- length(Coo$coo)
  # checks fac
  if (is.fac(Coo)) {
    .check(is.data.frame(Coo$fac),
           " $fac must be a data.frame")
    .check(identical(nrow(Coo$fac), n),
           " the number of rows in $fac must equal the number of shapes")
    # rename rows of fac
    rownames(Coo$fac) <- names(Coo)
  }
  # checks ldk if any
  if (is.ldk(Coo)){
    .check(identical(length(Coo$ldk), n),
           " the number of $ldk must equal the number of shapes")
    .check(length(unique(sapply(Coo$ldk, length)))==1,
           " the number of $ldk defined must be the same accross shapes")
  }
  Coo
}
#
# validate.Ldk <-
# if (is.cur(Ldk)){
#   sapply(L$cur[[1]], nrow)
# }

# str.* ----------------------
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

# class sub-printers --------
# Used in Coo/Coe printers
#'@export
.print.fac <- function(fac){
  nf <- ncol(fac)
  # here we print the number of classifiers
  if (nf == 0) {
    cat(" - $fac: No classifier defined in $fac\n")
  } else {
    if (nf<2) {
      cat(" - $fac:", nf, "classifier:\n")
    } else {
      cat(" - $fac:", nf, "classifiers:\n")}
    # here we print every classifier
    for (i in 1:nf) {
      if (is.numeric(fac[, i])){
        xi <- fac[, i]
        nas <- sum(is.na(xi))
        xi  <- xi[!is.na(xi)]
        xi.sum <- list(min=min(xi), med=median(xi), max=max(xi), mean=mean(xi), sd=sd(xi))
        xi.sum <- lapply(xi.sum, signif, 3)
        xi.sum$nas <- nas
        cat("     '", colnames(fac)[i], "'\t (numeric):\t ",
            #"min:", xi.sum$min,
            #", med:", xi.sum$med,
            #", max: ", xi.sum$max,
            #"mean:", xi.sum$mean,
            #", sd: ", xi.sum$sd,
            "mean: ", xi.sum$mean, ", sd: ", xi.sum$sd,
            ifelse(xi.sum$nas==0, ".\n", paste0(" (", xi.sum$nas, " NA).\n")), sep="")
      } else if (is.factor(fac[, i])){
        # case where the column is a factor
        lev.i <- levels(fac[, i])
        # cosmectics below
        if (sum(nchar(lev.i))>20){
          maxprint <- which(cumsum(nchar(lev.i))>20)[1]
          cat("     '", colnames(fac)[i], "'\t (factor - ", nlevels(fac[, i]), " lev.):\t ", paste(lev.i[1:maxprint], collapse=", "),
              " and ", length(lev.i) - maxprint, " more.\n", sep="")
        } else {
          cat("     '", colnames(fac)[i], "'\t (factor - ", nlevels(fac[, i]), " lev.):\t ", paste(lev.i, collapse=", "), ".\n", sep="")
        }
      } else {
        # any other case (eg logical)
        cat("     '", colnames(fac)[i], "'\t (", class(fac[, i]), ").\n", sep="")
      }
    }
  }
}

# component testers ---------
#' @export
is.fac   <- function(x) length(x$fac) > 0
#' @export
is.ldk   <- function(x) length(x$ldk) > 0
#' @export
is.cur   <- function(x) length(x$cur) > 0
#' @export
is.links <- function(x) is.matrix(x$links)



