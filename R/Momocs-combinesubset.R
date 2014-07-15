##### Combining or subsetting Momocs' classes

# bloody dirty todo
#' Create subsets of Coo objects
#' 
#' Pretty useful in morphometrics. Imagine you have a \link{Coo} or a \link{Coe} object,
#' that combines several different \emph{groups}, whatever \emph{groups} are : species, views, etc.
#' You may be interested in doing separated analyses (even if you could combine them later), then this
#' function will ease the process. See the examples below.
#' @rdname subset.Coo
#' @param x a \code{Coo} or a \link{Coe} object
#' @param subset logical taken from the \code{$fac} slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @examples
#' data(bot)
#' bot$fac
#' beers <- subset(bot, type=='beer')
#' whisk <- subset(bot, type=='whisky')
#' # or you may prefere indices
#' subset(bot, c(1, 13, 34, 37))
#' # and you can combine them :
#' data(olea)
#' olea$fac
#' subset(olea, domes=='cult' & view=='VL')
#' @export
subset.Coo <- function(x, subset, ...) {
    Coo <- x
    e <- substitute(subset)
    retain <- eval(e, Coo$fac, parent.frame())
    Coo2 <- Coo
    Coo2$coo <- Coo$coo[retain]
    if (length(Coo$ldk) > 0) 
        Coo2$ldk <- Coo$ldk[retain]
    if (ncol(Coo$fac) > 0) {
        Coo2$fac <- Coo$fac
        Coo2$fac <- as.data.frame(Coo2$fac[retain, ])
        names(Coo2$fac) <- names(Coo$fac)
        Coo2$fac <- .refactor(Coo2$fac)
    }
    return(Coo2)
}

#' @rdname subset.Coo
#' @export
subset.Coe <- function(x, subset, ...) {
    Coe <- x
    e <- substitute(subset)
    retain <- eval(e, Coe$fac, parent.frame())
    Coe2 <- Coe
    Coe2$coe <- Coe$coe[retain, ]
    if (ncol(Coe$fac) > 0) {
        Coe2$fac <- Coe$fac
        Coe2$fac <- as.data.frame(Coe2$fac[retain, ])
        names(Coe2$fac) <- names(Coe$fac)
        Coe2$fac <- .refactor(Coe2$fac)
    }
    return(Coe2)
}

# merge method for Out objects (experimental)

#' Combine Out objects
#' 
#' @param ... a list of Out objects
#' @seealso \link{subset.Coo}
#' @rdname combine
#' @export
combine <- function(...) {
    UseMethod("combine")
}

#' @rdname combine
#' @export
combine.Out <- function(...) {
    args <- list(...)
    Out <- Out(do.call(c, lapply(args, function(x) c(x$coo))))
    Out$fac <- do.call("rbind", lapply(args, function(x) x$fac))
    if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
        Out$ldk <- do.call("rbind", lapply(args, function(x) x$ldk))
    }
    return(Out)
}

#' @rdname combine
#' @export
combine.Opn <- combine.Out

#' @rdname combine
#' @export
combine.OutCoe <- function(...) {
    args <- list(...)
    # Out <- Out(do.call( c, lapply( args, c )))
    coeS <- do.call("cbind", lapply(args, function(x) x$coe))
    facS <- args[[1]]$fac
    methodS <- do.call(c, lapply(args, function(x) x$method))
    normS <- do.call(c, lapply(args, function(x) x$norm))
    OutCoe <- OutCoe(coe = coeS, fac = facS, method = methodS, 
        norm = normS)
    return(OutCoe)
}

##### end subset-combine 
