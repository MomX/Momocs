##### Combining or subsetting Momocs' classes

# bloody dirty todo particularly for combine
# have to find a way to do it in a more generic way.

#' Create subsets of Coo objects
#'
#' Pretty useful in morphometrics. Imagine you have a \link{Coo} or a \link{Coe} object,
#' that combines several different \emph{groups}, whatever \emph{groups} are : species, views, etc.
#' You may be interested in doing separated analyses (even if you could combine them later), then this
#' function will ease the process. \code{subset} needs to be passed with a logical subset;
#' \code{slice} just needs the factor to use to make subsets. See the examples below.
#' @rdname subset.Coo
#' @param x a \code{Coo} or a \link{Coe} object.
#' @param subset logical taken from the \code{$fac} slot, or indices. See examples.
#' @param fac the colum name in \code{fac} to use to slice your \code{Coo} or \code{Coe}.
#' @param ... useless here but maintains consistence with the generic subset.
#' @examples
#' data(bot)
#' bot$fac
#' ##### subset
#' # beers
#' subset(bot, type=='beer')
#' # whiskys
#' subset(bot, type=='whisky')
#' # or you may prefere indices
#' subset(bot, c(1, 13, 34, 37))
#' ##### and you can combine them :
#' data(olea)
#' olea$fac
#' subset(olea, domes=='cult' & view=='VL')
#' ##### slice
#' slice(bot, "type")
#' bp <- efourier(bot, 10)
#' slice(bp, type)
#' slice(olea, domes)
#' 
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

#' @rdname subset.Coo
#' @export
slice <- function(x, fac){
  UseMethod("slice")
}

#' @rdname subset.Coo
#' @export
slice.Coo <- function(x, fac){
  Coo <- x
  # hideous but works
  e <- substitute(fac)
  f <- eval(e, Coo$fac, parent.frame())
  fl <- levels(f)
  res <- list()
  for (i in fl) {
    Coo2 <- Coo
    retain <- which(f == i)
    Coo2$coo <- Coo$coo[retain]
    if (length(Coo$ldk) > 0) 
      Coo2$ldk <- Coo$ldk[retain]
    if (ncol(Coo$fac) > 0) {
      Coo2$fac <- Coo$fac
      Coo2$fac <- as.data.frame(Coo2$fac[retain, ])
      names(Coo2$fac) <- names(Coo$fac)
      Coo2$fac <- .refactor(Coo2$fac)
    }
    res[[i]] <- Coo2
  }
  return(res)}

#' @rdname subset.Coo
#' @export
slice.Coe <- function(x, fac){
  Coe <- x
  # hideous but works
  e <- substitute(fac)
  f <- eval(e, Coe$fac, parent.frame())
  fl <- levels(f)
  res <- list()
  for (i in fl) {
    Coe2 <- Coe
    retain <- which(f == i)
    Coe2$coe <- Coe$coe[retain, ]
    if (ncol(Coe$fac) > 0) {
      Coe2$fac <- Coe$fac
      Coe2$fac <- as.data.frame(Coe2$fac[retain, ])
      names(Coe2$fac) <- names(Coe$fac)
      Coe2$fac <- .refactor(Coe2$fac)
    }
    res[[i]] <- Coe2
  }
  return(res)}


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
  Out$fac <- .refactor(Out$fac)
  if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
    Out$ldk <- do.call("c", lapply(args, function(x) x$ldk))
  }
  return(Out)
}

#' @rdname combine
#' @export
combine.Opn <- combine.Out

#' @rdname combine
#' @export
combine.Ldk <- function(...) {
  args <- list(...)
  Ldk <- Ldk(do.call(c, lapply(args, function(x) c(x$coo))))
  Ldk$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  if (any(lapply(args, function(x) length(x$links)) != 0)) {
    Ldk$ldk <- do.call("c", lapply(args, function(x) x$links))
  }
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  Ldk$cuts <- cutS
  return(Ldk)
}


#' @rdname combine
#' @export
combine.OutCoe <- function(...) {
  args <- list(...)
  # Out <- Out(do.call( c, lapply( args, c )))
  coeS <- do.call("cbind", lapply(args, function(x) x$coe))
  facS <- args[[1]]$fac
  methodS <- do.call(c, lapply(args, function(x) x$method))
  normS <- do.call(c, lapply(args, function(x) x$norm))
  OutCoe <- OutCoe(coe = coeS, fac = facS, method = methodS, norm = normS)
  # bloody dirty #todo todo todo
  opn.i <- which(lapply(args, function(x) class(x)[1])=="OpnCoe")
  if (length(opn.i)>0) { 
    opn.i <- opn.i[1]
    OutCoe$baseline1 <- args[[opn.i]]$baseline1
    OutCoe$baseline2 <- args[[opn.i]]$baseline2
  }
 
    cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
    OutCoe$cuts <- cutS
    return(OutCoe)
  }
  
  #' @rdname combine
  #' @export
  combine.OpnCoe <- function(...) {
    args <- list(...)
    coeS <- do.call("cbind", lapply(args, function(x) x$coe))
    facS <- args[[1]]$fac
    methodS <- do.call(c, lapply(args, function(x) x$method))
    baseline1S <- do.call(c, lapply(args, function(x) x$baseline1))
    baseline2S <- do.call(c, lapply(args, function(x) x$baseline2))
    r2S <- do.call(c, lapply(args, function(x) x$r2))
    modS <- do.call(c, lapply(args, function(x) x$mod))
    OpnCoe <- OpnCoe(coe = coeS, fac = facS, method = methodS, baseline1=baseline1S, baseline2=baseline2S, mod=modS, r2=r2S)
    cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
    OpnCoe$cuts <- cutS
    return(OpnCoe)
  }
  
  ##### end subset-combine
  