##### Combining or subsetting Momocs' classes

# bloody dirty todo particularly for combine
# have to find a way to do it in a more generic way.

#' Create subsets of Coo objects
#'
#' Pretty useful in morphometrics. Imagine you have a \link{Coo} or a \link{Coe} object,
#' that combines several different \emph{groups}, whatever \emph{groups} are : species, views, etc.
#' You may be interested in doing separated analyses (even if you could combine them later), then this
#' function will ease the process. \code{subset} needs to be passed with a logical subset;
#' \code{slice} just needs the factor to use to make subsets. Not to be confunded with
#' the dplyr' slice verb. See the examples below.
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

# #' @rdname subset.Coo
# #' @export
# subset.PCA <- function(x, subset, ...){
#   PCA <- x
#   e <- substitute(subset)
#   retain <- eval(e, PCA$fac, parent.frame())
#   PCA2 <- PCA
#   PCA2$x <- PCA$x[retain, ]
#   if (ncol(PCA$fac) > 0) {
#     PCA2$fac <- PCA$fac
#     PCA2$fac <- as.data.frame(PCA2$fac[retain, ])
#     names(PCA2$fac) <- names(PCA$fac)
#     PCA2$fac <- .refactor(PCA2$fac)
#   }
#   return(PCA2)
# }

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
#' e.g. after a slicing, either manual or using \link{slice}. Note that on Coo object,
#' it combines row-wise (ie, merges shapes) ; but on Coe it combines column-wise
#' (merges coefficients). In the latter case, Coe must ahve the same number of shapes (not
#' necessarily the same number of coefficients). Also the $fac of the first Coe is retrieved.
#' A separate version may come at some point.
#' @param ... a list of Out(Coe), Opn(Coe), Ldk objects (but of the same class)
#' @seealso \link{subset.Coo}
#' @rdname combine.Coo
#' @examples
#' data(bot)
#' w <- subset(bot, type=="whisky")
#' b <- subset(bot, type=="beer")
#' combine(w, b)
#' # or, if you have many levels
#' bot_s <- slice(bot, type)
#'
#'
#' @export
combine <- function(...) {
  UseMethod("combine")
}
#' @rdname combine.Coo
#' @export
combine.list <- function(...){
  do.call(combine, ...)
}
#' @rdname combine.Coo
#' @export
combine.Out <- function(...) {
  args <- list(...)
  Out <- Out(do.call(c, lapply(args, function(x) c(x$coo))))
  Out$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  #Out$fac <- .refactor(Out$fac)
  if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
    Out$ldk <- do.call("c", lapply(args, function(x) x$ldk))
  }
  return(Out)
}

#' @rdname combine.Coo
#' @export
combine.Opn <- combine.Out

#' @rdname combine.Coo
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

#' @rdname combine.Coo
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

#' @rdname combine.Coo
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
  OpnCoe <- OpnCoe(coe = coeS, fac = facS, method = methodS,
                   baseline1=baseline1S, baseline2=baseline2S,
                   mod=modS, r2=r2S)
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  OpnCoe$cuts <- cutS
  return(OpnCoe)
}

#' Select/rename (ala dplyr) variables by name, using the $fac slot
#'
#' Extends this dplyr verb to \link{Coo} objects. See examples.
#' @param .data a \code{Coo} objects
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs should be maintained.
#' @note this method is quite experimental, check the result and do not hesitate to report a bug.
#' @return a Coo object
#' @seealso \link{filter.Coo}
#' @examples
#' data(olea)
#' olea
#' select(olea, cep, view) # drops domes and ind
#' select(olea, cepage=cep, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' \dontrun{
#' library(magrittr)
#' # only dorsal views, and 'cep' and 'domes' columns
#' filter(olea, view=="VD") %>% select(cep, domes)
#' }
#' @rdname select.Coo
#' @export
select <- function(.data, ...){
  UseMethod("select")
}
#' @rdname select.Coo
#' @export
select.default <- function(.data, ...){
  dplyr::select(.data, ...)
}
#' @rdname select.Coo
#' @export
select.Coo <- function(.data, ...){
  .data$fac <- select(.data$fac, ...)
  .data
}
#' @rdname select.Coo
#' @export
select.Coe <- select.Coo
#' @rdname select.Coo
#' @export
select.PCA <- select.Coo

#' Filter (ala dplyr) rows with matching conditions, using the $fac slot
#'
#' Extends this dplyr verb to \link{Coo} objects. See examples.
#' @param .data a \code{Coo} objects
#' @param ... Logical conditions
#' @details dplyr verbs are maintained.
#' @note this method is quite experimental, check the result and do not hesitate to report a bug.
#' @return a Coo object
#' @seealso \link{select.Coo}
#' @examples
#' data(bot)
#' filter(bot, type=="whisky")
#' @rdname filter.Coo
#' @export
filter <- function(.data, ...){
  UseMethod("filter")
}
#' @rdname filter.Coo
#' @export
filter.default <- function(.data, ...){
  dplyr::filter(.data, ...)
}
#' @rdname filter.Coo
#' @export
filter.Coo <- function(.data, ...){
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- filter(df, ...)
  subset(.data, df$.id)
}
#' @rdname filter.Coo
#' @export
filter.Coe <- filter.Coo
#' @rdname filter.Coo
#' @export
filter.PCA <- filter.Coo

##### end subset-combine
