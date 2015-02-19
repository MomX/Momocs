##### Combining or subsetting Momocs' classes

# subset -------------------------------
#' Create subsets of Coo objects
#'
#' 
#' Subset is a wrapper around dplyr's verbs and should not be used directly. 
#' Use \link{slice}, \link{filter}, \link{chop} and \link{select} instead.
#' 
#' \itemize{
#' \item \strong{select} is used to select columns from the $fac
#' \item \strong{slice} is used to select shapes based on their ids
#' \item \strong{chop} is a rougher slicing that accept a column of the $fac and return a list of Coo/Coe objects
#' \item \strong{filter} is used to create subset based on logical values taken from the $fac
#' \item \strong{mutate}
#' }
#' 
#' @rdname subset
#' @param x a \code{Coo} or a \link{Coe} object.
#' @param .data same
#' @param subset logical taken from the \code{$fac} slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @examples
#' data(olea)
#' ##### slice
#' slice(olea, 1) # if you only want the coordinates, try bot[1]
#' slice(olea, 1:20)
#' slice(olea, 21:30) 
#' 
#' ##### chop (a rougher slicing)
#' chop(olea, domes) # return a list that can be lapply-ed
#' 
#' ##### filter
#' filter(olea, domes=="cult")
#' filter(olea, domes=="cult", view=="VL")
#' 
#' ##### select
#' head(olea$fac)
#' select(olea, domes, view)
#' select(olea, -ind)
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

#' @rdname subset
#' @export
subset.Coe <- function(x, subset, ...) {
  Coe <- x
  e <- substitute(subset)
  retain <- eval(e, Coe$fac, parent.frame())
  Coe2 <- Coe
  Coe2$coe <- Coe$coe[retain, ]
  if (is.numeric(Coe2$coe)) Coe2$coe <- t(as.matrix(Coe2$coe)) # single shp case
  if (ncol(Coe$fac) > 0) {
    Coe2$fac <- Coe$fac
    Coe2$fac <- as.data.frame(Coe2$fac[retain, ])
    names(Coe2$fac) <- names(Coe$fac)
    Coe2$fac <- .refactor(Coe2$fac)
  }
  return(Coe2)
}

#' @rdname subset
#' @export
subset.PCA <- function(x, subset, ...){
  PCA <- x
  e <- substitute(subset)
  retain <- eval(e, PCA$fac, parent.frame())
  PCA2 <- PCA
  PCA2$x <- PCA$x[retain, ]
  if (ncol(PCA$fac) > 0) {
    PCA2$fac <- PCA$fac
    PCA2$fac <- as.data.frame(PCA2$fac[retain, ])
    names(PCA2$fac) <- names(PCA$fac)
    PCA2$fac <- .refactor(PCA2$fac)
  }
  return(PCA2)
}

# select -------------------------------
#' Select/rename (ala dplyr) variables by name, using the $fac slot on Momocs classes
#'
#' Extends this dplyr verb to \link{Coo} objects. See examples.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs should be maintained.
#' @note this method is quite experimental, check the result and do not hesitate to report a bug.
#' @return a Coo object
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
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
#' @rdname select
#' @examples
#' data(olea)
#' ##### slice
#' slice(olea, 1) # if you only want the coordinates, try bot[1]
#' slice(olea, 1:20)
#' slice(olea, 21:30) 
#' 
#' ##### chop (a rougher slicing)
#' chop(olea, domes) # return a list that can be lapply-ed
#' 
#' ##### filter
#' filter(olea, domes=="cult")
#' filter(olea, domes=="cult", view=="VL")
#' 
#' ##### select
#' head(olea$fac)
#' select(olea, domes, view)
#' select(olea, -ind)
#' 
#' @export
select <- function(.data, ...){
  UseMethod("select")
}
#' @rdname select
#' @export
select.default <- function(.data, ...){
  dplyr::select(.data, ...)
}
#' @rdname select
#' @export
select.Coo <- function(.data, ...){
  .data$fac <- select(.data$fac, ...)
  .data
}
#' @rdname select
#' @export
select.Coe <- select.Coo
#' @rdname select
#' @export
select.PCA <- select.Coo

# filter -------------------------------
#' Filter (ala dplyr) rows with matching conditions, using the $fac slot  on Momocs classes
#'
#' Extends this dplyr verb to \link{Coo} objects. See examples.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... Logical conditions
#' @details dplyr verbs are maintained.
#' @note this method is quite experimental, check the result and do not hesitate to report a bug.
#' @return a a \code{Coo}, \code{Coe}, \code{PCA} object
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @examples
#' data(olea)
#' ##### slice
#' slice(olea, 1) # if you only want the coordinates, try bot[1]
#' slice(olea, 1:20)
#' slice(olea, 21:30) 
#' 
#' ##### chop (a rougher slicing)
#' chop(olea, domes) # return a list that can be lapply-ed
#' 
#' ##### filter
#' filter(olea, domes=="cult")
#' filter(olea, domes=="cult", view=="VL")
#' 
#' ##### select
#' head(olea$fac)
#' select(olea, domes, view)
#' select(olea, -ind)
#' 
#' @rdname filter
#' @export
filter <- function(.data, ...){
  UseMethod("filter")
}
#' @rdname filter
#' @export
filter.default <- function(.data, ...){
  dplyr::filter(.data, ...)
}
#' @rdname filter
#' @export
filter.Coo <- function(.data, ...){
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- filter(df, ...)
  subset(.data, df$.id)
}
#' @rdname filter
#' @export
filter.Coe <- filter.Coo
#' @rdname filter
#' @export
filter.PCA <- filter.Coo


# slice ---------------------

#' Slice (ala dplyr) Momocs classes using rows ids on Momocs classes
#'
#' Extends this dplyr verb to \link{Coo} objects. See examples.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... Logical conditions
#' @details dplyr verbs are maintained.
#' @note this method is quite experimental, check the result and do not hesitate to report a bug.
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @return a \code{Coo}, \code{Coe}, \code{PCA} object
#' @examples
#' 
#' data(olea)
#' ##### slice
#' slice(olea, 1) # if you only want the coordinates, try bot[1]
#' slice(olea, 1:20)
#' slice(olea, 21:30) 
#' 
#' ##### chop (a rougher slicing)
#' chop(olea, domes) # return a list that can be lapply-ed
#' 
#' ##### filter
#' filter(olea, domes=="cult")
#' filter(olea, domes=="cult", view=="VL")
#' 
#' ##### select
#' head(olea$fac)
#' select(olea, domes, view)
#' select(olea, -ind)
#' @rdname slice
#' @export
slice <- function(.data, ...){
  UseMethod("slice")
}

#' @rdname slice
#' @export
slice.default <- function(.data, ...){
  dplyr::slice(.data, ...)
}


#' @rdname slice
#' @export
slice.Coo <- function(.data, ...){
  subset(.data, ...)}

#' @rdname slice
#' @export
slice.Coe <- function(.data, ...){
  subset(.data, ...)}

#' @rdname subset
#' @export
slice.PCA <- function(.data, ...){
  subset(.data, ...)}

# chop ----------------------

#' Rougher slicing that accepts a classifier ie a column name from the $fac on Momocs classes
#'
#' Returns a named (after every level) list that can be lapply-ed and combined. See examples.
#' @param .data a \code{Coo} or \code{Coe} object
#' @param fac a column name from the $fac
#' @return a named list of \code{Coo} or \code{Coe} objects
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @examples
#' data(bot)
#' @rdname chop
#' @export
chop <- function(.data, fac){
  UseMethod("chop")
}

#' @rdname chop
#' @export
chop.default <- function(.data, fac){
  warning("not defined")
}


#' @rdname chop
#' @export
chop.Coo <- function(.data, fac){
  Coo <- .data
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

#' @rdname chop
#' @export
chop.Coe <- function(.data, fac){
  Coe <- .data
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


# combine --------------------------------------
#' Combine Coo/Coe objects
#'
#' Combine \code{Coo} objects after a slicing, either manual or using \link{slice} or \link{chop}. Note that on Coo object,
#' it combines row-wise (ie, merges shapes) ; but on Coe it combines column-wise
#' (merges coefficients). In the latter case, Coe must ahve the same number of shapes (not
#' necessarily the same number of coefficients). Also the $fac of the first Coe is retrieved.
#' A separate version may come at some point.
#' @param ... a list of Out(Coe), Opn(Coe), Ldk objects (but of the same class)
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @rdname combine
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
#' @rdname combine
#' @export
combine.list <- function(...){
  do.call(combine, ...)
}
#' @rdname combine
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
  OpnCoe <- OpnCoe(coe = coeS, fac = facS, method = methodS,
                   baseline1=baseline1S, baseline2=baseline2S,
                   mod=modS, r2=r2S)
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  OpnCoe$cuts <- cutS
  return(OpnCoe)
}



##### end subset-combine
