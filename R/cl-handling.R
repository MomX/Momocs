##### Combining or subsetting Momocs' classes

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
#' select(olea, var, view) # drops domes and ind
#' select(olea, variety=var, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' \dontrun{
#' library(magrittr)
#' # only dorsal views, and 'var' and 'domes' columns
#' filter(olea, view=="VD") %>% select(var, domes)
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

# rename -------------------------------
#' Rename (ala dplyr) variables by name, using the $fac slot on Momocs classes
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
#' select(olea, var, view) # drops domes and ind
#' select(olea, variety=var, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' \dontrun{
#' library(magrittr)
#' # only dorsal views, and 'var' and 'domes' columns
#' filter(olea, view=="VD") %>% select(var, domes)
#' }
#' @rdname rename
#' @examples
#' data(olea)

#' @export
rename <- function(.data, ...){
  UseMethod("rename")
}
#' @rdname rename
#' @export
rename.default <- function(.data, ...){
  dplyr::rename(.data, ...)
}
#' @rdname rename
#' @export
rename.Coo <- function(.data, ...){
  .data$fac <- rename(.data$fac, ...)
  .data
}
#' @rdname rename
#' @export
rename.Coe <- rename.Coo

#' @rdname rename
#' @export
rename.PCA <- rename.Coo


# mutate -------------------------------
#' mutate (ala dplyr) variables by name, using the $fac slot on Momocs classes
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
#' select(olea, var, view) # drops domes and ind
#' select(olea, variety=var, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' \dontrun{
#' library(magrittr)
#' # only dorsal views, and 'var' and 'domes' columns
#' filter(olea, view=="VD") %>% select(var, domes)
#' }
#' @rdname mutate
#' @examples
#' data(bot)
#' # let's extract the centroid size and add it as a new variable
#' # this can be done in a single line:
#' bot2 <- mutate(bot, cs=sapply(bot$coo, coo_centsize))
#' bot2f <- efourier(bot2, 10)
#' bot2p <- PCA(bot2f)
#' plot2(bot2p, "cs")
#' @export
mutate <- function(.data, ...){
  UseMethod("mutate")
}
#' @rdname mutate
#' @export
mutate.default <- function(.data, ...){
  dplyr::mutate(.data, ...)
}
#' @rdname mutate
#' @export
mutate.Coo <- function(.data, ...){
  .data$fac <- mutate(.data$fac, ...)
  .data
}
#' @rdname mutate
#' @export
mutate.Coe <- mutate.Coo

#' @rdname mutate
#' @export
mutate.PCA <- mutate.Coo

# transmute -------------------------------
#' transmute (ala dplyr) variables by name, using the $fac slot on Momocs classes
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
#' select(olea, var, view) # drops domes and ind
#' select(olea, variety=var, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' \dontrun{
#' library(magrittr)
#' # only dorsal views, and 'var' and 'domes' columns
#' filter(olea, view=="VD") %>% select(var, domes)
#' }
#' @rdname transmute
#' @examples
#' data(bot)
#' # let's extract the centroid size and remove everything else
#' bot2 <- transmute(bot, cs=sapply(bot$coo, coo_centsize))
#' bot2f <- efourier(bot2, 10)
#' bot2p <- PCA(bot2f)
#' plot2(bot2p, "cs")
#' @export
transmute <- function(.data, ...){
  UseMethod("transmute")
}
#' @rdname transmute
#' @export
transmute.default <- function(.data, ...){
  dplyr::transmute(.data, ...)
}
#' @rdname transmute
#' @export
transmute.Coo <- function(.data, ...){
  .data$fac <- transmute(.data$fac, ...)
  .data
}
#' @rdname transmute
#' @export
transmute.Coe <- transmute.Coo

#' @rdname transmute
#' @export
transmute.PCA <- transmute.Coo

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

# arrange -------------------------------
#' Arrange (ala dplyr) rows with matching conditions, using the $fac slot on Momocs classes
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
#' head(olea$fac)
#' head(arrange(olea$fac, view, desc(domes)))
#' @rdname arrange
#' @export
arrange <- function(.data, ...){
  UseMethod("arrange")
}
#' @rdname arrange
#' @export
arrange.default <- function(.data, ...){
  dplyr::arrange(.data, ...)
}
#' @rdname arrange
#' @export
arrange.Coo <- function(.data, ...){
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- arrange(df, ...)
  subset(.data, df$.id)
}
#' @rdname arrange
#' @export
arrange.Coe <- arrange.Coo

#' @rdname arrange
#' @export
arrange.PCA <- arrange.Coo


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

# sample_n ---------------

#' Samples n shapes in Momocs objects
#'
#' Uses (and maintain) dplyr syntax and verb.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric how many shapes should we sample
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a $fac is defined; size is then applied within levels of this factor
#' @seealso \link{sample_frac}
#' @param ... additional arguments to dplyr::sample_n and to maintain generic compatibility
#' @examples
#'
#' data(bot)
#' bot
#' # samples 5 bottles no matter their type
#' sample_n(bot, 5)
#' # 5 bottles of beer and of whisky
#' table(sample_n(bot, 5, fac="type"))
#' # many repetitions
#' table(names(sample_n(bot, 400, replace=TRUE)))
#'
#' @rdname sample_n
#' @export
sample_n <- function(tbl, size, replace, ...){
  UseMethod("sample_n")
}

#' @rdname sample_n
#' @export
sample_n.default <- function(tbl, size, replace=FALSE, ...){
  dplyr::sample_n(tbl, size, replace, ...)
}

#' @rdname sample_n
#' @export
sample_n.Coo <- function(tbl, size, replace = FALSE, fac=NULL, ...){
  Coo <- tbl
  if (missing(fac)) {
    fac <- NULL
    N <- length(Coo)
    if (!replace & any(N)>size)
      stop(" * for at least one level, 'size' is too large for sampling without replacement")
  } else {
    fac <- Coo$fac[, fac]
    N <- table(fac)
    if (!replace & any(N)>size)
      stop(" * for at least one level, 'size' is too large for sampling without replacement")
  }

  if (is.null(fac)) {
    retain <- sample(N, size = size, replace = replace)
  } else {
    retain <- integer()
    for (i in seq(along=N)){
      x.i <- which(fac == levels(fac)[i])
      retain.i <- sample(x.i, size=size, replace=replace)
      retain <- append(retain, retain.i)
    }
  }
  #   return(retain)
  return(subset(Coo, retain))
}

#' @rdname sample_n
#' @export
sample_n.Coe <- sample_n.Coo

# sample_frac ---------------

#' Samples a fraction of shapes in Momocs objects
#'
#' Uses (and maintain) dplyr syntax and verb.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric (0 < numeric <= 1) the fraction of shapes to select
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a $fac is defined; size is then applied within levels of this factor
#' @note the resulting fraction is rounded with \link{ceiling}.
#' @seealso \link{sample_n}
#' @param ... additional arguments to dplyr::sample_frac and to maintain generic compatibility
#' @examples
#'
#' data(bot)
#' bot
#' # samples 50% of the bottles no matter their type
#' sample_frac(bot, 0.5)
#' # 80% bottles of beer and of whisky
#' table(sample_frac(bot, 0.8, fac="type"))
#' # bootstrap the same number of bootles of each type but with replacement
#' table(names(sample_frac(bot, 1, replace=TRUE)))
#'
#' @rdname sample_frac
#' @export
sample_frac <- function(tbl, size, replace, ...){
  UseMethod("sample_frac")
}

#' @rdname sample_frac
#' @export
sample_frac.default <- function(tbl, size=1, replace=FALSE, ...){
  dplyr::sample_frac(tbl, size, replace, ...)
}

#' @rdname sample_frac
#' @export
sample_frac.Coo <- function(tbl, size=1, replace = FALSE, fac=NULL, ...){
  if (size > 1 | size <= 0)
    stop(" * size must be >=0 and <= 1")
  Coo <- tbl
  if (missing(fac)) {
    fac <- NULL
    N <- length(Coo)
  } else {
    fac <- Coo$fac[, fac]
    N <- table(fac)
  }
  size <- ceiling(N * size)
  if (is.null(fac)) {
    retain <- sample(N, size = size, replace = replace)
  } else {
    retain <- integer()
    for (i in seq(along=N)){
      x.i <- which(fac == levels(fac)[i])
      retain.i <- sample(x.i, size=size[i], replace=replace)
      retain <- append(retain, retain.i)
    }
  }
  #   return(retain)
  return(subset(Coo, retain))
}

#' @rdname sample_frac
#' @export
sample_frac.Coe <- sample_frac.Coo



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
#' it combines row-wise (ie, merges shapes as a \code{c} would do) ; but on Coe it combines column-wise
#' (merges coefficients). In the latter case, Coe must have the same number of shapes (not
#' necessarily the same number of coefficients).
#' Also the $fac of the first Coe is retrieved.
#' A separate version may come at some point.
#' @param ... a list of Out(Coe), Opn(Coe), Ldk objects (but of the same class)
#' @note Note that the order of shapes or their coefficients
#' is not checked, so anything with the same number of rows will be merged.
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @rdname combine
#' @examples
#' data(bot)
#' w <- filter(bot, type=="whisky")
#' b <- filter(bot, type=="beer")
#' combine(w, b)
#' # or, if you have many levels
#' bot_s <- chop(bot, type)
#' bot_s$whisky
#' # note that you can apply something (single function or a more
#' # complex pipe) then combine everyone, since combine also works on lists
#' # eg:
#' # bot_s2 <- lapply(bot_s, efourier, 10)
#' # bot_sf <- combine(bot_s2)
#'
#' # pipe style
#' # library(magrittr)
#' # bot_sf <- lapply(bot_s, efourier, 10) %>% combine()
#' @export
combine <- function(...) {
  UseMethod("combine")
}
#' @rdname combine
#' @export
combine.list <- function(...){
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("* objects to combine must have the same number of items")
  do.call(combine, ...)
}
#' @rdname combine
#' @export
combine.Out <- function(...) {
  args <- list(...)
#   # we check
#   if (length(unique(sapply(args, length))) != 1)
#     stop("* objects to combine must have the same number of items")
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
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("* objects to combine must have the same number of items")
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
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("* objects to combine must have the same number of items")
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
  cutS <- do.call(c,  lapply(args, function(x) x$cuts))
  OutCoe$cuts <- cutS
  names(OutCoe$method) <- names(args)
  return(OutCoe)
}

#' @rdname combine
#' @export
combine.OpnCoe <- function(...) {
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("* objects to combine must have the same number of items")
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
  OpnCoe$cuts <- cutS
  return(OpnCoe)
}

# dissolve --------------------
#' Uncombine Coe objects
#'
#' After a combine for instance. But note that the $fac slot may be wrong since
#' combine...well combines... this $fac
#'
#' @param x a Coe object
#' @param retain the partition id to retain. Or their name if the partitions are named
#' (see x$method) eg after a chop
#'
#' @examples
#' data(bot)
#' w <- filter(bot, type=="whisky")
#' b <- filter(bot, type=="beer")
#' wf <- efourier(w, 10)
#' bf <- efourier(b, 10)
#' wbf <- combine(wf, bf)
#' dissolve(wbf, 1)
#' dissolve(wbf, 2)
#' @export
dissolve <- function(x, retain){
  UseMethod("dissolve")
}
#' @export
dissolve.default <- function(x, retain){
  stop("* only implemented on Coe objects so far")
}
#' @export
dissolve.Coe <- function(x, retain){
  if (length(x$method) < 2) return(x)
  partitions <- seq_along(x$method)
  if(is.character(retain)) retain <- match(retain, names(x$method))
  x2 <- x
  x2$norm   <- x$norm[retain]
  x2$method <- x$method[retain]
  x2$cuts   <- x$cuts[retain]
  x2$ldk    <- x2$ldk[retain]
  cols_begin <- cumsum(c(1, x$cuts))[1:length(x$cuts)]
  cols_end   <- cumsum(x$cuts)
  cols_retain <- numeric()
  for (i in retain){
    cols_retain <- append(cols_retain, cols_begin[i]:cols_end[i])
  }
  x2$coe <- x$coe[, cols_retain]
  return(x2)
}

##### end subset-combine
