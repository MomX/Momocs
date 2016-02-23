##### Combining or subsetting Momocs' classes

# select -------------------------------
#' Select (ala dplyr) on Momocs classes
#'
#' Select variables by name, from the \code{$fac}. See examples and \code{?dplyr::select}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
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
#' @examples
#' head(olea$fac)
#' # select some columns
#' select(olea, domes, view)
#' # remove some columns
#' select(olea, -ind)
#' # rename on the fly and select some columns
#' # you can use rename instead
#' select(olea, foo=domes)
#'
#' @export
select <- function(.data, ...){
  UseMethod("select")
}

#' @export
select.default <- function(.data, ...){
  dplyr::select(.data, ...)
}

#' @export
select.Coo <- function(.data, ...){
  #.data %<>% validate()
  .data$fac <- select(.data$fac, ...)
  .data
}

#' @export
select.Coe <- select.Coo

#' @export
select.PCA <- select.Coo

# rename -------------------------------
#' Rename (ala dplyr) on Momocs classes
#'
#' Rename variables by name, from the \code{$fac}. See examples and \code{?dplyr::rename}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' rename(olea, Ind=ind, View=view)
#' @export
rename <- function(.data, ...){
  UseMethod("rename")
}

#' @export
rename.default <- function(.data, ...){
  dplyr::rename(.data, ...)
}

#' @export
rename.Coo <- function(.data, ...){
  #.data %<>% validate()
  .data$fac <- rename(.data$fac, ...)
  .data
}

#' @export
rename.Coe <- rename.Coo

#' @export
rename.PCA <- rename.Coo


# mutate -------------------------------
#' mutate (ala dplyr) on Momocs classes
#'
#' Add new variables to the \code{$fac}. See examples and \code{?dplyr::mutate}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' mutate(olea, id=factor(1:length(olea)))
#' @export
mutate <- function(.data, ...){
  UseMethod("mutate")
}

#' @export
mutate.default <- function(.data, ...){
  dplyr::mutate(.data, ...)
}

#' @export
mutate.Coo <- function(.data, ...){
  #.data %<>% validate()
  .data$fac <- mutate(.data$fac, ...)
  .data
}

#' @export
mutate.Coe <- mutate.Coo


#' @export
mutate.PCA <- mutate.Coo

# transmute -------------------------------
#' transmute (ala dplyr) on Momocs classes
#'
#' Add new variables to the \code{$fac} and drop existing ones. See examples and \code{?dplyr::transmute}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' transmute(olea, id=factor(1:length(olea)))
#' @export
transmute <- function(.data, ...){
  UseMethod("transmute")
}

#' @export
transmute.default <- function(.data, ...){
  dplyr::transmute(.data, ...)
}

#' @export
transmute.Coo <- function(.data, ...){
  #.data %<>% validate()
  .data$fac <- transmute(.data$fac, ...)
  .data
}

#' @export
transmute.Coe <- transmute.Coo


#' @export
transmute.PCA <- transmute.Coo

# filter -------------------------------
#' filter (ala dplyr) on Momocs classes
#'
#' Return shapes with matching conditions, from the \code{$fac}. See examples and \code{?dplyr::filter}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... logical conditions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' # we retain on dorsal views
#' filter(olea, view=="VD")
#' # only dorsal views and Aglan+PicMa varieties
#' filter(olea, view=="VD", var %in% c("Aglan", "PicMa"))
#' # we create an id column and retain the 120 first shapes
#' olea %>% mutate(id=1:length(olea)) %>% filter(id > 120)
#' @export
filter <- function(.data, ...){
  UseMethod("filter")
}

#' @export
filter.default <- function(.data, ...){
  dplyr::filter(.data, ...)
}

#' @export
filter.Coo <- function(.data, ...){
  #.data %<>% validate()
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- filter(df, ...)
  subset(.data, df$.id)
}

#' @export
filter.Coe <- filter.Coo


#' @export
filter.PCA <- filter.Coo

# arrange -------------------------------
#' arrange (ala dplyr) on Momocs classes
#'
#' Arange shapes by variables, from the \code{$fac}. See examples and \code{?dplyr::arrange}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... logical conditions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' # we create a new column
#' olea %>% mutate(id=1:length(.)) %$% fac$id
#' # same but now, shapes are arranged in a desc order, based on id
#' olea %>% mutate(id=1:length(.)) %>% arrange(desc(id)) %$% fac$id
#' @export
arrange <- function(.data, ...){
  UseMethod("arrange")
}

#' @export
arrange.default <- function(.data, ...){
  dplyr::arrange(.data, ...)
}

#' @export
arrange.Coo <- function(.data, ...){
  #.data %<>% validate()
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- arrange(df, ...)
  subset(.data, df$.id)
}

#' @export
arrange.Coe <- arrange.Coo


#' @export
arrange.PCA <- arrange.Coo


# slice ---------------------

#' Slice (ala dplyr) on Momocs classes
#'
#' Select rows by position, based on \code{$fac}. See examples and \code{?dplyr::slice}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... logical conditions
#' @details dplyr verbs are maintained.
#' @family handling functions
#' @return a Momocs object of the same class.
#' @examples
#' olea
#' slice(olea, 1) # if you only want the coordinates, try bot[1]
#' slice(olea, 1:20)
#' slice(olea, 21:30)
#' @export
slice <- function(.data, ...){
  UseMethod("slice")
}

#' @export
slice.default <- function(.data, ...){
  dplyr::slice(.data, ...)
}

#' @export
slice.Coo <- function(.data, ...){
  #.data %<>% validate()
  subset(.data, ...)}

#' @export
slice.Coe <- function(.data, ...){
  subset(.data, ...)}

#' @export
slice.PCA <- function(.data, ...){
  subset(.data, ...)}

# sample_n ---------------

#' Samples n shapes in Momocs objects
#'
#' Sample n shapes from a Momocs object. See examples and \code{?dplyr::sample_n}.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric how many shapes should we sample
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a \code{$fac} is defined; size is then applied within levels of this factor
#' @param ... additional arguments to dplyr::sample_n and to maintain generic compatibility
#' @family handling functions
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
#' @export
sample_n <- function(tbl, size, replace, fac, ...){
  UseMethod("sample_n")
}

#' @export
sample_n.default <- function(tbl, size, replace=FALSE, ...){
  dplyr::sample_n(tbl, size, replace, ...)
}

#' @export
sample_n.Coo <- function(tbl, size, replace = FALSE, fac=NULL, ...){
  Coo <- tbl
  #Coo %<>% validate()
  if (missing(fac)) {
    fac <- NULL
    N <- length(Coo)
    if (!replace & any(N)>size)
      stop("for one level at least, 'size' is too large for sampling without replacement")
  } else {
    fac <- Coo$fac[, fac]
    N <- table(fac)
    if (!replace & any(N)>size)
      stop("for one level at least, 'size' is too large for sampling without replacement")
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

#' @export
sample_n.Coe <- sample_n.Coo

# sample_frac ---------------

#' Samples a fraction of shapes in Momocs objects
#'
#' Sample a fraction of shapes from a Momocs object. See examples and \code{?dplyr::sample_n}.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric (0 < numeric <= 1) the fraction of shapes to select
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a \code{$fac} is defined; size is then applied within levels of this factor
#' @note the resulting fraction is rounded with \link{ceiling}.
#' @param ... additional arguments to dplyr::sample_frac and to maintain generic compatibility
#' @family handling functions
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
#' @export
sample_frac <- function(tbl, size, replace, fac,...){
  UseMethod("sample_frac")
}

#' @export
sample_frac.default <- function(tbl, size=1, replace=FALSE, ...){
  dplyr::sample_frac(tbl, size, replace, ...)
}

#' @export
sample_frac.Coo <- function(tbl, size=1, replace = FALSE, fac=NULL, ...){
  if (size > 1 | size <= 0)
    stop("size must be >=0 and <= 1")
  Coo <- tbl
  #Coo %<>% validate()
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

#' @export
sample_frac.Coe <- sample_frac.Coo



# chop ----------------------

#' Rougher slicing that accepts a classifier ie a column name from the \code{$fac} on Momocs classes
#'
#' Returns a named (after every level) list that can be lapply-ed and combined. See examples.
#' @param .data a \code{Coo} or \code{Coe} object
#' @param fac a column name from the \code{$fac}
#' @return a named list of \code{Coo} or \code{Coe} objects
#' @family handling functions
#' @examples
#' data(bot)
#' @export
chop <- function(.data, fac){
  UseMethod("chop")
}

#' @export
chop.default <- function(.data, fac){
  warning("not defined")
}

#' @export
chop.Coo <- function(.data, fac){
  Coo <- .data
  #Coo %<>% validate()
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
    }
    res[[i]] <- Coo2
  }
  return(res)}

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
#' Also the \code{$fac} of the first Coe is retrieved.
#' A separate version may come at some point.
#' @param ... a list of Out(Coe), Opn(Coe), Ldk objects (but of the same class)
#' @note Note that the order of shapes or their coefficients
#' is not checked, so anything with the same number of rows will be merged.
#' @family handling functions
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
#' lapply(bot_s, efourier, 10) %>% combine()
#' @export
combine <- function(...) {
  UseMethod("combine")
}

#' @export
combine.list <- function(...){
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("objects to combine must have the same number of items")
  do.call(combine, ...)
}

#' @export
combine.Out <- function(...) {
  args <- list(...)
  #   # we check
  #   if (length(unique(sapply(args, length))) != 1)
  #     stop("objects to combine must have the same number of items")
  Out <- Out(do.call(c, lapply(args, function(x) c(x$coo))))
  Out$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  #Out$fac <- .refactor(Out$fac)
  if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
    Out$ldk <- do.call("c", lapply(args, function(x) x$ldk))
  }
  #Out %<>% validate()
  return(Out)
}

#' @export
combine.Opn <- combine.Out

#' @export
combine.Ldk <- function(...) {
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("objects to combine must have the same number of items")
  Ldk <- Ldk(do.call(c, lapply(args, function(x) c(x$coo))))
  Ldk$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  if (any(lapply(args, function(x) length(x$links)) != 0)) {
    Ldk$ldk <- do.call("c", lapply(args, function(x) x$links))
  }
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  Ldk$cuts <- cutS
  #Ldk %<>% validate()
  return(Ldk)
}

# #' @export
# combine.LdkCoe <- function(...) {
#   args <- list(...)
#   # we check
#   if (length(unique(sapply(args, length))) != 1)
#     stop("objects to combine must have the same number of items")
#   # Out <- Out(do.call( c, lapply( args, c )))
#   coeS <- do.call("cbind", lapply(args, function(x) x$coe))
#   facS <- args[[1]]$fac
#   # bloody dirty, todo
#   LdkCoe <- Ldk(args[[1]]$coo, fac=facS)
#   LdkCoe$coo <- coeS
#   class(LdkCoe) <- c("LdkCoe", class(LdkCoe))
#   return(LdkCoe)
# }


#' @export
combine.OutCoe <- function(...) {
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("objects to combine must have the same number of items")
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
  if (is.null(args[1]$cuts)) {
    cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  } else {
    cutS <- do.call(c,  lapply(args, function(x) x$cuts))
  }
  OutCoe$cuts <- cutS
  names(OutCoe$method) <- names(args)
  return(OutCoe)
}

#' @export
combine.OpnCoe <- function(...) {
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("objects to combine must have the same number of items")
  coeS <- do.call("cbind", lapply(args, function(x) x$coe))
  facS <- args[[1]]$fac
  methodS <- do.call(c, lapply(args, function(x) x$method))
  baseline1S <- do.call(c, lapply(args, function(x) x$baseline1))
  baseline2S <- do.call(c, lapply(args, function(x) x$baseline2))
  r2S <- do.call(c, lapply(args, function(x) x$r2))
  modS <- do.call(c, lapply(args, function(x) x$mod))
  if (is.null(args[[1]]$cuts)) {
    cutS <- do.call(c, lapply(args, function(x) ncol(x$coe)))
  } else {
    cutS <- do.call(c, lapply(args, function(x) x$cuts))
  }


  OpnCoe <- OpnCoe(coe = coeS, fac = facS, method = methodS,
                   baseline1=baseline1S, baseline2=baseline2S,
                   mod=modS, r2=r2S)
  OpnCoe$cuts <- cutS
  return(OpnCoe)
}

# dissolve --------------------
#' Uncombine Coe objects
#'
#' After a combine for instance. But note that the \code{$fac} slot may be wrong since
#' combine...well combines... this \code{$fac}
#'
#' @param x a Coe object
#' @param retain the partition id to retain. Or their name if the partitions are named
#' (see x$method) eg after a chop
#' @family handling functions
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
  stop("only implemented on Coe objects so far")
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
