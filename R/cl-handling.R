##### Combining or subsetting Momocs' classes


# rw_rule ---------------------
#' Rename levels with a rewriting rule
#' rw_rule stands for 'rewriting rule'. Typically useful to correct typos
#' at the import, or merge some levels within covariates. Drops levels silently.
#'
#' @param x any Momocs object
#' @param fac the id of the name of the $fac column to look for
#' @param from which level should be renamed
#' @param to which name ?
#' @return a Momocs object of the same type
#' @examples
#' data(bot)
#' rw_rule(bot, "type", "whisky", "agua_de_fuego") # 1 instead of "type" is fine too
#' @export
rw_rule <- function(x, fac, from, to){
  new_levels <- unique(c(levels(x$fac[, fac]), to))
  fac2 <- factor(x$fac[, fac], levels = new_levels)
  fac2[which(fac2==from)] <- to
  x$fac[, fac] <- droplevels(fac2)
  x}


# at_least ------------------------
#' Retains group with at least a certain number of individuals within
#'
#' Title and examples are self-speaking.
#'
#' @param x any Momocs object
#' @param y the id of name of the $fac column
#' @param N minimal number of individuals to retain the group
#' @note if N is too ambitious the original object is returned with a message
#' @examples
#' data(trilo)
#' table(trilo, "onto")
#' at_least(trilo, "onto", 9)
#' at_least(trilo, "onto", 16)
#' at_least(trilo, "onto", 2000) # too ambitious !
#' @export
at_least <- function(x, fac, N){
  retain <- x$fac[, fac] %in% names(which(table(x$fac[, fac]) >= N))
  if (!any(retain)) {
    cat(" * No group with at least", N, "indidivuals!\n\n")
    return(x)
  } else {
    subset(x, retain)
  }
}



# table --------------------------
#' Cross tabulation of Momocs objects
#'
#' Simply extends base \link{table} for a more convenient use on $fac slot.
#'
#' @param ... a list of, first, a Momocs object (Coo, Coe, PCA, etc.), then, column names in the $fac slot. If not specified,
#' returns a table on the entire $fac data.frame
#'
#' @examples
#' data(bot)
#' table(bot)
#' data(olea)
#' table(olea, "var", "domes")
#' table(olea)
#' @rdname table
#' @export
table <- function(...){
  UseMethod("table")
}

#' @rdname table
#' @export
table.default <- function(...){
  base::table(...)
}

#' @rdname table
#' @export
table.Coo <- function(...){
  args <- list(...)
  #    return(args)
  x <- args[[1]]
  if (length(x$fac)==0) stop(" * no $fac defined")
  if (length(args)>1) {
    # a little helper for mismatched colnames
    cn <- unlist(args[-1])
    matches <- match(cn, colnames(x$fac))
    if (any(is.na(matches))) {
      mispelled <- which(is.na(matches))
      stop(" * '", cn[mispelled], "' mispelled or not defined in $fac")
    }
    matches <- match(cn, names(x$fac))
    # single line avoids a title to be printed for the table
    base::table(x$fac[, unlist(args[-1])])
  } else {
    base::table(x$fac)
  }
}

#' @rdname table
#' @export
table.Coe <- table.Coo

#' @rdname table
#' @export
table.PCA <- table.Coo

#' @rdname table
#' @export
table.LDA <- table.Coo


# subset -------------------------------
#' Create subsets of Coo objects
#'
#'
#' Subset is a wrapper around dplyr's verbs and should not be used directly.
#' Use \link{slice}, \link{filter}, \link{chop} and \link{select} instead.
#'
#' \itemize{
#' \item \strong{rename} is used to rename columns from the $fac
#' \item \strong{mutate} is used to add new columns to the $fac
#' \item \strong{select} is used to select columns from the $fac
#' \item \strong{slice} is used to select shapes based on their ids
#' \item \strong{chop} is a rougher slicing that accept a column of the $fac and return a list of Coo/Coe objects
#' \item \strong{filter} is used to create subset based on logical values taken from the $fac
#' \item \strong{combine} not a dplyr verb but useful to combine after a subsetting
#' }
#'
#' @rdname subset
#' @param x a \code{Coo} or a \link{Coe} object.
#' @param .data same
#' @param subset logical taken from the \code{$fac} slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @seealso \link{select}, \link{filter}, \link{slice}, \link{chop}, \link{combine}.
#' @examples
#' # Do not use subset directly
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
  #   if (is.numeric(Coe2$coe)) Coe2$coe <- t(as.matrix(Coe2$coe)) # single shp case
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



#######

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
#' it combines row-wise (ie, merges shapes) ; but on Coe it combines column-wise
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
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("* objects to combine must have the same number of items")
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
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  OutCoe$cuts <- cutS
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
  cutS <- do.call(c,  lapply(args, function(x) ncol(x$coe)))
  OpnCoe$cuts <- cutS
  return(OpnCoe)
}
#' Removes shapes with incomplete slices
#'
#' Imagine you take three views of every object you study. Then, you can \link{slice},
#' \link{filter} or \link{chop} your entire dataset, do morphometrics on it,
#' then want to \link{combine} it. But if you have forgotten one view, or if it
#' was impossible to obtain, for one or more objects, combine will not work.
#' This function helps you to remove those ugly ducklings. See examples
#' @param x the object on which to remove uncomplete "by"
#' @param id of the objects, within the $fac slot
#' @param by which column of the $fac should objects have complete views
#' @examples
#' # we load olea
#' data(olea)
#' # we select the var Aglan since it is the only one complete
#' ol <- filter(olea, var == "Aglan")
#' # everything seems fine
#' table(ol, "view", "ind")
#' # indeed
#' rm_uncomplete(ol, id="ind", by="view")
#'
#' # we mess the ol object by removing a single shape
#' ol.pb <- slice(ol, -1)
#' table(ol.pb, "view", "ind")
#' # the counterpart has been removed with a notice
#' ol.ok <- rm_uncomplete(ol.pb, "ind", "view")
#' # now you can combine them
#' table(ol.ok, "view", "ind")
#' @export
rm_uncomplete <- function(x, id, by){
  tab <- table(x$fac[, c(id, by)])
  nb <- rowSums(tab)
  nb.u <- unique(nb)
  nb.tab <- table(nb)
  if (length(nb.u) == 1) {
    cat("* all ids have", nb.u, "slices\n")
    return(x)
  } else {
    most_frequent <- as.numeric(names(nb.tab[which.max(nb.tab)]))
    ugly_ducklings <- names(which(nb != most_frequent))
    cat("* those shapes did not have", most_frequent, "slices and has been removed:",
        paste(ugly_ducklings, collapse=", "), "\n")
    return(subset(x, ! id %in% ugly_ducklings))
  }
}

##### end subset-combine
