##### Combining or subsetting Momocs' classes

# fac_dispatcher
#' Brew and serve fac from Momocs object
#'
#' Ease various specifications for fac specification
#' when passed to Momocs objects. Intensively used (internally).
#'
#' `fac` can be:
#'  * a factor, passed on the fly
#'  * a column id from `$fac`
#'  * a column name from `fac`; if not found, return `NULL` with a message
#'  * a formula in the form: `~column_name` (from `$fac`, no quotes).
#'  It expresses more in a concise way. Also allows interacting on the fly. See examples.
#'  * a `NULL` returns a `NULL`, with a message
#'
#'
#' @param x a Momocs object (any `Coo`, `Coe`, `PCA`, etc.)
#' @param fac a specification to extract from `fac`
#'
#' @return a prepared `factor` (or a `numeric`). See examples
#' @family handling functions
#' @examples
#'
#' bot <- mutate(bot, s=rnorm(40), fake=factor(rep(letters[1:4], 10)))
#'
#' # factor, on the fly
#' fac_dispatcher(bot, factor(rep(letters[1:4], 10)))
#'
#' # column id
#' fac_dispatcher(bot, 1)
#'
#' # column name
#' fac_dispatcher(bot, "type")
#' # same, numeric case
#' fac_dispatcher(bot, "s")
#'
#' # formula interface
#' fac_dispatcher(bot, ~type)
#'
#' # formula interface + interaction on the fly
#' fac_dispatcher(bot, ~type+fake)
#'
#' # when passing NULL or non existing column
#' fac_dispatcher(42, NULL)
#' fac_dispatcher(bot, "loser")
#' @export
fac_dispatcher <- function(x, fac){
  # same reasonning than for exit
  # allow to pass fac=NULL
  if (is.null(fac))
    return(NULL)
  # factor case
  if (is.factor(fac))
    return(fac)
  # formula case
  if (inherits(fac, "formula")) {
    column_name <- attr(terms(fac), "term.labels")
    if (any(is.na(match(column_name, colnames(x$fac)))))
      stop("formula provided must match with $fac column names")
    fac <- x$fac[, column_name]

    # now we have a data.frame (and even a data_frame) all the time
    if (is.data.frame(fac) && ncol(fac) < 2){
      fac <- unlist(fac)
      if (is.character(fac)){
        message("factor passed was a character, and coerced to a factor.")
        fac <- factor(fac)
      }
    } else {
      fac <- factor(apply(fac, 1, paste, collapse = "_"))
    }

    return(fac)
  }
  # column case as character
  if (is.character(fac) && length(fac)==1) {
    if (!(fac %in% colnames(x$fac))){
      message("not a valid column specification, returning NULL")
      return(NULL)
    }
    fac <- x$fac[, fac]
    if (is.data.frame(fac)) #dplyr data_frame do not drop
      fac <- unlist(fac)
    return(fac)
  }
  # column case as numeric for column id
  if (is.numeric(fac) && length(fac)==1){
    if (fac > ncol(x$fac))
      stop("invalid column id")
    return(x$fac[, fac] %>% unlist)
  }
  # other case, return NULL
  # important for Momecs, but I think generally
  message("not a valid fac_dispatcher specification, returning NULL")
  return(NULL)
}


# subsetize -------------------------------
#' Subsetize various Momocs objects
#'
#'
#' Subsetize is a wrapper around dplyr's verbs and should NOT be used directly.
#'
#' @rdname subset
#' @param x a \code{Coo} or a \link{Coe} object.
#' @param subset logical taken from the \code{$fac} slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @family handling functions
#' @return a subsetted object of same class
#' @examples
#' # Do not use subset directly
#' @export
subsetize <- function(x, subset, ...){
  UseMethod("subsetize")
}

#' @export
subsetize.Coo <- function(x, subset, ...) {
  Coo <- x
  e <- substitute(subset)
  retain <- eval(e, Coo$fac, parent.frame())
  Coo2 <- Coo
  Coo2$coo <- Coo$coo[retain]
  if (is_ldk(Coo))
    Coo2$ldk <- Coo$ldk[retain]
  if (is_fac(Coo)) {
    #     if (is.logical(retain))
    #       retain <- which(retain)
    Coo2$fac <- Coo$fac[retain, ]
    # bloody dirty case where a factor is returned
    if (ncol(Coo$fac)==1 & is.factor(Coo2$fac)) {
      Coo2$fac <- tibble::as_tibble(Coo2$fac)
    }
    names(Coo2$fac) <- names(Coo$fac)
    Coo2$fac %<>% tibble::as_tibble()
    # Coo2$fac %<>% .refactor()
  }
  return(Coo2)
}

#' @export
subsetize.Coe <- function(x, subset, ...) {
  Coe <- x
  e <- substitute(subset)
  retain <- eval(e, Coe$fac, parent.frame())
  Coe2 <- Coe
  Coe2$coe <- Coe$coe[retain,, drop=FALSE ]
  #   if (is.numeric(Coe2$coe)) Coe2$coe <- t(as.matrix(Coe2$coe)) # single shp case
  if (ncol(Coe$fac) > 0) {
    Coe2$fac <- Coe$fac[retain,, drop=FALSE]
    # bloody dirty case where a factor is returned
    if (ncol(Coe$fac)==1 & is.factor(Coe2$fac)) {
      Coe2$fac <- tibble::as_tibble(Coe2$fac)
    }
    names(Coe2$fac) <- names(Coe$fac)
    Coe2$fac %<>% tibble::as_tibble()
    # Coe2$fac %<>% .refactor()
  }
  return(Coe2)
}

#' @export
subsetize.PCA <- function(x, subset, ...){
  PCA <- x
  e <- substitute(subset)
  retain <- eval(e, PCA$fac, parent.frame())
  PCA2 <- PCA
  PCA2$x <- PCA$x[retain, ]
  if (ncol(PCA$fac) > 0) {
    PCA2$fac <- PCA$fac
    PCA2$fac <- as.data.frame(PCA2$fac[retain, ])
    names(PCA2$fac) <- names(PCA$fac)
    #PCA2$fac %<>% tibble::as_tibble
    # PCA2$fac %<>% .refactor()
  }
  return(PCA2)
}

# select -------------------------------
#' Select columns by name
#'
#' Select variables by name, from the \code{$fac}. Selected variables
#' can also be renamed on the fly.
#' See examples and \code{?dplyr::select}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' select(olea, var, view) # drops domes and ind
#' select(olea, variety=var, domesticated_status=domes, view)
#' # combine with filter with magrittr pipes
#' # only dorsal views, and 'var' and 'domes' columns
#' filter(olea, view=="VD") %>% select(var, domes)
#' @examples
#' head(olea$fac)
#' # select some columns
#' select(olea, domes, view)
#' # remove some columns
#' select(olea, -ind)
#' # rename on the fly and select some columns
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
  #.data %<>% verify()
  .data$fac <- select(.data$fac, ...) %>% tibble::as_tibble()
  .data
}

#' @export
select.Coe <- select.Coo

#' @export
select.PCA <- select.Coo


# rename -------------------------------
#' Rename columns by name
#'
#' Rename variables, from the \code{$fac}. See examples and [dplyr::rename].
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... comma separated list of unquoted expressions
#' @details dplyr verbs are maintained.
#' @return a Momocs object of the same class.
#' @family handling functions
#' @examples
#' olea
#' rename(olea, variety=var, domesticated=domes) # rename var column
#'
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
  #.data %<>% verify()
  .data$fac <- rename(.data$fac, ...) %>% tibble::as_tibble()
  .data
}

#' @export
rename.Coe <- rename.Coo

#' @export
rename.PCA <- rename.Coo


# mutate -------------------------------
#' Add new variables
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
  #.data %<>% verify()
  .data$fac <- mutate(.data$fac, ...) %>% tibble::as_tibble()
  .data
}

#' @export
mutate.Coe <- mutate.Coo


#' @export
mutate.PCA <- mutate.Coo


# filter -------------------------------
#' Subset based on conditions
#'
#' Return shapes with matching conditions, from the \code{$fac}. See examples and \code{?dplyr::filter}.
#' @param .data a \code{Coo}, \code{Coe}, \code{PCA} object
#' @param ... logical conditions
#' @details dplyr verbs are maintained. You should probbaly not filter on PCA objects.
#' The latter are calculated using all individuals and filtering may lead to false conclusions.
#' If you want to highlith some individuals, see examples in [plot_PCA].
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
  #.data %<>% verify()
  df <- .data$fac
  df <- dplyr::mutate(df, .id=1:nrow(df))
  df <- dplyr::filter(df, ...)
  .data <- subsetize(.data, df$.id)
  .data$fac %<>% tibble::as_tibble()
  # .data$fac %<>% .refactor()
  .data
}

#' @export
filter.Coe <- filter.Coo


#' @export
filter.PCA <- filter.Coo

# arrange -------------------------------
#' Arrange rows by variables
#'
#' Arrange shapes by variables, from the \code{$fac}. See examples and \code{?dplyr::arrange}.
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
  #.data %<>% verify()
  df <- .data$fac
  df <- mutate(df, .id=1:nrow(df))
  df <- arrange(df, ...)
  .data <- subsetize(.data, df$.id)
  .data$fac %<>% tibble::as_tibble()
  .data
}

#' @export
arrange.Coe <- arrange.Coo


#' @export
arrange.PCA <- arrange.Coo


# slice ---------------------

#' Subset based on positions
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
  #.data %<>% verify()
  .data %<>% subsetize(...)
  # .data$fac %<>% .refactor()
  .data
  }

#' @export
slice.Coe <- function(.data, ...){
  .data %<>% subsetize(...)
  # .data$fac %<>% .refactor()
  .data
  }

#' @export
slice.PCA <- function(.data, ...){
  .data %<>% subsetize(...)
  # .data$fac %<>% .refactor()
  .data
  }

# sample_n ---------------

#' Sample n shapes
#'
#' Sample n shapes from a Momocs object. See examples and \code{?dplyr::sample_n}.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric how many shapes should we sample
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a \code{$fac} is defined; size is then applied within levels of this factor
#' @param ... additional arguments to dplyr::sample_n and to maintain generic compatibility
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#'
#' # samples 5 bottles no matter their type
#' sample_n(bot, 5)
#' # 5 bottles of beer and of whisky
#' table(sample_n(bot, 5, fac="type")$type)
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
  if (size==0) return(slice(Coo, 0))
  #Coo %<>% verify()
  if (missing(fac)) {
    fac <- NULL
    N <- length(Coo)
    if (!replace & any(N)>size)
      stop("for one level at least, 'size' is too large for sampling without replacement")
  } else {
    fac <- fac_dispatcher(Coo, fac)
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
  return(subsetize(Coo, retain))
}

#' @export
sample_n.Coe <- sample_n.Coo

# sample_frac ---------------

#' Sample a fraction of shapes
#'
#' Sample a fraction of shapes from a Momocs object. See examples and \code{?dplyr::sample_n}.
#'
#' @param tbl a Momocs object (Coo, Coe)
#' @param size numeric (0 < numeric <= 1) the fraction of shapes to select
#' @param replace logical whether sample should be done with ot without replacement
#' @param fac a column name if a \code{$fac} is defined; size is then applied within levels of this factor
#' @note the resulting fraction is rounded with \link{ceiling}.
#' @param ... additional arguments to dplyr::sample_frac and to maintain generic compatibility
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#'
#' # samples 50% of the bottles no matter their type
#' sample_frac(bot, 0.5)
#' # 80% bottles of beer and of whisky
#' table(sample_frac(bot, 0.8, fac="type")$fac)
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
  if (size > 1 | size < 0)
    stop("size must be >0 and <= 1")
  Coo <- tbl
  if (size==0) return(slice(Coo, 0))
  #Coo %<>% verify()
  if (missing(fac)) {
    fac <- NULL
    N <- length(Coo)
  } else {
    fac <- fac_dispatcher(Coo, fac)
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
  return(subsetize(Coo, retain))
}

#' @export
sample_frac.Coe <- sample_frac.Coo



# chop ----------------------

#' Split to several objects based on a factor
#'
#' Rougher slicing that accepts a classifier
#' ie a column name from the \code{$fac} on Momocs classes.
#' Returns a named (after every level) list that can be lapply-ed and combined. See examples.
#'
#' @param .data a \code{Coo} or \code{Coe} object
#' @param fac a column name from the \code{$fac}
#' @return a named list of \code{Coo} or \code{Coe} objects
#' @family handling functions
#' @examples
#'  olea %>%
#'       filter(var == "Aglan") %>% # to have a balanced nb of 'view'
#'       chop(~view) %>%    # split into a list of 2
#'       npoly %>%          # separately apply npoly
#'                          # strict equivalent to lapply(npoly)
#'       combine %>%       # recombine
#'       PCA %>% plot      # an illustration of the 2 views
#'       # treated separately
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
  #Coo %<>% verify()
  # hideous but works
  # e <- substitute(fac)
  # f <- eval(e, Coo$fac, parent.frame())
  if (!is.factor(fac))
    f <- fac_dispatcher(.data, fac)
  else
    f <- fac
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
      Coo2$fac <- tibble::as_tibble(Coo2$fac[retain, ])
      names(Coo2$fac) <- names(Coo$fac)
    }
    res[[i]] <- Coo2
  }
  return(res)
  }

#' @export
chop.Coe <- function(.data, fac){
  Coe <- .data
  if (!is.factor(fac))
    f <- fac_dispatcher(.data, fac)
  else
    f <- fac
  fl <- levels(f)

  res <- list()
  for (i in fl) {
    Coe2 <- Coe
    retain <- which(f == i)
    Coe2$coe <- Coe$coe[retain, ]
    if (ncol(Coe$fac) > 0) {
      Coe2$fac <- Coe$fac
      Coe2$fac <- tibble::as_tibble(Coe2$fac[retain, ])
      names(Coe2$fac) <- names(Coe$fac)
    }
    res[[i]] <- Coe2
  }
  return(res)
  }


# combine --------------------------------------
#' Combine several objects
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
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#' w <- filter(bot, type=="whisky")
#' b <- filter(bot, type=="beer")
#' combine(w, b)
#' # or, if you have many levels
#' bot_s <- chop(bot, ~type)
#' bot_s$whisky
#' # note that you can apply something (single function or a more
#' # complex pipe) then combine everyone, since combine also works on lists
#' # eg:
#' # bot_s2 <- efourier(bot_s, 10) # equivalent to lapply(bot_s, efourier, 10)
#' # bot_sf <- combine(bot_s2)
#'
#' # pipe style
#' efourier(bot_s, 10) %>% combine()
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
  Out$fac %<>% tibble::as_tibble()
  if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
    Out$ldk <- do.call("c", lapply(args, function(x) x$ldk))
  }
  # ensure $fac is a data_frame
  Out$fac %<>% tibble::as_tibble()
  #Out %<>% verify()
  return(Out)
}

#' @export
combine.Opn <- function(...) {
  args <- list(...)
  #   # we check
  #   if (length(unique(sapply(args, length))) != 1)
  #     stop("objects to combine must have the same number of items")
  Opn <- Opn(do.call(c, lapply(args, function(x) c(x$coo))))
  Opn$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  Opn$fac %<>% tibble::as_tibble()
  if (any(lapply(args, function(x) length(x$ldk)) != 0)) {
    Opn$ldk <- do.call("c", lapply(args, function(x) x$ldk))
  }
  # ensure $fac is a data_frame
  Opn$fac %<>% tibble::as_tibble()
  #Opn %<>% verify()
  return(Opn)
}

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

  # ensure $fac is a data_frame
  Ldk$fac %<>% tibble::as_tibble()

  #Ldk %<>% verify()
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
  facS <- args[[1]]$fac %>% tibble::as_tibble()
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

  # ensure $fac is a data_frame
  OutCoe$fac %<>% tibble::as_tibble()

  return(OutCoe)
}

#' @export
combine.OpnCoe <- function(...) {
  args <- list(...)
  # we check
  if (length(unique(sapply(args, length))) != 1)
    stop("objects to combine must have the same number of items")
  coeS <- do.call("cbind", lapply(args, function(x) x$coe))
  facS <- args[[1]]$fac %>% tibble::as_tibble()
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

  # ensure $fac is a data_frame
  OpnCoe$fac %<>% tibble::as_tibble()


  return(OpnCoe)
}

# dissolve --------------------
#' Dissolve Coe objects
#'
#' the opposite of combine, typically used after it. Note that the \code{$fac} slot may be wrong since
#' combine...well combines... this \code{$fac}. See examples.
#'
#' @param x a Coe object
#' @param retain the partition id to retain. Or their name if the partitions are named
#' (see x$method) eg after a chop
#' @return a Momocs object of same class
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
#'
#' # or using chop (yet combine here makes no sense)
#' bw <- bot %>% chop(~type) %>% lapply(efourier, 10) %>% combine
#' bw %>% dissolve(1)
#' bw %>% dissolve(2)
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

# # #' # table --------------------------
# # #' # #' Cross tables
# #' # #'
# #' # #' Simply extends base \link{table} for a more convenient use on $fac slot.
# #' # #'
# #' # #' @param ... a list of, first, a Momocs object (Coo, Coe, PCA, etc.), then, column names in the $fac slot. If not specified,
# #' # #' returns a table on the entire $fac data.frame
# #' # #'
# #' # #' @family handling functions
# #' # #' @examples
# #' # #' table(olea, "var", "domes")
# #' # #' table(olea)
# #' # #' @rdname table
# #' # #' @export
# #' table <- function(...){
# #'   UseMethod("table")
# #' }
# #'
# #' # #' @rdname table
# #' # #' @export
# #' table.default <- function(...){
# #'   base::table(...)
# #' }
# #'
# #' # #' @rdname table
# #' # #' @export
# #' table.Coo <- function(...){
# #'   args <- list(...)
# #'   #    return(args)
# #'   x <- args[[1]]
# #'   if (!is_fac(x)) stop("no $fac defined")
# #'   if (length(args)>1) {
# #'     # a little helper for mismatched colnames
# #'     cn <- unlist(args[-1])
# #'     matches <- match(cn, colnames(x$fac))
# #'     if (any(is.na(matches))) {
# #'       mispelled <- which(is.na(matches))
# #'       stop(cn[mispelled], "' mispelled or not defined in $fac")
# #'     }
# #'     matches <- match(cn, names(x$fac))
# #'     # single line avoids a title to be printed for the table
# #'     base::table(x$fac[, unlist(args[-1])])
# #'   } else {
# #'     base::table(x$fac)
# #'   }
# #' }
# #'
# #' # #' @rdname table
# #' # #' @export
# #' table.Coe <- table.Coo
# #'
# #' # #' @rdname table
# #' # #' @export
# #' table.PCA <- table.Coo
# #'
# #' # #' @rdname table
# #' # #' @export
# #' table.LDA <- table.Coo


# rw_fac ---------------------
# TODO replace with forcats
#' Renames levels on Momocs objects
#'
#' rw_fac stands for 'rewriting rule'. Typically useful to correct typos
#' at the import, or merge some levels within covariates. Drops levels silently.
#'
#' @param x any Momocs object
#' @param fac the id of the name of the $fac column to look for ([fac_dispatcher] not yet supported)
#' @param from which level(s) should be renamed; passed as a single or several characters
#' @param to which name should be used to rename this/these levels
#' @return a Momocs object of the same class
#' @family handling functions
#' @examples
#' # single renaming
#' rw_fac(bot, "type", "whisky", "agua_de_fuego")$type # 1 instead of "type" is fine too
#' # several renaming
#' bot2 <- mutate(bot, fake=factor(rep(letters[1:4], 10)))
#' rw_fac(bot2, "fake", c("a", "e"), "ae")$fake
#' @export
rw_fac <- function(x, fac, from, to){
  fac2 <- fac_dispatcher(x, fac)
  levels(fac2) <- c(to, levels(fac2))
  for (i in seq_along(from)) {
    fac2[which(fac2==from[i])] <- to
  }
  x$fac[, fac] <- droplevels(fac2)
  x
}

# at_least ------------------------
#' Retain groups with at least n shapes
#'
#' Examples are self-speaking.
#'
#' @param x any Momocs object
#' @param fac the id of name of the $fac column
#' @param N minimal number of individuals to retain the group
#' @note if N is too ambitious the original object is returned with a message
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#' table(trilo$onto)
#' at_least(trilo, "onto", 9)
#' at_least(trilo, "onto", 16)
#' at_least(trilo, "onto", 2000) # too ambitious !
#' @export
at_least <- function(x, fac, N){
  n <- fac_dispatcher(x, fac)
  retain <- n %in% names(which(table(n) >= N))
  if (!any(retain)) {
    message("no group with at least ", N, " indidivuals")
    return(slice(x, 0))
  } else {
    subsetize(x, retain)
  }
}

# rm ------------
#' Remove shapes with incomplete slices
#'
#' Imagine you take three views of every object you study. Then, you can \link{slice},
#' \link{filter} or \link{chop} your entire dataset, do morphometrics on it,
#' then want to \link{combine} it. But if you have forgotten one view, or if it
#' was impossible to obtain, for one or more objects, combine will not work.
#' This function helps you to remove those ugly ducklings. See examples
#' @param x the object on which to remove uncomplete "by"
#' @param id of the objects, within the $fac slot
#' @param by which column of the $fac should objects have complete views
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#' # we load olea
#' data(olea)
#' # we select the var Aglan since it is the only one complete
#' ol <- filter(olea, var == "Aglan")
#' # everything seems fine
#' table(ol$view, ol$ind)
#' # indeed
#' rm_uncomplete(ol, id="ind", by="view")
#'
#' # we mess the ol object by removing a single shape
#' ol.pb <- slice(ol, -1)
#' table(ol.pb$view, ol.pb$ind)
#' # the counterpart has been removed with a notice
#' ol.ok <- rm_uncomplete(ol.pb, "ind", "view")
#' # now you can combine them
#' table(ol.ok$view, ol.ok$ind)
#' @export
rm_uncomplete <- function(x, id, by){
  tab <- table(x$fac[, c(id, by)])
  nb <- rowSums(tab)
  nb.u <- unique(nb)
  nb.tab <- table(nb)
  if (length(nb.u) == 1) {
    message("all ids have ", nb.u, " slices")
    return(x)
  } else {
    most_frequent <- as.numeric(names(nb.tab[which.max(nb.tab)]))
    ugly_ducklings <- names(which(nb != most_frequent))
    remove_rows <- which(x$fac[, id] %in% ugly_ducklings)
    message("those shapes did not have ", most_frequent,
            " slices and has been removed: ",
            paste(ugly_ducklings, collapse=", "))
    return(subsetize(x, -remove_rows))
  }
}

# rm ------------
#' Remove shapes with missing data in fac
#'
#' Any row (or within a given column if `by` is specified) containing `NA` in `$fac` and the corresponding shapes in `$coo`, lines in `$coe` or other objects
#' will also be dropped.
#' @param x the object on which to NA
#' @param by which column of the $fac should objects have complete views
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#' bot$fac$type[3] <- NA
#' bot$fac$fake[9] <- NA
#'
#' bot %>% length()
#' bot %>% rm_missing() %>% length
#' bot %>% rm_missing("fake") %>% length()
#' @export
rm_missing <- function(x, by){
  UseMethod("rm_missing")
}
#' @export
rm_missing.default <- function(x, by){
  .check(is_fac(x),
         "no fac to filter with")
  if (missing(by))
    filter(x, !apply(is.na(x$fac), 1, any))
  else
    filter(x, !is.na(x$fac[[by]]))
}

#' Removes harmonics from Coe objects
#'
#' Useful to drop harmonics on Coe objects. Should only work for
#' Fourier-based approached since it looks for \code{[A-D][1-drop]} pattern.
#'
#' @param x Coe object
#' @param drop numeric number of harmonics to drop
#' @return a Momocs object of same class
#' @family handling functions
#' @examples
#' data(bot)
#' bf <- efourier(bot)
#' colnames(rm_harm(bf, 1)$coe)
#' @export
rm_harm <- function(x, drop=1){
  if (drop==0 | !is.numeric(drop)) return(x)
  regex <- paste0("[A-D][1-", drop,"]")
  x$coe <- x$coe[, -grep(regex, colnames(x$coe))]
  x
}

# rescale ------------------

#' Rescale coordinates from pixels to real length units
#'
#' Most of the time, (x, y) coordinates are recorded in pixels. If we want to have
#' them in mm, cm, etc. we need to convert them and to rescale them. This functions
#' does the job for the two cases: i) either an homogeneous rescaling factor,
#' e.g. if all pictures were taken using the very same magnification or ii) with various
#' magnifications. More in the Details section
#'
#' @param x any \code{Coo} object
#' @param scaling_factor numeric an homogeneous scaling factor. If all you (x, y) coordinates
#' have the same scale
#' @param scale_mapping either a data.frame or a path to read such a data.frame. It MUST contain
#' three columns in that order: magnification found in `$fac`, column `"magnification_col"`, pixels, real length unit.
#' Column names do not matter but must be specified, as read.table reads with \code{header=TRUE} Every
#' different magnification level found in `$fac`, column `"magnification_col"` must have its row.
#' @param magnification_col the name or id of the $fac column to look for magnification levels for every image
#' @param ... additional arguments (besides header=TRUE) to pass to read.table if 'scale_mapping' is a path
#' @details The i) case above is straightforward, if 1cm is 500pix long on all your pictures,
#' just call \code{rescale(your_Coo, scaling_factor=1/500)} and all coordinates will be in cm.
#'
#' The ii) second case is more subtle. First you need to code in your \link{Coo} object, in the fac
#' slot, a column named, say "mag", for magnification. Imagine you have 4 magnifications: 0.5, 1, 2 and 5,
#' we have to indicate for each magnification, how many pixels stands for how many units in the real world.
#'
#' This information is passed as a data.frame, built externally or in R, that must look like this:
#' \preformatted{
#' mag   pix    cm
#' 0.5   1304   10
#' 1     921    10
#' 2     816    5
#' 5     1020   5
#' }.
#'
#' We have to do that because, for optical reasons, the ratio pix/real_unit, is not a linear
#' function of the magnification.
#'
#' All shapes will be centered to apply (the single or the different) scaling_factor.
#'
#' @note This function is simple but quite complex to detail. Feel free to contact me should you have any
#' problem with it. You can just access its code (type \code{rescale}) and reply it yourself.
#' @return a Momocs object of same class
#' @family handling functions
#' @export
rescale <- function(x, scaling_factor, scale_mapping, magnification_col, ...){
  # homogeneous case
  if (!missing(scaling_factor)){
    x <- coo_center(x)
    x$coo <- lapply(x$coo, function(x) x*scaling_factor)
    return(x)
  }
  # multiple magnification case
  # if a path is provided we read it
  if (is.character(scale_mapping))
    scale_mapping <- utils::read.table(scale_mapping, header=TRUE, ...)
  # we prepare the two cols and match
  mag_orig <- x$fac[, magnification_col] %>% as.numeric()
  mag_rule <- scale_mapping[, 1] %>% as.character %>% as.numeric()
  mag_match <- match(mag_orig, mag_rule)
  # we check a bit
  match_found <- mag_orig %in% mag_rule
  if (any(!(match_found))) {
    stop("those magnification were absent from the file",
         unique(mag_orig[which(!(match_found))]))
  }
  # we center x to be able to just apply a multiplying factor
  x <- coo_center(x)
  mag_factor <- (scale_mapping[, 3] / scale_mapping[, 2])[mag_match]
  for (i in seq_along(x$coo)) {
    x$coo[[i]] <- x$coo[[i]] * mag_factor[i]
  }
  return(x)
}

##### end subset-combine
