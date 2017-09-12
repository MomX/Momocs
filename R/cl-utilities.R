
# table --------------------------
#' Cross-tabulates objects
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
  if (!is.fac(x)) stop("no $fac defined")
  if (length(args)>1) {
    # a little helper for mismatched colnames
    cn <- unlist(args[-1])
    matches <- match(cn, colnames(x$fac))
    if (any(is.na(matches))) {
      mispelled <- which(is.na(matches))
      stop(cn[mispelled], "' mispelled or not defined in $fac")
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


# ldk setters/getters ----------------------------------------------------------

#' Defines new landmarks on Out and Opn objects
#'
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @family ldk/slidings methods
#' @examples
#' \dontrun{
#' data(bot)
#' bot <- bot[1:5] # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by def_ldk...
#' bot2 <- def_ldk(bot, 3)
#' stack(bot2)
#' bot2$ldk
#' }
#' @export
#' @export
def_ldk <- function(Coo, nb.ldk) {
  UseMethod("def_ldk")
}
#' @export
def_ldk.Out <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop("'nb.ldk' must be specified")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    cat(i, "/", length(Coo$coo), " ")
    Coo$ldk[[i]] <- coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk)
  }
  return(Coo)
}
#' @export
def_ldk.Opn <- def_ldk.Out

#' Adds new landmarks on Out and Opn objects
#'
#' Helps to add new landmarks on a \code{Coo} object on top of existing ones.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to add on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @details Note that if no landmarks are already defined,
#' then this function is equivalent to \link{def_ldk}.
#' @family ldk/slidings methods
#' @examples
#' \dontrun{
#' data(hearts)
#' hearts <- slice(hearts, 1:5) # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by def_ldk...
#' hearts2 <- add_ldk(hearts, 3)
#' stack(hearts2)
#' hearts2$ldk
#' }
#' @export
add_ldk <- function(Coo, nb.ldk) {
  UseMethod("add_ldk")
}
#' @export
add_ldk.Out <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop("'nb.ldk' must be specified")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    cat(i, "/", length(Coo$coo), " ")
    Coo$ldk[[i]] <- c(Coo$ldk[[i]], coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk))
  }
  return(Coo)
}
#' @export
add_ldk.Opn <- add_ldk.Out


#' Retrieves landmarks coordinates
#'
#' See Details for the different behaviors implemented.
#'
#' @param Coo an Out, Opn or Ldk object
#' @return a list of shapes
#' @details Different behaviors depending on the class of the object:
#' \itemize{
#' \item \link{Ldk}: retrieves landmarks.
#' \item Ldk with slidings defined: retrieves only the fixed landmarks, not the sliding ones.
#' See also \link{get_slidings}.
#' \item \link{Out} landmarks from \code{$ldk} and \code{$coo}, if any.
#' \item \link{Opn}: same as above.
#' }
#' @family ldk/slidings methods
#' @examples
#' # Out example
#' ldk.h <- get_ldk(hearts)
#' stack(Ldk(ldk.h))
#'
#' # on Ldk (no slidings)
#' get_ldk(wings) # equivalent to wings$coo
#'
#' # on Ldk (slidings)
#' get_ldk(chaff)
#' get_ldk(chaff) %>% Ldk %>% fgProcrustes(tol=0.1) %>% stack
#' @export
get_ldk <- function(Coo) {
  UseMethod("get_ldk")
}

#' @export
get_ldk.Ldk <- function(Coo){
  # sliding case
  # we need to retrieve all sliding landmarks
  # (including first and last from all partitions)
  if (is.slidings(Coo)){
    all_ids <- 1:unique(coo_nb(Coo))
    sliding_ids <- Coo %>% slidings_scheme() %$% id %>%
      apply(1, function(x) x[1]:x[2])  %>% as.numeric()
    fixed_ids   <- all_ids[-sliding_ids]
    ldk <- lapply(Coo$coo, function(x) x[fixed_ids, ])
    return(ldk)
  } else {
    Coo$coo
  }
}

#' @export
get_ldk.Out <- function(Coo) {
  if (!is.ldk(Coo))
         return(NULL)
  coo <- Coo$coo
  ldk <- Coo$ldk
  ref <- array(NA, dim = c(length(ldk[[1]]), ncol(coo[[1]]),
                           length(coo)))
  for (i in seq(along = coo)) {
    ref[, , i] <- coo[[i]][ldk[[i]], ]
  }
  # cases where single ldk (otherwise converted to a numeric)
  res <- lapply(a2l(ref), function(x) matrix(x, ncol=2))
  return(res)
}
#' @export
get_ldk.Opn <- get_ldk.Out

#' Rearrange, (select and reorder) landmarks to retain
#'
#' Helps reorder and retain landmarks by simply changing the order in which they
#' are recorded in the \code{Coo} objects. Note that for \code{Out} and \code{Opn}
#'  objects, this rearranges the \code{$ldk} component. For \code{Ldk}, it rearranges
#'   the \code{$coo} directly.
#'
#' @param Coo any appropriate \code{Coo} object (typically an \code{Ldk})
#' with landmarks inside
#' @param new_ldk_ids a vector of numeric with the ldk to retain \emph{and}
#' in the right order (see below)
#' @family ldk/slidings methods
#' @examples
#' # Out example
#' hearts %>% slice(1) %T>% stack %$% ldk
#' hearts %>% rearrange_ldk(c(4, 1)) %>%
#'        slice(1) %T>%stack %$% ldk
#'
#' # Ldk example
#' wings %>% slice(1) %T>% stack %$% coo
#' wings %>% rearrange_ldk(c(1, 3, 12:15)) %>%
#'       slice(1) %T>% stack %$% coo
#' @export
rearrange_ldk <- function(Coo, new_ldk_ids){
  UseMethod("rearrange_ldk")
}

#' @export
rearrange_ldk.default <- function(Coo, new_ldk_ids){
  message("* only defined on Coo objects")
}

#' @export
rearrange_ldk.Out <- function(Coo, new_ldk_ids){
  if (is.null(get_ldk(Coo))){
    message("* no ldk to arrange")
    return(Coo)
  }
  Coo$ldk %<>% lapply(function(.) .[new_ldk_ids])
  return(Coo)
}

#' @export
rearrange_ldk.Opn <- rearrange_ldk.Out

#' @export
rearrange_ldk.Ldk <- function(Coo, new_ldk_ids){
  if (is.null(get_ldk(Coo))){
    message("* no ldk to arrange")
    stop()
  }
  Coo$coo %<>% lapply(function(.) .[new_ldk_ids, ])
  return(Coo)
}

# sliding getters/setters ------------------------------------------------------

# prepares a matrix for sliding landmarks ($slidings in Ldk)
.slidings_matrix <- function(nrow=0){
  matrix(NA, nrow=nrow, ncol=3, dimnames = list(NULL, c("before", "slide", "after")))
}

# given a partition, create and fill a $slidings matrix
# partition can be passed as numeric, or list of numeric
# for both cases, the first and last points are considered fixed
# and not allowed to slide
# .slidings_def_from_partition(4:9)
# .slidings_def_from_partition(list(4:9, 34:45))
# .slidings_def_from_partition("error")
.slidings_def_from_partition <- function(x){
  .check(is.numeric(x) | is.list(x), "partition(s) must be a numeric or a list of numeric")
  # single partition passed as a numeric
  if (is.numeric(x)){
    .check(length(x)>=3, "partition(s) must contain at least 3 points")
    # first and last points are fixed
    x_sliding <- x[2]:x[length(x)-1]
    # prepare the slidings matrix
    slidings <- .slidings_matrix(length(x_sliding))
    # fill it
    for (i in seq_along(x_sliding))
      slidings[i, ] <- x_sliding[i] + c(-1, 0, 1)
  }
  # multi partition cased, passed as a list
  if (is.list(x)){
    .check(all(sapply(x, is.numeric)),
           "all partitions must be numeric")
    slidings_list <- vector("list", length(x))
    for (i in seq_along(x))
      slidings_list[[i]] <- .slidings_def_from_partition(x[[i]])
    slidings <- do.call(rbind, slidings_list)
  }
  # return this beauty
  return(slidings)
}

# deduces partition scheme from sliding matrix
.slidings_scheme <- function(x){
  .check(is.matrix(x), "slidings must be a matrix")
  .check(ncol(x)==3,   "slidings must be a 3-columns matrix")
  d <- which(diff(x[, 1])>1)
  # nb of partitions
  n <- length(d)+1
  # deduce their position
  id <- cbind(c(x[1, 1], x[d+1, 1]), c(x[d, 3], x[nrow(x), 3]))
  # cosmetics
  dimnames(id) <- list(paste0("partition", 1:nrow(id)), c("start", "end"))
  return(list(n=n, id=id))
}


#' Extracts partitions of sliding coordinates
#'
#' Helper function that deduces (likely to be a reminder)
#' partition scheme from \code{$slidings} of \code{Ldk} objects.
#'
#' @param Coo an Ldk object
#' @return a list with two components: \code{n} the number of partition; \code{id}
#' their position. Or a NULL if no slidings are defined
#' @family ldk/slidings methods
#' @examples
#' # no slidings defined a NULL is returned with a message
#' slidings_scheme(wings)
#'
#' # slidings defined
#' slidings_scheme(chaff)
#'
#' @export
slidings_scheme <- function(Coo){
  UseMethod("slidings_scheme")
}

#' @export
slidings_scheme.default <- function(Coo){
  stop("only defined on Ldk")
}

#' @export
slidings_scheme.Ldk <- function(Coo){
  if(!is.slidings(Coo)){
    message("no sliding defined")
    return(NULL)}
  .slidings_scheme(Coo$slidings)
}

#' Defines sliding landmarks matrix
#' @param Coo an \link{Ldk} object
#' @param slidings a matrix, a numeric or a list of numeric. See Details
#' @details \code{$slidings} in \link{Ldk} must be a 'valid' matrix: containing
#' ids of coordinates, none of them being lower than 1 and higher the number of coordinates
#' in \code{$coo}.
#'
#' \code{slidings} matrix contains 3 columns (\code{before}, \code{slide}, \code{after}).
#' It is inspired by \code{geomorph} and should be compatible with it.
#'
#' This matrix can be passed directly if the \code{slidings} argument is a matrix. Of course,
#' it is strictly equivalent to \code{Ldk$slidings <- slidings}.
#'
#' \code{slidings} can also be passed as "partition(s)", when sliding landmarks
#' identified by their ids (which are a row number) are consecutive in the \code{$coo}.
#'
#' A single partition can be passed either as a numeric (eg \code{4:12}), if points
#' 5 to 11 must be considered as sliding landmarks (4 and 12 being fixed); or as a list of numeric.
#'
#' See examples below.
#' @family ldk/slidings methods
#' @examples
#' #waiting for a sliding dataset...
#'
#' @export
def_slidings <- function(Coo, slidings){
  UseMethod("def_slidings")
}

#' @export
def_slidings.default <- function(Coo, slidings){
  stop("only defined on Ldk")
}

#' @export
def_slidings.Ldk <- function(Coo, slidings){
  .check((is.numeric(slidings) | is.matrix(slidings) | is.list(slidings)),
         "sliding must be a matrix, a numeric or a list of numeric")
  # matrix case
  if (is.matrix(slidings))
    Coo$slidings <- slidings
  else
    Coo$slidings <- .slidings_def_from_partition(slidings)
  return(Coo)
}

#' Extracts sliding landmarks coordinates
#'
#' From an \link{Ldk} object.
#'
#' @param Coo an Ldk object
#' @param partition numeric which one(s) to get.
#' @return a list of list(s) of coordinates.
#' @family ldk/slidings methods
#' @examples
#' # for each example below a list with partition containign shapes is returned
#' # extracts the first partition
#' get_slidings(chaff, 1) %>% names()
#' # the first and the fourth
#' get_slidings(chaff, c(1, 4)) %>%  names()
#' # all of them
#' get_slidings(chaff) %>%  names
#' # here we want to see it
#' get_slidings(chaff, 1)[[1]] %>%  Ldk %>% stack
#' @export
get_slidings <- function(Coo, partition){
  UseMethod("get_slidings")
}

#' @export
get_slidings.default <- function(Coo, partition){
  stop("only defined on Ldk")
}

#' @export
get_slidings.Ldk <- function(Coo, partition){
  .check(is.slidings(Coo), "no slidings defined")
  # we retrieve the scheme
  scheme <- .slidings_scheme(Coo$slidings)
  n  <- scheme$n
  # all by default
  if (missing(partition))
    partition <- 1:n
  id <- scheme$id[partition, ]
  if (is.numeric(id)) id <- matrix(id, ncol=2)
  # nice try
  .check(all(partition<=n), "some partition do not exist")
  # prepare the nest
  slidings <- vector("list", length(partition))
  names(slidings) <- rownames(scheme$id)[partition]
  # loop and grab
  for (i in 1:nrow(id)){
    slidings[[i]] <- lapply(Coo$coo, coo_extract, id[i, 1]:id[i, 2])
  }
  return(slidings)
}

# class append/prependers ------------
# app/pre-pend classes
.prepend_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(class_to_add, .)
  x
}

.append_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(., class_to_add)
  x
}
# class testers -------------
#' Various class/component testers
#'
#' Class testers test if any of the classes of an object is of a given class. For instance
#' is.PCA on a PCA object (both 'PCA' and 'prcomp') will return TRUE.
#' Component testers check if a particular component (eg $fac, etc.) is present.
#' @param x the object to test
#' @return TRUE/FALSE
#' @note all \code{is_*} have an \code{is.*} alias
#' (eg \code{is_Coo} is the same as \code{is.Coo}) but the \code{.} alias will
#' be deprecated at some point.
#' @examples
#' data(bot)
#' is_Coo(bot)
#' is_Out(bot)
#' is_Ldk(bot)
#' @name is
#' @export
is_Coo <- function(x){
  ifelse(any(class(x) == "Coo"), TRUE, FALSE)
}
is.Coo <- is_Coo

#' @rdname is
#' @export
is_PCA <- function(x){
  ifelse(any(class(x) == "PCA"), TRUE, FALSE)
}
is.PCA <- is_PCA

#' @rdname is
#' @export
is_LDA <- function(x){
  ifelse(any(class(x) == "LDA"), TRUE, FALSE)
}
is.LDA <- is_LDA

#' @rdname is
#' @export
is_Out <- function(x){
  ifelse(any(class(x) == "Out"), TRUE, FALSE)
}
is.Out <- is_Out

#' @rdname is
#' @export
is_Opn <- function(x){
  ifelse(any(class(x) == "Opn"), TRUE, FALSE)
}
is.Opn <- is_Opn

#' @rdname is
#' @export
is_Ldk <- function(x){
  ifelse(any(class(x) == "Ldk"), TRUE, FALSE)
}
is.Ldk <- is_Ldk

#' @rdname is
#' @export
is_Coe <- function(x){
  ifelse(any(class(x) == "Coe"), TRUE, FALSE)
}
is.Coe <- is_Coe

#' @rdname is
#' @export
is_OutCoe <- function(x){
  ifelse(any(class(x) == "OutCoe"), TRUE, FALSE)
}
is.OutCoe <- is_OutCoe

#' @rdname is
#' @export
is_OpnCoe <- function(x){
  ifelse(any(class(x) == "OpnCoe"), TRUE, FALSE)
}
is.OpnCoe <- is_OpnCoe

#' @rdname is
#' @export
is_LdkCoe <- function(x){
  ifelse(any(class(x) == "LdkCoe"), TRUE, FALSE)
}
is.LdkCoe <- is_LdkCoe

#' @rdname is
#' @export
is_TraCoe <- function(x){
  ifelse(any(class(x) == "TraCoe"), TRUE, FALSE)
}
is.TraCoe <- is_TraCoe


#' @rdname is
#' @export
is_shp <- function(x){
  if (is.matrix(x))
    if (ncol(x)==2 & all(!is.na(x)))
      return(TRUE)
  FALSE
}
is.shp <- is_shp

#' @rdname is
#' @export
is_fac <- function(x) length(x$fac) > 0
is.fac <- is_fac

#' @rdname is
#' @export
is_ldk   <- function(x) length(x$ldk) > 0
is.ldk <- is_ldk

#' @rdname is
#' @export
is_slidings   <- function(x) length(x$slidings) > 0
is.slidings <- is_slidings

#' @rdname is
#' @export
is_links <- function(x) is.matrix(x$links)
is.links <- is_links
