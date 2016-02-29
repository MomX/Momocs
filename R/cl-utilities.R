
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
  if (length(x$fac)==0) stop("no $fac defined")
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
  if (is.ldk(Coo))
    Coo2$ldk <- Coo$ldk[retain]
  if (is.fac(Coo)) {
    #     if (is.logical(retain))
    #       retain <- which(retain)
    Coo2$fac <- dplyr::slice(Coo$fac, retain)
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
    Coe2$fac <- Coe$fac[retain, ]
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


# rw_rule ---------------------
#' Rename levels with a rewriting rule
#'
#' rw_rule stands for 'rewriting rule'. Typically useful to correct typos
#' at the import, or merge some levels within covariates. Drops levels silently.
#'
#' @param x any Momocs object
#' @param fac the id of the name of the $fac column to look for
#' @param from which level(s) should be renamed; passed as a single or several characters
#' @param to which name ?
#' @return a Momocs object of the same type
#' @examples
#' data(bot)
#' # single renaming
#' rw_rule(bot, "type", "whisky", "agua_de_fuego") # 1 instead of "type" is fine too
#' # several renaming
#' bot2 <- mutate(bot, fake=factor(rep(letters[1:4], 10)))
#' rw_rule(bot2, "fake", c("a", "e"), "ae")$fake
#' @export
rw_rule <- function(x, fac, from, to){
  new_levels <- unique(c(levels(x$fac[, fac]), to))
  fac2 <- factor(x$fac[, fac], levels = new_levels)
  for (i in seq_along(from)) {
    fac2[which(fac2==from[i])] <- to
  }
  x$fac[, fac] <- droplevels(fac2)
  x
}

# at_least ------------------------
#' Retains group with at least a certain number of individuals within
#'
#' Title and examples are self-speaking.
#'
#' @param x any Momocs object
#' @param fac the id of name of the $fac column
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
    message("no group with at least ", N, " indidivuals")
    return(x)
  } else {
    subset(x, retain)
  }
}

# rm ------------
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
    message("* all ids have ", nb.u, " slices")
    return(x)
  } else {
    most_frequent <- as.numeric(names(nb.tab[which.max(nb.tab)]))
    ugly_ducklings <- names(which(nb != most_frequent))
    remove_rows <- which(x$fac[, id] %in% ugly_ducklings)
    message("those shapes did not have ", most_frequent,
            " slices and has been removed: ",
            paste(ugly_ducklings, collapse=", "))
    return(subset(x, -remove_rows))
  }
}

#' Removes harmonics
#'
#' Useful to drop harmonics on Coe objects. Should only work for
#' Fourier-based approached since it looks for \code{[A-D][1-drop]} pattern.
#'
#' @param x Coe object
#' @param drop numeric number of harmonics to drop
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
#' three columns in that order: magnification found in $fac[, "magnification_col"], pixels, real length unit.
#' Column names do not matter but must be specified, as read.table reads with \code{header=TRUE} Every
#' different magnification level found in $fac[, "magnification_col"] must have its row.
#' @param magnification_col the name or id of the $fac column to look for magnification levels for every image
#' @param ... additional arguments (besides header=TRUE) to pass to read.table if 'scale_mapping' is a path
#' @details The i) case above is straightforward, if 1cm is 500pix long on all your pictures,
#' just call \code{rescale(your_Coo, scaling_factor=1/500)} and all coordinates will be in cm.
#'
#' The ii) second case is more subtle. First you need to code in your /link{Coo} object, in the fac
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
    scale_mapping <- read.table(scale_mapping, header=TRUE, ...)
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

# ldk setters/getters ----------------------------------------------------------

#' Define landmarks on Out and Opn objects
#'
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @return an Out or an Opn object with some landmarks defined
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
def_ldk <- function(Coo, nb.ldk) {
  UseMethod("def_ldk")
}
#' @export
def_ldk.Out <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop("'nb.ldk' must be specified")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    Coo$ldk[[i]] <- coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk)
  }
  return(Coo)
}
#' @export
def_ldk.Opn <- def_ldk.Out

#' Retrieve landmarks coordinates
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
#' @seealso \link{def_ldk}, \link{get_slidings}, \link{fgProcrustes}
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
  .check(is.ldk(Coo),
         "this object has no $ldk")
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

# sliding getters/setters ------------------------------------------------------

# prepare a matrix for sliding landmarks ($slidings in Ldk)
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


#' Extract partition of sliding coordinates
#'
#' Helper function that deduces (likely to be a reminder)
#' partition scheme from \code{$slidings} of \code{Ldk} objects.
#'
#' @param Coo an Ldk object
#' @return a list with two components: \code{n} the number of partition; \code{id}
#' their position. Or a NULL if no slidings are defined
#'
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

#' Define sliding landmarks matrix
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

#' Extract sliding landmarks coordinates
#'
#' From an \link{Ldk} object.
#'
#' @param Coo an Ldk object
#' @param partition numeric which one(s) to get.
#' @return a list of list(s) of coordinates.
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

# class testers -------------
#' Various class/component testers
#'
#' Class testers test if any of the classes of an object is of a given class. For instance
#' is.PCA on a PCA object (both 'PCA' and 'prcomp') will return TRUE.
#' Component testers check if a particular component (eg $fac, etc.) is present.
#' @param x the object to test
#' @return TRUE/FALSE
#' @examples
#' data(bot)
#' is.Coo(bot)
#' is.Out(bot)
#' is.Ldk(bot)
#' @name is
#' @export
is.Coo <- function(x){
  ifelse(any(class(x) == "Coo"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.PCA <- function(x){
  ifelse(any(class(x) == "PCA"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.LDA <- function(x){
  ifelse(any(class(x) == "LDA"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.Out <- function(x){
  ifelse(any(class(x) == "Out"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.Opn <- function(x){
  ifelse(any(class(x) == "Opn"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.Ldk <- function(x){
  ifelse(any(class(x) == "Ldk"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.Coe <- function(x){
  ifelse(any(class(x) == "Coe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.OutCoe <- function(x){
  ifelse(any(class(x) == "OutCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.OpnCoe <- function(x){
  ifelse(any(class(x) == "OpnCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.LdkCoe <- function(x){
  ifelse(any(class(x) == "LdkCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is.shp <- function(x){
  if (is.matrix(x))
    if (ncol(x)==2 & all(!is.na(x)))
      return(TRUE)
  FALSE
}

#' @rdname is
#' @export
is.fac   <- function(x) length(x$fac) > 0

#' @rdname is
#' @export
is.ldk   <- function(x) length(x$ldk) > 0

#' @rdname is
#' @export
is.slidings   <- function(x) length(x$slidings) > 0

#' @rdname is
#' @export
is.links <- function(x) is.matrix(x$links)

