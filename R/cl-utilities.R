
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
  regex <- paste0("[A-D][1-", drop,"]")
  x$coe <- x$coe[, -grep(regex, colnames(x$coe))]
  x
}

# class testers -------------
#' Tests if an object is of a given class
#'
#' Tests if any of the classes of an object is of a given class. For instance
#' is.PCA on a PCA object (both 'PCA' and 'prcomp') will return TRUE
#' @param x the object to test
#' @return TRUE/FALSE
#' @examples
#' data(bot)
#' is.Coo(bot)
#' is.Out(bot)
#' is.Ldk(bot)
#' @rdname is.Momocs
#' @export
is.Coo <- function(x){
  ifelse(any(class(x) == "Coo"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.PCA <- function(x){
  ifelse(any(class(x) == "PCA"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.LDA <- function(x){
  ifelse(any(class(x) == "LDA"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.Out <- function(x){
  ifelse(any(class(x) == "Out"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.Opn <- function(x){
  ifelse(any(class(x) == "Opn"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.Ldk <- function(x){
  ifelse(any(class(x) == "Ldk"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.Coe <- function(x){
  ifelse(any(class(x) == "Coe"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.OutCoe <- function(x){
  ifelse(any(class(x) == "OutCoe"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.OpnCoe <- function(x){
  ifelse(any(class(x) == "OpnCoe"), TRUE, FALSE)
}
#' @rdname is.Momocs
#' @export
is.LdkCoe <- function(x){
  ifelse(any(class(x) == "LdkCoe"), TRUE, FALSE)
}
