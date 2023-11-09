##### Miscellaneous functions

# utils ---------

which_collinear <- function(x, tol=1-1e-4){
  # correlation matrix
  cor_m <- cor(x)
  # dont need the diag and upper tri
  cor_m[upper.tri(cor_m, diag = TRUE)] <- 0
  # find which are above threshold
  apply(cor_m, 2, function(.x) any(abs(.x) > tol)) %>%
    which
}

remove_collinear <- function(x, tol=1-1e-4){
  ids <- which_collinear(x, tol=tol)
  # if none, return x
  if (length(ids)==0)
    return(x)

  # otherwise return x minus collinear columns
  # with message if verbose
  if (.is_verbose()){
    if (is.null(colnames(x)))
      message("removed ", length(ids), " collinear columns")
    else
      message("removed these collinear columns: ", paste(colnames(x)[ids], collapse=", "))
  }
  x[, -ids]
}


which_constant <- function(x, tol=1e-4){
  which(apply(x, 2, sd) < tol)
}

remove_constant <- function(x, tol=1e-4){
  ids <- which_constant(x, tol=tol)
  # if none, return x
  if (length(ids)==0)
    return(x)

  # otherwise return x minus collinear columns
  # with message if verbose
  if (.is_verbose()){
    if (is.null(colnames(x)))
      message("removed ", length(ids), " constant columns")
    else
      message("removed these constant columns: ", paste(colnames(x)[ids], collapse=", "))
  }
  x[, -ids]
}

reinsert_columns <- function(x_keep, x_drop, ids_keep, ids_drop){
  # if one of ids is empty, return the other x
  if (length(ids_drop)==0)
    return(x_keep)
  if (length(ids_keep)==0)
    return(x_drop)

  # a prototypic new matrix filled with NAs
  x_back <- cbind(x_keep, x_drop)
  dimnames(x_back) <- NULL # safer
  x_back[] <- NA
  # ensure sets are disjuncts
  n <- ncol(x_back)
  .check(length(unique(c(ids_keep, ids_drop)))==n,
         "set are not disjunct are matrices do not have the right dimensions")
  # bind them back - safer than sapply when single LD
  lapply(1:n,
         # one of the two which will return an integer(0)
         function(i) cbind(x_keep[, which(ids_keep==i)], x_drop[, which(ids_drop==i)])) %>%
    do.call("cbind", .)
}

