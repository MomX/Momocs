# perm ----------

#' Permutes and breed Coe (and others) objects
#'
#' This methods applies permutations column-wise on the \code{coe} of any
#' \link{Coe} object but relies on a function that can be used on any matrix.
#' For a Coe object, it uses \link{sample} on every column (or row) with (or without)
#' replacement.
#' @param x the object to permute
#' @param margin numeric whether 1 or 2 (rows or columns)
#' @param size numeric the required size for the final object, same size by default.
#' @param replace logical, whether to use \link{sample} with replacement
#' @param ... useless here
#' @family farming
#' @examples
#' m <- matrix(1:12, nrow=3)
#' m
#' perm(m, margin=2, size=5)
#' perm(m, margin=1, size=10)
#'
#' bot.f <- efourier(bot, 12)
#' bot.m <- perm(bot.f, 80)
#' bot.m
#' panel(bot.m)
#' @rdname perm
#' @export
perm <- function(x, ...){UseMethod("perm")}
#' @rdname perm
#' @export
perm.default <- function(x, margin=2, size, replace=TRUE, ...){
  if (missing(size)) size <- dim(x)[ifelse(margin==1, 2, 1)]
  xp <- apply(x, margin, sample, size=size, replace=replace)
  return(xp)}
#' @rdname perm
#' @export
perm.Coe <- function(x, size, replace=TRUE, ...){
  if (missing(size)) size <- nrow(x$coe)
  coe <- perm(x$coe, margin=2, sample, size=size, replace=replace)
  rownames(coe) <- paste0("id", 1:size)
  x$coe <- coe
  x$fac <- data.frame()
  x}

# breed --------
#' Jitters Coe (and others) objects
#'
#' This methods applies column-wise on the \code{coe} of any
#' \link{Coe} object but relies on a function that can be used on any matrix. It
#' simply uses \link{rnorm} with the mean and sd calculated for every column (or row).
#' For a \code{Coe} object, on every colum, randomly generates coefficients values
#' centered on the mean of the column, and with a sd equals to it standard deviates
#' multiplied by \code{rate}.
#' @param x the object to permute
#' @param fac a column, a formula or a column id from `$fac`
#' @param margin numeric whether 1 or 2 (rows or columns)
#' @param size numeric the required size for the final object, same size by default
#' @param rate numeric the number of sd for \link{rnorm}, 1 by default.
#' @param ... useless here
#' @family farming
#' @examples
#' m <- matrix(1:12, nrow=3)
#' breed(m, margin=2, size=4)
#' breed(m, margin=1, size=10)
#'
#' bot.f <- efourier(bot, 12)
#' bot.m <- breed(bot.f, size=80)
#' bot.m %>% PCA %>% plot
#'
#' # breed fac wise
#' bot.f %>%  breed(~type, size=50) %>% PCA %>% plot(~type)
#' @rdname breed
#' @export
breed <- function(x, ...){
  UseMethod("breed")
}

#' @rdname breed
#' @export
breed.default <- function(x, fac, margin=2, size, rate=1, ...){
  if (missing(size)) size <- dim(x)[ifelse(margin==1, 2, 1)]
  apply(x, margin, function(x) rnorm(size, mean(x), rate*sd(x)))
}

#' @rdname breed
#' @export
breed.Coe <- function(x, fac, size, rate=1, ...){
  if (missing(fac)){
    if (missing(size)) size <- nrow(x$coe)
    coe <- breed(x$coe, margin=2, size=size, rate=rate)
    rownames(coe) <- paste0("id", 1:size)
    x$coe <- coe
    x$fac <- data.frame()
    return(x)
  } else {
    f <- .fac_dispatcher(x, fac)
    # breed group wise
    x2 <- x %>% chop(f) %>% lapply(breed, size=size, rate=rate, ...)
    # retrieves all matrices of coefficients and rbind them
    coes <- lapply(x2, function(.) `$`(., coe)) %>% do.call("rbind", .)
    # creates a single column $fac for all Coes in the list
    facs <- for (i in seq_along(x2)){
      x2[[i]]$fac <- data.frame(group=rep(names(x2)[i], length(x2[[i]])))
    }
    # retrieves all $fac (freshly created) and rbind them
    facs <- lapply(x2, function(.) `$`(., fac)) %>% do.call("rbind", .)
    colnames(facs) <- colnames(x$fac)
    final <- x2[[1]] # final OutCoe shoudl looks like any other Coes in the list
    final$coe <- coes
    final$fac <- facs
    return(final)
  }
}


