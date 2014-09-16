#' Permute Coe (and others) objects
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
#' @seealso \link{mutate}
#' @keywords Coe
#' @examples
#' m <- matrix(1:12, nrow=3)
#' m
#' perm(m, margin=2, size=5)
#' perm(m, margin=1, size=10)
#' 
#' data(bot)
#' bot.f <- eFourier(bot, 12)
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

#' Mutate Coe (and others) objects
#' 
#' This methods applies column-wise on the \code{coe} of any 
#' \link{Coe} object but relies on a function that can be used on any matrix. It
#' simply uses \link{rnorm} with the mean and sd calculated for every column (or row).
#' For a \code{Coe} object, on every colum, randomly generates coefficients values
#' centered on the mean of the column, and with a sd equals to it standard deviates
#' multiplied by \code{rate}.
#' @param x the object to permute
#' @param margin numeric whether 1 or 2 (rows or columns)
#' @param size numeric the required size for the final object, same size by default
#' @param rate numeric the number of sd for \link{rnorm}, 1 by default.
#' @param ... useless here
#' @seealso \link{perm}
#' @keywords Coe
#' @examples
#' m <- matrix(1:12, nrow=3)
#' mutate(m, margin=2, size=4)
#' mutate(m, margin=1, size=10)
#' 
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.m <- mutate(bot.f, 80)
#' bot.m
#' panel(bot.m)
#' @rdname mutate
#' @export
mutate <- function(x, ...){UseMethod("mutate")}
#' @rdname mutate
#' @export
mutate.default <- function(x, margin=2, size, rate=1, ...){
  if (missing(size)) size <- dim(x)[ifelse(margin==1, 2, 1)]
  apply(x, margin, function(x) rnorm(size, mean(x), rate*sd(x)))}
#' @rdname mutate
#' @export
mutate.Coe <- function(x, size, rate=1, ...){
  if (missing(size)) size <- nrow(x$coe)
  coe <- mutate(x$coe, margin=2, size=size, rate=rate)
  rownames(coe) <- paste0("id", 1:size)
  x$coe <- coe
  x$fac <- data.frame()
  x}
