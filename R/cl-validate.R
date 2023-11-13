#' Validates Coo objects
#'
#' No validation for S3 objects, so this method is a (cheap) attempt at checking
#' \link{Coo} objects, \link{Out}, \link{Opn} and \link{Ldk} objects.
#'
#' Implemented before all morphometric methods and handling verbs.
#' To see what is checked, try eg \code{Momocs:::verify.Coo}
#' @param Coo any Coo object
#' @return a Coo object.
#' @examples
#' verify(bot)
#' bot[12] <- NA
#' # you would not use try, but here we cope with R CMD CHECK standards
#' plop <- try(verify(bot), silent=TRUE)
#' class(plop)
#'
#' verify(hearts)
#' hearts$ldk[[4]] <- c(1, 2)
#' # same remark
#' plop2 <- try(verify(hearts), silent=TRUE)
#' class(plop2)
#' @export
verify <- function(Coo){
  UseMethod("verify")
}

#' @export
verify.default <- function(Coo){
  stop("only implemented on Coo")
}

#' @export
verify.Coo <- function(Coo){
  # checks coo
  Coo <- coo_check(Coo)
  n <- length(Coo$coo)

  # checks fac
  if (is_fac(Coo)) {
    fac <- Coo$fac
    .check(is.data.frame(fac),
           "$fac must be a data.frame")
    .check(identical(nrow(fac), n),
           "the number of rows in $fac must equal the number of shapes")
  }

  # checks ldk if any
  if (is_ldk(Coo)){
    ldk <- Coo$ldk
    .check(identical(length(ldk), n),
           "the number of $ldk must equal the number of shapes")
    .check(length(unique(sapply(ldk, length)))==1,
           "the number of $ldk defined must be the same across shapes")
    .check(all(coo_nb(Coo) >= sapply(ldk, max)),
           "at least one shape as a $ldk id higher than its number of coordinates")
  }

  # ldk
  if (is_Ldk(Coo))
    .check(length(unique(coo_nb(Coo)))==1,
           "number of coordinates must be the same for Ldk")

  #checks slidings if any
  if (is_slidings(Coo)){
    .check(is.matrix(Coo$slidings),
           "slidings must be a matrix")
    .check(ncol(Coo$slidings)==3,
           "slidings must be a 3-columns matrix")
    .check(min(coo_nb(Coo)) >= nrow(unique(Coo$slidings)), # >?
           "number of sliding must be lower than number of coordinates")
  }
  # ensure data_frame
  Coo$fac <- Coo$fac %>% tibble::as_tibble()
  return(Coo)
}
