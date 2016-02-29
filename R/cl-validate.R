#' Validates Coo objects
#'
#' No validation for S3 objects, so this method is a (cheap) attempt at checking
#' \link{Coo} objects, \link{Out}, \link{Opn} and \link{Ldk} objects.
#'
#' Implemented before all morphometric methods and handling verbs.
#' To see what is checked, try eg \code{Momocs:::validate.Coo}
#' @param Coo any Coo object
#' @return a Coo object.
#' @examples
#' \dontrun{
#' validate(bot)
#' bot[12] <- NA
#' validate(bot)
#'
#' validate(hearts)
#' hearts$ldk[[4]] <- c(1, 2)
#' validate(hearts)
#' }
#' @export
validate <- function(Coo){
  UseMethod("validate")
}

#' @export
validate.default <- function(Coo){
  stop("only implemented on Coo")
}

#' @export
validate.Coo <- function(Coo){
  # checks coo
  Coo <- coo_check(Coo)
  n <- length(Coo$coo)

  # checks fac
  if (is.fac(Coo)) {
    fac <- Coo$fac
    .check(is.data.frame(fac),
           "$fac must be a data.frame")
    .check(identical(nrow(fac), n),
           "the number of rows in $fac must equal the number of shapes")
  }

  # checks ldk if any
  if (is.ldk(Coo)){
    ldk <- Coo$ldk
    .check(identical(length(ldk), n),
           "the number of $ldk must equal the number of shapes")
    .check(length(unique(sapply(ldk, length)))==1,
           "the number of $ldk defined must be the same accross shapes")
    .check(all(coo_nb(Coo) >= sapply(ldk, max)),
           "at least one shape as a $ldk id higher than its number of coordinates")
  }

  # ldk
  if (is.Ldk(Coo))
    .check(length(unique(coo_nb(Coo)))==1,
           "number of coordinates must be the same for Ldk")

  #checks slidings if any
  if (is.slidings(Coo)){
    .check(is.matrix(Coo$slidings),
           "slidings must be a matrix")
    .check(ncol(Coo$slidings)==3,
           "slidings must be a 3-columns matrix")
    .check(min(coo_nb(Coo)) >= nrow(unique(Coo$slidings)), # >?
           "number of sliding must be lower than number of coordinates")
  }
  return(Coo)
}
