#' Traditional morphometrics class
#'
#' Defines the builder for traditional measurement class in Momocs.
#' Is is intended to ease calculations, data handling and multivariate statistics
#' just ad the other Momocs' classes
#' @param coe a matrix of measurements
#' @param fac a data.frame for covariates
#' @family classes
#' @examples
#' data(iris)
#' # let's (more or less) rebuild the flower dataset
#' fl <- TraCoe(iris[, 1:4], data.frame(sp=iris$Species))
#' fl %>% PCA() %>% plot("sp")
#' @export
TraCoe <- function(coe = matrix(), fac = data.frame()) {
  TraCoe <- list(coe = coe, fac = fac)
  TraCoe <- structure(list(coe = coe, fac = fac), class=c("TraCoe", "Coe"))
  return(TraCoe)
}

#' @export
print.TraCoe <- function(x, ...) {
  TraCoe <- x
  ### Header
  cat("A TraCoe object ", rep("-", 20), "\n", sep = "")
  shp.nb <- nrow(TraCoe$coe)
  var.nb <- ncol(TraCoe$coe)
  cat(" - $coe:", shp.nb, "shapes described with", var.nb, "variables\n")
  # we print the fac
  .print.fac(TraCoe$fac)
}
