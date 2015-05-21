
#' Tests if an object is og a given class
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
