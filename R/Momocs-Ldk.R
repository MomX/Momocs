
# 1. Ldk builder and domestic functions -------------------------------------------

#' Builds an Ldk object
#'
#' In Momocs, \code{Ldk} classes objects are wrapping around 
#' lists of configurations of landmarks, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{stack}) 
#' and specific methods (e.g. todo-Procrustes can be applied.
#'  \code{Ldk} objects are primarily \code{\link{Coo}} objects.
#' 
#' All the shapes in x must have the same number of landmarks. If you are 
#' trying to make an Ldk object from an Out or an Opn object, try \link{coo.sample}.
#' 
#' @param x a \code{list} of matrices of \eqn{(x; y)} coordinates,
#' or an array, an Ldk object.
#' @param links a matrix of "links" between landmarks, mainly for plotting
#' @param fac (optionnal) a \code{data.frame} of factors, 
#' specifying the grouping structure
#' @return an \code{Ldk} object
#' @seealso \link{Coo}, \link{Out}, link{Opn}.
#' @keywords Ldk
#' @aliases Ldk Ldk.default Ldk.Ldk Ldk.list LdkCoe
#' @examples
#' methods(class=Ldk)
#' @export
Ldk <- function(x, links=NULL, fac=data.frame()){UseMethod("Ldk")}

#' @export
Ldk.default <- function(x, links=NULL, fac=data.frame()){
  cat(" * an Ldk object can only be build from a list, an array or an Ldk object")}

#' @export
Ldk.list  <- function(x, links=NULL, fac=data.frame()){
  Ldk <- list(coo=x, links=links, fac=fac)
  if (!is.null(Ldk$fac)) Ldk$fac <- .refactor(Ldk$fac)
  class(Ldk) <- c("Ldk", "Coo")
  return(Ldk)}

#' @export
Ldk.array  <- function(x, links=NULL, fac=data.frame()){
  x <- a2l(x)
  Ldk(x, links=links, fac=fac)}

#' @export
Ldk.Coo <- function(x, links=NULL, fac=data.frame()){
  nb.ldk <- sapply(x$coo, length)
  if (length(unique(nb.ldk))>1) stop(" * shapes do not have the same number of landmarks.")
  Ldk(x=x$coo, links=x$links, fac=x$fac)}


# The print method for Ldk objects
#' @export
print.Ldk <- function(x, ...){
  Ldk <- x
  ### Header
  cat("An Ldk object (see ?Ldk) with: \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- length(Ldk)
  coo.len <- sapply(Ldk$coo, nrow)
  coo.closed <- sapply(Ldk$coo, is.closed)
  # number of open outlines
  cat(" -", coo.nb, "configurations of landmarks\n")
  # one random outline
  eg <- sample(length(Ldk$coo), 1)
  coo.eg <- Ldk$coo[[eg]]
  colnames(coo.eg) <- c("x", "y")
  cat(" - One random configuration in $coo: '", names(Ldk$coo)[eg], "':\n", sep="")
  if (nrow(coo.eg) > 5) {
    print(coo.eg[1:5, ], print.gap=2)
    cat("etc.\n")
  } else {
    print(coo.eg, print.gap=2)
    cat("\n\n")}
  # number of coordinates
  cat(" -", round(mean(coo.len )), "+/-", round(sd(coo.len)), 
      "coordinates per configuration\n")
  # number of grouping factors
  df <- Ldk$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", 
                                       length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}
