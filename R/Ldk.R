
# 1. Ldk builder and domestic functions -------------------------------------------

#' Builds an Ldk object
#'
#' In Momocs, \code{Ldk} classes objects are wrapping around 
#' lists of configurations of landmarks, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{stack}) 
#' and specific methods (e.g. todo-Procrustes can be applied.
#'  \code{Ldk} objects are primarily \code{\link{Coo}} objects.
#' 
#' @param coo.list \code{list} of matrices of \eqn{(x; y)} coordinates
#' @param links a matrix of "links" between landmarks, mainly for plotting
#' @param fac (optionnal) a \code{data.frame} of factors, 
#' specifying the grouping structure
#' @return an \code{Ldk} object
#' @details These methods can be applied on \code{Ldk} objects:
#' \enumerate{
#' \item Handling: \code{subset, coo.} family;
#' \item Graphics: \code{plot, stack, panel};
#' \item Morpho: todo
#' }
#'
#' @seealso \link{Coo}, \link{Out}, link{Opn}.
#' @keywords Ldk
#' @aliases Ldk
#' @export
Ldk  <- function(coo.list, links=matrix(), fac=data.frame()){
  Ldk <- list(coo=coo.list, links=links, fac=fac)
  if (!is.null(Ldk$fac)) Ldk$fac <- .refactor(Ldk$fac)
  class(Ldk) <- c("Ldk", "Coo")
  return(Ldk)}

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


