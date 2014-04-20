
# 1. Opn builder and domestic functions -------------------------------------------

#' Builds an Opn object
#'
#' In Momocs, Opn classes objects are lists of \bold{open} outlines,
#' on which generic methods such as plotting methods (e.g. stack()) 
#' and specific methods (e.g. polynomials()) can be applied.
#' 
#' They must be built from a list (or an array) of (x; y) coordinates matrices.
#'  
#' @export Opn
#' 
#' @param coo.list a list of (x; y) matrices of coordinates.
#' @param ldk (optionnal) a list of landmarks on these coordinates
#'  (provided as the row numbers) for every outline
#' @param fac (optionnal) a data.frame of factors, 
#' specifying the grouping structure.
#' @return a \code{Out} object.
#' @family Opn
#' @keywords Opn
#' @examples
Opn  <- function(coo.list, ldk=list(), fac=data.frame()){
  Opn <- list(coo=coo.list, ldk=ldk, fac=fac)
  if (!is.null(Opn$fac)) Out$fac <- .refactor(Out$fac)
  class(Opn) <- c("Opn", "Out")
  return(Opn)}

# The print method for Out objects
#' @S3method print Opn
print.Opn <- function(x, ...){
  Opn <- x
  ### Header
  cat("An Opn object (see ?Opn) with: \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- length(Opn)
  coo.len <- sapply(Opn$coo, nrow)
  coo.closed <- sapply(Opn$coo, is.closed)
  # number of open outlines
  cat(" -", coo.nb, " open outlines\n")
  # one random outline
  eg <- sample(length(Opn$coo), 1)
  coo.eg <- Opn$coo[[eg]]
  colnames(coo.eg) <- c("x", "y")
  cat(" - One random open outline in $coo: '", names(Opn$coo)[eg], "':\n", sep="")
  if (nrow(coo.eg) > 5) {
    print(coo.eg[1:5, ], print.gap=2)
    cat("etc.\n")
  } else {
    print(coo.eg, print.gap=2)
    cat("\n\n")}
  # number of coordinates
  cat(" -", round(mean(coo.len )), "+/-", round(sd(coo.len)), 
      "coordinates per outline\n")
  # outlines closed or not
  if (all(coo.closed)) {
    cat(" - All outlines are closed\n")
  } else {
    if (any(!coo.closed)) {
      cat(" - All outlines are unclosed\n")
    } else {
      cat(" -", sum(coo.closed), "outlines are closed\n")}}
  # number of landmarks
  if (length(Opn$ldk)!=0) {
    cat(" -", length(Opn$ldk[[1]]), "landmark(s) defined\n")
  } else {
    cat(" - No landmark defined\n")}
  # number of grouping factors
  df <- Opn$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}
