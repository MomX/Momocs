
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
  if (!is.null(Opn$fac)) Opn$fac <- .refactor(Opn$fac)
  class(Opn) <- c("Opn", "Coo")
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
  cat(" -", coo.nb, "open outlines\n")
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
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", 
                                       length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}

# 2. OpnCoe definition -------------------------------------------------------
OpnCoe <- function(coe=matrix(), fac=data.frame(),
                   method=character(), baseline1=numeric(), baseline2=numeric(), mod=list()){
  if (missing(method)) stop("a method must be provided to OpnCoe")
  OpnCoe <- list(coe=coe, fac=fac, method=method, baseline1, baseline2, mod=mod)
  class(OpnCoe) <- c("OpnCoe", "Coe")
  return(OpnCoe)}


# 3. Morpho on Opn -------------------------------------------------------------

rawPolynomials <- function(Opn, degree, baseline1, baseline2){
  UseMethod("rawPolynomials")}

rawPolynomials.Opn <- function(Opn, degree,
                               baseline1=c(-1, 0), baseline2=c(1, 0), nb.pts=120){
  #we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if(missing(nb.pts)){
      nb.pts <- min.pts
      cat(" * 'nb.pts' missing and set to: ", 
              nb.pts, "\n")
    } else {
      nb.pts <- min.pts
      cat(" * at least one outline has less coordinates than 'nb.pts':", 
              nb.pts, "\n")}}
  if (missing(degree)){
    degree <- 5
    cat(" * 'degree' missing and set to: ", degree, "\n")}
  #we normalize
  Opn <- coo.sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo.baseline, 
                ldk1=1, ldk2=nb.pts, t1=baseline1, t2=baseline2)
  #we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow=length(Opn), ncol=degree+1, 
                dimnames=list(rn, cn))
  mod <- list()
  #the loop
  for (i in seq(along=coo)){
    mod <- polynomials(coo[[i]], n=degree, orthogonal=FALSE)
    #mod[[i]] <- pol
    coe[i, ] <- mod$coefficients}
  #mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "rawPolynomials"
  return(OpnCoe(coe=coe, fac=Opn$fac, method=method, 
                baseline1=baseline1, baseline2=baseline2, mod=mod))}

# orthoPolynomials
orthoPolynomials <- function(Opn, degree, baseline1, baseline2, nb.pts){
  UseMethod("orthoPolynomials")}

orthoPolynomials.Opn <- function(Opn, degree,
                                 baseline1=c(-1, 0), baseline2=c(1, 0), nb.pts=120){
  #we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if(missing(nb.pts)){
      nb.pts <- min.pts
      cat(" * 'nb.pts' missing and set to: ", 
          nb.pts, "\n")
    } else {
      nb.pts <- min.pts
      cat(" * at least one outline has less coordinates than 'nb.pts':", 
          nb.pts, "\n")}}
  if (missing(degree)){
    degree <- 5
    cat(" * 'degree' missing and set to: ", degree, "\n")}
  #we normalize
  Opn <- coo.sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo.baseline, 
                ldk1=1, ldk2=nb.pts, t1=baseline1, t2=baseline2)
  #we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow=length(Opn), ncol=degree+1, 
                dimnames=list(rn, cn))
  mod <- list()
  #the loop
  for (i in seq(along=coo)){
    mod <- polynomials(coo[[i]], n=degree, orthogonal=TRUE)
    #mod[[i]] <- pol
    coe[i, ] <- mod$coefficients}
  #mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "orthoPolynomials"
  return(OpnCoe(coe=coe, fac=Opn$fac, method=method, 
                baseline1=baseline1, baseline2=baseline2, mod=mod))}


#natSplines
#cubicSplines
#Bezier
