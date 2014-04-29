
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

# 2. Opn calibration ---------------------------------------------------------
nqual <- function(Opn, ...){UseMethod("nqual")}
nqual.Opn <-
  function(Opn, method=c("rawPolynomials", "orthoPolynomials"),
           id, 
           n.range = c(2, 3, 4, 6, 8, 10),
           smooth.it=0,
           baseline1=c(-1, 0), baseline2=c(1, 0),
           plot.method=c("panel", "stack")[1],
           legend = TRUE,
           legend.title = "Degree",
           palette = col.india,
           shp.border="#1A1A1A",
           ...){
    if (missing(id)) id <- sample(length(Opn$coo), 1)
    if (missing(method)) {
      cat(" * Method not provided. orthoPolynomials is used.\n")
      orthogonal   <- TRUE
    } else {
      p <- pmatch(tolower(method), c("rawpolynomials", "orthopolynomials"))
      if (is.na(p)) { warning(" * Unvalid method. orthoPolynomials is used.\n")
      } else {
        orthogonal <- switch(p, TRUE,   FALSE)}}
    
    # check for too ambitious harm.range
    if (max(n.range) > (min(sapply(Opn$coo, nrow))- 1)) {
      n.range <- (min(sapply(Opn$coo, nrow))- 1)
      cat(" * n.range was too high and set to: ", n.range, ".\n")}
    coo <- Opn$coo[[id]]
    if (smooth.it  != 0) coo <- coo.smoothcurve(coo, smooth.it)
    coo <- coo.baseline(coo, ldk1=1, ldk2=nrow(coo), t1=baseline1, t2=baseline2)
    res <- list()
    for (i in seq(along=n.range)) {
      res[[i]] <- polynomials.i(polynomials(coo, n=n.range[i], orthogonal=orthogonal))}
    # plotting
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    cols <- paste0(palette(length(n.range)), "EE")
    if (plot.method=="stack") {
      #to initiate the plot but stack may be a better option for that part
      coo.plot(coo, border=shp.border, lwd=1)
      for (i in seq(along=n.range)){
        lines(res[[i]], col=cols[i])}
      if (legend) {
        legend("topright", legend = as.character(n.range), bty="n",
               col = cols, lty = 1, lwd=1, cex=0.7,
               title = legend.title)}
    } else {
      if (plot.method=="panel") {
        #par(oma=c(1, 1, 3, 0))
        pos <- coo.list.panel(res, borders=cols, cols=par("bg"), poly=FALSE)
        if (legend) {text(x=pos[, 1], y=pos[, 2],
                          as.character(n.range))}
        title(names(Opn)[id], cex=1.3)}}}

# 3. OpnCoe definition -------------------------------------------------------
OpnCoe <- function(coe=matrix(), fac=data.frame(),
                   method=character(), baseline1=numeric(), baseline2=numeric(), mod=list()){
  if (missing(method)) stop("a method must be provided to OpnCoe")
  OpnCoe <- list(coe=coe, fac=fac, method=method,
                 baseline1=baseline1, baseline2=baseline2, mod=mod)
  class(OpnCoe) <- c("OpnCoe", "Coe")
  return(OpnCoe)}

# The print method for Out objects
print.OpnCoe <- function(x, ...){
  OpnCoe <- x
  p <- pmatch(OpnCoe$method, c("rawPolynomials", "orthoPolynomials"))
  met <- switch(p, "raw Polynomials", "orthogonal Polynomials")
  ### Header
  cat("An OpnCoe object [", met, "analysis ] (see ?OpnCoe) \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- nrow(OpnCoe$coe) #nrow method ?
  degree  <- ncol(OpnCoe$coe)
  # number of outlines and harmonics
  cat(" -", coo.nb, "open outlines described (an lm object in $mod)\n")
  cat(" -", degree, "degree (+Intercept) \n")
  cat(" - registered on the baseline: [(", 
      OpnCoe$baseline1[1], "; ",OpnCoe$baseline1[2], ") - (", 
      OpnCoe$baseline2[1], "; ",OpnCoe$baseline2[2], ")]\n", sep="")
  # lets show some of them for a quick inspection
  cat(" - Polynomials coefficients from random open outlines in $coe: \n")
  row.eg <- sort(sample(coo.nb, 5, replace=FALSE))
  print(signif(OpnCoe$coe[row.eg, ], 3))
  cat("etc.\n")
  # number of grouping factors
  df <- OpnCoe$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined in $fac:\n")
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", 
                                       length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}

# 3. Opn morphometrics ---------------------------------------------------------
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
