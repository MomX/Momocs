
# 1. Out builder and domestic functions -------------------------------------------

#' Builds an Out object
#'
#' In Momocs, Out classes objects are lists of closed outlines, on which generic methods
#' such as plotting methods (e.g. stack()) and specific methods (e.g. efourier()) can be applied.
#' 
#' They must be built from a list (or an array) of coordinates.
#'  
#' @export Out Coo 
#' 
#' @param coo a matrix of (x,y) coordinates or a \code{Out} object.
#' @param ldk (optionnal) a list of landmarks on these coordinates (provided as the row numbers) for every outline
#' @param fac (optionnal) a data.frame of factors, specifying the grouping structure.
#' @return a \code{Out} object.
#' @aliases Coo
#' @family Out
#' @keywords Out
#' @examples
#' coo.list <- list(out1=matrix(1:10, nc=2),
#' out2=matrix(1:20, nc=2))
#' anOutobject <- Out(coo.list)
#' anOutobject
# Out class builder
Out  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=list(), fac=fac)
  class(Out) <- "Out"
  return(Out)}

# For historical reasons.
# Before version 1.xxx 'Out' classes used to be S4 classes and called 'Coo'
# See http://www.jstatsoft.org/v56/i13
Coo  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=list(), fac=fac)
  class(Out) <- "Out"
  return(Out)}


# The print method for Out objects
print.Out <- function(Out){
  ### Header
  cat("An Out object (see ?Out) with: \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- length(Out)
  coo.len <- sapply(Out$coo, nrow)
  coo.closed <- sapply(Out$coo, is.closed)
  # number of outlines
  cat(" -", coo.nb, "outlines\n")
  # one random outline
  eg <- sample(length(Out), 1)
  coo.eg <- Out$coo[[eg]]
  colnames(coo.eg) <- c("x", "y")
  cat(" - One random outline in $coo: '", names(Out)[eg], "':\n", sep="")
  if (nrow(coo.eg) > 5) {
    print(coo.eg[1:5, ], print.gap=2)
    cat("etc.\n")
  } else {
    print(coo.eg, print.gap=2)
    cat("\n\n")}
  # number of coordinates
  cat(" -", round(mean(coo.len )), "+/-", round(sd(coo.len )), "coordinates per outline\n")
  # outlines closed or not
  if (all(coo.closed)) {
    cat(" - All outlines are closed\n")
  } else {
    if (any(!coo.closed)) {
      cat(" - All outlines are unclosed\n")
    } else {
      cat(" -", sum(coo.closed), "outlines are closed\n")}}
  # number of landmarks
  if (length(Out$ldk)!=0) {
    cat(" -", length(Out$ldk[[1]]), "landmark(s) defined\n")
  } else {
    cat(" - No landmark defined\n")}
  # number of grouping factors
  df <- Out$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      cat("     ", colnames(df)[i], ": ", levels(df[, i]),"\n")}}}

# allows to maintain the tradition str() behaviour
#' @export str.Out "[.Out" "[[.Out" length.Out names.Out "names<-.Out" print.Out
#' @export plot.Out stack.Out panel panel.Out 
str.Out <- function(Out){
  ls.str(Out)}

# Out can be indexing both to [ ] and [[ ]]
# and returns the corresponding coordinate(s)
# We define some getters
"[.Out" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

"[[.Out" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

# length on an Out return the length of Out$coo, ie the number of coordinates
length.Out <- function(Out) {
  return(length(Out$coo))}

# names() on a Out retrieves the names of the Out$coo
names.Out <- function(Out){
  return(names(Out$coo))}

# which can in return may be named using names(Out) <- 
"names<-.Out" <- function(x, value){
  names(x$coo) <- value
  return(x)}

# candidate for the dirtiest function ever...
subset.Out <- function(Out, subset){
  e <- substitute(subset)
  retain <- eval(e, Out$fac, parent.frame())
  Out2 <- Out
  Out2$coo <- Out$coo[retain]
  if (length(Out$ldk)>0) Out2$ldk <- Out$ldk[retain]
  if (ncol(Out$fac)>0) {
    Out2$fac <- Out$fac
    Out2$fac <- as.data.frame(Out2$fac[retain, ])
    names(Out2$fac) <- names(Out$fac)
Out2$fac <- .refactor(Out2$fac)
  }
  return(Out2)}

# 2. Out plotting methods ----------------------------------------------------
# The main plot method that when plot(Out)
# For a quick investigation of the shpes included in a Coo object
plot.Out <- function(Out, id, ...){
  if (missing(id)) {
    repeat{
      id <- sample(length(Out), 1)
      coo.plot(Out$coo[[id]], main=names(Out)[id], ...)
      readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}
  if (id[1]=="all") { id <- 1:length(Out)}
  if (is.numeric(id)){
    if (length(id)==1) {
      coo.plot(Out$coo[[id]], main=names(Out)[id], ...)
    } else {
      for (i in seq(along=id)) {
        coo.plot(Out$coo[[id[i]]], main=names(Out)[id[i]], ...)
        readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}}}

# stack(Out) shows all the shapes stacked on the same plane
stack.Out <- function(x, cols, borders,
                      points=FALSE, first.point=TRUE, centroid=TRUE,
                      ldk=TRUE, ldk.pch=3, ldk.col="red", ldk.cex=1, xy.axis=TRUE){
  Out <- x
  if (missing(cols)) {
    cols     <- rep(NA, length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#33333355", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))}
  op <- par(mar=c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Out$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)
  if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}
  for (i in 1:length(Out)) {
    coo.draw(Out$coo[[i]], col=cols[i], border=borders[i],
             points=points, first.point=TRUE, centroid=centroid)}
  if (ldk & length(Out$ldk)!=0) {
    points(Out[Out$ldk, ], pch=ldk.pch, col=ldk.col, cex=ldk.cex)}}

# panel(Out) for a family picture of the shapes
panel <- function(x, ...){UseMethod("panel")}
panel.Out <- function(Out, cols, borders, names=NULL, cex.names=0.6, ...){
  
  if (missing(cols)) {
    cols     <- rep("#33333322", length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))}
  pos <- coo.list.panel(Out$coo, cols=cols, borders=borders, ...)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Out), cex=cex.names)
    } else {    
      if (length(names)!=length(Out)) stop("* 'names' and Out lengths differ.")
      text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}



# 3. Out methods (calibration) ----------------------------------------------------

#hpow
hpow <- function(...){UseMethod("hpow")}
hpow.Out <- function(Out, method="efourier", id=1:length(Out),
                     nb.h=16, drop=1, smooth.it=0, plot=TRUE,
                     title="Fourier coefficients power spectrum",
                     lineat.y=c(0.9, 0.95, 0.99, 0.999), bw=0.1){
              probs <- c(1, 0.5, 0)
              # for one signle outline
              if (missing(method)) {
                cat(" * Method not provided. efourier is used.\n")
                method   <- efourier
              } else {
                p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
                if (is.na(p)) { warning("Unvalid method. efourier is used.")
                } else {
                  method   <- switch(p, efourier,   rfourier,   tfourier)}}
              res <- matrix(nrow=length(id), ncol=(nb.h-drop))
              x <- (drop+1) : nb.h
              for (i in seq(along=id)) {
                xf  <- method(Out$coo[[id[i]]], nb.h = nb.h, smooth.it = smooth.it)
                pow <- harm.pow(xf)[x]
                res[i, ] <-  (cumsum(pow)/sum(pow))}
              res <- apply(res, 2, quantile, probs=probs)
              rownames(res) <- c("Max", "Med", "Min")
              colnames(res) <- paste("h", x, sep="")
              if (plot){
                plot(NA, xlim = range(x), ylim = c(min(res), 1), las=1, yaxs="i", 
                     xlab = "Harmonic rank", ylab = "Cumulative harmonic power",
                     main=title, sub=paste0("(", length(id), " outlines included)"), axes=FALSE)
                axis(1, at=x) ; axis(2)
                abline(h=lineat.y, lty=2, col="grey90")
                segments(x,    res[1, ], x,    res[3, ], lwd=0.5)
                segments(x-bw, res[1, ], x+bw, res[1, ], lwd=0.5)
                segments(x-bw, res[3, ], x+bw, res[3, ], lwd=0.5)
                lines(x, res[2, ], type="o", pch=20, cex=0.6) 
                box()
              }
              return(res)}

# 4. Out methods (Fourier analysis) ------------------------------------------
eFourier     <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  UseMethod("eFourier")}

eFourier.Out <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1 #todo
    warning(" * at least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q,"\n")}
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:4], each = nb.h), rep(1:nb.h, times = 4))
  coe <- matrix(ncol = 4 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) { #todo: vectorize ?
    ef <- efourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, verbose = TRUE)
    if (norm) {
      ef <- efourier.norm(ef, start=start)
      if (ef$A[1] < 0) {
        ef$A <- (-ef$A)
        ef$B <- (-ef$B)
        ef$C <- (-ef$C)
        ef$D <- (-ef$D)
        ef$lnef <- (-ef$lnef)}
      coe[i, ] <- c(ef$A, ef$B, ef$C, ef$D)
    } else {
      coe[i, ] <- c(ef$an, ef$bn, ef$cn, ef$dn)}}
  return(OutCoe(coe=coe, fac=Out$fac, method="eFourier", norm=norm))}

rFourier <- function(Out, nb.h = 40, nb.smooth = 0, norm =TRUE){
  UseMethod("rFourier")}

rFourier.Out <- function(Out, nb.h = 40, smooth.it = 0, norm = TRUE) {
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * nb.h not provided and set to", nb.h, "\n")}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1 #todo
    warning("* at least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q,"\n")}
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) {
    rf <- rfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, norm=norm, verbose = TRUE) #todo: vectorize
    coe[i, ] <- c(rf$an, rf$bn)}
  return(OutCoe(coe, fac=Out$fac, method="rFourier"))}

tFourier <- function(Out, nb.h = 40, smooth.it = 0, norm =TRUE){
  UseMethod("tFourier")}

tFourier.Out <- function(Out, nb.h=40, smooth.it = 0, norm=TRUE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- if (q >= 32) { 32 } else { q }
    cat(paste("  * nb.h not provided and set to", nb.h, "\n"))}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1
    warning("At least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q, "\n")}
  coo <-Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) {
    tf <- tfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, norm=norm, verbose=TRUE)
    coe[i, ] <- c(tf$an, tf$bn)}
  return(OutCoe(coe, fac=Out$fac, method="tFourier"))}


# 5. OutCoe definition -------------------------------------------------------


OutCoe <- function(coe.matrix, fac=data.frame(), method, norm){
  if (missing(method)) stop("a method must be provided to Coe")
  OutCoe <- list(coe=coe.matrix, fac=fac, method=method, norm=norm)
  class(OutCoe) <- "OutCoe"
  return(OutCoe)}

# The print method for Out objects
print.OutCoe <- function(OutCoe){
  p <- pmatch(OutCoe$method, c("eFourier", "rFourier", "tFourier"))
  met <- switch(p, "elliptical Fourier", "radii variation", "tangent angle")
  ### Header
  cat("An OutCoe object [", met, "analysis ] (see ?OutCoe) \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- nrow(OutCoe$coe) #nrow method ?
  harm.nb <- ncol(OutCoe$coe)/ifelse(p == 1, 4, 2)
  # number of outlines and harmonics
  cat(" -", coo.nb, "outlines described\n")
  cat(" -", coo.nb, "harmonics\n")
  # lets show some of them for a quick inspection
  cat(" - Some harmonic coefficients from random outlines in $coe: \n")
  row.eg <- sort(sample(coo.nb, 5, replace=FALSE))
  col.eg <- coeff.sel(retain=ifelse(harm.nb > 3, 3, harm.nb), drop=0,
                      nb.h=harm.nb, cph=ifelse(p==1, 4, 2))
  print(signif(OutCoe$coe[row.eg, col.eg], 3))
  cat("etc.\n")
  # number of grouping factors
  df <- OutCoe$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      cat("     ", colnames(df)[i], ": ", levels(df[, i]),"\n")}}}

# 6. OutCoe plotting methods -------------------------------------------------

boxplot.OutCoe <- function(OutCoe, retain, drop, palette=col.gallus,
                           title= "Variation of harmonic coefficients", legend=TRUE){
              # we deduce and prepare
              x <- OutCoe$coe
              nb.h  <- ncol(x)/4
              cph   <- 4
              if (missing(retain)) retain <- nb.h
              if (missing(drop)) drop <- 0
              cs    <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=cph)
              range <- (drop+1):retain
              # we save the old par and prepare the plot
              op <- par(no.readonly = TRUE)
              on.exit(par(op))
              cols <- palette(cph)

                mv <- max(abs(range(x[, cs])))
                ylim <- c(-mv, mv) 

              plot(NA, ylim=ylim, xlim=range(range)+c(-0.6,0.6),
                   xlab="Harmonic rank", ylab="Coefficient value", main=title,
                   axes=FALSE, xaxs="i")
              abline(v=range+0.4, col="grey80")
              abline(h=0, col="grey80")
              for (i in 1:cph) {  
                boxplot(x[,(i-1)*nb.h+range],
                        range=0, boxwex=0.2, at=range-0.6 + (i*0.2),
                        col=cols[i], names=FALSE, border=cols[i], axes=FALSE, add=TRUE)
              }
              axis(1, at=range-0.1, labels=range)  
              axis(2)
              if (legend) {
                legend("topright", legend = LETTERS[1:cph], bty="o",
                       fill = cols, bg="#FFFFFFBB",
                       cex=0.7, inset=0.005,
                       title = " Harmonic coefficients ")
              }
              box()
            }


hist.OutCoe <-
  function(OutCoe, retain, drop, palette=col.gallus,
           title= "Variation of harmonic coefficients", legend=TRUE){
  # we deduce and prepare
  x <- OutCoe$coe
  nb.h  <- ncol(x)/4
  cph   <- 4
  if (missing(retain)) retain <- nb.h
  if (missing(drop)) drop <- 0
  cs    <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=cph)
  range <- (drop+1):retain
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  cols <- palette(cph)
  layout(matrix(1:length(cs), ncol=cph, byrow=TRUE))
  par(oma=c(2, 2, 5, 2), mar=rep(2, 4))
  h.names <- paste(rep(LETTERS[1:cph], each=retain-drop), range, sep="")
  cols    <- rep(cols, each=retain-drop)
  for (i in seq(along=cs)) { # thx Dufour, Chessel and Lobry
    h  <- x[, cs[i]] 
    h0 <- seq(min(h), max(h), len=50)
    y0 <- dnorm(h0, mean(h), sd(h))
    hist(h, main=h.names[i], col=cols[i], proba=TRUE, xlab="", ylab="", las=1)
    abline(v=mean(h), lwd=1)
    lines(h0, y0, col = "black", lwd = 2)}
  title(main=title, cex.main=2, font=2, outer=TRUE)}

# 6. OutCoe methods ----------------------------------------------------------
pca <- function(x, ...){UseMethod("pca")}
pca.OutCoe <- function(OutCoe){
  PCA <- prcomp(OutCoe$coe, scale.=FALSE, center=TRUE)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  class(PCA) <- c("OutPCA", class(PCA))
  return(PCA)}

plot.OutPCA <- function(#basics
  PCA, fac, xax=1, yax=2, 
  #color choice
  col="black", pch=20, palette=col.summer2,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp=1,
  size.shp=20, border.shp="#00000055", col.shp="#00000011",
  #stars
  stars=TRUE,
  #ellipses
  ellipses=TRUE, conf=0.5,
  #convexhulls
  chull=TRUE,
  #labels
  labels=TRUE,
  #axisnames
  axisnames=TRUE,
  #axisvar
  axisvar=TRUE,
  #eigen
  eigen=TRUE,
  #
  rug=TRUE,
  title=substitute(PCA)
){
  xy <- PCA$x[, c(xax, yax)]
  # we check and prepare
  if (!missing(fac)) {
    if (!is.factor(fac)) { fac <- factor(PCA$fac[, fac]) }
    if (missing(col)) {
      col.groups <- palette(nlevels(fac))
      col <- col.groups[fac]}
    if (!missing(pch)) {
      if (length(pch)==nlevels(fac)) { pch <- pch[fac] }}}
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  .frame(xy, center.origin, zoom=zoom)
  if (grid) .grid(xy)
  .morphospace(xy, pos.shp=pos.shp, rot=PCA$rotation[, c(xax, yax)], mshape=PCA$mshape,
               size.shp=size.shp, border.shp=border.shp, col.shp=col.shp)
  if (!missing(fac)) {
    if (stars)    .stars(xy, fac, col.groups)
    if (ellipses) .ellipses(xy, fac, conf=conf, col.groups) #+conf
    if (chull)    .chull(xy, fac, col.groups)
    if (labels)   .labels(xy, fac, col.groups)
    if (rug)      .rug(xy, fac, col.groups)
  } else {
    if (rug)      .rug(xy, NULL, col)
  }
  points(xy, pch=pch, col=col)
  if (axisnames)  .axisnames(xax, yax)
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax)
  box()}

# manova
manova.default <- manova
manova <- function(...){UseMethod("manova")}
manova.OutCoe <- function(OutCoe, fac, retain, drop){
  if (missing(fac)) stop("'fac' must be provided")
  fac <- OutCoe$fac[, fac]
  x <- OutCoe$coe
  if (missing(drop)) {
    if (OutCoe$norm) {
      drop <- 1
      cat(" * 1st harmonic removed (because of normalization)\n")
    } else {
      drop <- 0 }}
  
  nb.h <- ncol(x)/4
  fr   <- floor((ncol(x)-2)/4) #full rank efourier
  
  if (!missing(retain)) {
    if ((retain - drop) > fr) {
      retain <- fr
      if (retain > nb.h) retain <- nb.h
      cat("'retain' was too high and the matrix not of full rank. Analysis done with", retain, "harmonics\n")}
  } else {
    retain <- fr
    if (retain > nb.h) {retain <- nb.h}
    cat("* Analysis done with", retain, "harmonics\n")}
  
  harm.sel <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=4)
  mod <- summary(manova(x[,harm.sel]~fac), test="Hotelling")
  return(mod)}

#meanshapes
meanshapes <- function(...){UseMethod("meanshapes")}
meanshapes.OutCoe <- function(OutCoe, fac, nb.pts=120){
  nb.h <-  ncol(OutCoe$coe)/4
  if (missing(fac)) {
    cat("* no 'fac' provided. Returns meanshape.")
    coe.meanshape <- apply(OutCoe$coe, 2, mean)
    xf <- coeff.split(coe.meanshape, nb.h, 4)
    return(efourier.i(xf, nb.pts=nb.pts))}
  
  f <- OutCoe$fac[, fac]
  fl <- levels(f)
  res <- list()
  
  for (i in seq(along=fl)){
    coe.i <- OutCoe$coe[f==fl[i], ]
    if (is.matrix(coe.i)) {
      coe.i <- apply(coe.i, 2, mean)}
    xf <- coeff.split(cs=coe.i, nb.h=nb.h, cph=4)
    res[[i]] <- efourier.i(xf, nb.h=nb.h, nb.pts=nb.pts)}
  names(res) <- fl
  return(res)}



# 0. Out TODO ----------------------------------------------------------------

#c OutCoe
# boxpltoCoe
#hist
#hcontrib
#deprecate ellipse par
#manova
#meanshape
#clust
#discri
#hquant
#hqual
#clust
#todo Out.check
# Out.nbh check etc. coderepété sur 10 lines
# smooth.it -> nb.s ?


