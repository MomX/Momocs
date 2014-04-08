
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
hqual <- function(...){UseMethod("hqual")}
hqual.Out <-
  function(Out, method=c("efourier", "rfourier", "tfourier"),
           id, 
           harm.range = c(1, 2, 4, 8, 16, 32),
           smooth.it=0,
           scale=TRUE, center=TRUE, align=TRUE,
           plot.method=c("panel", "stack")[1],
           legend = TRUE,
           legend.title = "Nb of harmonics",
           palette = col.india,
           shp.col=NA,
           shp.border="#1A1A1A",
           ...){
    if (missing(id)) id <- sample(length(Out$coo), 1)
              if (missing(method)) {
                cat(" * Method not provided. efourier is used.\n")
                method   <- efourier
                method.i <- efourier.i 
              } else {
                p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
                if (is.na(p)) { warning(" * Unvalid method. efourier is used.\n")
                } else {
                  method   <- switch(p, efourier,   rfourier,   tfourier)
                  method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)}}

              # check for too ambitious harm.range
              if (max(harm.range) > (min(sapply(Out$coo, nrow))/2 + 1)) {
                harm.range <- floor(seq(1, q/2 - 1, length=6))
                cat(" * harm.range was too high and set to: ", harm.range, ".\n")}
              coo <- Out$coo[[id]]
              if (scale)  coo <- coo.scale(coo)
              if (center) coo <- coo.center(coo)
              if (align)  coo <- coo.align(coo)
              res <- list()
              for (i in seq(along=harm.range)) {
                res[[i]] <- method.i(method(coo, nb.h=max(harm.range), smooth.it=smooth.it), nb.h=harm.range[i])}
              # plotting
              op <- par(mar=c(3, 3, 2, 1))
              on.exit(par(op))
              cols <- paste0(palette(length(harm.range)), "EE")
              if (plot.method=="stack") {
                coo <- coo.smooth(coo, smooth.it)
                 coo.plot(coo, border=shp.border, col=shp.col,
                          lwd=2, points=FALSE, main=names(Out)[id], ...)
                 for (i in seq(along=harm.range)) {lines(res[[i]], col=cols[i], lwd=1)}
                 if (legend) {
                   legend("topright", legend = as.character(harm.range), bty="n",
                          col = cols, lty = 1, lwd=1, cex=0.7,
                          title = legend.title)}
               } else {
                 if (plot.method=="panel") {
                   #par(oma=c(1, 1, 3, 0))
                   pos <- coo.list.panel(res, cols=cols)
                   if (legend) {text(x=pos[, 1], y=pos[, 2],
                                     as.character(harm.range))}
                   title(names(Out)[id], cex=1.3)
                 }}}

hquant <- function(...){UseMethod("hquant")}
hquant.Out <- 
  function(Coo,
           method = c("efourier", "rfourier", "tfourier"),
           id        = 1,
           smooth.it = 0,
           harm.range = seq(4, 20, 4),
           norm.centsize = TRUE,
           dist.method = edm.nearest,
           dist.nbpts = 120,
           plot = TRUE,
           dev.plot=TRUE,
           title = "Deviations along the outline",
           legend = TRUE,
           legend.title = "# harmonics",
           palette = col.summer,
           lineat.y=c(0.5, 0.1, 0.01)){
    if (missing(method)) {
      cat("  * Method not provided. efourier is used.\n")
      method   <- efourier
      method.i <- efourier.i 
    } else {
      p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
      if (is.na(p)) { warning("Unvalid method. efourier is used.")
      } else {
        method   <- switch(p, efourier,   rfourier,   tfourier)
        method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)}}
    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(sapply(Coo$coo[id], nrow))
    nb.h.best  <- floor(min.nb.pts/2)-1
    # we handle too ambitious harm.range
    if (max(harm.range) > nb.h.best) {
      harm.range <- floor(seq(4, nb.h.best, length=6))
      cat("  * 'harm.range' was too high and set to: ", harm.range, ".\n")}
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2*nb.h.best, dist.nbpts)
    nr <- length(harm.range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim=c(nr, nc, nk),
                 dimnames=list(paste0("h", harm.range),
                               paste("pt", 1:nb.pts),
                               names(Coo)[id]))
    # progressbar
    if (nk > 5) {
      pb <- txtProgressBar(1, nk)
      t <- TRUE } else {t <- FALSE}
    # the core loops that will calculate deviations
    for (ind in seq(along=id)) {
      coo <- Coo$coo[[id[ind]]]
      # below, the best possible fit
      coo.best <- method.i(method(coo, nb.h=nb.h.best, smooth.it=smooth.it), nb.pts=nb.pts)
      for (i in seq(along=harm.range)) {
        # for each number of harmonics we calculate deviation with the FUN=method
        coo.i <- method.i(method(coo, nb.h=harm.range[i], smooth.it=smooth.it), nb.pts=nb.pts)
        res[i, , ind] <- dist.method(coo.best, coo.i)
      }
      # we normalize by the centroid size
      if (norm.centsize) {res[,,ind] <- res[,,ind]/coo.centsize(coo)}
      if (t) setTxtProgressBar(pb, ind)}
    # below we manage for single/several individuals
    if (nk > 1) { # if more than 1, we calculate median and sd
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
    } else {
      m <- res[,,1]
      d <- NULL}
    # plotting stuff
    if (plot) {
      cols <- palette(nr)
      if (nk > 1) {ylim <- c(0, max(m+d, na.rm=TRUE))} else {ylim <- range(m)}
      if (norm.centsize) {
        ylab = "Deviation (in % of the centroid size)"
      } else {
        ylab = "Deviation (in original units)"}
      plot(NA, xlim=c(1, nc), ylim=ylim,
           xlab="Points sampled along the outline",
           ylab=ylab, main=title,
           xaxs="i", yaxs="i", axes=FALSE)
      axis(1, at=seq(0, dist.nbpts, length=5))
      axis(2)
      abline(h=lineat.y, lty=2, col="grey90")
      # if you want deviations, here they are
      if (dev.plot) {
        if (nk > 1) {dev.plot(m, d, cols=cols) } else {
          for (i in 1:nr) {
            lines(1:ncol(m), m[i, ], col=cols[i])}}}
      # same for legend
      if (legend) {
        legend("topright", legend = as.character(harm.range), bty="o",
               col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", inset=0.005, cex=0.7,
               title = legend.title)}
      box() }
    return(list(res=res, m=m, d=d))}

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

Ptolemy <- function(...){UseMethod("Ptolemy")}
Ptolemy.Out <- function(Coo,
                     id=1,
                     t=seq(0, 2*pi, length=7)[-1],
                     nb.h=3,
                     nb.pts=360,
                     palette=col.sari,
                     legend=FALSE) {    # we prepare and deduce
              op <- par(no.readonly = TRUE)
              on.exit(par(op))
              par(xpd=NA)
              cols <- palette(nb.h)
              coo <- coo.center(Coo$coo[[id]])
              #k <- floor(length(coo$x)/4)
              coo.plot(coo, main=names(Coo)[id])
              # now we calculate for every harmonic
              coo.ef  <- efourier(coo, nb.h)
              coo.efi <- efourier.i(coo.ef, nb.h, nb.pts)
              vect   <- matrix(nrow=nb.h, ncol=2)
              vect <- rbind(c(0, 0), vect)
              for (i in seq(along=t)) {
                for(j in 1:nb.h) {
                  vect[j+1, 1] <- coo.ef$an[j] * cos(j * t[i]) + coo.ef$bn[j] * sin(j * t[i])
                  vect[j+1, 2] <- coo.ef$cn[j] * cos(j * t[i]) + coo.ef$dn[j] * sin(j * t[i])}
                vs <- apply(vect, 2, cumsum)
                for (j in 1:nb.h){
                  lh   <- efourier.shape(coo.ef$an[1:j], coo.ef$bn[1:j],
                                         coo.ef$cn[1:j], coo.ef$dn[1:j],
                                         nb.h=j, nb.pts=nb.pts, plot=FALSE)
                  ellh <- efourier.shape(coo.ef$an[j], coo.ef$bn[j],
                                         coo.ef$cn[j], coo.ef$dn[j],
                                         nb.h=1, nb.pts=nb.pts, plot=FALSE)
                  lines(lh, col=paste(cols[j], "22", sep=""), lwd=0.8)
                  lines(ellh[,1] + vs[j, 1], ellh[,2] + vs[j, 2],
                        col=cols[j], lwd=1)
                  points(vs[j+1, 1], vs[j+1, 2], col=cols[j], cex=0.8)
                  arrows(vs[j, 1], vs[j, 2], vs[j+1, 1], vs[j+1, 2],
                         col=cols[j], angle=10, length=0.05, lwd=1.2)
                }
              }
              points(0, 0, pch=20, col=cols[1])
              if (legend) {
                legend("topright", legend = as.character(1:nb.h), bty="o",
                       col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", cex=0.7,
                       title = "Number of harmonics")}
            }




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

# 6. OutCoe visualisation methods -------------------------------------------------

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

hcontrib <- function(...){UseMethod("hcontrib")}
hcontrib.OutCoe <- function(
  OutCoe,
  id      = 1,
  harm.range,
  amp.h   = c(0, 0.5, 1, 2, 5, 10),
  palette = col.heat,
  title   = "Harmonic contribution"){
  #if missing id meanshape
  x <- OutCoe$coe
  nb.h <- ncol(x)/4
  if (missing(harm.range)) {
    harm.range <- ifelse (nb.h > 6, 1:6, 1:nb.h) }
    harm.range <- 1:nb.h
  mult <- rep(1, nb.h)
  method.i <- efourier.i
  xf <- list(an=x[id, 1:nb.h + 0*nb.h], bn=x[id, 1:nb.h + 1*nb.h],
             cn=x[id, 1:nb.h + 2*nb.h], dn=x[id, 1:nb.h + 3*nb.h])
  res <- list()
  p <- 1 # dirty
  for (j in seq(along=harm.range)){
    for (i in seq(along=amp.h)){
      mult.loc    <- mult
      mult.loc[harm.range[j]] <- amp.h[i]
      xfi <- lapply(xf, function(x) x*mult.loc)
      res[[p]] <-
        method.i(xfi)
      p <- p+1}}
  
  cols <- rep(palette(length(amp.h)), length(harm.range))
  coo.list.panel(res, dim=c(length(amp.h), length(harm.range)),
                 byrow=FALSE, cols=cols, mar=c(5.1, 5.1, 4.1, 2.1))
  axis(1, at=(1:length(harm.range))-0.5,
       labels=harm.range, line=2, lwd=1, lwd.ticks=0.5)
  mtext("Harmonic rank", side=1, line=4)
  axis(2, at=(1:length(amp.h))-0.5,
       labels=rev(amp.h), line=1, lwd=1, lwd.ticks=0.5)
  mtext("Amplification factor", side=2, line=3)
  title(main=title)
return(res)}


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

clust <- function(...){UseMethod("clust")}
clust.OutCoe <- function(OutCoe, fac,
                        method = "euclidean", type="unrooted", palette=col.summer2, ...){
                if (missing(fac)) {
                    cols <- rep("black", nrow(OutCoe$coe))
                } else {
                  facs <- OutCoe$fac[, fac]
                  cols <- palette(nlevels(facs))[facs]
                }
                dist.mat <- dist(OutCoe$coe, method=method)
                OutCoe.hc <- hclust(dist.mat)
                op <- par(no.readonly = TRUE)
                on.exit(par(op))
                par(oma=rep(0, 4), mar=rep(0,4))
                plot(as.phylo.hclust(OutCoe.hc), tip.color=cols, type=type, ...)
                return(list(dist.mat=dist.mat, hclust=OutCoe.hc))}

meanshapes <- function(...){UseMethod("meanshapes")}
meanshapes.OutCoe <- function(OutCoe, fac, nb.pts=120){
  nb.h <-  ncol(OutCoe$coe)/4
  if (missing(fac)) {
    cat("* no 'fac' provided. Returns meanshape.\n")
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

#discri
#hquant
#hqual
#todo Out.check
# Out.nbh check etc. coderepété sur 10 lines
# smooth.it -> nb.s ?


