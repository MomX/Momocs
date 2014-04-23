# 1. Out -----------------------------------------------------------------------

#' Builds an Out object
#'
#' In Momocs, Out classes objects are lists of \bold{closed} outlines, 
#' on which generic methods such as plotting methods (e.g. stack()) 
#' and specific methods (e.g. efourier()) can be applied.
#' 
#' They must be built from a list (or an array) of (x; y) coordinates matrices.
#'  
#' @export Out
#' 
#' @param coo.list a list of (x; y) matrices of coordinates.
#' @param ldk (optionnal) a list of landmarks on these coordinates 
#' (provided as the row numbers) for every outline
#' @param fac (optionnal) a data.frame of factors, 
#' specifying the grouping structure.
#' @return a \code{Out} object.
#' @aliases Coo
#' @family Out
#' @keywords Out
#' @examples
#' 
#' #coo.list <- list(out1=matrix(1:10, nc=2),
#' #out2=matrix(1:20, nc=2))
#' #anOutobject <- Out(coo.list)
#' #anOutobject
Out  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=ldk, fac=fac)
  if (!is.null(Out$fac)) Out$fac <- .refactor(Out$fac)
  class(Out) <- c("Out", "Coo")
  return(Out)}

# The print method for Out objects
#' @S3method print Out
print.Out <- function(x, ...){
  Out <- x
  ### Header
  cat("An Out object (see ?Out) with: \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- length(Out)
  coo.len <- sapply(Out$coo, nrow)
  coo.closed <- sapply(Out$coo, is.closed)
  # number of outlines
  cat(" -", coo.nb, "outlines\n")
  # one random outline
  eg <- sample(length(Out$coo), 1)
  coo.eg <- Out$coo[[eg]]
  colnames(coo.eg) <- c("x", "y")
  cat(" - One random outline in $coo: '", names(Out$coo)[eg], "':\n", sep="")
  if (nrow(coo.eg) > 5) {
    print(coo.eg[1:5, ], print.gap=2)
    cat("etc.\n")
  } else {
    print(coo.eg, print.gap=2)
    cat("\n\n")}
  # number of coordinates
  cat(" -", round(mean(coo.len )), "+/-", round(sd(coo.len )),
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
      lev.i <- levels(df[, i])
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", 
                                       length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}


# 2. Out calibration -----------------------------------------------------------
hqual <- function(Out, ...){UseMethod("hqual")}
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

hquant <- function(Coo, ...){UseMethod("hquant")}
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
           legend.title = "Nb of harmonics",
           palette = col.summer,
           lineat.y=c(0.5, 0.1, 0.01), ...){
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
        legend("topright", legend = as.character(harm.range), bty="n",
               col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", inset=0.005, cex=0.7,
               title = legend.title)}
      box() }
    return(list(res=res, m=m, d=d))}

#hpow
hpow <- function(Out, ...){UseMethod("hpow")}
hpow.Out <- function(Out, method="efourier", id=1:length(Out),
                     nb.h=16, drop=1, smooth.it=0, plot=TRUE,
                     title="Fourier coefficients power spectrum",
                     lineat.y=c(0.9, 0.95, 0.99, 0.999), bw=0.1, ...){
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

# 3. OutCoe definition -------------------------------------------------------
OutCoe <- function(coe=matrix(), fac=data.frame(), method, norm){
  if (missing(method)) stop("a method must be provided to OpnCoe")
  OutCoe <- list(coe=coe, fac=fac, method=method, norm=norm)
  class(OutCoe) <- c("OutCoe", "Coe")
  return(OutCoe)}

# The print method for Out objects
print.OutCoe <- function(x, ...){
  OutCoe <- x
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

# 4. Out morphometrics ---------------------------------------------------------
#' Calculates elliptical Fourier transforms on Out objects
#'
#' A wrapper for \link{efourier} to be applied on Out objects.
#' @rdname eFourier-Out
#' @export eFourier 
#' @S3method eFourier Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients. See Details.
#' @param start logical whether to consider the first point as homologous
#' @examples
#' data(bot)
#' eFourier(bot, 12)
eFourier <- function(Out, nb.h, smooth.it, norm, start){
  UseMethod("eFourier")}

eFourier.Out <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h)) {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if(nb.h > (q+1)*2) {
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


#' Calculates radius lengths Fourier analysis on Out objects
#' 
#' A wrapper for \link{rfourier} to be applied on Out objects.
#' @rdname rFourier-Out
#' @export rFourier 
#' @S3method rFourier Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients. See Details.
#' @examples
#' data(bot)
#' rFourier(bot, 12)
rFourier <- function(Out, nb.h, smooth.it, norm){
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
  return(OutCoe(coe=coe, fac=Out$fac, method="rFourier", norm=norm))}

#' Calculates tangent angle Fourier analysis on Out objects
#' 
#' A wrapper for \link{tfourier} to be applied on Out objects.
#' @rdname tFourier-Out
#' @export tFourier 
#' @S3method tFourier Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients. See Details.
#' @examples
#' data(bot)
#' tFourier(bot, 12)
tFourier <- function(Out, nb.h, smooth.it, norm){
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
  return(OutCoe(coe=coe, fac=Out$fac, method="tFourier", norm=norm))}

#' Ptolemaic ellipses and illustration of eFourier
#' 
#' Calculate and display Ptolemaic ellipses which illustrates 
#' intuitively the principle behing elliptical Fourier analysis.
#' @export Ptolemy
#' @S3method Ptolemy Out
#' @param Out The \code{Out} object on which to display Ptolemaic ellipses.
#' @param id The \code{id} on which to display Ptolemaic ellipses.
#' @param t A \code{vector} af angles (in radians) on which to display ellipses.
#' @param nb.h \code{integer}. The number of harmonics to display.
#' @param nb.pts \code{integer}. The number of points to use to display shapes.
#' @param palette A color palette.
#' @param legend \code{logical}. Whether to plot the legend box.
#' @references 
#' This method has been inspired by the figures found in the followings papers.
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#'  \emph{Computer Graphics and Image Processing} \bold{18}: 236-258.
#' Crampton JS. 1995. Elliptical Fourier shape analysis of fossil bivalves: 
#' some practical considerations. \emph{Lethaia} \bold{28}: 179-186.
#' @seealso \link{efourier}. 
#' An intuitive explanation of elliptic Fourier analysis can be found in 
#' the \bold{Details} section of the \link{efourier} function.
#' @examples
#' data(hearts)
#' Ptolemy(hearts, 1)
Ptolemy <- function(Out, id, t, nb.h, nb.pts, palette, legend){UseMethod("Ptolemy")}
Ptolemy.Out <- function(Out,
                     id=1,
                     t=seq(0, 2*pi, length=7)[-1],
                     nb.h=3,
                     nb.pts=360,
                     palette=col.sari,
                     legend=FALSE) {
  # we prepare and deduce
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd=NA)
  cols <- palette(nb.h)
  coo <- coo.center(Out$coo[[id]])
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
           title = "Number of harmonics")}}


# XXX. Out TODO ----------------------------------------------------------------
#c OutCoe
#discri
#todo Out.check
# Out.nbh check etc. coderepété sur 10 lines
# smooth.it -> nb.s ?


