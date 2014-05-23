# 1. Out -----------------------------------------------------------------------

#' Builds an Out object
#'
#' In Momocs, \code{Out} classes objects are lists of \bold{closed} outlines,
#' on which generic methods such as plotting methods (e.g. \link{stack}) 
#' and specific methods (e.g. \link{eFourier} can be applied.
#'  \code{Out} objects are primarily \code{\link{Coo}} objects.
#' 
#' @param coo.list \code{list} of matrices of \eqn{(x; y)} coordinates
#' @param ldk (optionnal) \code{list} of landmarks as row number indices
#' @param fac (optionnal) a \code{data.frame} of factors, 
#' specifying the grouping structure
#' @return an \code{Out} object
#' @details These methods can be applied on \code{Out} objects:
#' \enumerate{
#' \item Handling: \code{subset, coo.} family;
#' \item Graphics: \code{plot, stack, panel};
#' \item Calibration : \code{hqual, hquant, hpow};
#' \item Morpho: \code{eFourier, tFourier, tFourier, Ptolemy}.
#' }
#'
#' @seealso \link{Coo}, \link{Opn}
#' @keywords Out
#' @aliases Out
#' @export
Out  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=ldk, fac=fac)
  if (!is.null(Out$fac)) Out$fac <- .refactor(Out$fac)
  class(Out) <- c("Out", "Coo")
  return(Out)}

# merge method for Out objects (experimental)

#' Combine Out objects
#' 
#' @param ... a list of Out objects
#' @export
combine <- function(...){UseMethod("combine")}
#' @export
combine.Out <- function(...){
  args <- list(...)
  Out     <- Out(do.call( c, lapply( args, c )))
  Out$fac <- do.call("rbind", lapply(args, function(x) x$fac))
  if (any(lapply(args, function(x) length(x$ldk))!=0)){
    Out$ldk <- do.call("rbind", lapply(args, function(x) x$ldk))}
  return(Out)}

# The print method for Out objects
#' @export
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
#' Graphical calibration for Out objects
#' 
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number.
#' 
#' @aliases hqual
#' @param Out the \code{Out} object on which to hqual
#' @param method any method from \code{c("efourier", "rfourier", "tfourier")}
#' @param id the shape on which to perform hqual
#' @param harm.range vector of harmonics on which to perform hqual
#' @param smooth.it numeric, number of smoothing iterations
#' @param scale logical whether to scale the shape
#' @param center logical whether to center the shape
#' @param align logical whether to align th shape
#' @param plot.method either \code{"\link{panel}"} or \code{"\link{stack}"}
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param shp.col a color for the shape (\code{NA} by default)
#' @param shp.border a color for the border of the shape
#' @param ... additional parameters to fed \link{coo.plot}
#' @keywords Out
#' @examples
#' data(bot)
#' hqual(bot)
#' @export
hqual <- 
  function(Out,
           method=c("efourier", "rfourier", "tfourier"),
           id, 
           harm.range = c(1, 2, 4, 8, 16, 32),
           smooth.it=0,
           scale=TRUE, center=TRUE, align=TRUE,
           plot.method=c("panel", "stack")[1],
           legend = TRUE,
           legend.title = "Nb of harmonics",
           palette = col.india,
           shp.col=NA,
           shp.border="#1A1A1A", ...){UseMethod("hqual")}
#' @export
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

#' Quantitative calibration, through deviations, for Out objects
#' 
#' Calculate deviations from original and reconstructed shapes using a
#' range of harmonic number.
#' 
#' @aliases hquant
#' @param Coo the \code{Out} object on which to hquant
#' @param method any method from \code{c("efourier", "rfourier", "tfourier")}
#' @param id the shape on which to perform hquant
#' @param harm.range vector of harmonics on which to perform hquant
#' @param smooth.it numeric, number of smoothing iterations
#' @param norm.centsize logical whether to normalize deviation by the centroid size
#' @param dist.method a method such as \link{edm.nearest} to calculate deviations
#' @param dist.nbpts numeric the number of points to use for deviations calculations
#' @param plot logical whether to plot the results
#' @param dev.plot logical whether to plot deviations
#' @param title a title for the plot
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param lineat.y vector of numeric for drawing horizontal lines
#' @keywords Out
#' @examples
#' data(bot)
#' hqual(bot)
#' @export
hquant <- 
  function(Coo,
           method = c("efourier", "rfourier", "tfourier"),
           id        = 1,
           harm.range = seq(4, 20, 4),
           smooth.it = 0,
           norm.centsize = TRUE,
           dist.method = edm.nearest,
           dist.nbpts = 120,
           plot = TRUE,
           dev.plot=TRUE,
           title = "Deviations along the outline",
           legend = TRUE,
           legend.title = "Nb of harmonics",
           palette = col.summer,
           lineat.y=c(0.5, 0.1, 0.01)){UseMethod("hquant")}

#' @export
hquant.Out <- 
  function(Coo,
           method = c("efourier", "rfourier", "tfourier"),
           id        = 1,
           harm.range = seq(4, 20, 4),
           smooth.it = 0,
           norm.centsize = TRUE,
           dist.method = edm.nearest,
           dist.nbpts = 120,
           plot = TRUE,
           dev.plot=TRUE,
           title = "Deviations along the outline",
           legend = TRUE,
           legend.title = "Nb of harmonics",
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
        legend("topright", legend = as.character(harm.range), bty="n",
               col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", inset=0.005, cex=0.7,
               title = legend.title)}
      box() }
    invisible(list(res=res, m=m, d=d))}

#' Quantitative calibration, through harmonic power, for Out objects
#' 
#' Estimates the number of harmonics required for the three Fourier methods
#' implemented so far in Momocs: elliptical Fourier analysis 
#' (see \link{efourier}), radii variation analysis (see \link{tfourier}) 
#' and tangent angle analysis (see \link{tfourier}). 
#' It returns and can plot cumulated harmonic power whether dropping 
#' the first harmonic or not. 
#' 
#' @aliases hpow
#' @param Out the \code{Out} object on which to hpow
#' @param method any method from \code{c("efourier", "rfourier", "tfourier")}
#' @param id the shape on which to perform hpow. All by default
#' @param nb.h numeric the maximum number of harmonic
#' @param drop numeric the number of harmonics to drop for the cumulative sum
#' @param smooth.it numeric, number of smoothing iterations
#' @param plot logical whether to plot the result or simply return the matrix
#' @param title a title for the plot
#' @param lineat.y vector of numeric for drawing horizontal lines
#' @param bw width of horizontal segments drawn for each harmonic.
#' @return a matrix containing cumulated harmonic power for each harmonic. 
#' @details 
#' The power of a given harmonic \eqn{n} is calculated as follows for 
#' elliptical Fourier analysis and the n-th harmonic:
#' \deqn{HarmonicPower_n \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}}
#' and as follows for radii variation and tangent angle:
#' \deqn{
#' HarmonicPower_n= \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}
#' }
#' @keywords Out
#' @examples
#' data(bot)
#' hpow(bot)
#' @export
hpow <- 
  function(Out, method="efourier", id=1:length(Out),
           nb.h=16, drop=1, smooth.it=0, plot=TRUE,
           title="Fourier coefficients power spectrum",
           lineat.y=c(0.9, 0.95, 0.99, 0.999), bw=0.1){UseMethod("hpow")}
#' @export
hpow.Out <- 
  function(Out, method="efourier", id=1:length(Out),
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
           main=title,sub=paste0("(", length(id), " outlines included)"), axes=FALSE)
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
#' Builds an OutCoe object
#'
#' In Momocs, \code{OutCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot}) 
#' and specific methods can be applied.
#'  \code{OutCoe} objects are primarily \code{\link{Coe}} objects.
#' 
#' @param coe \code{matrix} of harmonic coefficients
#' @param fac (optionnal) a \code{data.frame} of factors, 
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param norm the normalisation used to obtain these coefficients
#' @return an \code{OutCoe} object
#' @details These methods can be applied on \code{Out} objects:
#' \enumerate{
#' \item Graphics: \code{hist, boxplot};
#' \item Multivariate analyses: \code{pca, etc.}.
#' }
#'
#' @seealso \link{Coe}, \link{OpnCoe}
#' @keywords OutCoe
#' @aliases OutCoe
#' @export
OutCoe <- function(coe=matrix(), fac=data.frame(), method, norm){
  if (missing(method)) stop("a method must be provided to OutCoe")
  OutCoe <- list(coe=coe, fac=fac, method=method, norm=norm)
  class(OutCoe) <- c("OutCoe", "Coe")
  return(OutCoe)}

#' @export
combine.OutCoe <- function(...){
  args <- list(...)
  #Out     <- Out(do.call( c, lapply( args, c )))
  coeS <- do.call("cbind", lapply(args, function(x) x$coe))
  facS <- args[[1]]$fac
  methodS <- do.call(c, lapply(args, function(x) x$method))
  normS <- do.call(c, lapply(args, function(x) x$norm))
  OutCoe <- OutCoe(coe=coeS, fac=facS, method=methodS, norm=normS)
  return(OutCoe)}
  
##### TO FIX FOR Combined OutCoe
# The print method for Out objects
#' @export
print.OutCoe <- function(x, ...){
  OutCoe <- x
  p <- pmatch(OutCoe$method[1], c("eFourier", "rFourier", "tFourier"))
  met <- switch(p, "elliptical Fourier", "radii variation", "tangent angle")
  ### Header
  cat("An OutCoe object [", met, "analysis ] (see ?OutCoe) \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- nrow(OutCoe$coe) #nrow method ?
  harm.nb <- ncol(OutCoe$coe)/ifelse(p == 1, 4, 2)
  # number of outlines and harmonics
  cat(" -", coo.nb, "outlines described\n")
  cat(" -", harm.nb, "harmonics\n")
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
    cat(" -", nf, "grouping factor(s) defined in $fac:\n")
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      if (length(lev.i)>10) lev.i <- c(lev.i[1:10], " ... ", 
                                       length(lev.i)-10, "more")
      cat("     ", colnames(df)[i], ": ", lev.i,"\n")}}}

# 4. Out morphometrics ---------------------------------------------------------
#' Calculates elliptical Fourier transforms on Out objects
#'
#' A wrapper for \link{efourier} to be applied on Out objects.
#' @rdname eFourier-Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the coefficients using \link{efourier.norm}
#' @param start logical whether to consider the first point as homologous
#' @seealso \link{efourier}, \link{efourier.norm}
#' @examples
#' data(bot)
#' eFourier(bot, 12)
#' @export
eFourier <- function(Out, nb.h, smooth.it, norm, start){
  UseMethod("eFourier")}
#' @export
eFourier.Out <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) 
  if (missing(nb.h)) {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if(nb.h > q) {
    nb.h <- q # should not be 1 #todo
    cat(" * at least one outline has no more than", q*2, "coordinates.\n",
        "* 'nb.h' has been set to", q,"harmonics.\n")}
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
  coe[abs(coe)<1e-12] <- 0 #not elegant but round normalized values to 0
  return(OutCoe(coe=coe, fac=Out$fac, method="eFourier", norm=norm))}

#' Calculates radius lengths Fourier analysis on Out objects
#' 
#' A wrapper for \link{rfourier} to be applied on Out objects.
#' @rdname rFourier-Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients.
#' @seealso \link{rfourier}
#' @examples
#' data(bot)
#' rFourier(bot, 12)
#' @export
rFourier <- function(Out, nb.h, smooth.it, norm){
  UseMethod("rFourier")}
#' @export
rFourier.Out <- function(Out, nb.h = 40, smooth.it = 0, norm = TRUE) {
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h))  {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * nb.h not provided and set to", nb.h, "\n")}
  if(nb.h  > q) {
    nb.h <- q # should not be 1 #todo
    cat(" * at least one outline has no more than", q*2, "coordinates.\n", 
        "* 'nb.h' has been set to", q,"harmonics.\n")}
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
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients. See Details.
#' @seealso \link{tfourier}
#' @examples
#' data(bot)
#' tFourier(bot, 12)
#' @export
tFourier <- function(Out, nb.h, smooth.it, norm){
  UseMethod("tFourier")}
#' @export
tFourier.Out <- function(Out, nb.h=40, smooth.it = 0, norm=TRUE){
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h))  {
    nb.h <- if (q >= 32) { 32 } else { q }
    cat(paste("  * nb.h not provided and set to", nb.h, "\n"))}
  if(nb.h  > q) {
    nb.h <- q # should not be 1
    cat(" * At least one outline has no more than", q*2, "coordinates.\n", 
        "* 'nb.h' has been set to", q, "harmonics.\n")}
  coo <-Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) {
    tf <- tfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, norm=norm, verbose=TRUE)
    coe[i, ] <- c(tf$an, tf$bn)}
  return(OutCoe(coe=coe, fac=Out$fac, method="tFourier", norm=norm))}

# 5. Out + landmarks ---------------------------------------------------------

#' Define landmarks on Out and Opn objects
#' 
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that 
#' correspond to the nearest points clicked on every outlines are 
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @keywords Out Opn Ldk
#' @examples
#' \dontrun{
#' data(bot)
#' bot <- bot[1:5] # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by defLandmarks...
#' bot2 <- defLandmarks(bot, 3) 
#' stack(bot2)
#' bot2$ldk
#' }
#' @export
defLandmarks <- function(Coo, nb.ldk){UseMethod("defLandmarks")}
#' @export
defLandmarks.Coo <- function(Coo, nb.ldk){
  if (missing(nb.ldk)) stop(" * 'nb.ldk' must be specified.")
  ldk <- list()
  for (i in seq(along=Coo$coo)){
    Coo$ldk[[i]] <- coo.ldk(Coo$coo[[i]], nb.ldk=nb.ldk)
  }
  return(Coo)}

#' Retrieve landmarks coordinates from Opn and Out objects
#' 
#' In \link{Out} and \link{Opn} classes, landmarks (if any) are stored as
#' row indices. This methods allows to retrieve the corresponding (x; y) coordinates.
#' @param Coo a Coo object, either Out or Opn
#' @return an array of coordinates X (x; y) coordinates X number of shapes.
#' @seealso \link{defLandmarks}, \link{gProcrustes}
#' @keywords Out Opn Ldk
#' @examples
#' data(hearts)
#' ldk.h <- getLandmarks(hearts) 
#' stack(Ldk(a2l(ldk.h)))
#' ldk.h
#' @export
getLandmarks <- function(Coo){UseMethod("getLandmarks")}
#' @export
getLandmarks.Out <- function(Coo){
  coo <- Coo$coo
  ldk <- Coo$ldk
  ref <- array(NA, dim=c(length(ldk[[1]]), ncol(coo[[1]]), length(coo)))
  for (i in seq(along=coo)){
    ref[,,i] <- coo[[i]][ldk[[1]], ]}
  return(ref)}
#' @export
getLandmarks.Opn <- getLandmarks.Out

#' Performs a general Procrustes alignment on Opn and Out objects
#' 
#' It relies on the $ldk slot and on the procGPA function in the shapes package.
#' @param Coo a Coo object (only Out so far)
#' @param ... additional arguments to be passed to procGPA
#' @return a procGPA-aligned Coo object
#' @seealso \link{defLandmarks} to define landmarks on your shapes and 
#' \link{coo.bookstein} for another "alignment" approach. \link{procGPA} for the
#' core function. Thanks to Dryden et al. !
#' @keywords Out Opn Ldk
#' @examples
#' \dontrun{
#' data(hearts)
#' stack(hearts) # raw hearts
#' hearts2 <- gProcrustes(hearts)
#' stack(hearts2) #procGPA-aligned hearts
#' hearts3 <- coo.bookstein(hearts, 2, 4)
#' stack(hearts3) # bookstein baseline
#' hearts4 <- coo.slide(hearts3, 2)
#' stack(hearts4) #bookstein baseline + first point on the second landmarks
#' }
#' @export
gProcrustes <- function(Coo, ...){UseMethod("gProcrustes")}
#' @export
gProcrustes.Out <- function(Coo, ...){
  if (length(Coo$ldk)==0) stop(" * No landmarks defined. See ?add.ldk")
  Coo2 <- coo.center(coo.scale(Coo))
  ref  <- getLandmarks(Coo2)
  tar <- procGPA(ref, tol1=1e-30, proc.output=TRUE, ...)$rotated
  # would benefit to be handled by coo.baseline
  for (i in 1:length(Coo2)) {
    tari <- tar[, , i]
    refi <- ref[, , i]
    t1x <- tari[1, 1]
    t1y <- tari[1, 2]
    t2x <- tari[2, 1]
    t2y <- tari[2, 2]
    r1x <- refi[1, 1]
    r1y <- refi[1, 2]
    r2x <- refi[2, 1]
    r2y <- refi[2, 2]
    # translation
    t <- tari[1, ] - refi[1, ]
    refi <- coo.trans(refi, t[1], t[2])
    # rotation  
    tx <- t2x - t1x
    ty <- t2y - t1y
    rx <- r2x - r1x
    ry <- r2y - r1y
    vi <- vecs.param(rx, ry, tx, ty)
    coo.i <- Coo2$coo[[i]]
    coo.i <- coo.trans(coo.i, t[1]-t1x, t[2]-t1y)
    coo.i <- coo.i / vi$r.norms
    coo.i <- coo.rotate(coo.i, -vi$d.angle)
    coo.i <- coo.trans(coo.i, t1x, t1y)
    Coo2$coo[[i]] <- coo.i
  }
  return(Coo2)}

#' @export
gProcrustes.Opn <- gProcrustes.Out

