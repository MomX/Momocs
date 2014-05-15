# The graphics R file for everyhting graphics. Some internals used elsewhere.

# 1. Main plotters  ------------------------------------------------------------
#' 
#' A simple wrapper for plotting shapes. Widely used in Momocs.
#' @export coo.plot
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param xlim If \code{coo.plot} is called and \code{coo} is missing, then a
#' vector of length 2 specifying the \code{ylim} of the ploting area.
#' @param ylim If \code{coo.plot} is called and \code{coo} is missing, then a
#' vector of length 2 specifying the \code{ylim} of the ploting area.
#' @param border A color for the shape border.
#' @param col A color to fill the shape polygon.
#' @param lwd The \code{lwd} for drawing shapes.
#' @param lty The \code{lty} for drawing shapes.
#' @param points \code{logical}. Whether to display points. If missing and
#' number of points is < 100, then points are plotted.
#' @param first.point \code{logical} whether to plot or not the first point.
#' @param centroid \code{logical}. Whether to display centroid.
#' @param xy.axis \code{logical}. Whether to draw the xy axis.
#' @param pch The \code{pch} for points.
#' @param cex The \code{cex} for points.
#' @param main \code{character}. A title for the plot.
#' @param plot.new \code{logical} whether to plot or not a new frame.
#' @return No returned value.
#' @seealso coo.draw
#' @keywords graphics
#' @examples
#' 
#' data(bot)
#' coo.plot(bot[1])

coo.plot <- function(coo, xlim, ylim, border="#333333", col=NA, lwd=1, lty=1,
                     points=FALSE, first.point=TRUE, centroid=TRUE, xy.axis=TRUE,
                     pch=1, cex=0.5, main, plot.new=TRUE){
  coo <- coo.check(coo)
  if (plot.new) {
    # we setup coo.plot graphical parameters
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    if (!missing(xlim) | !missing(ylim)) {
      if (missing(xlim)){ xlim <- ylim } else {ylim <- xlim }
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE,
           xlim=xlim, ylim=ylim)
    } else {
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)}
    if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}}
  polygon(coo, col=col, border=NA)
  lines(coo, col=border, lwd=lwd, lty=lty)
  # we handle coordinate points
  if (missing(points)) { if (nrow(coo)<=120) points(coo, pch=pch, cex=cex, col=border)}
  if (points) { points(coo, pch=pch, cex=cex, col=border) }
  if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20)}
  if (centroid) {
    cent <- coo.centpos(coo)
    points(cent[1], cent[2], pch=3, col=border, cex=cex)}
  if (!missing(main)) title(main=main)} 

#' Adds a single outline on the current plot.
#' 
#' \code{coo.draw} is a light version of \link{coo.plot} that simply adds a
#' shape on the active plot.
#' @export
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param ... optional parameters for coo.plot
#' @keywords graphics
#' @examples
#' data(bot)
#' b1 <- bot[4]
#' b2 <- bot[5]
#' coo.plot(b1)
#' coo.draw(b2, border="red")
coo.draw <- function(coo, ...){
  coo.plot(coo, plot.new=FALSE, ...)}

#' Plots differences between two configuratins
#' 
#' Draws "lollipops" between two configurations
#' @export
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param type either "lolli" or "arrow" to draw segments or arrows between pairs of points.
#' @param ... optional parameters for coo.plot
#' @keywords graphics
#' @examples
#' data(bot)
#' b1 <- coo.center(coo.sample(bot[4], 24))
#' b2 <- b1*1.2
#' coo.lolliplot(b1, b2)
coo.lolliplot <- function(coo1, coo2, type=c("lolli", "arrow")[1]){
  wdw <- apply(rbind(coo1, coo2), 2, function(x) max(abs(x)))
  plot(NA, xlim=c(-wdw[1], wdw[1]), ylim=c(-wdw[2], wdw[2]), asp=1)
  for (i in 1:nrow(coo1)){
    segments(coo1[i, 1], coo1[i, 2], coo2[i, 1], coo2[i, 2])
  }
  points(coo2, pch=20, cex=0.8)}

#' "Templates" list and matrix of coordinates.
#' 
#' \code{coo.template} returns \code{coo} so that the shape it is centered on
#' the origin and inscribed in a size-side square, also centered on the origin;
#' see \link{coo.list.panel} for an illustration of this function.
#' 
#' @export coo.template
#' @usage coo.template(coo, size)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param size \code{numeric}. Indicates the length of the side "inscribing"
#' the shape.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.list.panel}.
#' @keywords graphics
#' @examples
#' 
#' data(bot)
#' coo <- bot[1]
#' coo.plot(coo.template(coo), xlim=c(-1, 1), ylim=c(-1, 1))
#' rect(-0.5, -0.5, 0.5, 0.5)
#' 
#' s <- 0.01
#' coo.plot(coo.template(coo, s))
#' rect(-s/2, -s/2, s/2, s/2)

coo.template   <- function(coo, size=1) {
  # only for matrices
  coo      <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift    <-  expected - observed
  return(coo.trans(coo, shift[1], shift[2]))}

#' Plots sets of shapes.
#' 
#' \code{coo.list.panel} plots a list of shapes if passed with a list of
#' coordinates. Outlines are templated and on the same graphical window with
#' the help of \link{coo.template}.
#' 
#' @export coo.list.panel
#' @param coo.list A \code{list} of coordinates
#' @param dim A \code{vector} of the form \code{(nb.row, nb.cols)} to specify
#' the panel display. If missing, shapes are arranged in a square.
#' @param byrow \code{logical}. Whether to succesive shape by row or by col.
#' @param fromtop \code{logical}. Whether to display shapes from the top of the
#' plotting region.
#' @param mar A \code{vector} to define margins.
#' @param cols A \code{vector} of colors to fill shapes.
#' @param borders A \code{vector} of colors to draw shape borders.
#' @param reorder a factor or a numeric to reorder shapes, colors and borders.
#' @param poly logical whether to use polygon or lines to draw shapes.
#' mainly for use for outlines and open outlines.
#' @return Returns (invisibly) a \code{data.frame} with position of shapes that
#' can be used for other sophisticated plotting design.
#' @seealso \link{coo.plot} and \link{coo.template}.
#' @keywords graphics
#' @examples
#' data(bot)
#' coo.list.panel(bot$coo)
#' x <- coo.list.panel(bot$coo)
#' x # positions of shapes returned invisibly 
#' # axis(1) ; axis(2) # that's a single graphical window
#' 
#' data(bot)
#' coo <- bot$coo
#' ord <- sapply(coo, coo.eccentricity.eigen)
#' pos <- coo.list.panel(coo, reorder=ord)
#' text(pos, labels=signif(ord[order(ord)], 3))

coo.list.panel <- function(coo.list, dim, byrow=TRUE,
                           fromtop=TRUE, mar=rep(0, 4),
                           cols, borders, reorder=NULL, poly=TRUE){
  coo.list <- lapply(coo.list, coo.check)
  if (!is.null(reorder)) {
    coo.list <- coo.list[order(reorder)]}
  # if dim is missing, we define a square
  n <- length(coo.list)
  if(missing(dim)) {
    nc  <- ceiling(sqrt(n))
    nr  <- ceiling(n/nc)
    dim <- c(nr, nc)}
  k   <- dim[1]*dim[2]
  if (k < n) stop(" * dim[1]*dim[2] must be >= the length of coo.list")
  pos <- matrix(1:k, dim[1], dim[2], byrow=byrow)
  if (fromtop & dim[1]>1) { pos <- pos[dim[1]:1, ] }
  # we prepare the panel
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar=mar, oma=rep(0.2, 4))
  plot(NA, asp=1,
       xlim=c(0, dim[2]),
       ylim=c(0, dim[1]),
       xaxs="i", yaxs="i", frame=FALSE, ann=FALSE, axes=FALSE)
  # we template and plot shapes
  coo.tp  <- lapply(coo.list, coo.template, size=0.95)
  if (missing(cols))    { cols      <- rep("grey95", n) }
  if (missing(borders)) { borders   <- rep("grey20", n) }
  
  if (!is.null(reorder)) {
    cols <- cols[order(reorder)]
    borders <- borders[order(reorder)]}
  
  res <- data.frame(pos.x=numeric(), pos.y=numeric())
  if (poly) {
    for (i in 1:n){
      trans <- which(pos==i, arr.ind=TRUE) - 0.5
      res[i, ] <- c(trans[2], trans[1])
      polygon(coo.tp[[i]][, 1] + trans[2],
              coo.tp[[i]][, 2] + trans[1],
              col=cols[i], border=borders[i])}
  } else {
    for (i in 1:n){
      trans <- which(pos==i, arr.ind=TRUE) - 0.5
      res[i, ] <- c(trans[2], trans[1])
      lines(  coo.tp[[i]][, 1] + trans[2],
              coo.tp[[i]][, 2] + trans[1],
              col=borders[i])}}
  invisible(res)}


# 2. Secondary plotters ------------------------------------------------------

#' Extract structure from filenames
#' 
#' A very simple (and fast) image plotter.
#' @export lf.structure
#' @param img a matrix of an image, such as those obtained with \link{readJPEG}.
#' @keywords import
img.plot <- function(img){
  # dirty here but made for convenience
  # to have a fast img plotter..
  if (!is.matrix(img)) { 
    img <- (img[,,1] + img[,,2] + img[,,3])/3 }
  op <- par(mar=rep(0.25, 4))
  on.exit(par(op))
  h <- nrow(img)
  w <- ncol(img)
  plot(NA, xlim=c(1, w), ylim=c(1, h), asp=1,
       frame=FALSE, axes=FALSE, ann=FALSE)
  rasterImage(img, 1, 1, w, h, interpolate=FALSE)
  .title(paste(w, h, sep=" x "))
  box()}

#' Momocs' "oscilloscope" for periodic functions.
#' 
#' Shape analysis deals with curve fitting, whether \eqn{x(t)} and \eqn{y(t)}
#' positions along the curvilinear abscissa or radius/tangent angle variation.
#' We may need to represent these single or double periodic functions that are
#' ajusted by Fourier-based method. \code{coo.oscillo} and \code{coo.oscillo1}
#' compute and provide standardized plot when given a matrix of coordinates or
#' a vector, respectively. These functions are mainly used for development
#' purpose but are included in the package.
#' 
#' @aliases coo.oscillo
#' @usage coo.oscillo(coo, rug = TRUE, legend = TRUE, 
#' cols = col.gallus(2), nb.pts=12)
#' @param coo A list or a matrix of coordinates.
#' @param rug \code{logical}. Whether to display a pseudo rug, that indicate if
#' the derivate is positive.
#' @param legend \code{logical}. Whether to add a legend.
#' @param cols A \code{vector} of two colors for lines.
#' @param nb.pts \code{integer}. The number or reference points, sampled
#' equidistantly along the curvilinear abscissa and added on the oscillo
#' curves.
#' @keywords graphics
#' @examples
#' 
#' data(bot)
#' coo.oscillo(bot[1])
#' 
#' @export coo.oscillo
coo.oscillo <- function(coo, rug=TRUE, legend=TRUE,
                        cols=col.gallus(2), nb.pts=12){
  coo <- coo.check(coo)
  nr <- nrow(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]
  
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  layout(matrix(1:2, ncol=2), widths=c(1, 2))
  par(mar=c(3, 3, 2 , 1))
  coo.plot(coo, points=FALSE, first.point=FALSE)
  box()
  refs <- round(seq(1, nr, length=nb.pts+1)[-(nb.pts+1)])
  text(coo[refs, 1], coo[refs, 2], labels=as.character(1:nb.pts), cex=0.7)
  ry <- max(abs(range(c(dx, dy))))
  par(mar=c(3, 3, 2 , 1))
  plot(NA, xlim=c(1, nr), xlab="",
       ylim=c(-ry, ry)*1.1,   ylab="Deviation",
       las=1, frame=FALSE, axes=FALSE)
  axis(2, cex.axis=2/3, las=1)
  lines(dx, col=cols[1])
  lines(dy, col=cols[2])
  text((1:nr)[refs], dx[refs],
       labels=as.character(1:nb.pts), cex=0.7, col=cols[1])
  text((1:nr)[refs], dy[refs],
       labels=as.character(1:nb.pts), cex=0.7, col=cols[2])
  mtext("Deviation", side=2, line=1.5)
  box()
  if (legend) {
    legend("bottomright",
           legend = c(expression(x[i] - x[0]), expression(y[i] - y[0])),
           col = cols, bg="#FFFFFFCC", 
           cex=0.7, lty = 1, lwd=1, inset=0.05, bty="n")}}

#' Plots deviation
#' 
#' Calculates and plots series with associated error bars. This function is used 
#' internally by methods based on deviations for one one many outlines.
#' Yet, it provides a quick way to create plots of series, possibly with deviations, from scratch.
#' @export
#' @usage dev.plot(mat, dev, cols, x=1:ncol(mat),
#' lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
#' plot=FALSE, main="Deviation plot", xlab="", ylab="Deviations")
#' @param mat A \code{matrix} containing one or many lines (as individuals) with the corresponding y values (as cols).
#' @param dev A \code{matrix} of the same dimension as mat but containing the deviation from the \code{mat} matrix.
#' @param cols {A \code{vector} of \code{ncol(mat)} colors.}
#' @param x An alternative vector of values for every column of \code{mat}.
#' @param lines \code{logical}. Whether to draw lines for mean values.
#' @param poly \code{logical}. Whether to draw polygons for mean + dev values.
#' @param segments \code{logical}. Whether to draw segments for these mean + dev values.
#' @param bw \code{numeric}. The width of the errors bars to draw.
#' @param plot \code{logical}. Whether to plot a new graphical window.
#' @param main \code{character}. A title for the plot.
#' @param xlab \code{character}. A title for the x-axis.
#' @param ylab \code{character}. A title for the y-axis.
#' @keywords graphics
#' @examples
#' # we prepare some fake data
#' foo.mat  <- matrix(1:10, nr=3, nc=10, byrow=TRUE) + rnorm(30, sd=0.5)
#' foo.mat  <- foo.mat + matrix(rep(c(0, 2, 5), each=10), 3, byrow=TRUE)
#' foo.dev  <- matrix(abs(rnorm(30, sd=0.5)), nr=3, nc=10, byrow=TRUE)
#' # some possible tuning
#' dev.plot(foo.mat, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, cols=col.sari(3), poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, cols=col.summer(6)[4:6], plot=TRUE)
dev.plot       <- function(mat, dev, cols, x=1:ncol(mat), 
                           lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
                           plot=FALSE, main="Deviation plot", xlab="", ylab="Deviations") {
  # we prepare and check a bit
  r <- nrow(mat)
  if (!missing(dev)){
    if (any(dim(mat)!=dim(dev))) {
      stop("mat and dev must be of the same dimension")}}
  if (missing(cols))   {cols <- rep("#000000", r)}
  if (length(cols)!=r) {cols <- rep("#000000", r)}
  # we call a new plot if required
  if (plot) {
    if (missing(dev)) {
      ylim <- range(mat)
    } else { ylim <- c(min(mat+dev), max(mat+dev)) }
    plot(NA, xlim=range(x), ylim=ylim, main=main, xlab=xlab, ylab=ylab, las=1, xaxs="i", yaxs="i")
    axis(1, at=1:ncol(mat))}
  # if a deviation matrix is provided
  if (!missing(dev)){
    for (i in 1:r){
      # if required, we draw the background polygons
      if (poly) {
        polygon(x = c(x, rev(x)),
                y = c(mat[i, ] - dev[i, ], rev(c(mat[i, ] + dev[i, ]))),
                col=paste0(cols[i], "55"), border=NA)}
      # if required we draw the dev segments
      if (segments) {
        segments(x,    mat[i, ] - dev[i, ], x, mat[i, ]    + dev[i, ], col=cols[i], lwd=0.5)
        segments(x-bw, mat[i, ] - dev[i, ], x+bw, mat[i, ] - dev[i, ], col=cols[i], lwd=0.5)
        segments(x-bw, mat[i, ] + dev[i, ], x+bw, mat[i, ] + dev[i, ], col=cols[i], lwd=0.5)}}}
  # if a dev matrix is not provided, we simply draw lines
  if (lines) {
    for (i in 1:nrow(mat)) {
      if (lines) {
        lines(x, mat[i, ], col=cols[i], type="o", cex=0.25, pch=20)}}}}

#' Draws colored segments from a matrix of coordinates.
#' 
#' Given a matrix of (x; y) coordinates, draws segments between every points
#' defined by the row of the matrix and uses a color to display an information.
#' 
#' 
#' @usage dev.segments(coo, cols, lwd = 1)
#' @param coo A matrix of coordinates.
#' @param cols A vector of color of \code{length = nrow(coo)}.
#' @param lwd The \code{lwd} to use for drawing segments.
#' @keywords graphics
#' @examples
#' 
#' # we load some data
#' data(bot)
#' guinness <- coo.sample(bot[9], 100)
#' 
#' # we calculate the diff between 48 harm and one with 6 harm.
#' out.6    <- efourier.i(efourier(guinness, nb.h=6), nb.pts=120)
#' 
#' # we calculate deviations, you can also try 'edm'
#' dev <- edm.nearest(out.6, guinness) / coo.centsize(out.6)
#' 
#' # we prepare the color scale
#' d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
#' cols  <- paste0(col.summer(20)[d.cut], "CC")
#' 
#' # we draw the results
#' coo.plot(guinness, main="Guiness fitted with 6 harm.", points=FALSE)
#' par(xpd=NA)
#' dev.segments(out.6, cols=cols, lwd=4)
#' coo.draw(out.6, lty=2, points=FALSE, col=NA)
#' par(xpd=FALSE)
#' 
#' @export dev.segments
dev.segments <-function(coo, cols, lwd=1){
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i+1, 1], coo[i+1, 2],
             col=cols[i], lwd=lwd)}}

#' Confidence ellipses
#' 
#' Draw (gaussian) confidence ellipses
#' @export conf.ell
#' @param x numeric values on the x axis
#' @param y numeric values on the y axis
#' @param conf the level of confidence
#' @param nb.pts the number of points to return, to draw the ellipsis
#' @keywords graphics
#' @return a matrix of (x; y) coordinates to draw the ellipsis
conf.ell <- function(x, y, conf=0.95, nb.pts = 60){
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]}
  centroid <- apply(cbind(x,y), 2, mean)
  theta.i <- seq(0, 2*pi, length = nb.pts + 1)[-c(nb.pts+1)]
  z <- cbind(cos(theta.i), sin(theta.i))
  rad <- qnorm((1 - conf)/2, mean=0, sd=1, lower.tail=FALSE)
  vcvxy <- var(cbind(x,y))
  r <- cor(x, y)
  M1 <- matrix(c(1,1,-1,1), nrow=2, ncol=2)
  M2 <- matrix(c(var(x), var(y)), nrow=2, ncol=2)
  M3 <- matrix(c(1+r, 1-r), nrow=2, ncol=2, byrow=TRUE)
  ellpar <- M1 * sqrt(M2 * M3/2)
  ell <- t(centroid + rad * ellpar %*% t(z))
  colnames(ell) <- c("x", "y")
  return(ell)}

# 3. Coo / Out / Opn plotters --------------------------------------------------
#' Plot on Coo (Out/Opn) objects: quick review
#' 
#' Allows to plot shapes from Coo objects
#' @method plot Coo
#' @export plot.Coo
#' @param x the Coo object
#' @param id the id of the shape to plot, if not provided a 
#' random shape is plotted
#' @param ... further arguments to be passed to \link{coo.plot}
#' @keywords Coo
plot.Coo <- function(x, id, ...){
  Coo <- x
  if (missing(id)) {
    repeat{
      id <- sample(length(Coo), 1)
      coo.plot(Coo$coo[[id]], main=names(Coo)[id], ...)
      readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}
  if (id[1]=="all") { id <- 1:length(Coo)}
  if (is.numeric(id)){
    if (length(id)==1) {
      coo.plot(Coo$coo[[id]], main=names(Coo)[id], ...)
    } else {
      for (i in seq(along=id)) {
        coo.plot(Coo$coo[[id[i]]], main=names(Coo)[id[i]], ...)
        readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}}}

#' Plot on Coo objects: stacks all shapes
#' 
#' Plots all the outlines from a \code{Coo} on the same graph with graphical 
#' options.
#' @method stack Coo
#' @export stack.Coo
#' @param x The \code{Coo} object to plot.
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param points logical whether to draw or not points
#' @param first.point logical whether to draw or not the first point
#' @param centroid logical whether to draw or not the centroid
#' @param ldk \code{logical}. Whether to display landmarks (if any).
#' @param ldk.pch A \code{pch} for these landmarks.
#' @param ldk.col A color for these landmarks.
#' @param ldk.cex A \code{cex} fro these landmarks
#' @param xy.axis whether to draw or not the x and y axes
#' @param ... further arguments to be passed to coo.plot
#' @seealso \link{panel}, \link{plot.Coo}.
#' @examples
#' data(mosquito)
#' stack(mosquito, borders="#1A1A1A22", first.point=FALSE)
#' data(hearts)
#' stack(hearts)
#' stack(hearts, ldk=FALSE)
#' stack(hearts, borders="#1A1A1A22", ldk=TRUE, ldk.col=col.summer(4), ldk.pch=20)
stack.Coo <- function(x, cols, borders,
                      points=FALSE, first.point=TRUE, centroid=TRUE,
                      ldk=TRUE, ldk.pch=3, ldk.col="#FF000055", ldk.cex=0.5,
                      xy.axis=TRUE, ...){
  Coo <- x
  if (missing(cols)) {
    cols     <- rep(NA, length(Coo))}
  if (length(cols)!=length(Coo)) {
    cols     <- rep(cols[1], length(Coo))}
  if (missing(borders)) {
    borders     <- rep("#33333355", length(Coo))}
  if (length(borders)!=length(Coo)) {
    borders     <- rep(borders[1], length(Coo))}
  op <- par(mar=c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Coo$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)
  if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}
  for (i in 1:length(Coo)) {
    coo.draw(Coo$coo[[i]], col=cols[i], border=borders[i],
             points=points, first.point=TRUE, centroid=centroid)
    if (ldk & length(Coo$ldk)!=0) {
      points(Coo$coo[[i]][Coo$ldk[[i]], ], pch=ldk.pch, col=ldk.col, cex=ldk.cex)}}}

#' Plot on Coo objects: family picture
#' 
#' Plots all the outlines from a \code{Coo} side by side
#'
#' @export panel
#' @export
#' @export
#' @param Coo The \code{Coo (Out/Opn)} object  to plot.
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param fac a factor within the $fac slot for colors
#' @param reorder a factor or a numeric to reorder shapes
#' @param palette a color \link{palette}
#' @param names whether to plot names or not. If TRUE uses shape names, otherwise
#' pass a character for the names of the files
#' @param cex.names a cex for the names
#' @param ... further arguments to be passed to \link{coo.list.panel}
#' @seealso \link{stack.Coo}, \link{plot.Coo}.
#' @examples
#' data(mosquito)
#' panel(mosquito, names=TRUE, cex.names=0.5)
#' data(olea)
#' panel(olea)
panel <- function(Coo, cols, borders, fac, reorder, palette=col.summer, names=NULL, cex.names=0.6, ...){UseMethod("panel")}
panel.Out <- function(Coo, cols, borders, fac, reorder=NULL, palette=col.summer, names=NULL, cex.names=0.6, ...){
  Out <- Coo
  if (!missing(fac)){
    
    if (missing(cols)){
      cols <- palette(nlevels(Coo$fac[, fac]))[Coo$fac[, fac]]
    } else {
      cols <- cols[Coo$fac[, fac]]
    }
  }
  if (missing(cols)) {
    cols     <- rep(NA, length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))} 
  if (!missing(reorder)) reorder <- Out$fac[, reorder]
  pos <- coo.list.panel(Out$coo, cols=cols, borders=borders, reorder=reorder, poly=TRUE, ...)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Out), cex=cex.names)
    } else {    
      if (length(names)!=length(Out)) {
        if (is.null(reorder)) {
          text(pos[,1], pos[,2], labels=Coo$fac[, names], cex=cex.names)
        } else {
          text(pos[,1], pos[,2], labels=Coo$fac[, names][order(reorder)], cex=cex.names)
        }
      } else {
      text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}}     
panel.Opn <- function(Coo, cols, borders, fac, reorder=NULL, palette=col.summer, names=NULL, cex.names=0.6, ...){
  Opn <- Coo
  if (!missing(fac)){
    
    if (missing(cols)){
      cols <- palette(nlevels(Coo$fac[, fac]))[Coo$fac[, fac]]
    } else {
      cols <- cols[Coo$fac[, fac]]
    }
  }
  if (missing(cols)) {
    cols     <- rep(NA, length(Opn))}
  if (length(cols)!=length(Opn)) {
    cols     <- rep(cols[1], length(Opn))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Opn))}
  if (length(borders)!=length(Opn)) {
    cols     <- rep(borders[1], length(Opn))} 
  if (!missing(reorder)) reorder <- Opn$fac[, reorder]
  pos <- coo.list.panel(Opn$coo, cols=cols, borders=borders, reorder=reorder, poly=FALSE, ...)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Opn), cex=cex.names)
    } else {    
      if (length(names)!=length(Opn)) {
        if (is.null(reorder)) {
          text(pos[,1], pos[,2], labels=Coo$fac[, names], cex=cex.names)
        } else {
          text(pos[,1], pos[,2], labels=Coo$fac[, names][order(reorder)], cex=cex.names)
        }
      } else {
        text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}}     


# 4. Coe / OutCoe / OpnCoe plotters ------------------------------------------
#' Boxplot on OutCoe matrices of harmonic coefficients
#' 
#' Allows to explore diversity of coefficients from OutCoe objects,
#' typically obtain after a eFourier, tFourier, rFourier on an Out object.
#' @method boxplot OutCoe
#' @export boxplot.OutCoe
#' @param x the \link{OutCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param legend logical whether to add a legend
#' @param ... useless here but maintain the consistency with generic boxplot
#' @seealso \link{hist.OutCoe}
#' @keywords OutCoe graphics
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' boxplot(bot.f)
boxplot.OutCoe <- function(x, retain, drop, palette=col.gallus,
                           title= "Variation of harmonic coefficients",
                           legend=TRUE, ...){
  # we deduce and prepare
  OutCoe <- x
  x <- OutCoe$coe
  nb.h  <- ncol(x)/4
  cph   <- 4
  if (missing(retain)) retain <- 6
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
       axes=FALSE, xaxs="i", frame=FALSE)
  abline(v=range+0.4, col="grey80", lty=2)
  abline(h=0, col="grey80")
  for (i in 1:cph) {
    xi <- x[,(i-1)*nb.h+range]
    boxplot(xi,
            range=0, boxwex=0.2, at=range-0.6 + (i*0.2),
            col=cols[i], names=FALSE, border=cols[i], axes=FALSE, add=TRUE)}
  axis(1, at=range-0.1, labels=range)  
  axis(2)
  if (legend) {
    legend("topright", legend = LETTERS[1:cph], bty="n",
           fill = cols, border=NA, bg="#FFFFFFBB",
           cex=0.7, inset=0.005,
           title = "Harmonic coefficients")}}

#' Boxplot on OpnCoe matrices of polynomial coefficients
#' 
#' Allows to explore variability of these coefficients, typically after a
#' rawPolynomials or orthoPolynomials on Opn objects.
#' @export boxplot.OpnCoe
#' @param x the \link{OpnCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param ... useless here but maintain the consistency with generic boxplot
#' @seealso \link{hist.OpnCoe}
#' @keywords OpnCoe graphics
#' @examples
#' data(olea)
#' olea.p <- rawPolynomials(olea, 5)
#' boxplot(olea.p)
boxplot.OpnCoe <- function(x, retain, drop, palette=col.gallus,
                           title= "Variation of polynomials coefficients", ...){
  # we deduce and prepare
  #   OutCoe <- x
  #   x <- OutCoe$coe
  x <- x$coe
  degree  <- ncol(x)
  if (missing(retain)) retain <- degree
  if (missing(drop)) drop <- 0
  cs    <- (drop+1):retain
  h.names <- colnames(x)[cs]
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  cols <- palette(degree)
  mv <- max(abs(range(x[, cs])))
  ylim <- c(-mv, mv) 
  plot(NA, ylim=ylim, xlim=range(cs)+c(-0.6,0.6),
       ylab="Coefficient value", main=title,
       axes=FALSE, xaxs="i", frame=FALSE)
  abline(v=cs, col="grey80", lty=2)
  abline(h=0, col="grey80")
  for (i in seq(along=cs)) {
    boxplot(x[, cs[i]],
            range=0, boxwex=0.2, at=i,
            col=cols[i], names=FALSE, border=cols[i], axes=FALSE, add=TRUE)}
  axis(1, at=cs, labels=h.names)  
  axis(2)}

#' Histogram on OutCoe matrices of harmonic coefficients
#' 
#' Explores the distribution of harmonic coefficient values
#' @method hist OutCoe
#' @export hist.OutCoe
#' @param x the \link{OutCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param ... useless here but maintain the consistency with generic hist
#' @seealso \link{boxplot.OutCoe}
#' @keywords OutCoe graphics
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' hist(bot.f)
hist.OutCoe <-
  function(x, retain, drop, palette=col.gallus,
           title= "Variation of harmonic coefficients", ...){
    # we deduce and prepare
    #OutCoe <- x
    #x <- OutCoe$coe
    x <- x$coe
    nb.h  <- ncol(x)/4 #todo: restore rfourier, tfourier
    cph   <- 4
    if (missing(retain)) retain <- 4
    if (missing(drop)) drop <- 0
    cs    <- coeff.sel(retain=retain, drop=drop, nb.h=nb.h, cph=cph)
    h.names <- colnames(x)[cs]
    # we save the old par and prepare the plot
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    cols <- palette(cph)
    layout(matrix(1:length(cs), ncol=cph, byrow=TRUE))
    #par(oma=c(2, 2, 5, 2), mar=rep(2, 4)) #former version, also below
    par(oma=c(2, 2, 5, 2), mar=c(2, 0.5, 1.5, 0.5))
    cols    <- rep(cols, each=retain-drop)
    for (i in seq(along=cs)) { #thx Dufour, Chessel and Lobry
      h  <- x[, cs[i]] 
      #hist(h, main=h.names[i], col=cols[i], proba=TRUE, xlab="", ylab="", las=1)
      hx <- hist(h, main=h.names[i], freq=FALSE, col=cols[i], axes=FALSE, las=1)
      if (sd(h)>1e-10) { #eg. when coeff are not normalized
        h0 <- seq(min(h), max(h), len=50)
        y0 <- dnorm(h0, mean(h), sd(h))
        abline(v=mean(h), lwd=1, lty=2)
        lines(h0, y0, col = "black", lwd = 1)
        at <- c(range(hx$mids), mean(hx$mids))
        axis(1, at=at, labels=signif(at, 3), cex.axis=0.75)}}
    title(main=title, cex.main=1.5, outer=TRUE)
    layout(matrix(1))}

#' Histogram on OpnCoe matrices of polynomials coefficients
#' 
#' Explores the distribution of polynomials coefficient values
#' @method hist OpnCoe
#' @export hist.OpnCoe
#' @param x the \link{OpnCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param hist.per.row numeric the number of histograms per row
#' @param ... useless here but maintain the consistency with generic hist
#' @keywords OpnCoe graphics
#' @seealso \link{boxplot.OpnCoe}
#' @examples
#' data(olea)
#' olea.p <- orthoPolynomials(olea, 5)
#' hist(olea.p)
hist.OpnCoe <-
  function(x, retain=4, drop, palette=col.gallus,
           title= "Variation of polynomials coefficients",
           hist.per.row=3, ...){
    # we deduce and prepare
    #OpnCoe <- x
    #x <- OpnCoe$coe
    x <- x$coe
    degree  <- ncol(x)
    if (missing(retain)) retain <- degree
    if (missing(drop)) drop <- 0
    cs    <- (drop+1):retain
    h.names <- colnames(x)[cs]
    # we save the old par and prepare the plot
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    nc <- length(cs)
    cols <- palette(nc)
    lay.nr <- ceiling(nc/hist.per.row)
    layout(matrix(1:nc, nrow=lay.nr, ncol=hist.per.row, byrow=TRUE))
    par(oma=c(2, 2, 5, 2), mar=c(2, 0.5, 1.5, 0.5))
    for (i in seq(along=cs)) { #thx Dufour, Chessel and Lobry
      h  <- x[, cs[i]] 
      #hist(h, main=h.names[i], col=cols[i], proba=TRUE, xlab="", ylab="", las=1)
      hx <- hist(h, main=h.names[i], freq=FALSE, col=cols[i], axes=FALSE, las=1)
      if (sd(h)>1e-10) { #eg. when coeff are not normalized
        h0 <- seq(min(h), max(h), len=50)
        y0 <- dnorm(h0, mean(h), sd(h))
        abline(v=mean(h), lwd=1, lty=2)
        lines(h0, y0, col = "black", lwd = 1)
        at <- c(range(hx$mids), mean(hx$mids))
        axis(1, at=at, labels=signif(at, 3), cex.axis=0.75)}}
    title(main=title, cex.main=1.5, outer=TRUE)
    layout(matrix(1))}

#' Calculates and displays the contribution of every harmonic on shape
#' 
#' Applies \code{amp.h} values for each of the \code{harm.range} harmonics 
#' when reconstruction shapes. This thus help to visualize the respective 
#' contribution of every harmonic, in other words their contribution 
#' in describing shapes.
#' @export harm.contrib
#' @aliases harm.contrib
#' @export
#' @param OutCoe the \link{OutCoe} object
#' @param id numeric the id of the shape to consider
#' @param harm.range a vector of harmonics
#' @param amp.h a vector of amplification factor
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @seealso \link{degree.contrib}
#' @keywords OutCoe graphics
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' harm.contrib(bot.f)
harm.contrib <- function(OutCoe, id, harm.range, amp.h, palette, title){
  UseMethod("harm.contrib")}
harm.contrib.OutCoe <- function(
  OutCoe,
  id=1,
  harm.range =1:6,
  amp.h   = c(0, 0.5, 1, 2, 5, 10),
  palette = col.hot,
  title   = "Harmonic contribution"){
  #if missing id meanshape
  x <- OutCoe$coe
  nb.h <- ncol(x)/4
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
      res[[p]] <- method.i(xfi)
      p <- p+1}}
  
  cols <- rep(palette(length(amp.h)), length(harm.range))
  coo.list.panel(res, dim=c(length(amp.h), length(harm.range)),
                 byrow=FALSE, cols=cols, mar=c(5.1, 5.1, 4.1, 2.1))
  axis(1, at=(1:length(harm.range))-0.5,
       labels=harm.range, line=2, lwd=1, lwd.ticks=0.5)
  mtext("Harmonic rank", side=1, line=4)
  axis(2, at=(1:length(amp.h))-0.5,
       labels=rev(amp.h), line=1, lwd=1, lwd.ticks=0.5, las=2)
  mtext("Amplification factor", side=2, line=3)
  title(main=title)}

#' Calculates and displays the contribution of every polynomial on shape
#' 
#' Applies \code{amp.d} values for each of the \code{degree.range} polynomials 
#' when reconstruction shapes. This thus help to visualize the respective 
#' contribution of every coefficient, in other words their contribution 
#' in describing shapes.
#' @export degree.contrib
#' @aliases degree.contrib
#' @export
#' @param OpnCoe the \link{OpnCoe} object
#' @param id numeric the id of the shape to consider
#' @param degree.range a vector of polynomials
#' @param amp.d a vector of amplification factor
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @seealso \link{harm.contrib}
#' @keywords OpnCoe graphics
#' @examples
#' data(olea)
#' olea.p <- rawPolynomials(olea, 6)
#' degree.contrib(olea.p)
degree.contrib <- function(
  OpnCoe, id, degree.range, amp.d, palette, title){UseMethod("degree.contrib")}
degree.contrib.OpnCoe <- function(
  OpnCoe,
  id      = 1,
  degree.range,
  amp.d   = c(0, 0.5, 1, 2, 5, 10),
  palette = col.hot,
  title   = "Degree contribution"){
  #if missing id meanshape
  coe <- OpnCoe$coe[id, ]
  degree <- length(coe)-1
  if (missing(degree.range)) { degree.range <- 1:degree}
  mult <- rep(1, degree)
  ortho <- ifelse(OpnCoe$method=="rawPolynomials", TRUE, FALSE)
  pol <- list(coeff=coe, ortho=ortho, 
              baseline1=OpnCoe$baseline1, baseline2=OpnCoe$baseline2)
  res <- list()
  p <- 1 # dirty
  for (j in seq(along=degree.range)){
    for (i in seq(along=amp.d)){
      mult.loc    <- mult
      mult.loc[degree.range[j]] <- amp.d[i]
      pol$coeff <- coe*mult.loc
      res[[p]] <- polynomials.i(pol)
      p <- p+1}}
  
  cols <- rep(palette(length(amp.d)+4)[-(1:4)], degree)
  coo.list.panel(res, dim=c(length(amp.d), degree),
                 byrow=FALSE, borders=cols, mar=c(5.1, 5.1, 4.1, 2.1), poly=FALSE)
  axis(1, at=(1:(degree+1))-0.5,
       labels=names(coe), line=2, lwd=1, lwd.ticks=0.5)
  mtext("Degree", side=1, line=4)
  axis(2, at=(1:length(amp.d))-0.5,
       labels=rev(amp.d), line=1, lwd=1, lwd.ticks=0.5, las=2)
  mtext("Amplification factor", side=2, line=3)
  title(main=title)}

# 5. Morphospace functions -----------------------------------------------------
# stupid function
.mprod <- function(m, s){
  res <- m
  for (i in 1:ncol(m)) { res[, i] <- m[, i]*s[i] }
  return(res)}

.morphospacePCA <- function(PCA, xax, yax, pos.shp,
                            amp.shp=1, size.shp=15, pts.shp=60,
                            col.shp="#00000011", border.shp="#00000055"){
  
  xy     <- PCA$x[, c(xax, yax)]
  rot    <- PCA$rotation[, c(xax, yax)]
  mshape <- PCA$mshape
  #we define the position of shapes
  pos <- pos.shapes(xy, pos.shp=pos.shp)
  # according to the type of morphometrics applied, we reconstruct shapes
  method <- PCA$method
  ## outlines
  if (method=="eFourier"){
    shp <- pca2shp.efourier(pos=pos, rot=rot,
                            mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp)
    cd <- TRUE}
  if (method=="rFourier"){
    shp <- pca2shp.rfourier(pos=pos, rot=rot,
                            mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp)
    cd <- TRUE}
  if (method=="tFourier"){
    shp <- pca2shp.tfourier(pos=pos, rot=rot,
                            mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp)
    cd <- TRUE}
  ## open outlines
  if (method=="orthoPolynomials"){
    shp <- pca2shp.polynomials(pos=pos, rot=rot,
                               mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp, ortho=TRUE,
                               baseline1=PCA$baseline1, baseline2=PCA$baseline2)
    cd <- FALSE}
  if (method=="rawPolynomials"){
    shp <- pca2shp.polynomials(pos=pos, rot=rot,
                               mshape=mshape, amp.shp=amp.shp, pts.shp=pts.shp, ortho=FALSE,
                               baseline1=PCA$baseline1, baseline2=PCA$baseline2)
    cd <- FALSE}
  width   <- (par("usr")[4] - par("usr")[3]) / size.shp
  shp     <- lapply(shp, coo.scale, 1/width)
  if (cd) {
    garbage <- lapply(shp, coo.draw, col=col.shp, border=border.shp, points=FALSE)
  } else {
    garbage <- lapply(shp, lines, col=border.shp)}}

#' Calculates nice positions on a plan for drawing shapes
#' 
#' @export pos.shapes
#' @param xy todo
#' @param pos.shp the way shape should be positionned
#' @param nb.shp the total number of shapes
#' @param nr.shp the number of rows to position shapes
#' @param nc.shp the number of cols to position shapes
#' @param circle.r.shp if circle, its radius
#' @keywords graphics
pos.shapes <- function(xy, pos.shp=c("range", "circle", "xy")[1],
                       nb.shp=12, nr.shp=6, nc.shp=5, circle.r.shp){
  if (is.data.frame(pos.shp) | is.matrix(pos.shp)) {
    return(as.matrix(pos.shp))}
  if (pos.shp=="xy"){
    return(xy)}
  if (pos.shp=="circle") {
    if (missing(circle.r.shp)) {
      # mean distance from origin
      circle.r.shp <- mean(apply(xy, 1, function(x) sqrt(sum(x^2))))}
    t <- seq(0, 2*pi, len=nb.shp+1)[-(nb.shp+1)]
    pos <- cbind(circle.r.shp*cos(t), circle.r.shp*sin(t))
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="range") {
    pos <- expand.grid(seq(min(xy[, 1]), max(xy[, 1]), len=nr.shp),
                       seq(min(xy[, 2]), max(xy[, 2]), len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="full") {
    #     w <- par("usr")
    #     pos <- expand.grid(seq(w[1], w[2], len=nr.shp),
    #                        seq(w[3], w[4], len=nc.shp))
    w <- par("usr")
    pos <- expand.grid(seq(par("xaxp")[1]*0.9, par("xaxp")[2]*0.9, len=nr.shp),
                       seq(par("yaxp")[1]*0.9, par("yaxp")[2]*0.9, len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)   
  }
  # if a non-valid method is passed
  return(xy)}

#' Calculates shapes from PC plane: efourier
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @export
pca2shp.efourier <- function (pos, rot, mshape, amp.shp=1, pts.shp=60) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/4
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ])*amp.shp
    coe        <- mshape + apply(ax.contrib, 1, sum)
    xf         <- coeff.split(coe)
    coo        <- efourier.i(xf, nb.h = nb.h, nb.pts=pts.shp)
    # reconstructed shapes are translated on their centroid
    #if (trans) {
    dx <- pos[i, 1] - coo.centpos(coo)[1] 
    dy <- pos[i, 2] - coo.centpos(coo)[2] 
    coo <- coo.trans(coo, dx, dy)
    #}
    res[[i]] <- coo}
  return(res)}

#' Calculates shapes from PC plane: rfourier
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @export
pca2shp.rfourier <- function (pos, rot, mshape, amp.shp=1, pts.shp=60) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ])*amp.shp
    coe        <- mshape + apply(ax.contrib, 1, sum)
    xf         <- coeff.split(coe, cph=2)
    coo        <- rfourier.i(xf, nb.h = nb.h, nb.pts=pts.shp)
    # reconstructed shapes are translated on their centroid
    #if (trans) {
    dx <- pos[i, 1] - coo.centpos(coo)[1] 
    dy <- pos[i, 2] - coo.centpos(coo)[2] 
    coo <- coo.trans(coo, dx, dy)
    #}
    res[[i]] <- coo}
  return(res)}

#' Calculates shapes from PC plane: tfourier
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @export
pca2shp.tfourier <- function (pos, rot, mshape, amp.shp=1, pts.shp=60) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ])*amp.shp
    coe        <- mshape + apply(ax.contrib, 1, sum)
    xf         <- coeff.split(coe, 2)
    coo        <- tfourier.i(xf, nb.h = nb.h, nb.pts=pts.shp, force2close=TRUE)
    # reconstructed shapes are translated on their centroid
    #if (trans) {
    dx <- pos[i, 1] - coo.centpos(coo)[1] 
    dy <- pos[i, 2] - coo.centpos(coo)[2] 
    coo <- coo.trans(coo, dx, dy)
    #}
    res[[i]] <- coo}
  return(res)}

#' Calculates shapes from PC plane: polynomials
#' 
#' @param pos the position on two PC axis
#' @param rot the corresponding loadings
#' @param mshape the meanshape
#' @param amp.shp amplification factor for the shape deformation
#' @param pts.shp number of points to reconstruct the shape
#' @param ortho logical whether working with raw or orthogonal polynomials
#' @param baseline1 the (x; y) coordinates of the first baseline point
#' @param baseline2 the (x; y) coordinates of the second baseline point
#' @export
#' 
pca2shp.polynomials <- function (pos, rot, mshape, amp.shp=1, pts.shp=60, ortho,
                                 baseline1, baseline2) {
  if (ncol(pos) != ncol(rot))     stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  degree <- length(mshape)
  n  <- nrow(pos)
  # an empy pol object
  pol <- list(coeff=rep(NA, degree), ortho=ortho, 
              baseline1=baseline1, baseline2=baseline2)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ])*amp.shp
    pol$coeff        <- mshape + apply(ax.contrib, 1, sum)
    coo        <- polynomials.i(pol, nb.pts=pts.shp, reregister=TRUE)
    pol$coeff <- rep(NA, degree)
    # reconstructed shapes are translated on their centroid
    #if (trans) {
    dx <- pos[i, 1] - coo.centpos(coo)[1] 
    dy <- pos[i, 2] - coo.centpos(coo)[2] 
    coo <- coo.trans(coo, dx, dy)
    res[[i]] <- coo}
  #}
  return(res)}

# 6. PCA -----------------------------------------------------------------------
#create an empty frame
#' @export
.frame <- function(xy, center.origin=FALSE, zoom=1){
  if (center.origin) {
    w <- zoom*max(abs(xy))
    plot(NA, xlim=c(-w, w), ylim=c(-w, w),  asp=1, axes=FALSE, frame=TRUE) 
  } else {
    w <- zoom*apply(abs(xy), 2, max)
    plot(xy, xlim=c(-w[1], w[1]), ylim=c(-w[2], w[2]),
         type="n", asp=1,  axes=FALSE, frame=TRUE)}}

#grid layer
#' @export
.grid <- function(xy, nb.grids=3){
  m <- max(abs(xy))
  g <- seq(0, m, length=nb.grids)
  g <- c(g[-1]*-1, g[-1])
  abline(h=g, v=g, col="grey90", lty=2)
  abline(h=0, v=0, col="grey80")}

#rug
#' @export
.rug <- function(xy, fac, col){
  if (is.null(fac)) {
    rug(xy[, 1], ticksize=0.015, side=1, col="black")
    rug(xy[, 2], ticksize=0.015, side=2, col="black")
  } else {
    for (i in seq(along=levels(fac))) {
      rug(xy[fac==levels(fac)[i], 1], ticksize=0.015, lwd=1, side=1, col=col[i])
      rug(xy[fac==levels(fac)[i], 2], ticksize=0.015, lwd=1,  side=2, col=col[i])}}}

#confidence ellipses
#' @export
.ellipses <- function(xy, fac, conf=0.5, col){
  for (i in seq(along=levels(fac))) {
    pts.i <- xy[fac==levels(fac)[i], ]
    ell.i <- conf.ell(x=pts.i, conf=conf)
    lines(coo.close(ell.i), col=col[i])
    points(coo.centpos(pts.i)[1], coo.centpos(pts.i)[2], pch=3, col=col[i])}}

#convex hulls
#' @export
.chull <- function(coo, fac, col, lty){
  for (i in seq(along=levels(fac))) {
    chull.i <- coo.chull(coo[fac==levels(fac)[i], ])
    lines(coo.close(chull.i), col=col[i], lty=lty)}}

#add labels
#' @export
.labels <- function(xy, fac, col){
  for (i in seq(along=levels(fac))) {
    cent.i <- coo.centpos(xy[fac==levels(fac)[i], ])
    text(cent.i[1], cent.i[2], labels=levels(fac)[i], col=col[i], pos=3)}}

#add 'stars'
#' @export
.stars <- function(xy, fac, col){
  col.i <- paste0(col, "55")
  for (i in seq(along=levels(fac))) {
    pts.i  <- xy[fac==levels(fac)[i], ]
    cent.i <- coo.centpos(pts.i)
    for (j in 1:nrow(pts.i)){
      segments(cent.i[1], cent.i[2], pts.i[j, 1], pts.i[j, 2], col=col.i[i])}}}

#add eigen
#' @export
.eigen <- function(ev, xax, yax, ratio=0.12){
  plt0 <- par("plt")
  on.exit(par(plt = plt0))
  g <- 0.015
  w <- min(c(plt0[2]-plt0[1]), plt0[4]-plt0[3])*ratio
  par(plt = c(plt0[2]-w-g, plt0[2]-g, plt0[3]+g*1.5, plt0[3]+w+g*1.5), xpd=NA, new = TRUE)
  cols <- rep("grey80", 5)
  cols[c(xax,yax)] <- "grey40"
  var <- ev^2
  cs.var <- cumsum(var)/sum(var)
  k <- ifelse(max(c(xax, yax))>5, max(c(xax, yax)), 5)
  barplot(var[1:k], axes=FALSE, col=cols, border=NA)
  text(-0.7, par("usr")[3], labels="Eigenvalues",
       pos=4, cex=0.6, srt=90, col="grey40")}

#names axes
#' @export
.axisnames <- function(xax, yax){
  gx <- strwidth("PCN")/1.75
  gy <- strheight("PCN")/1.8
  text(par("usr")[2]-gx, gy, col="grey40", cex=0.8,
       labels=paste0("PC", xax))
  text(-gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0("PC", yax), srt=90)}

#adds var captured
#' @export
.axisvar <- function(ev, xax, yax){
  var <- ev^2
  var <- signif(100*var/sum(var), 3)
  gx <- strwidth("00.0%")/1.75
  gy <- strheight("00.0%")/1.8
  text(par("usr")[2]-gx, -gy, col="grey40", cex=0.8,
       labels=paste0(var[xax], "%"))
  text(+gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0(var[yax], "%"), srt=90)}

#adds title to pca plots
#' @export
.title <- function(title){
  pos <- par("usr")
  text(pos[1], pos[3]+ strheight(title), labels=title, pos=4)}

#' Plots Principal Component Analysis
#' 
#' The Momocs' PCA plotter with many graphical options and morphospaces.
#' @method plot PCA
#' @export plot.PCA
#' @param x an object of class "PCA", typically obtained with \link{pca}
#' @param fac factor, or a name or the column id from the $fac slot
#' @param xax the first PC axis
#' @param yax the second PC axis
#' @param col a color for the points (either global, for every level of the fac
#' or for every individual, see examples)
#' @param pch a pch for the points (either global, for every level of the fac
#' or for every individual, see examples)
#' @param cex the size of the points
#' @param palette a \link{palette}
#' @param center.origin logical whether to center the plot onto the origin
#' @param zoom to keep your distances
#' @param grid logical whether to draw a grid
#' @param nb.grids and how many of them
#' @param morphospace logical whether to add the morphological space
#' @param pos.shp either "full", "range", "circle", "xy" 
#' or a data.frame for \link{pos.shapes}
#' @param amp.shp amplification factor for shape deformation
#' @param size.shp the size of the shapes
#' @param pts.shp the number of points fro drawing shapes
#' @param border.shp the border color of the shapes
#' @param col.shp the color of the shapes
#' @param stars logical whether to draw "stars"
#' @param ellipses logical whether to draw confidence ellipses
#' @param conf the level of confidence
#' @param chull logical whether to draw a convex hull
#' @param chull.lty if yes, its linetype
#' @param labels logical whether to add labels for groups
#' @param axisnames logical whether to add PC names
#' @param axisvar logical whether to draw the variance they explain
#' @param eigen logical whether to draw a plot of the eigen values
#' @param rug logical whether to add rug to margins
#' @param title character a name for the plot
#' @param ... useless here, just to fit the generic plot
#' @details Widely inspired by the philosophy behind graphical functions
#' of the ade4 R package.
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.p <- pca(bot.f)
#' plot(bot.p, 1)
plot.PCA <- function(#basics
  x, fac, xax=1, yax=2, 
  #color choice
  col="black", pch=20, cex=0.5, palette=col.summer,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp.shp=1, size.shp=20,
  pts.shp=60, border.shp="#00000055", col.shp="#00000011",
  #stars
  stars=FALSE,
  #ellipses
  ellipses=TRUE, conf=0.5,
  #convexhulls
  chull=TRUE, chull.lty=3,
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
  title=substitute(x), ...
){
  PCA <- x
  xy <- PCA$x[, c(xax, yax)]
  # we check and prepare
  if (!missing(fac)) {
    if (!is.factor(fac)) { fac <- factor(PCA$fac[, fac]) }
    if (!missing(col) & length(col)==nlevels(fac)) {
      col.groups <- col
      col <- col.groups[fac]
    } else {
      col.groups <- palette(nlevels(fac))
      col <- col.groups[fac]
    } 
    if (!missing(pch)) {
      if (length(pch)==nlevels(fac)) { pch <- pch[fac] }}}
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  .frame(xy, center.origin, zoom=zoom)
  if (grid) .grid(xy)
  if (morphospace) {
    .morphospacePCA(PCA, xax=xax, yax=yax, pos.shp=pos.shp,
                    amp.shp=1, size.shp=size.shp, pts.shp=pts.shp,
                    col.shp=col.shp, border.shp=border.shp)}
  if (!missing(fac)) {
    if (stars)    .stars(xy, fac, col.groups)
    if (ellipses) .ellipses(xy, fac, conf=conf, col.groups) #+conf
    if (chull)    .chull(xy, fac, col.groups, chull.lty)
    if (labels)   .labels(xy, fac, col.groups)
    if (rug)      .rug(xy, fac, col.groups)
  } else {
    if (rug)      .rug(xy, NULL, col)
  }
  points(xy, pch=pch, col=col, cex=cex)
  if (axisnames)  .axisnames(xax, yax)
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax)
  box()}


# 7. LDA ------------------------------------------------------------------


# 8. Thin plate splines plotters -----------------------------------------------
#' Thin Plate Splines for 2D data.
#' 
#' \code{tps2d} is the core function for Thin Plate Splines. It is used
#' internally but might be useful elsewhere.
#' 
#' 
#' @usage tps2d(grid0, fr, to)
#' @param grid0 A matrix of coordinates on which to calculate deformations.
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @return Returns a matrix of \code{(x; y)} coordinates with TPS-interpolated
#' deformations.
#' @seealso The \link{tps.grid},\link{tps.iso}, \link{tps.arr} functions use
#' \code{tps2d}.
#' @keywords coo Utilities
#' @export tps2d
tps2d <- function(grid0, fr, to){
  if (is.closed(fr)) fr <- coo.unclose(fr)
  if (is.closed(to)) to <- coo.unclose(to)
  p  <- nrow(fr)
  q  <- nrow(grid0)
  P  <- matrix(NA, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      r2     <- sum((fr[i,]-fr[j,])^2)
      P[i,j] <- r2*log(r2)}}
  P[is.na(P)] <- 0
  Q  <- cbind(1, fr)
  L  <- rbind(cbind(P, Q), cbind(t(Q), matrix(0,3,3)))
  m2 <- rbind(to, matrix(0, 3, 2))
  coefx <- solve(L)%*%m2[, 1]
  coefy <- solve(L)%*%m2[, 2]
  fx <- function(fr, grid0, coef) {
    Xn <- numeric(q)
    for (i in 1:q) {
      Z     <- apply((fr-matrix(grid0[i, ], p, 2, byrow=TRUE))^2, 1, sum)
      Xn[i] <- coef[p+1]+coef[p+2]*grid0[i,1]+coef[p+3]*grid0[i,2]+
        sum(coef[1:p]*(Z*log(Z)))}
    return(Xn)}
  grid1 <- cbind(fx(fr, grid0, coefx), fx(fr, grid0, coefy))
  return(grid1)}

#' Deformation grids using Thin Plate Splines.
#' 
#' \code{tps.grid} calculates and plots deformation grids between two
#' configurations.
#' 
#' @export tps.grid
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param grid.outside A \code{numeric} that indicates how much the grid
#' extends beyond the range of outlines. Expressed as a proportion of the
#' latter.
#' @param grid.size A \code{numeric} to specify the number of grid cells on the
#' longer axis on the outlines.
#' @param grid.col A color for drawing the grid.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.col Two colors for filling the shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- mshapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.grid(fr, to, amp=3, grid.size=40)

tps.grid <- function(fr, to, amp=1, grid.outside = 0.2,
                     grid.size = 40, grid.col = "grey80",
                     shp = TRUE, shp.col = rep(NA, 2), shp.border=col.gallus(2),
                     shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  # simple magnification
  if (!missing(amp)) to <- to + (to-fr)*amp
  # we prepare the grid
  x1     <- min(to[, 1])
  x2     <- max(to[, 1])
  y1     <- min(to[, 2])
  y2     <- max(to[, 2])
  rx     <- x2 - x1
  ry     <- y2 - y1
  dim.grid <- if (rx > ry) { 
    c(grid.size, round(grid.size*ry / rx))
  } else {
    c(round(grid.size*rx / ry), grid.size) }
  xgrid0 <- seq(x1-rx*grid.outside, x2+rx*grid.outside, length=dim.grid[1])
  ygrid0 <- seq(y1-ry*grid.outside, y2+ry*grid.outside, length=dim.grid[2])
  grid0 <- as.matrix(expand.grid(xgrid0, ygrid0))
  grid1 <- tps2d(grid0, fr, to)
  op <- par(mar=rep(0, 4))
  on.exit(par(op))
  wdw <- apply(rbind(grid0, grid1), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1,
       ann=FALSE, axes=FALSE, mar=rep(0, 4))
  for (i in 1:dim.grid[2]) {
    lines(grid1[(1:dim.grid[1]) + (i-1)*dim.grid[1],], col=grid.col)}
  for (i in 1:dim.grid[1]) {
    lines(grid1[(1:dim.grid[2]) * dim.grid[1]-i+1,],   col=grid.col)}
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1],
             lwd=shp.lwd[1], lty=shp.lty[1], points=FALSE)
    coo.draw(to, border=shp.border[2], col=shp.col[2],
             lwd=shp.lwd[2], lty=shp.lty[2], points=FALSE)}}

#' Deformation "vector field" using Thin Plate Splines.
#' 
#' \code{tps.arr}(ows) calculates deformations between two configurations and
#' illustrate them using arrows.
#' 
#' @export
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param palette A color palette such those included in Momocs or produced
#' with \link{colorRampPalette}.
#' @param arr.nb A \code{numeric}. The number of arrows to calculate.
#' @param arr.levels A \code{numeric}. The number of levels for the color of
#' arrows.
#' @param arr.len A \code{numeric}. The length of arrows.
#' @param arr.ang A \code{numeric}. The angle for arrows' heads.
#' @param arr.lwd A \code{numeric}. The \code{lwd} for drawing arrows.
#' @param arr.col If \code{palette} is not used the color for arrwos.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.col Two colors for filling the shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- mshapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.arr(fr, to, arr.nb=400, palette=col.sari, amp=3)
#' 

tps.arr <- function(fr, to, amp=1, palette = col.summer,
                    arr.nb = 200, arr.levels = 100, arr.len = 0.1,
                    arr.ang = 20, arr.lwd = 0.75, arr.col = "grey50",
                    shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), arr.nb, type="regular")@coords
  grid1     <- tps2d(grid0, fr, to)
  # grille simple, on affiche d'abord les deux courbes
  op <- par(mar=rep(0, 4))
  on.exit(par(op))
  wdw      <- apply(rbind(fr, to), 2, range)
  plot(NA, xlim=wdw[, 1]*1.05, ylim=wdw[, 2]*1.05, asp=1,
       axes=FALSE, ann=FALSE, mar=rep(0,4))
  if (missing(arr.levels)) {arr.levels = arr.nb}
  if (!missing(palette)) {
    q.lev   <- cut(edm(grid0, grid1), breaks=arr.levels, labels=FALSE)
    arr.cols <- palette(arr.levels)[q.lev]
  } else {
    arr.cols <- rep(arr.col, nrow(grid0))}
  arrows(grid0[, 1], grid0[, 2], grid1[, 1], grid1[, 2],
         length=arr.len, angle=arr.ang, lwd=arr.lwd, col=arr.cols)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1],
             lwd=shp.lwd[1], lty=shp.lty[1], points=FALSE)
    coo.draw(to, border=shp.border[2], col=shp.col[2],
             lwd=shp.lwd[2], lty=shp.lty[2], points=FALSE)}}

#' Deformation isolines using Thin Plate Splines.
#' 
#' \code{tps.iso} calculates deformations between two configurations and map
#' them with or without isolines.
#' 
#' @export
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param palette A color palette such those included in Momocs or produced
#' with \link{colorRampPalette}.
#' @param iso.levels \code{numeric}. The number of levels for mapping the
#' deformations.
#' @param iso.nb A \code{numeric}. The number of points to use for the
#' calculation of deformation.
#' @param cont \code{logical}. Whether to draw contour lines.
#' @param cont.col A color for drawing the contour lines.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- mshapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.iso(fr, to, iso.nb=2000, amp=3)
#' 

tps.iso <- function(fr, to, amp=1, palette = col.summer,
                    iso.nb = 1000, iso.levels = 12, cont=TRUE, cont.col="black",
                    shp = TRUE, shp.border=col.gallus(2),
                    shp.lwd = c(1, 1), shp.lty = c(1, 1)){  
  # if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), iso.nb, type="regular")@coords
  grid1  <- tps2d(grid0, fr, to)
  def    <- edm(grid0, grid1)
  x1     <- length(unique(grid0[,1]))
  y1     <- length(unique(grid0[,2]))
  im     <- matrix(NA,x1,y1)
  xind   <- (1:x1)[as.factor(rank(grid0[,1]))]
  yind   <- (1:y1)[as.factor(rank(grid0[,2]))]
  n      <- length(xind)
  for (i in 1:n) im[xind[i], yind[i]] <- def[i]
  iso.cols <- palette(iso.levels)
  x <- sort(unique(grid0[,1]))
  y <- sort(unique(grid0[,2]))
  op <- par(mar=rep(1, 4))
  on.exit(par(op))
  image(x, y, im, col=iso.cols, asp=1, xlim=range(x)*1.05, ylim=range(y)*1.05,
        axes=FALSE, frame=FALSE, ann=FALSE)
  if (cont) {contour(x, y, im, nlevels=iso.levels,
                     add=TRUE, drawlabels=FALSE, col=cont.col, lty=2)}
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=NA,
             lwd=shp.lwd[1], lty=shp.lty[1], points=FALSE)
    coo.draw(to, border=shp.border[2], col=NA,
             lwd=shp.lwd[2], lty=shp.lty[2], points=FALSE)}}

# 0. Color palettes ------------------------------------------------------------

#' Colors, colors, colors.
#' @name col.summer
#' @title Some color palettes.
#' @usage col.summer(n)
#' col.spring(n)
#' col.autumn(n)
#' col.solarized(n)
#' col.gallus(n)
#' col.blackgallus(n)
#' col.hot(n)
#' col.cold(n)
#' col.sari(n)
#' col.india(n)
#' col.bw(n)
#' @export col.spring col.summer col.autumn col.solarized col.gallus col.blackgallus col.hot col.cold col.sari col.india col.bw
#' @aliases col.spring col.summer col.autumn col.solarized col.gallus col.blackgallus col.hot col.cold col.sari col.india col.bw
#' @param n the number of colors to generate from the color palette
#' @return color codes (hexadecimal format)
#' @keywords graphics
#' @examples
#' barplot(1:10, col=col.summer(10), main="col.summer")
#' barplot(1:10, col=col.spring(10), main="col.spring")
#' barplot(1:10, col=col.autumn(10), main="col.autumn")
#' barplot(1:10, col=col.solarized(10), main="col.solarized")
#' barplot(1:10, col=col.gallus(10), main="col.gallus")
#' barplot(1:10, col=col.blackgallus(10), main="col.blackgallus")
#' barplot(1:10, col=col.hot(10), main="col.hot")
#' barplot(1:10, col=col.cold(10), main="col.cold")
#' barplot(1:10, col=col.sari(10), main="col.sari")
#' barplot(1:10, col=col.india(10), main="col.india")
#' barplot(1:10, col=col.bw(10), main="col.bw")
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
col.spring <- colorRampPalette(c("#a3baff", "#ffff7f", "#ff9797"))

col.autumn <- colorRampPalette(c("#3353b3", "#b1b100", "#b32222"))
col.solarized <- colorRampPalette(c("#b58900", "#cb4b16", "#dc322f", "#d33682",
                                    "#6c71c4", "#268bd2", "#2aa198", "#859900"))
col.gallus <- colorRampPalette(c("#025D8C", "#FFFFFF", "#A80000"))
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
col.hot <- colorRampPalette(c("#F2F2F2","#A80000"))
col.cold <- colorRampPalette(c("#F2F2F2","#025D8C"))
col.sari <- colorRampPalette(c("#551A8B", "#FF7F00"))
col.india <- colorRampPalette(c("#FF9933", "#138808"))
col.bw <- colorRampPalette(c("#FFFFFF", "#000000"))
