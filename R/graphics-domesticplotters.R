##### The graphics R file for everyhting graphics. Some internals used elsewhere.

#' Plots a single shape
#' 
#' A simple wrapper around \link{plot} for plotting shapes. Widely used in Momocs
#' in other graphical functions, in methods, etc.
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
#' @param plot logical whether to plot something or just to create an empty plot.
#' @param zoom a numeric to take your distances.
#' @return No returned value.
#' @seealso coo.draw
#' @keywords Graphics
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' coo.plot(bot[2], plot.new=FALSE) # equivalent to coo.draw(bot[2]) 
#' coo.plot(b, zoom=2)
#' coo.plot(b, border="blue")
#' coo.plot(b, first.point=FALSE, centroid=FALSE)
#' coo.plot(b, points=TRUE, pch=20)
#' coo.plot(b, xy.axis=FALSE, lwd=2, col="#F2F2F2")
#' @export
coo.plot <- function(coo, xlim, ylim, border="#333333", col=NA, lwd=1, lty=1,
                     points=FALSE, first.point=TRUE, centroid=TRUE, xy.axis=TRUE,
                     pch=1, cex=0.5, main, plot.new=TRUE, plot=TRUE, zoom=1){ #todo zoom
  coo <- coo.check(coo)
  if (plot.new) {
    # we setup coo.plot graphical parameters
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    # if zoom if provided, we define wlim and ylim manually
    if (!missing(zoom)){
      wdw.range <- apply(coo, 2, range)
      wdw.diff <- apply(wdw.range, 2, diff)
      add.wdw <- (max(wdw.diff)/2)*zoom
      max.wdw <- which.max(wdw.diff)
      xlim <- ylim <- c(wdw.range[1, max.wdw] - add.wdw, wdw.range[1, max.wdw] + add.wdw) }
    # if xlim or ylim are provided
    if (!missing(xlim) | !missing(ylim)) { 
      if (missing(xlim)){ xlim <- ylim }
      if (missing(ylim)){ ylim <- xlim }
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE,
           xlim=xlim, ylim=ylim)
    } else {
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)}
    if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}}
  if (plot) {
    polygon(coo, col=col, border=NA)
    lines(coo, col=border, lwd=lwd, lty=lty)
    # we handle coordinate points
    if (missing(points)) { if (nrow(coo)<=120) points(coo, pch=pch, cex=cex, col=border)}
    if (points) { points(coo, pch=pch, cex=cex, col=border) }
    if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20, cex=2/3)}
    if (centroid) {
      cent <- coo.centpos(coo)
      points(cent[1], cent[2], pch=3, col=border, cex=cex)}
    if (!missing(main)) title(main=main)}}

#' Adds a shape to the current plot
#' 
#' \code{coo.draw} is simply a \link{coo.plot} with \code{plot.new=FALSE}, ie 
#' that adds a shape on the active plot.
#' @param coo a \code{list} or a \code{matrix} of coordinates.
#' @param ... optional parameters for \link{coo.plot}
#' @keywords Graphics
#' @examples
#' data(bot)
#' b1 <- bot[4]
#' b2 <- bot[5]
#' coo.plot(b1)
#' coo.draw(b2, border="red") # all coo.plot arguments will work for coo.draw
#' @export
coo.draw <- function(coo, ...){
  coo.plot(coo, plot.new=FALSE, ...)}

#' Plots (lollipop) differences between two configurations
#' 
#' Draws "lollipops" between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param pch a pch for the points
#' @param cex a cex for the points
#' @param ... optional parameters to fed \link{points} and \link{segments}.
#' @seealso \link{coo.arrows}
#' @keywords Graphics
#' @examples
#' data(olea)
#' coo.lolli(coo.sample(olea[3], 50), coo.sample(olea[6], 50))
#' @export
coo.lolli <- function(coo1, coo2, pch=20, cex=0.5, ...){
  coo.plot(rbind(coo1, coo2), plot=FALSE)
  coo1 <- coo.check(coo1)
  coo2 <- coo.check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")}
  s <- seq(nrow(coo1)-1)
  segments(coo1[s, 1], coo1[s, 2], coo2[s+1, 1], coo2[s+1, 2], ...)
  points(coo2[, 1], coo2[, 2], pch=pch, cex=cex, ...)}

#' Plots (lollipop) differences between two configurations
#' 
#' Draws "arrows" between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param length a length for the arrows.
#' @param angle an angle for the arrows
#' @param ... optional parameters to fed \link{arrows}.
#' @seealso \link{coo.arrows}
#' @keywords Graphics
#' @examples
#' data(olea)
#' coo.arrows(coo.sample(olea[3], 50), coo.sample(olea[6], 50))
#' @export
coo.arrows <- function(coo1, coo2, length=0.1, angle=20, ...){
  coo.plot(rbind(coo1, coo2), plot=FALSE)
  coo1 <- coo.check(coo1)
  coo2 <- coo.check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")}
  s <- seq(nrow(coo1)-1)
  arrows(coo1[s, 1], coo1[s, 2], coo2[s+1, 1], coo2[s+1, 2],
         length=length, angle=angle,...)}

#' "Templates" shapes
#' 
#' \code{coo.template} returns shape centered on the origin and inscribed in a \code{size}-side square
#' 
#' See \link{coo.list.panel} for an illustration of this function. The morphospaces
#' functions also take profit of this function. May be useful to develop other graphical functions.
#' 
#' @usage coo.template(coo, size)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param size \code{numeric}. Indicates the length of the side "inscribing"
#' the shape.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.list.panel}.
#' @keywords Graphics
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
#' @export
coo.template   <- function(coo, size=1) {
  # only for matrices
  coo      <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift    <-  expected - observed
  coo <- coo.trans(coo, shift[1], shift[2])
  #if (keep.pos) {coo2 <- coo.trans(coo2, coo.centpos(coo)[1], coo.centpos(coo)[2])}
  return(coo)}

#' Plots sets of shapes.
#' 
#' \code{coo.list.panel} plots a list of shapes if passed with a list of
#' coordinates. Outlines are "templated" (see \link{coo.template} and will be drawn
#' the same graphical window. Mainly used by \link{panel.Coo} functions.
#' 
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
#' @param points logical if poly is set to FALSE whether to add points
#' @param points.pch if points is TRUE, a pch for these points
#' @param points.cex if points is TRUE, a cex for these points
#' @param points.col if points is TRUE, a col  for these points
#' @return Returns (invisibly) a \code{data.frame} with position of shapes that
#' can be used for other sophisticated plotting design.
#' @seealso \link{coo.plot} and \link{coo.template}.
#' @keywords Graphics
#' @examples
#' data(bot)
#' coo.list.panel(bot$coo) # equivalent to panel(bot)
#' x <- coo.list.panel(bot$coo)
#' x # positions of shapes returned invisibly 
#' # axis(1) ; axis(2) # that's a single graphical window
#' data(bot)
#' coo <- bot$coo
#' ord <- sapply(coo, coo.eccentricity.eigen)
#' pos <- coo.list.panel(coo, reorder=ord)
#' text(pos, labels=signif(ord[order(ord)], 3))
#' @export
coo.list.panel <- function(coo.list, dim, byrow=TRUE,
                           fromtop=TRUE, mar=rep(0, 4),
                           cols, borders, reorder=NULL, poly=TRUE,
                           points=FALSE, points.pch=3, points.cex=0.2,
                           points.col="#333333"){
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
              col=borders[i])
      if (points) {
#         if (!missing(points.col)) {
#           col <- rep(points.col, length(coo.list))
#         }
        points(coo.tp[[i]][, 1] + trans[2],
               coo.tp[[i]][, 2] + trans[1],
               col=points.col, pch=points.pch, cex=points.cex)}}}
  invisible(res)}

# ldk plotters ---------------------------------------------------------

#' Add landmarks labels
#' 
#' @param ldk a matrix of (x; y) coordinates: where to plot the labels
#' @param d how far from the coordinates, on a (centroid-landmark) segment
#' @param cex the cex for the label
#' @param ... additional parameters to fed \link{text}
#' @keywords Graphics
#' @examples
#' data(wings)
#' coo.plot(wings[1])
#' ldk.labels(wings[1])
#' # closer and smaller
#' coo.plot(wings[1])
#' ldk.labels(wings[1], d=0.05, cex=0.5)
#' @export
ldk.labels <- function(ldk, d=0.1, cex=0.8, ...){
  ldk <- coo.check(ldk)
  centpos <- coo.centpos(ldk)
  dm <- median(coo.centdist(ldk))
  for (i in 1:nrow(ldk)){
    dxy <- ed(centpos, ldk[i, ])
    labxy <- edi(centpos, ldk[i, ], (dxy+dm*d)/dxy)
    text(labxy[1], labxy[2], labels=i, cex=cex,  ...)}}

#' Draws links between landmarks
#' 
#' Cosmetics only but useful to visualize shape variation.
#' 
#' @param ldk a matrix of (x; y) coordinates
#' @param links a matrix of links. On the first column the starting-id,
#' on the second column the ending-id (id= the number of the coordinate)
#' @param col a color to draw the links
#' @param pch a pch for the edges
#' @param ... additional parameters to fed \link{segments}
#' @keywords Graphics
#' @export
# todo
ldk.links <- function(ldk, links, col="black", pch=20, ...){
  ldk <- ldk.check(ldk)
  links <- coo.check(links)
  for (i in 1:nrow(links)){
    segments(ldk[links[i, 1], 1],ldk[links[i, 1], 2],
             ldk[links[i, 2], 1],ldk[links[i, 2], 2],
             col=col, pch=pch, ...)}}

#' Draws confidence ellipses for landmark positions
#' 
#' @param ldk an array (or a list) of landmarks
#' @param conf the confidence level (normal quantile, 0.5 by default)
#' @param col the color for the ellipse
#' @param ell.lty an lty for the ellipse
#' @param ax logical whether to draw ellipses axes
#' @param ax.lty an lty for ellipses axes
#' @seealso \link{ldk.contour}, \link{ldk.chull}
#' @keywords Graphics
#' @examples
#' data(wings)
#' coo.plot(mshape(wings))
#' ldk.confell(wings$coo)
#' @export
ldk.confell <- function(ldk, conf=0.5, col="grey40",
                        ell.lty=1, ax=TRUE, ax.lty=2){
  ldk <- ldk.check(ldk)
  for (i in 1:dim(ldk)[1]){
    if (all(apply(ldk[i,,], 1, var)!=0)) {
      xy.i <- t(ldk[i,,])
      ell.i <- conf.ell(xy.i[, 1], xy.i[, 2], conf=conf, nb.pts=360)
      lines(ell.i$ell, col=col, lty=ell.lty, lwd=1)
      if (ax){
        segments(ell.i$seg[1, 1], ell.i$seg[1, 2],
                 ell.i$seg[2, 1], ell.i$seg[2, 2], lty=ax.lty, col=col, lwd=1)
        segments(ell.i$seg[3, 1], ell.i$seg[3, 2],
                 ell.i$seg[4, 1], ell.i$seg[4, 2], lty=ax.lty, col=col, lwd=1)}}}}

#' Draws kernel density contours around landmark
#' 
#' Using \link{kde2d} in the MASS package.
#' @param ldk an array (or a list) of landmarks
#' @param nlevels the number of contour lines
#' @param grid.nb the grid.nb
#' @param col a color for drawing the contour lines
#' @seealso \link{kde2d}, \link{ldk.confell}, \link{ldk.chull}
#' @keywords Graphics
#' @examples
#' data(wings)
#' coo.plot(mshape(wings))
#' ldk.contour(wings$coo)
#'  @export
ldk.contour <- function(ldk, nlevels=5, grid.nb=50, col="grey60") {
  ldk <- ldk.check(ldk)
  for (i in 1:dim(ldk)[1]){
    kx <- ldk[i,1,]
    ky <- ldk[i,2,]
    if (all(sd(kx)>0, sd(ky)>0)){
      k <- kde2d(kx, ky, n=grid.nb)
      contour(k$x, k$y, k$z, nlevels=nlevels,
              add=TRUE, drawlabels=FALSE, col=col)}}}

#' Draws convex hulls around landmark positions
#' 
#' A wrapper that uses \link{coo.chull}
#' @param ldk an array (or a list) of landmarks
#' @param col a color for drawing the convex hull
#' @param lty an lty for drawing the convex hulls
#' @seealso \link{coo.chull}, \link{chull}, \link{ldk.confell}, \link{ldk.contour}
#' @keywords Graphics
#' @examples
#' data(wings)
#' coo.plot(mshape(wings))
#' ldk.chull(wings$coo)
#' @export
ldk.chull <- function(ldk, col="grey40", lty=1){
  ldk <- ldk.check(ldk)
  nl <- dim(ldk)[1]
  for (i in 1:nl){
    ind.i <- chull(ldk[i,1,], ldk[i,2,])
    coo.draw(coo.close(t(ldk[i,,ind.i])),
             border=col, col=NA, lty=lty,
             points=FALSE, first.point=FALSE, centroid=FALSE)}}

# 3. various plotters ------------------------------------------------------

#' Extract structure from filenames
#' 
#' A very simple image plotter. If not provided with an imagematrix, will
#' ask you to choose interactively a \code{.jpeg} image.
#' 
#' @param img a matrix of an image, such as those obtained with \link{readJPEG}.
#' @keywords Import
#' @export
img.plot <- function(img){
  # dirty here but made for convenience
  # to have a fast img plotter..
  if (missing(img)) img <- readJPEG(source = file.choose())
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
#' @keywords Graphics
#' @examples 
#' data(bot)
#' coo.oscillo(bot[1])
#' 
#' @export
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
#' 
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
#' @keywords Graphics
#' @examples
#' # we prepare some fake data
#' foo.mat  <- matrix(1:10, nr=3, nc=10, byrow=TRUE) + rnorm(30, sd=0.5)
#' foo.mat  <- foo.mat + matrix(rep(c(0, 2, 5), each=10), 3, byrow=TRUE)
#' foo.dev  <- matrix(abs(rnorm(30, sd=0.5)), nr=3, nc=10, byrow=TRUE)
#' dev.plot(foo.mat, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, plot=TRUE)
#' # some possible tuning
#' dev.plot(foo.mat, foo.dev, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, cols=col.sari(3), poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
#' dev.plot(foo.mat, foo.dev, cols=col.summer(6)[4:6], plot=TRUE)
#' @export
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
#' @usage dev.segments(coo, cols, lwd = 1)
#' @param coo A matrix of coordinates.
#' @param cols A vector of color of \code{length = nrow(coo)}.
#' @param lwd The \code{lwd} to use for drawing segments.
#' @keywords Graphics
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
#' @export
dev.segments <-function(coo, cols, lwd=1){
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i+1, 1], coo[i+1, 2],
             col=cols[i], lwd=lwd)}}

#' Confidence ellipses
#' 
#' Draw (gaussian) confidence ellipses
#' @param x numeric values on the x axis
#' @param y numeric values on the y axis
#' @param conf the level of confidence
#' @param nb.pts the number of points to return, to draw the ellipsis
#' @return a list with $ell coordinates of the ellipse and $seg coordinates
#' of its vertices
#' @keywords Graphics
#' @return a matrix of (x; y) coordinates to draw the ellipsis
#' @examples
#' x <- rnorm(100, sd=3)
#' y <- rnorm(100)
#' plot(x, y, asp=1)
#' ce095 <- conf.ell(x, y, conf=0.95) # no need for conf arg since it's .95 by default
#' ce090 <- conf.ell(x, y, conf=0.90)
#' ce050 <- conf.ell(x, y, conf=0.50)
#' cols <- col.hot(10)
#' lines(ce050$ell, col=cols[5]) # you can also coo.close(ce050$ell)
#' lines(ce090$ell, col=cols[8])
#' lines(ce095$ell, col=cols[9])
#' segments(ce095$seg[1, 1], ce095$seg[1, 2], ce095$seg[2, 1], ce095$seg[2, 2])
#' segments(ce095$seg[3, 1], ce095$seg[3, 2], ce095$seg[4, 1], ce095$seg[4, 2])
#' @export
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
  #stupid approximation
  ell.al  <- coo.align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]),
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  return(list(ell=ell, seg=seg))}

##### Graphics misc

#' @export
.grid.sample <- function(..., nside=10, over=1){
  wdw <- apply(rbind(...), 2, range)
  wdw <- coo.scale(wdw, scale=1/over)
  by <- min(apply(wdw, 2, diff))/nside
  xr <- seq(wdw[1, 1], wdw[2, 1], by=by)
  yr <- seq(wdw[1, 2], wdw[2, 2], by=by)
  grid <- expand.grid(xr, yr)
  return(as.matrix(grid))}

#' @export
# returns the size of the graphical window
.wdw <- function(){
  wdw <- par("usr")
  x <- wdw[2] - wdw[1]
  y <- wdw[4] - wdw[3]
  return(c(x, y))}

##### end basic plotters
