##### PCA plotters

#todo: add deformation grids on the extreme PC axes (pos / meanshape)
#' Plots Principal Component Analysis
#' 
#' The Momocs' PCA plotter with many graphical options and morphospaces.
#' @method plot PCA
#' @param x an object of class "PCA", typically obtained with \link{PCA}
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
#' @param nb.shp (pos.shp="circle") the number of shapes on the compass
#' @param nc.shp (pos.shp="full" or "range) the number of shapes per column
#' @param nr.shp (pos.shp="full" or "range) the number of shapes per row
#' @param pts.shp the number of points fro drawing shapes
#' @param border.shp the border color of the shapes
#' @param col.shp the color of the shapes
#' @param stars logical whether to draw "stars"
#' @param ellipses logical whether to draw confidence ellipses
#' @param conf the level of confidence
#' @param ellipsesax logical whether to draw ellipse axes
#' @param lty.ellipsesax if yes, the lty for them
#' @param chull logical whether to draw a convex hull
#' @param chull.lty if yes, its linetype
#' @param loadings logical whether to add loadings for every variables
#' @param labels logical whether to add labels for groups
#' @param axisnames logical whether to add PC names
#' @param axisvar logical whether to draw the variance they explain
#' @param eigen logical whether to draw a plot of the eigen values
#' @param rug logical whether to add rug to margins
#' @param title character a name for the plot
#' @param ... useless here, just to fit the generic plot
#' @details Widely inspired by the philosophy behind graphical functions
#' of the ade4 R package.
#' @keywords Graphics
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' bot.p
#' plot(bot.p, morpho=FALSE)
#' plot(bot.p, "type") 
#' 
#' data(olea)
#' op <- rawPolynomials(olea, 5)
#' op.p <- PCA(op)
#' op.p
#' plot(op.p, 1, morpho=TRUE)
#' 
#' data(wings)
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wpp <- PCA(wp)
#' wpp
#' plot(wpp, 1)
#' @export
plot.PCA <- function(#basics
  x, fac, xax=1, yax=2, 
  #color choice
  col="black", pch=20, cex=0.3, palette=col.autumn,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp.shp=1,
  size.shp=15, nb.shp=12, nr.shp=6, nc.shp=5,
  pts.shp=60, border.shp="#00000088", col.shp="#00000011",
  #stars
  stars=FALSE,
  #ellipses
  ellipses=TRUE, conf=0.5, ellipsesax=FALSE, lty.ellipsesax=2,
  #convexhulls
  chull=TRUE, chull.lty=3,
  #loadings
  loadings=FALSE,
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
    if (!missing(col)){
      if (length(col)==nlevels(fac)) {
        col.groups <- col
        col <- col.groups[fac]
      } else {
        col.groups <- rep(col[1], nlevels(fac))
        col <- rep(col[1], nrow(xy))}
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
  if (grid) .grid(nb.grids)
  if (morphospace & !is.null(PCA$method) & length(PCA$method)<2) {
    .morphospacePCA(PCA, xax=xax, yax=yax, pos.shp=pos.shp, nb.shp=nb.shp, nr.shp=nr.shp, nc.shp=nc.shp,
                    amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
                    col.shp=col.shp, border.shp=border.shp)}
  if (!missing(fac)) {
    if (stars)      .stars(xy, fac, col.groups)
    if (ellipsesax) .ellipsesax(xy, fac, conf, col.groups, lty.ellipsesax)
    if (ellipses)   .ellipses(xy, fac, conf, col.groups) #+conf
    if (chull)      .chull(xy, fac, col.groups, chull.lty)
    if (labels)     .labels(xy, fac, col.groups)
    if (rug)        .rug(xy, fac, col.groups)
  } else {
    if (rug)        .rug(xy, NULL, col)
  }
  points(xy, pch=pch, col=col, cex=cex)
    if (loadings)   .loadings(PCA$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "PC")
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax, ev.names="Eigenvalues")
  box()}

#' Plots a combination of the three first PCs
#' 
#' Creates a 2 x 3 layout with, from top to bottom and form left to right: PC1-PC2,
#' PC1-PC3, PC2-3, and the barplot of eigenvalues percentages.
#' @param PCA a \link{PCA} object
#' @param ... additional arguments to fed \link{plot.PCA}
#' @keywords Graphics
#' @rdname plot3.PCA
#' @examples 
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' plot3(bot.p) # no groups
#' plot3(bot.p, 1) # groups
#' plot3(bot.p, "type", pos.shp="circle") # all plot.PCA args should work
#' @export
plot3 <- function(PCA, ...){UseMethod("plot3")}
#' @rdname plot3.PCA
#' @export
plot3.PCA <- function(PCA, ... ){
  op1 <- par(mfrow=c(2, 2))
  on.exit(par(op1))
  # The three plot.PCA plots
  plot(PCA, xax=1, yax=2, title=paste0(substitute(PCA),": ", "PC1-PC2"), eigen=TRUE, ...)
  plot(PCA, xax=1, yax=3, title=paste0(substitute(PCA),": ", "PC1-PC3"), eigen=TRUE, ...)
  plot(PCA, xax=2, yax=3, title=paste0(substitute(PCA),": ", "PC2-PC3"), eigen=TRUE, ...)
  # The eigen value plot
  op2 <- par(mar=c(2, 3, 4, 3), xpd=NA)
  var <- PCA$sdev^2
  pc <- 100*var/sum(var)
  cols <- rep("grey80", 5)
  cols[1:3] <- "grey40"
  v <- pc[1:5]
  bp <- barplot(v, col=cols, border=NA, axes=FALSE, main="Eigenvalues")
  text(bp, v+2, labels = paste0(round(v, 1), "%"))
  axis(1, at = bp, labels=paste0("PC", 1:5), line = -1, tick = FALSE)
}

##### end PCA plotters

