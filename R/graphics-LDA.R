
#' Plots Linear Discriminant Analysis
#' 
#' The Momocs' LCA plotter with many graphical options and morphospaces 
#' (the latter being experimental so far).
#' @method plot LDA
#' @param x an object of class "LDA", typically obtained with \link{LDA}
#' @param xax the first PC axis
#' @param yax the second PC axis
#' @param points logical whether to plot points
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
#' @param lwd.shp the line width for these shapes
#' @param col.shp the color of the shapes
#' @param stars logical whether to draw "stars"
#' @param ellipses logical whether to draw confidence ellipses
#' @param conf.ellipses numeric the quantile for the (bivariate gaussian) confidence ellipses 
#' @param ellipsesax logical whether to draw ellipse axes
#' @param conf.ellipsesax one or more numeric, the quantiles for the (bivariate gaussian) ellipses axes
#' @param lwd.ellipsesax if yes, one or more numeric for the line widths
#' @param lty.ellipsesax if yes, the lty with which to draw these axes
#' @param chull logical whether to draw a convex hull
#' @param chull.lty if yes, its linetype
#' @param density whether to add a 2d density kernel estimation (based on \link{kde2d})
#' @param lev.density if yes, the number of levels to plot (through \link{image})
#' @param contour whether to add contour lines based on 2d density kernel
#' @param lev.contour if yes, the (approximate) number of lines to draw
#' @param n.kde2d the number of bins for \link{kde2d}, ie the 'smoothness' of density kernel
#' @param delaunay logical whether to add a delaunay 'mesh' between points
#' @param labelsgroups logical whether to add labels for groups
#' @param cex.labelsgroups ifyes, a numeric for the size of the labels
#' @param rect.labelsgroups logical whether to add a rectangle behind groups names
#' @param abbreviate.labelsgroups if yes, whether to abbreviate group names
#' @param axisnames logical whether to add PC names
#' @param axisvar logical whether to draw the variance they explain
#' @param eigen logical whether to draw a plot of the eigen values
#' @param rug logical whether to add rug to margins
#' @param title character a name for the plot
#' @param ... useless here, just to fit the generic plot
#' @details Widely inspired by the philosophy behind graphical functions
#' of the ade4 R package.
#' @seealso \link{LDA}, \link{plotCV}, \link{plot.PCA}.
#' @keywords Multivariate, Graphics
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.l <- LDA(bot.f, "type")
#' plot(bot.l)
#' 
#' bot.f$fac$fake <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, "fake")
#' plot(bot.l)
#' @export
plot.LDA <- function( x, xax=1, yax=2,
  #color choice
  points=TRUE, col="#000000", pch=20, cex=.cex(nrow(LDA$mod.pred$x)), palette=col.summer2,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp.shp=1,
  size.shp=15, nb.shp=12, nr.shp=6, nc.shp=5,
  pts.shp=60, border.shp="#00000032", lwd.shp=1, col.shp="#00000019",
  #stars
  stars=FALSE,
  #ellipses
  ellipses=FALSE, conf.ellipses=0.5,
  #ellipsesax
  ellipsesax=TRUE, conf.ellipsesax=c(0.5, 0.75, 0.9), 
  lty.ellipsesax=1, lwd.ellipsesax=sqrt(2), 
  #convexhulls
  chull=FALSE, chull.lty=3,
  #kde2d
  density=FALSE, lev.density=20,
  contour = FALSE, lev.contour=3, n.kde2d=100,
  #delaunay
  delaunay=FALSE,
  #loadings
  #loadings=FALSE,
  #labels
  labelsgroups=TRUE, cex.labelsgroups=0.8, 
  rect.labelsgroups=TRUE, abbreviate.labelsgroups=FALSE,
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
  LDA <- x
  fac <- LDA$fac
  # we check and prepare
  if (nlevels(fac) <= 2) { # case of 2 levels and a single LD
    xy <- LDA$mod.pred$x[, 1]
  } else {   
    xy <- LDA$mod.pred$x[, c(xax, yax)]
  }
  ### we check and prepare
  # col handling
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
  # pch handling
  if (!missing(pch)) {
    if (length(pch)==nlevels(fac)) { pch <- pch[fac] }
  } else {
    if (nlevels(fac) < 10) {
      pch <- .pch()[fac]
    } else {
      pch <- 20
    }
  }
  # case of 2 levels and a single LD
  if (nlevels(fac) <= 2){
    op <- par(mfrow=c(2, 1), oma=c(0, 0, 0, 0), mar=c(4, 1, 3, 1 ))
    on.exit(op)
    hist.range <- range(xy)
    hist(xy[fac==levels(fac)[1]], xlim=hist.range,
         ylab=NA, xlab="LD1", main=levels(fac)[1], 
         col=palette(2)[1], axes=FALSE); axis(1)
    hist(xy[fac==levels(fac)[2]], xlim=hist.range,
         ylab=NA, xlab="LD1", main=levels(fac)[2],
         col=palette(2)[2], axes=FALSE); axis(1)
    par(mfrow=c(1, 1))
    return()
  }
  # cosmectics
  if ((density) & missing(contour)) contour <- TRUE
  if ((density) & missing(ellipses)) ellipses <- FALSE
  if ((density) & missing(rect.labelsgroups)) rect.labelsgroups <- FALSE
  if (missing(rug) & nlevels(fac)>6) rug <- FALSE
  # we prepare the graphic window
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  # we initate it
  .frame(xy, center.origin, zoom=zoom)
  # then the layers
  if (grid)    .grid(nb.grids)
  if (density) .density(xy, fac, levels= lev.density, col=col.groups, transp=0.3, n.kde2d=n.kde2d)
  if (contour) .contour(xy, fac, levels= lev.contour, col=col.groups, transp=ifelse(density, 0.5, 0.3), n.kde2d=n.kde2d)
  if (delaunay) .delaunay(xy, fac, col.groups)
  # morphospace handling - a big baby
  if (morphospace & length(LDA$method)<2) {
    if(LDA$method=="eFourier") {
      .morphospaceLDA(LDA, xax=xax, yax=yax, pos.shp=pos.shp,
                      amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
                      col.shp=col.shp, border.shp=border.shp)}}
  if (is.factor(fac)) {
    if (stars)      .stars(xy, fac, col.groups)
    if (ellipsesax) .ellipsesax(xy, fac, conf.ellipsesax, col.groups, lty.ellipsesax, lwd.ellipsesax)
    if (ellipses)   .ellipses(xy, fac, conf.ellipses, col.groups) #+conf
    if (chull)      .chull(xy, fac, col.groups, chull.lty)
    if (labelsgroups)     .labelsgroups(xy, fac, col.groups,
                                        cex=cex.labelsgroups, rect=rect.labelsgroups,
                                        abbreviate=abbreviate.labelsgroups)     
    if (rug)        .rug(xy, fac, col.groups)
  } else {
    if (rug)        .rug(xy, NULL, col)
  }
  if (points) points(xy, pch=pch, col=col, cex=cex)
  #if (loadings)   .loadings(PCA$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "PC")
  if (axisvar)    .axisvar(LDA$mod$svd, xax, yax)
  .title(title)
  if (eigen)     .eigen(LDA$mod$svd, xax, yax, ev.names="Proportion of trace")
  box()}

#   opar <- par(mar = par("mar"), xpd=FALSE)
#   on.exit(par(opar))
#   par(mar = rep(0.1, 4)) #0.1
#   
#   .frame(xy, center.origin, zoom=zoom)
#   if (grid) .grid(nb.grids)
#   if (morphospace & length(LDA$method)<2) {
#     if(LDA$method=="eFourier") {
#       .morphospaceLDA(LDA, xax=xax, yax=yax, pos.shp=pos.shp,
#                       amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
#                       col.shp=col.shp, border.shp=border.shp)}}
#   if (stars)      .stars(xy, fac, col.groups)
#   if (ellipsesax) .ellipsesax(xy, fac, conf, col.groups, lty.ellipsesax)
#   if (ellipses)   .ellipses(xy, fac, conf, col.groups) #+conf
#   if (chull)      .chull(xy, fac, col.groups, chull.lty)
#   if (labelsgroups)     .labelsgroups(xy, fac, col.groups, 
#                                       cex=cex.labelsgroups, abbreviate=abbreviate.labelsgroups)
#   if (rug)        .rug(xy, fac, col.groups)
#   points(xy, pch=pch, col=col, cex=cex)
#   if (axisnames)  .axisnames(xax, yax, "LD")
#   if (axisvar)    .axisvar(LDA$mod$svd, xax, yax)
#   .title(title)
#   # should be called differently #todo
#   if (eigen)     .eigen(LDA$mod$svd, xax, yax, ev.names="Proportion of trace")
#   box()}
