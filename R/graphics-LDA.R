
#' Plots Linear Discriminant Analysis
#' 
#' The Momocs' LCA plotter with many graphical options and morphospaces 
#' (the latter being experimental so far).
#' @method plot LDA
#' @param x an object of class "LDA", typically obtained with \link{LDA}
#' @param xax the first LD axis
#' @param yax the second LD axis
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
#' @param ellipsesax logical whether to draw ellipse axes
#' @param lty.ellipsesax if yes, the lty for them
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
#' bot.l <- LDA(bot.f, "type")
#' plot(bot.l)
#' 
#' bot.f$fac$fake <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, "fake")
#' plot(bot.l)
#' @export
plot.LDA <- function(#basics
  x, xax=1, yax=2, 
  #color choice
  col="black", pch, cex=0.8, palette=col.autumn,
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
  ellipses=FALSE, conf=0.5, ellipsesax=FALSE, lty.ellipsesax=2,
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
  LDA <- x
  fac <- LDA$fac
  # we check and prepare
  if (nlevels(fac) <= 2) { # case of 2 levels and a single LD
    xy <- LDA$mod.pred$x[, 1]
  } else {   
    xy <- LDA$mod.pred$x[, c(xax, yax)]
  }
  # we check and prepare
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
  if (!missing(pch)){
    if(length(pch)==nlevels(fac)) {
      pch <- pch[fac] 
    } else {
      pch <- pch}
  } else {
    pch <- as.numeric(fac)    
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
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  .frame(xy, center.origin, zoom=zoom)
  if (grid) .grid(nb.grids)
    if (morphospace & length(LDA$method)<2) {
      if(LDA$method=="eFourier") {
      .morphospaceLDA(LDA, xax=xax, yax=yax, pos.shp=pos.shp,
                      amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
                      col.shp=col.shp, border.shp=border.shp)}}
  if (stars)      .stars(xy, fac, col.groups)
  if (ellipsesax) .ellipsesax(xy, fac, conf, col.groups, lty.ellipsesax)
  if (ellipses)   .ellipses(xy, fac, conf, col.groups) #+conf
  if (chull)      .chull(xy, fac, col.groups, chull.lty)
  if (labels)     .labels(xy, fac, col.groups)
  if (rug)        .rug(xy, fac, col.groups)
  points(xy, pch=pch, col=col, cex=cex)
  if (axisnames)  .axisnames(xax, yax, "LD")
  if (axisvar)    .axisvar(LDA$mod$svd, xax, yax)
  .title(title)
   # should be called differently #todo
  if (eigen)     .eigen(LDA$mod$svd, xax, yax, ev.names="Proportion of trace")
  box()}
