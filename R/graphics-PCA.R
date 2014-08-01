##### PCA plotters

### if high nb of group or if long, abbreviate auto
### change labelsgroups in lda
### cahnge confell in lda
### integrate .cex

#todo: add deformation grids on the extreme PC axes (pos / meanshape)
#' Plots Principal Component Analysis
#'
#' The Momocs' PCA plotter with many graphical options and morphospaces.
#' @method plot PCA
#' @param x an object of class "PCA", typically obtained with \link{PCA}
#' @param fac factor, or a name or the column id from the $fac slot, or a formula combining colum names
#' from the $fac slot (cf. examples)
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
#' @param loadings logical whether to add loadings for every variables
#' @param labelsgroups logical whether to add labels for groups
#' @param cex.labelsgroups ifyes, a numeric for the size of the labels
#' @param rect.labelsgroups logical whether to add a rectangle behind groups names
#' @param abbreviate.labelsgroups if yes, whether to abbreviate group names
#' @param axisnames logical whether to add PC names
#' @param axisvar logical whether to draw the variance they explain
#' @param eigen logical whether to draw a plot of the eigen values
#' @param rug logical whether to add rug to margins
#' @param title character a name for the plot
#' @param box whether to draw a box around the plotting region
#' @param ... useless here, just to fit the generic plot
#' @details Widely inspired by the philosophy behind graphical functions
#' of the ade4 R package.
#' @seealso \link{plot.LDA}
#' @keywords Multivariate, Graphics
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
#' plot(op.p, ~ domes + cep, morpho=TRUE) # use of formula
#'
#' data(wings)
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wpp <- PCA(wp)
#' wpp
#' plot(wpp, 1)
#' @export
plot.PCA <- function(x, fac, xax=1, yax=2,
   #color choice
   points=TRUE, col="#000000", pch=20, cex=.cex(nrow(PCA$x)), palette=col.summer2,
   #.frame
   center.origin=FALSE, zoom=1,
   #.grid
   grid=TRUE, nb.grids=3,
   #shapes
   morphospace=TRUE, pos.shp="full", amp.shp=1,
   size.shp=15, nb.shp=12, nr.shp=6, nc.shp=5,
   pts.shp=60, border.shp=.transp("#000000", 0.5), lwd.shp=1, col.shp=.transp("#000000", 0.9),
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
   loadings=FALSE,
   #labels
   labelsgroups=TRUE, cex.labelsgroups=0.8,
   rect.labelsgroups=FALSE, abbreviate.labelsgroups=FALSE,
   #axisnames
   axisnames=TRUE,
   #axisvar
   axisvar=TRUE,
   #eigen
   eigen=TRUE,
   #
   rug=TRUE,
   title=substitute(x), box=TRUE, ...
){
  PCA <- x
  xy <- PCA$x[, c(xax, yax)]
  ### we check and prepare evrtything related to groups
  # fac handling
  if (missing(fac)) { # mainly for density and contour
    fac <- NULL
    col.groups <- col
  } else {
    if (class(fac)=="formula"){
      f0 <- PCA$fac[, attr(terms(fac), "term.labels")]
      fac <- interaction(f0)}
    if (!is.factor(fac)) { fac <- factor(PCA$fac[, fac]) }
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
  if (morphospace & !is.null(PCA$method) & length(PCA$method)<2) {
    .morphospacePCA(PCA, xax=xax, yax=yax, pos.shp=pos.shp,
                    nb.shp=nb.shp, nr.shp=nr.shp, nc.shp=nc.shp,
                    amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
                    col.shp=col.shp, border.shp=border.shp, lwd.shp=lwd.shp)}
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
  if (loadings)   .loadings(PCA$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "PC")
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax, ev.names="Eigenvalues")
  if (box) box()
}

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
plot3.PCA <- function(PCA,  ... ){
  op1 <- par(mfrow=c(2, 2))
  on.exit(par(op1))
  # The three plot.PCA plots
  plot(PCA, xax=1, yax=2, title=paste0(substitute(PCA),": ", "PC1-PC2"), ...)
  plot(PCA, xax=1, yax=3, title=paste0(substitute(PCA),": ", "PC1-PC3"), ...)
  plot(PCA, xax=2, yax=3, title=paste0(substitute(PCA),": ", "PC2-PC3"), ...)
  # The eigen value plot
  op2 <- par(mar=c(3, 8, 4, 8), xpd=NA)
  var <- PCA$sdev^2
  pc <- 100*var/sum(var)
  cols <- rep("grey80", 5)
  cols[1:3] <- "grey40"
  v <- pc[1:5]
  bp <- barplot(v, col=cols, border=NA, axes=FALSE, main="Eigenvalues")
  text(bp, v+2, labels = paste0(round(v, 1), "%"), cex=0.8)
  axis(1, at = bp, labels=paste0("PC", 1:5), line = -1, tick = FALSE, cex.axis=0.8)
}


#' Boxplot on PCA objects
#'
#' @method boxplot PCA
#' @param x an object of class "PCA", typically obtained with \link{PCA}
#' @param fac factor, or a name or the column id from the $fac slot
#' @param nax the range of PC axis
#' @param cols a vector of colors is palette is not provided
#' @param palette a color palette
#' @param fixed.axes logical whether axes shoudl have the same scaled
#' @param center.origin if TRUE before, whether to center the origin
#' @param ...  further arguments to feed \link{boxplot}
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' boxplot(bot.p, 1)
#' @export
boxplot.PCA <- function(x, fac, nax=1:4, cols, palette=col.qual,
                        fixed.axes=TRUE, center.origin=TRUE, ...){
  xy <- x$x[, nax]
  if (missing(fac)){
    fac <- factor(rep("foo", nrow(xy)))
    no.fac <- TRUE
  } else {
    no.fac <- FALSE
  }

  if (!is.factor(fac)) { fac <- factor(x$fac[, fac]) }
  fl <- levels(fac)
  fn <- nlevels(fac)
  if (missing(cols)){ cols <- palette(fn) }
  if (no.fac) { cols <- "grey20" }
  if (fixed.axes){
    yl <- range(xy)
    if (center.origin) {
      yl <- max(abs(yl))
      yl <- c(-yl, yl)
    }
    op <- par(mfrow=c(length(nax), 1), oma=c(3, 0, 0, 3), mar=c(1, 3, 2, 0), lend=2)
    on.exit(par(op))
    for (i in seq(along=nax)){
      boxplot(xy[, nax[i]] ~ fac, ylim=yl, at=fn:1, horizontal=TRUE,
              col=cols, boxcol=NA, medlwd=1, medcol=par("bg"), whisklty=1, outpch=1,
              axes=FALSE, boxwex=1/3, main=paste0("PC", nax[i]), ...)}
    axis(1)
  } else {
    op <- par(mfrow=c(length(nax), 1), oma=c(3, 0, 0, 3), mar=c(3, 3, 2, 0), lend=2)
    for (i in seq(along=nax)){
      boxplot(xy[, nax[i]] ~ fac, at=fn:1, horizontal=TRUE,
              col=cols, boxcol=NA, medlwd=1, medcol=par("bg"), whisklty=1, outpch=1,
              axes=FALSE, boxwex=1/3, main=paste0("PC", nax[i]), ...)
      axis(1)}
  }
  if (!no.fac) {
    legend("topright", legend = levels(fac), fill = cols, bty="n", border = NA)
  }
}




##### end PCA plotters

