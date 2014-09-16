
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
#' @param bg color for the background
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
#' @param labels logical whether to add point labels
#' @param col.labels a color for these labels
#' @param cex.labels a cex for these labels
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
#' @param old.par whether to restore the old \link{par}. Set it to \code{FALSE} if you want to reuse the graphical window.
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
plot.LDA <- function(x, xax=1, yax=2,
  #color choice
  points=TRUE, col="#000000", pch=20, cex=0.5, palette=col.solarized,
  #.frame
  center.origin=FALSE, zoom=1, bg=par("bg"),
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp.shp=1,
  size.shp=1, nb.shp=12, nr.shp=6, nc.shp=5,
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
  labels=FALSE,
  col.labels=par("bg"),
  cex.labels=0.6,
  #labelsgroups
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
  title=substitute(x), box=TRUE, old.par=TRUE, ...
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
  if (old.par) on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  # we initate it
  .frame(xy, center.origin, zoom=zoom, bg=bg)
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
  if (labels) text(xy[, 1], xy[, 2], col=col.labels, cex=cex.labels)
  #if (loadings)   .loadings(PCA$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "LD")
  if (axisvar)    .axisvar(LDA$mod$svd, xax, yax)
  .title(title)
  if (eigen)     .eigen(LDA$mod$svd, xax, yax, ev.names="Proportion of trace")
  if (box) box()}

#' Plots a cross-validation table as an heatmap
#' 
#' Either with frequencies (or percentages) plus marginal sums,
#' and values as heatmaps. Used in Momocs for plotting cross-validation tables
#' but may be used for any table (likely with \code{freq=FALSE}).
#' 
#' @param x a (typically cross-correlation) table to plot
#' @param freq whether to use row-wise frequencies
#' @param palette a color palette such as \link{col.heat} to use,
#' if \code{cols} is not used.
#' @param levels number of levels, otherwise the highest cell in the table
#' @param cols a vector of colors
#' @param pc whether to use percentages
#' @param margin whether to add marginal sums
#' @param cex a cex for all values
#' @seealso \link{LDA}, \link{plot.LDA}, \link{plotCV2}.
#' @keywords Multivariate Graphics
#' @examples
#' data(bot)
#' bot.p <- PCA(eFourier(bot, 12))
#' bot.l <- LDA(bot.p, 1)
#' tab <- bot.l$CV.tab
#' tab
#' plotCV(tab)
#' 
#' data(olea)
#' ol <- LDA(PCA(rawPolynomials(olea, nb.pts=50)), "cep")
#' plotCV(ol$CV.tab)
#' # raw counts
#' plotCV(tab, freq=FALSE, palette=col.india)
#' # any other count table
#' m <- matrix(runif(120, 0, 6), 12)
#' tab <- as.table(round(m)) 
#' plotCV(tab, palette=terrain.colors, levels=5, cex=0.8)
#' @export
plotCV <- function(x, freq=TRUE, 
                    palette=col.heat, levels=20, cols,
                    pc=TRUE, margin=TRUE, cex=1){
  tab <- x
  tab <- t(tab)
  tab <- tab[, ncol(tab):1 ]
  print(tab)
  # if required, we return frequencies (and percentages)
  # but by default we forbid freq on small samples
  #   if (missing(freq) & sum(tab) < 50) freq <- FALSE
  if (freq) tab <- apply(tab, 2, function(x) x/ sum(x)) * ifelse(pc, 100, 1)
  #   if (freq) tab <- tab/sum(tab) * ifelse(pc, 100, 1)
  
  # here start the graphics
  op <- par(xpd=NA, mar=c(5, 5, 4, 1))
  on.exit(par(op))
  # cosmetics
  if (missing(cols)) cols <- palette(levels)
  if (any(tab==0)) cols[1] <- par("bg")
  #breaks <- seq(0, sum(tab)/ncol(tab), length=length(cols)+1)
  # the core piece
  image(x=0:nrow(tab), y=0:ncol(tab), z=tab,
        asp=1, ann=FALSE, axes=FALSE, col=cols, frame=FALSE)
  # draw the grid
  xn <- nrow(tab)
  yn <- ncol(tab)
  segments(0:xn, 0, 0:xn, yn)
  segments(0, 0:yn, xn, 0:yn)
  
  # if the table has names, we add them
  names <- names(dimnames(tab))
  if (length(names) != 0){
    text(yn/2, -0.5, labels=names[1], font=2)
    text(-0.5, xn/2, labels=names[2], srt=90, font=2)}
  
  text(-0.1, 1:yn - 0.5, rev(rownames(x)), 
       cex=cex, adj = 1, font=2)
  text(1:xn - 0.5, yn+0.1, colnames(x),
       cex=cex, adj = c(0.5, 0), font=2)
  
  # if freq are used, from now on, we transform the table into a 
  # reasonable number of digits to plot
  
  # grand total
  if (TRUE){
    segments(xn, 0, xn+0.05, -0.05)
    text(xn+0.1, -0.1, sum(tab), cex=cex*0.8, adj=c(0, 1))
  }
  arrows(-0.1, yn+0.1, 0, yn, length=0.1)
  
  # we plot the values
  xx <- rep(1:xn - 0.5, times=yn)
  yy <- rep(1:yn - 0.5, each=xn)
  if (freq) 
    tab2 <- signif(tab, log10(sum(tab)))
  else
    tab2 <- tab
  text(xx, yy, tab2, cex=cex)
  
  # marginal sums
  if (margin){
    text(1:xn - 0.5, - 0.1, rowSums(tab2), cex=cex*0.8, adj=c(0.5, 1))
    text(xn+0.1, 1:yn - 0.5, colSums(tab), cex=cex*0.8, adj =0 )
  }
}

#' Plots a cross-correlation table
#' 
#' Or any contingency/confusion table. A simple graphic representation based on variable
#' width and/or color for arrows or segments, based on the relative frequencies.
#' 
#' @param x an \link{LDA} object, a table or a squared matrix
#' @param links.FUN a function to draw the links: eg \link{segments} (by default), \link{arrows}, etc.
#' @param col logical whether to vary the color of the links
#' @param col0 a color for the default link (when \code{col = FALSE})
#' @param col.breaks the number of different colors
#' @param palette a color palette, eg \link{col.summer}, \link{col.hot}, etc.
#' @param lwd logical whether to vary the width of the links
#' @param lwd0 a width for the default link (when \code{lwd = FALSE})
#' @param gap.dots numeric to set space between the dots and the links
#' @param pch.dots a pch for the dots
#' @param gap.names numeric to set the space between the dots and the group names
#' @param cex.names a cex for the names
#' @param legend logical whether to add a legend
#' @param ... useless here.
#' @seealso \link{LDA}, \link{plot.LDA}, \link{plotCV}.
#' @keywords Multivariate Graphics
#' @examples
#' # Below various table that you can try. We will use the last one for the examples.
#' \dontrun{
#' #pure random
#' a <- sample(rep(letters[1:4], each=10))
#' b <- sample(rep(letters[1:4], each=10))
#' tab <- table(a, b)
#' 
#' # veryhuge + some structure 
#' a <- sample(rep(letters[1:10], each=10))
#' b <- sample(rep(letters[1:10], each=10))
#' tab <- table(a, b)
#' diag(tab) <- round(runif(10, 10, 20))
#' 
# more structure
#' tab <- matrix(c(8, 3, 1, 0, 0,
#'                 2, 7, 1, 2, 3,
#'                 3, 5, 9, 1, 1,
#'                 1, 1, 2, 7, 1,
#'                 0, 9, 1, 4, 5), 5, 5, byrow=TRUE)
#' tab <- as.table(tab)
#' }
#' # good prediction
#' tab <- matrix(c(8, 1, 1, 0, 0,
#'                1, 7, 1, 0, 0,
#'                 1, 2, 9, 1, 0,
#'                 1, 1, 1, 7, 1,
#'                 0, 0, 0, 1, 8), 5, 5, byrow=TRUE)
#' tab <- as.table(tab)
#' 
#' 
#' plotCV2(tab)
#' plotCV2(tab, arrows) # if you prefer arrows
#' plotCV2(tab, lwd=FALSE, lwd0=1, palette=col.india) # if you like india but not lwds
#' plotCV2(tab, col=FALSE, col0='pink') # only lwd
#' plotCV2(tab, col=FALSE, lwd0=10, cex.names=2) # if you're getting old
#' plotCV2(tab, col=FALSE, lwd=FALSE) # pretty but useless
#' plotCV2(tab, col.breaks=2) # if you think it's either good or bad
#' plotCV2(tab, pch=NA) # if you do not like dots
#' plotCV2(tab, gap.dots=0) # if you want to 'fill the gap'
#' plotCV2(tab, gap.dots=1) # or not
#' 
#' #trilo examples
#' data(trilo)
#' trilo.f <- eFourier(trilo, 8)
#' trilo.l <- LDA(trilo.f, 'onto')
#' trilo.l$CV.tab
#' plotCV2(trilo.l$CV.tab) 
#' 
#' # olea example
#' data(olea)
#' op <- orthoPolynomials(olea, 5)
#' opl <- LDA(op, 'cep')$CV.tab
#' plotCV2(opl)
#' @export
plotCV2 <- function(x, links.FUN = arrows, col = TRUE, 
                    col0 = "black", col.breaks = 5, palette = col.heat, lwd = TRUE, 
                    lwd0 = 5, gap.dots = 0.2, pch.dots = 20, gap.names = 0.25, 
                    cex.names = 1, legend = TRUE, ...) {
  # to maintain the generic
  tab <- x
  # we check a bit
  if (ncol(x) != nrow(x)) 
    stop(" * a table or a squared matrix must be passed.")
  # we deduce xy positions
  gap.mid <- 3
  n <- nrow(tab)
  x.dots <- c(rep(1, n), rep(1 + gap.mid, n))
  y.dots <- rep(1:n, 2)
  x1.link <- rep(1 + gap.dots, n)
  x2.link <- rep(1 + gap.mid - gap.dots, n)
  y.link <- y.dots
  # we initiate the graphics window: no margins and 'butt'
  # lines end
  op <- par(mar = rep(0, 4), lend = 1)
  leg.y1 <- ifelse(legend, 0, 0.5)
  plot(NA, xlim = c(0.8, gap.mid + 1.2), ylim = c(leg.y1, n + 
                                                    0.5))
  # we deduce the 'lwd matrix'
  if (lwd) {
    tab.lwd <- apply(tab, 1, function(x) x/sum(x))
    tab.lwd <- tab.lwd * lwd0
  } else {
    if (missing(lwd0)) 
      lwd0 <- 1  # to avoid too puffy segments
    tab.lwd <- matrix(lwd0, nrow = n, ncol = n)
  }
  # we decude the 'col matrix'
  if (col) {
    cols <- palette(col.breaks)[as.numeric(cut(tab, breaks = col.breaks))]
  } else {
    cols <- rep(col0, n^2)
  }
  # since cols is not yet a matrix, allows a parallel coding in
  # 'segments' below
  tab.cols <- matrix(cols, n, n, byrow = TRUE)
  # the loop that draws the segments
  for (i in 1:n) {
    for (j in 1:n) {
      links.FUN(x1.link[i], y.link[i], x2.link[j], y.link[j], 
                lwd = tab.lwd[i, j], col = tab.cols[i, j])
    }
  }
  # we add dots and classes names
  points(x.dots, y.dots, pch = pch.dots)
  text(x.dots, y.dots + gap.names, labels = unlist(dimnames(tab)), 
       cex = cex.names)
  if (legend) {
    text(1, 1/3, labels = names(dimnames(tab))[1], cex = cex.names, 
         font = 2)
    text(1 + gap.mid, 1/3, labels = names(dimnames(tab))[2], 
         cex = cex.names, font = 2)
  }
  # we restore the graphics parameters
  par(op)
}




