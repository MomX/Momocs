##### The graphics R file for everyhting graphics. Some internals
##### used elsewhere.

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
#' @param poly logical whether to use \link{polygon} and \link{lines} to draw the shape,
#' or just \link{points}. In other words, whether the shape should be considered as a configuration
#' of landmarks or not (eg a closed outline).
#' @param plot.new \code{logical} whether to plot or not a new frame.
#' @param plot logical whether to plot something or just to create an empty plot.
#' @param zoom a numeric to take your distances.
#' @param ... further arguments for use in coo.plot methods. See examples.
#' @return No returned value.
#' @seealso coo.draw
#' @keywords Graphics
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' coo.plot(bot[2], plot.new=FALSE) # equivalent to coo.draw(bot[2]) 
#' coo.plot(b, zoom=2)
#' coo.plot(b, border='blue')
#' coo.plot(b, first.point=FALSE, centroid=FALSE)
#' coo.plot(b, points=TRUE, pch=20)
#' coo.plot(b, xy.axis=FALSE, lwd=2, col='#F2F2F2')
#' @aliases coo.plot
#' @rdname coo.plot
#' @export
coo.plot <- function(coo, ...) {
  UseMethod("coo.plot")
}

#' @rdname coo.plot
#' @export
coo.plot.default <- function(coo, xlim, ylim, border = "#333333", 
                             col = NA, lwd = 1, lty = 1, points = FALSE, first.point = TRUE, 
                             centroid = TRUE, xy.axis = TRUE, pch = 1, cex = 0.5, main = NA, 
                             poly = TRUE, plot.new = TRUE, plot = TRUE, zoom = 1, ...) {
  # todo zoom
  coo <- coo.check(coo)
  # if 'plot.new=TRUE' we have initiate the graphical window
  if (plot.new) {
    # we setup coo.plot graphical parameters
    op <- par(mar = c(3, 3, 2, 1))
    on.exit(par(op))
    # if zoom if provided, we define wlim and ylim manually
    if (!missing(zoom)) {
      half.side <- max(apply(coo, 2, function(x) diff(range(x))))/2
      add.xy <- half.side * zoom
      orig.xy <- coo.centpos(coo)
      xlim <- c(orig.xy[1] - add.xy, orig.xy[1] + add.xy)
      ylim <- c(orig.xy[2] - add.xy, orig.xy[2] + add.xy)
    }
    # if xlim or ylim are provided
    if (!missing(xlim) | !missing(ylim)) {
      if (missing(xlim)) {
        xlim <- ylim
      }
      if (missing(ylim)) {
        ylim <- xlim
      }
      plot(coo, type = "n", asp = 1, las = 1, cex.axis = 2/3, 
           ann = FALSE, frame = FALSE, xlim = xlim, ylim = ylim)
    } else {
      plot(coo, type = "n", asp = 1, las = 1, cex.axis = 2/3, 
           ann = FALSE, frame = FALSE)
    }
    if (xy.axis) {
      abline(h = 0, v = 0, col = "grey80", lty = 2)
    }
  }
  # if 'plot.new=FALSE', we simply have to draw the shape in
  # the existing window 'plot' is meant to initialize a
  # graphical window without plotting a shape
  if (plot) {
    # 'poly'=FALSE allows to plot only points, eg for landmarks
    if (!missing(poly)) {
      if ((!poly) & missing(points)) 
        points <- TRUE
    }
    if (poly) {
      polygon(coo, col = col, border = NA)
      lines(coo, col = border, lwd = lwd, lty = lty)
    }
    # we handle coordinate points if very few points and 'points'
    # is missing we draw them by default
    if (missing(points)) {
      if (nrow(coo) <= 60) 
        points <- TRUE
    }
    if (points) {
      points(coo, pch = pch, cex = cex, col = border)
    }
    if (first.point) {
      points(coo[1, 1], coo[1, 2], col = border, pch = 20, 
             cex = 2/3)
    }
    if (centroid) {
      cent <- coo.centpos(coo)
      points(cent[1], cent[2], pch = 3, col = border, cex = cex)
    }
    if (!missing(main)) 
      title(main = main)
  }
}

#' @rdname coo.plot
#' @export
coo.plot.ldk <- function(coo, cex = 1, poly = FALSE, ...) {
  coo.plot.default(coo, cex = cex, poly = poly, ...)
}

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
#' coo.draw(b2, border='red') # all coo.plot arguments will work for coo.draw
#' @export
coo.draw <- function(coo, ...) {
  coo.plot(coo, plot.new = FALSE, ...)
}

#' Plots (lollipop) differences between two configurations
#' 
#' Draws 'lollipops' between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param pch a pch for the points
#' @param cex a cex for the points
#' @param main character a title for the plot
#' @param ... optional parameters to fed \link{points} and \link{segments}.
#' @seealso \link{coo.arrows}
#' @keywords Graphics
#' @examples
#' data(olea)
#' coo.lolli(coo.sample(olea[3], 50), coo.sample(olea[6], 50))
#' @export
coo.lolli <- function(coo1, coo2, pch = 20, cex = 0.5, main = NA, 
                      ...) {
  coo.plot(rbind(coo1, coo2), plot = FALSE)
  coo1 <- coo.check(coo1)
  coo2 <- coo.check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")
  }
  s <- seq(nrow(coo1) - 1)
  segments(coo1[s, 1], coo1[s, 2], coo2[s, 1], coo2[s, 2], 
           ...)
  points(coo2[, 1], coo2[, 2], pch = pch, cex = cex, ...)
  if (!missing(main)) 
    title(main = main)
}

#' Plots (lollipop) differences between two configurations
#' 
#' Draws 'arrows' between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param length a length for the arrows.
#' @param angle an angle for the arrows
#' @param main character a title for the plot
#' @param ... optional parameters to fed \link{arrows}.
#' @seealso \link{coo.arrows}
#' @keywords Graphics
#' @examples
#' data(olea)
#' coo.arrows(coo.sample(olea[3], 50), coo.sample(olea[6], 50))
#' @export
coo.arrows <- function(coo1, coo2, length = 0.1, angle = 20, 
                       main = NA, ...) {
  coo.plot(rbind(coo1, coo2), plot = FALSE, main = main)
  coo1 <- coo.check(coo1)
  coo2 <- coo.check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")
  }
  s <- seq(nrow(coo1) - 1)
  arrows(coo1[s, 1], coo1[s, 2], coo2[s, 1], coo2[s, 2], length = length, 
         angle = angle, ...)
  if (!missing(main)) 
    title(main = main)
}

#' 'Templates' shapes
#' 
#' \code{coo.template} returns shape centered on the origin and inscribed in a \code{size}-side square
#' 
#' See \link{coo.list.panel} for an illustration of this function. The morphospaces
#' functions also take profit of this function. May be useful to develop other graphical functions.
#' 
#' @usage coo.template(coo, size)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param size \code{numeric}. Indicates the length of the side 'inscribing'
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
coo.template <- function(coo, size){UseMethod("coo.template")}
#' @export
coo.template.default <- function(coo, size = 1) {
  # only for matrices
  coo <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift <- expected - observed
  coo <- coo.trans(coo, shift[1], shift[2])
  # if (keep.pos) {coo2 <- coo.trans(coo2, coo.centpos(coo)[1],
  # coo.centpos(coo)[2])}
  return(coo)
}
#' @export
coo.template.Coo <- function(coo, size=1){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.template, size=size)
  return(Coo)
}



#' Plots sets of shapes.
#' 
#' \code{coo.list.panel} plots a list of shapes if passed with a list of
#' coordinates. Outlines are 'templated' (see \link{coo.template} and will be drawn
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
coo.list.panel <- function(coo.list, dim, byrow = TRUE, fromtop = TRUE, 
                           mar = rep(0, 4), cols, borders, reorder = NULL, poly = TRUE, 
                           points = FALSE, points.pch = 3, points.cex = 0.2, points.col = "#333333") {
  coo.list <- lapply(coo.list, coo.check)
  if (!is.null(reorder)) {
    coo.list <- coo.list[order(reorder)]
  }
  # if dim is missing, we define a square
  n <- length(coo.list)
  if (missing(dim)) {
    nc <- ceiling(sqrt(n))
    nr <- ceiling(n/nc)
    dim <- c(nr, nc)
  }
  k <- dim[1] * dim[2]
  if (k < n) 
    stop(" * dim[1]*dim[2] must be >= the length of coo.list")
  pos <- matrix(1:k, dim[1], dim[2], byrow = byrow)
  if (fromtop & dim[1] > 1) {
    pos <- pos[dim[1]:1, ]
  }
  # we prepare the panel
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar = mar, oma = rep(0.2, 4))
  plot(NA, asp = 1, xlim = c(0, dim[2]), ylim = c(0, dim[1]), 
       xaxs = "i", yaxs = "i", frame = FALSE, ann = FALSE, axes = FALSE)
  # we template and plot shapes
  coo.tp <- lapply(coo.list, coo.template, size = 0.95)
  if (missing(cols)) {
    cols <- rep("grey95", n)
  }
  if (missing(borders)) {
    borders <- rep("grey20", n)
  }
  
  if (!is.null(reorder)) {
    cols <- cols[order(reorder)]
    borders <- borders[order(reorder)]
  }
  
  res <- data.frame(pos.x = numeric(), pos.y = numeric())
  if (poly) {
    for (i in 1:n) {
      trans <- which(pos == i, arr.ind = TRUE) - 0.5
      res[i, ] <- c(trans[2], trans[1])
      polygon(coo.tp[[i]][, 1] + trans[2], coo.tp[[i]][, 
                                                       2] + trans[1], col = cols[i], border = borders[i])
    }
  } else {
    for (i in 1:n) {
      trans <- which(pos == i, arr.ind = TRUE) - 0.5
      res[i, ] <- c(trans[2], trans[1])
      lines(coo.tp[[i]][, 1] + trans[2], coo.tp[[i]][, 
                                                     2] + trans[1], col = borders[i])
      if (points) {
        # if (!missing(points.col)) { col <- rep(points.col,
        # length(coo.list)) }
        points(coo.tp[[i]][, 1] + trans[2], coo.tp[[i]][, 
                                                        2] + trans[1], col = points.col, pch = points.pch, 
               cex = points.cex)
      }
    }
  }
  invisible(res)
}

# ldk plotters
# ---------------------------------------------------------

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
ldk.labels <- function(ldk, d = 0.05, cex = 2/3, ...) {
  op <- par(xpd = NA)
  on.exit(par(op))
  ldk <- coo.check(ldk)
  centpos <- coo.centpos(ldk)
  dm <- median(coo.centdist(ldk))
  for (i in 1:nrow(ldk)) {
    dxy <- ed(centpos, ldk[i, ])
    labxy <- edi(centpos, ldk[i, ], (dxy + dm * d)/dxy)
    text(labxy[1], labxy[2], labels = i, cex = cex, ...)
  }
}

#' Draws links between landmarks
#' 
#' Cosmetics only but useful to visualize shape variation.
#' 
#' @param ldk a matrix of (x; y) coordinates
#' @param links a matrix of links. On the first column the starting-id,
#' on the second column the ending-id (id= the number of the coordinate)
#' @param ... additional parameters to fed \link{segments}
#' @keywords Graphics
#' @export
# todo
ldk.links <- function(ldk, links, ...) {
  ldk <- ldk.check(ldk)
  links <- coo.check(links)
  for (i in 1:nrow(links)) {
    segments(ldk[links[i, 1], 1], ldk[links[i, 1], 2], ldk[links[i, 
                                                                 2], 1], ldk[links[i, 2], 2], ...)
  }
}

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
ldk.confell <- function(ldk, conf = 0.5, col = "grey40", ell.lty = 1, 
                        ax = TRUE, ax.lty = 2) {
  ldk <- ldk.check(ldk)
  for (i in 1:dim(ldk)[1]) {
    if (all(apply(ldk[i, , ], 1, var) != 0)) {
      xy.i <- t(ldk[i, , ])
      ell.i <- conf.ell(xy.i[, 1], xy.i[, 2], conf = conf, 
                        nb.pts = 360)
      lines(ell.i$ell, col = col, lty = ell.lty, lwd = 1)
      if (ax) {
        segments(ell.i$seg[1, 1], ell.i$seg[1, 2], ell.i$seg[2, 
                                                             1], ell.i$seg[2, 2], lty = ax.lty, col = col, 
                 lwd = 1)
        segments(ell.i$seg[3, 1], ell.i$seg[3, 2], ell.i$seg[4, 
                                                             1], ell.i$seg[4, 2], lty = ax.lty, col = col, 
                 lwd = 1)
      }
    }
  }
}

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
ldk.contour <- function(ldk, nlevels = 5, grid.nb = 50, col = "grey60") {
  ldk <- ldk.check(ldk)
  for (i in 1:dim(ldk)[1]) {
    kx <- ldk[i, 1, ]
    ky <- ldk[i, 2, ]
    if (all(sd(kx) > 0, sd(ky) > 0)) {
      k <- kde2d(kx, ky, n = grid.nb)
      contour(k$x, k$y, k$z, nlevels = nlevels, add = TRUE, 
              drawlabels = FALSE, col = col)
    }
  }
}

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
ldk.chull <- function(ldk, col = "grey40", lty = 1) {
  ldk <- ldk.check(ldk)
  nl <- dim(ldk)[1]
  for (i in 1:nl) {
    ind.i <- chull(ldk[i, 1, ], ldk[i, 2, ])
    coo.draw(coo.close(t(ldk[i, , ind.i])), border = col, 
             col = NA, lty = lty, points = FALSE, first.point = FALSE, 
             centroid = FALSE)
  }
}

#' Plots deviation
#' 
#' Calculates and plots series with associated error bars. This function is used 
#' internally by methods based on deviations for one one many outlines.
#' Yet, it provides a quick way to create plots of series, possibly with deviations, from scratch.
#' 
#' @usage dev.plot(mat, dev, cols, x=1:ncol(mat),
#' lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
#' plot=FALSE, main='Deviation plot', xlab='', ylab='Deviations')
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
dev.plot <- function(mat, dev, cols, x = 1:ncol(mat), lines = TRUE, 
                     poly = TRUE, segments = FALSE, bw = 0.1, plot = FALSE, main = "Deviation plot", 
                     xlab = "", ylab = "Deviations") {
  # we prepare and check a bit
  r <- nrow(mat)
  if (!missing(dev)) {
    if (any(dim(mat) != dim(dev))) {
      stop("mat and dev must be of the same dimension")
    }
  }
  if (missing(cols)) {
    cols <- rep("#000000", r)
  }
  if (length(cols) != r) {
    cols <- rep("#000000", r)
  }
  # we call a new plot if required
  if (plot) {
    if (missing(dev)) {
      ylim <- range(mat)
    } else {
      ylim <- c(min(mat + dev), max(mat + dev))
    }
    plot(NA, xlim = range(x), ylim = ylim, main = main, xlab = xlab, 
         ylab = ylab, las = 1, xaxs = "i", yaxs = "i")
    axis(1, at = 1:ncol(mat))
  }
  # if a deviation matrix is provided
  if (!missing(dev)) {
    for (i in 1:r) {
      # if required, we draw the background polygons
      if (poly) {
        polygon(x = c(x, rev(x)), y = c(mat[i, ] - dev[i, 
                                                       ], rev(c(mat[i, ] + dev[i, ]))), col = paste0(cols[i], 
                                                                                                     "55"), border = NA)
      }
      # if required we draw the dev segments
      if (segments) {
        segments(x, mat[i, ] - dev[i, ], x, mat[i, ] + 
                   dev[i, ], col = cols[i], lwd = 0.5)
        segments(x - bw, mat[i, ] - dev[i, ], x + bw, 
                 mat[i, ] - dev[i, ], col = cols[i], lwd = 0.5)
        segments(x - bw, mat[i, ] + dev[i, ], x + bw, 
                 mat[i, ] + dev[i, ], col = cols[i], lwd = 0.5)
      }
    }
  }
  # if a dev matrix is not provided, we simply draw lines
  if (lines) {
    for (i in 1:nrow(mat)) {
      if (lines) {
        lines(x, mat[i, ], col = cols[i], type = "o", 
              cex = 0.25, pch = 20)
      }
    }
  }
}

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
#' cols  <- paste0(col.summer(20)[d.cut], 'CC')
#' 
#' # we draw the results
#' coo.plot(guinness, main='Guiness fitted with 6 harm.', points=FALSE)
#' par(xpd=NA)
#' dev.segments(out.6, cols=cols, lwd=4)
#' coo.draw(out.6, lty=2, points=FALSE, col=NA)
#' par(xpd=FALSE)
#' 
#' @export
dev.segments <- function(coo, cols, lwd = 1) {
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i + 1, 1], coo[i + 
                                                        1, 2], col = cols[i], lwd = lwd)
  }
}

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
conf.ell <- function(x, y, conf = 0.95, nb.pts = 60) {
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  centroid <- apply(cbind(x, y), 2, mean)
  theta.i <- seq(0, 2 * pi, length = nb.pts + 1)[-c(nb.pts + 
                                                      1)]
  z <- cbind(cos(theta.i), sin(theta.i))
  rad <- qnorm((1 - conf)/2, mean = 0, sd = 1, lower.tail = FALSE)
  vcvxy <- var(cbind(x, y))
  r <- cor(x, y)
  M1 <- matrix(c(1, 1, -1, 1), nrow = 2, ncol = 2)
  M2 <- matrix(c(var(x), var(y)), nrow = 2, ncol = 2)
  M3 <- matrix(c(1 + r, 1 - r), nrow = 2, ncol = 2, byrow = TRUE)
  ellpar <- M1 * sqrt(M2 * M3/2)
  ell <- t(centroid + rad * ellpar %*% t(z))
  colnames(ell) <- c("x", "y")
  # stupid approximation
  ell.al <- coo.align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]), 
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  return(list(ell = ell, seg = seg))
}

##### Graphics misc

#' @export
.grid.sample <- function(..., nside = 10, over = 1) {
  wdw <- apply(rbind(...), 2, range)
  wdw <- coo.scale(wdw, scale = 1/over)
  by <- min(apply(wdw, 2, diff))/nside
  xr <- seq(wdw[1, 1], wdw[2, 1], by = by)
  yr <- seq(wdw[1, 2], wdw[2, 2], by = by)
  grid <- expand.grid(xr, yr)
  return(as.matrix(grid))
}

#' @export
# returns the size of the graphical window
.wdw <- function() {
  wdw <- par("usr")
  x <- wdw[2] - wdw[1]
  y <- wdw[4] - wdw[3]
  return(c(x, y))
}


#' Plots a cross-correlation table
#' 
#' Or any contingency table. A simple graphic representation based on variable
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
#' @seealso \link{LDA}, \link{plot.LDA}
#' @keywords Multivariate
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
#' plotCV(tab)
#' plotCV(tab, arrows) # if you prefer arrows
#' plotCV(tab, lwd=FALSE, lwd0=1, palette=col.india) # if you like india but not lwds
#' plotCV(tab, col=FALSE, col0='pink') # only lwd
#' plotCV(tab, col=FALSE, lwd0=10, cex.names=2) # if you're getting old
#' plotCV(tab, col=FALSE, lwd=FALSE) # pretty but useless
#' plotCV(tab, col.breaks=2) # if you think it's either good or bad
#' plotCV(tab, pch=NA) # if you do not like dots
#' plotCV(tab, gap.dots=0) # if you want to 'fill the gap'
#' plotCV(tab, gap.dots=1) # or not
#' 
#' #trilo examples
#' data(trilo)
#' trilo.f <- eFourier(trilo, 8)
#' trilo.l <- LDA(trilo.f, 'onto')
#' trilo.l$CV.tab
#' plotCV(trilo.l$CV.tab) # but see below, 'works' directly on the LDA
#' plotCV(trilo.l)
#' 
#' # olea example
#' data(olea)
#' op <- orthoPolynomials(olea, 5)
#' opl <- LDA(op, 'cep')
#' plotCV(opl)
#' @rdname plotCV
#' @export
plotCV <- function(x, ...) {
  UseMethod("plotCV")
}
#' @rdname plotCV
#' @export
plotCV.LDA <- function(x, ...) {
  plotCV(x$CV.tab, ...)
}
#' @rdname plotCV
#' @export
plotCV.default <- function(x, links.FUN = arrows, col = TRUE, 
                           col0 = "black", col.breaks = 5, palette = col.gallus, lwd = TRUE, 
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
    text(1, 1/3, labels = "True\nGroups", cex = cex.names, 
         font = 2)
    text(1 + gap.mid, 1/3, labels = "Classified\nGroups", 
         cex = cex.names, font = 2)
  }
  # we restore the graphics parameters
  par(op)
}

# Illustration / teaching ---------------------------------

#' Momocs' 'oscilloscope' for Fourier-based approaches
#' 
#' Shape analysis deals with curve fitting, whether \eqn{x(t)} and \eqn{y(t)}
#' positions along the curvilinear abscissa and/or radius/tangent angle variation.
#' These functions are mainly intended for (self-)teaching of Fourier-based methods.
#' @param coo A list or a matrix of coordinates.
#' @param method character among \code{c('efourier', 'rfourier', 'tfourier', 'all')}. 
#' \code{'all'} by default
#' @param nb.pts \code{integer}. The number or reference points, sampled
#' equidistantly along the curvilinear abscissa and added on the oscillo
#' curves.
#' @keywords Graphics
#' @examples 
#' data(shapes)
#' coo.oscillo(shapes[4])
#' coo.oscillo(shapes[4], 'efourier')
#' coo.oscillo(shapes[4], 'rfourier')
#' coo.oscillo(shapes[4], 'tfourier')
#' #tfourier is prone to high-frequency noise but smoothing can help
#' coo.oscillo(coo.smooth(shapes[4], 10), 'tfourier') 
#' @export
coo.oscillo <- function(coo, method = c("efourier", "rfourier", 
                                        "tfourier", "all")[4], nb.pts = 24) {
  # we preapre a couple of things for coming graphics
  labels <- 1:nb.pts
  sampled <- round(seq(1, nrow(coo), len = nb.pts + 1)[-(nb.pts + 
                                                           1)])
  coo.lite <- coo[sampled, ]  # equivalent to coo.sample
  # we define a layout
  if (method == "all") {
    layout(matrix(1:4, ncol = 2, byrow = TRUE))
  } else {
    layout(matrix(1:2, ncol = 2, byrow = TRUE))
  }
  
  # the original shape
  coo.plot(coo, first.point = FALSE)
  text(coo.lite, labels = labels, cex = 0.7, font = 2)
  
  if (any(method == c("all", "efourier"))) {
    # efourier
    dxy <- coo.dxy(coo)
    plot(NA, xlim = c(1, nrow(coo)), ylim = c(range(unlist(dxy))), 
         main = "Elliptical analysis", xlab = "Points along the outline", 
         ylab = "Deviation from the first point (pixels)")
    lines(dxy$dx, col = "red")
    text(sampled, dxy$dx[sampled], labels = labels, col = "red", 
         cex = 0.7, font = 2)
    lines(dxy$dy, col = "blue")
    text(sampled, dxy$dy[sampled], labels = labels, col = "blue", 
         cex = 0.7, font = 2)
    legend("bottomright", legend = c(expression(x[i] - x[0]), 
                                     expression(y[i] - y[0])), col = c("red", "blue"), 
           bg = "#FFFFFFCC", cex = 0.7, lty = 1, lwd = 1, inset = 0.05, 
           bty = "n")
  }
  
  if (any(method == c("all", "rfourier"))) {
    # rfourier
    dr <- coo.centdist(coo)
    plot(NA, xlim = c(1, nrow(coo)), ylim = range(dr), main = "Radius variation", 
         xlab = "Points along the outline", ylab = "Radius length (pixels)")
    lines(dr, col = "black")
    text(sampled, dr[sampled], labels = labels, col = "black", 
         cex = 0.7, font = 2)
  }
  # tfourier
  if (any(method == c("all", "tfourier"))) {
    dt <- coo.tangle(coo)
    plot(NA, xlim = c(1, nrow(coo)), ylim = range(dt), main = "Tangent angle", 
         xlab = "Points along the outline", ylab = "Tangent angle (radians)")
    # lines((1:nrow(coo))[sampled], dt[sampled], lty=2,
    # col='black')
    lines(dt, col = "black")
    text(sampled, dt[sampled], labels = labels, col = "black", 
         cex = 0.7, font = 2)
  }
  # we restore the layout
  layout(matrix(1))
}

#' Ptolemaic ellipses and illustration of eFourier
#'
#' Calculate and display Ptolemaic ellipses which illustrates
#' intuitively the principle behing elliptical Fourier analysis.
#'
#' @param coo a matrix of (x; y) coordinates
#' @param t A \code{vector} af angles (in radians) on which to display ellipses
#' @param nb.h \code{integer}. The number of harmonics to display
#' @param nb.pts \code{integer}. The number of points to use to display shapes
#' @param zoom numeric a zoom factor for \link{coo.plot}
#' @param palette a color palette
#' @param legend \code{logical}. Whether to plot the legend box
#' @param ... additional parameters to feed \link{coo.plot}
#' @keywords Graphics
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
#' data(shapes)
#' cat <- shapes[4]
#' Ptolemy(cat, main="An EFT cat")
#' @export
Ptolemy <- function(coo, t = seq(0, 2 * pi, length = 7)[-1],
                    nb.h = 3, nb.pts = 360, palette = col.heat,
                    zoom=5/4, legend = TRUE, ...) {
  coo <- coo.center(coo)
  # we prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  coo.plot(coo, zoom=zoom, ...)
  par(xpd = NA)
  cols <- palette(nb.h+1)[-1]
  # k <- floor(length(coo$x)/4)
  
  # now we calculate for every harmonic
  coo.ef <- efourier(coo, nb.h)
  coo.efi <- efourier.i(coo.ef, nb.h, nb.pts)
  vect <- matrix(nrow = nb.h, ncol = 2)
  vect <- rbind(c(0, 0), vect)
  for (i in seq(along = t)) {
    for (j in 1:nb.h) {
      vect[j + 1, 1] <- coo.ef$an[j] * cos(j * t[i]) + 
        coo.ef$bn[j] * sin(j * t[i])
      vect[j + 1, 2] <- coo.ef$cn[j] * cos(j * t[i]) + 
        coo.ef$dn[j] * sin(j * t[i])
    }
    vs <- apply(vect, 2, cumsum)
    for (j in 1:nb.h) {
      lh <- efourier.shape(coo.ef$an[1:j], coo.ef$bn[1:j],
                           coo.ef$cn[1:j], coo.ef$dn[1:j],
                           nb.h = j, nb.pts = nb.pts,
                           plot = FALSE)
      ellh <- efourier.shape(coo.ef$an[j], coo.ef$bn[j],
                             coo.ef$cn[j], coo.ef$dn[j],
                             nb.h = 1, nb.pts = nb.pts,
                             plot = FALSE)
      # and we plot all ellipses, arrows, etc.
      lines(lh, col=cols[j], lwd=0.2)
      lines(ellh[, 1] + vs[j, 1], ellh[, 2] + vs[j, 2], col = cols[j], lwd=0.5)
      #final points
      #points(vs[j + 1, 1], vs[j + 1, 2], col = cols[j], cex = 0.8)
      arrows(vs[j, 1], vs[j, 2], vs[j + 1, 1], vs[j + 1, 2], 
             col = cols[j], angle = 10, length = 0.15,lwd = 1.2)
    }
  }
  #centroid
  #points(0, 0, pch = 20, col = cols[1])
  if (legend) {
    legend("topright", legend = as.character(1:nb.h), bty = "n",
           col = cols, lwd = 2, seg.len=1, title = "Harmonics", cex=3/4)}}


##### end basic plotters 
