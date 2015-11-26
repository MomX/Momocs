# coo plotters #######
#' Plots a single shape
#'
#' A simple wrapper around \link{plot} for plotting shapes. Widely used in Momocs
#' in other graphical functions, in methods, etc.
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param xlim If \code{coo_plot} is called and \code{coo} is missing, then a
#' vector of length 2 specifying the \code{ylim} of the ploting area.
#' @param ylim If \code{coo_plot} is called and \code{coo} is missing, then a
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
#' @param ... further arguments for use in coo_plot methods. See examples.
#' @return No returned value.
#' @family plotting functions
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo_plot(b)
#' coo_plot(bot[2], plot.new=FALSE) # equivalent to coo_draw(bot[2])
#' coo_plot(b, zoom=2)
#' coo_plot(b, border='blue')
#' coo_plot(b, first.point=FALSE, centroid=FALSE)
#' coo_plot(b, points=TRUE, pch=20)
#' coo_plot(b, xy.axis=FALSE, lwd=2, col='#F2F2F2')
#' @aliases coo_plot
#' @rdname coo_plot
#' @export
coo_plot <- function(coo, ...) {
  UseMethod("coo_plot")
}

#' @rdname coo_plot
#' @export
coo_plot.default <- function(coo, xlim, ylim, border = "#333333",
                             col = NA, lwd = 1, lty = 1, points = FALSE, first.point = TRUE,
                             centroid = TRUE, xy.axis = TRUE, pch = 1, cex = 0.5, main = NA,
                             poly = TRUE, plot.new = TRUE, plot = TRUE, zoom = 1, ...) {
  # todo zoom
  coo <- coo_check(coo)
  # if 'plot.new=TRUE' we have initiated the graphical window
  if (plot.new) {
    # we setup coo_plot graphical parameters
    op <- par(mar = c(3, 3, 2, 1))
    on.exit(par(op))
    # if zoom if provided, we define wlim and ylim manually
    if (!missing(zoom)) {
      half.side <- max(apply(coo, 2, function(x) diff(range(x))))/2
      add.xy <- half.side * zoom
      orig.xy <- coo_centpos(coo)
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
        angle <- atan2(coo[2, 2] - coo[1, 2], coo[2, 1] - coo[1, 1]) * (180 / pi) - 90
        text(coo[1, 1], coo[1, 2], labels = "^", cex=1, srt=angle)
    }
    if (centroid) {
      cent <- coo_centpos(coo)
      points(cent[1], cent[2], pch = 3, col = border, cex = cex)
    }
    if (!missing(main))
      title(main = main)
  }
}

#' Adds a shape to the current plot
#'
#' \code{coo_draw} is simply a \link{coo_plot} with \code{plot.new=FALSE}, ie
#' that adds a shape on the active plot.
#' @param coo a \code{list} or a \code{matrix} of coordinates.
#' @param ... optional parameters for \link{coo_plot}
#' @examples
#' data(bot)
#' b1 <- bot[4]
#' b2 <- bot[5]
#' coo_plot(b1)
#' coo_draw(b2, border='red') # all coo_plot arguments will work for coo_draw
#' @export
coo_draw <- function(coo, ...) {
  coo_plot(coo, plot.new = FALSE, ...)
}

#' Plots (lollipop) differences between two configurations
#'
#' Draws 'lollipops' between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param pch a pch for the points (default to NA)
#' @param cex a cex for the points
#' @param ... optional parameters to fed \link{points} and \link{segments}.
#' @family thin plate splines functions
#' @examples
#' data(olea)
#' coo_lolli(coo_sample(olea[3], 50), coo_sample(olea[6], 50))
#' title("A nice title !")
#' @export
coo_lolli <- function(coo1, coo2,
                      pch = NA, cex = 0.5, ...) {
  coo_plot(rbind(coo1, coo2), plot = FALSE)
  coo1 <- coo_check(coo1)
  coo2 <- coo_check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")
  }
  s <- seq(nrow(coo1) - 1)
  segments(coo1[s, 1], coo1[s, 2],
           coo2[s, 1], coo2[s, 2],  ...)
  points(coo2[, 1], coo2[, 2],
         pch = pch, cex = cex, ...)
}

#' Plots (lollipop) differences between two configurations
#'
#' Draws 'arrows' between two configurations.
#' @param coo1 A \code{list} or a \code{matrix} of coordinates.
#' @param coo2 A \code{list} or a \code{matrix} of coordinates.
#' @param length a length for the arrows.
#' @param angle an angle for the arrows
#' @param ... optional parameters to fed \link{arrows}.
#' @family plotting functions
#' @examples
#' data(olea)
#' coo_arrows(coo_sample(olea[3], 50), coo_sample(olea[6], 50))
#' title("Hi there !")
#' @export
coo_arrows <- function(coo1, coo2,
                       length = coo_centsize(coo1)/15, angle = 20, ...) {
  coo_plot(rbind(coo1, coo2), plot = FALSE)
  coo1 <- coo_check(coo1)
  coo2 <- coo_check(coo2)
  if (nrow(coo1) != nrow(coo2)) {
    stop(" * coo1 and coo2 have different number of coordinates.")
  }
  s <- seq(nrow(coo1) - 1)
  arrows(coo1[s, 1], coo1[s, 2], coo2[s, 1], coo2[s, 2], length = length,
         angle = angle, ...)
}


#' Plots differences as (colored) segments aka a ruban
#'
#' Useful to display differences between shapes
#' @param coo a shape, typically a mean shape
#' @param dev numeric a vector of distances or anythinh relevant
#' @param  palette the color palette to use or any palette
#' @param normalize logical whether to normalize (TRUE by default) distances
#' @param ... other paremeters to fed segments, eg lwd (see examples)
#' @return nothing
#' @examples
#' data(bot)
#' ms <- mshapes(efourier(bot , 10), "type")
#' b <- ms$shp$beer
#' w <- ms$shp$whisky
#' # we obtain the mean shape, then euclidean distances between points
#' m <- mshapes(list(b, w))
#' d <- edm(b, w)
#' # First plot
#' coo_plot(m, plot=FALSE)
#' coo_draw(b)
#' coo_draw(w)
#' coo_ruban(m, d, lwd=5)
#'
#' #Another example
#' coo_plot(m, plot=FALSE)
#' coo_ruban(m, d, palette=col_summer2, lwd=5)
#'
#' #If you want linewidth rather than color
#' coo_plot(m, plot=FALSE)
#' coo_ruban(m, d, palette=col_black, lwd=.normalize(d)*10)
#' @family plotting functions
#' @export
coo_ruban <- function(coo, dev,
                     palette=col_heat, normalize=TRUE, ...){
  if (nrow(coo) != length(dev))
    stop("'coo' and 'dev' must have the same number of rows")
  if(normalize) dev <- .normalize(dev)
  nr <- nrow(coo)
  xy <- cbind(coo, coo_slide(coo, nr))
  cols <- palette(nr)[cut(dev, breaks = nr)]
  segments(xy[, 1], xy[, 2], xy[, 3], xy[, 4], col=cols, ...)
}




#' Plots sets of shapes.
#'
#' \code{coo_listpanel} plots a list of shapes if passed with a list of
#' coordinates. Outlines are 'templated' (see \link{coo_template} and will be drawn
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
#' @examples
#' data(bot)
#' coo_listpanel(bot$coo) # equivalent to panel(bot)
#' x <- coo_listpanel(bot$coo)
#' x # positions of shapes returned invisibly
#' # axis(1) ; axis(2) # that's a single graphical window
#' data(bot)
#' coo <- bot$coo
#' ord <- sapply(coo, coo_eccentricityeigen)
#' pos <- coo_listpanel(coo, reorder=ord)
#' text(pos, labels=signif(ord[order(ord)], 3))
#' @family plotting functions
#' @export
coo_listpanel <- function(coo.list, dim, byrow = TRUE, fromtop = TRUE,
                           mar = rep(0, 4), cols, borders, reorder = NULL, poly = TRUE,
                           points = FALSE, points.pch = 3, points.cex = 0.2, points.col = "#333333") {
  coo.list <- lapply(coo.list, coo_check)
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
  coo_tp <- lapply(coo.list, coo_template, size = 0.95)
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
      polygon(coo_tp[[i]][, 1] + trans[2], coo_tp[[i]][,
                                                       2] + trans[1], col = cols[i], border = borders[i])
    }
  } else {
    for (i in 1:n) {
      trans <- which(pos == i, arr.ind = TRUE) - 0.5
      res[i, ] <- c(trans[2], trans[1])
      lines(coo_tp[[i]][, 1] + trans[2],
            coo_tp[[i]][, 2] + trans[1],
            col = borders[i])
      if (points) {
        # if (!missing(points.col)) { col <- rep(points.col,
        # length(coo.list)) }
        points(coo_tp[[i]][, 1] + trans[2],
               coo_tp[[i]][, 2] + trans[1],
               col = points.col, pch = points.pch,
               cex = points.cex)
      }
    }
  }
  invisible(res)
}

# ldk plotters #######

# we already have the rdfile above
#' @rdname coo_plot
#' @export
ldk_plot <- function(coo, ...){
  coo_plot(coo, poly=FALSE, first.point = FALSE, ...)
}

#' Add landmarks labels
#'
#' @param ldk a matrix of (x; y) coordinates: where to plot the labels
#' @param d how far from the coordinates, on a (centroid-landmark) segment
#' @param cex the cex for the label
#' @param ... additional parameters to fed \link{text}
#' @examples
#' data(wings)
#' coo_plot(wings[1])
#' ldk_labels(wings[1])
#' # closer and smaller
#' coo_plot(wings[1])
#' ldk_labels(wings[1], d=0.05, cex=0.5)
#' @export
ldk_labels <- function(ldk, d = 0.05, cex = 2/3, ...) {
  op <- par(xpd = NA)
  on.exit(par(op))
  ldk <- coo_check(ldk)
  centpos <- coo_centpos(ldk)
  dm <- median(coo_centdist(ldk))
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
#' @family ldk functions
#' @export
ldk_links <- function(ldk, links, ...) {
  ldk <- ldk_check(ldk)
  links <- coo_check(links)
  for (i in 1:nrow(links)) {
    segments(ldk[links[i, 1], 1], ldk[links[i, 1], 2],
             ldk[links[i, 2], 1], ldk[links[i, 2], 2], ...)
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
#' @examples
#' data(wings)
#' coo_plot(mshapes(wings))
#' ldk_confell(wings$coo)
#' @family ldk functions
#' @export
ldk_confell <- function(ldk, conf = 0.5, col = "grey40", ell.lty = 1,
                        ax = TRUE, ax.lty = 2) {
  ldk <- ldk_check(ldk)
  for (i in 1:dim(ldk)[1]) {
    if (all(apply(ldk[i, , ], 1, var) != 0)) {
      xy.i <- t(ldk[i, , ])
      ell.i <- conf_ell(xy.i[, 1], xy.i[, 2], conf = conf,
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
#' @seealso \link{kde2d}, \link{ldk_confell}, \link{ldk_chull}
#' @examples
#' data(wings)
#' coo_plot(mshapes(wings))
#' ldk_contour(wings$coo)
#' @family ldk functions
#' @export
ldk_contour <- function(ldk, nlevels = 5, grid.nb = 50, col = "grey60") {
  ldk <- ldk_check(ldk)
  for (i in 1:dim(ldk)[1]) {
    kx <- ldk[i, 1, ]
    ky <- ldk[i, 2, ]
    if (all(sd(kx) > 0, sd(ky) > 0)) {
      k <- MASS::kde2d(kx, ky, n = grid.nb)
      contour(k$x, k$y, k$z, nlevels = nlevels, add = TRUE,
              drawlabels = FALSE, col = col)
    }
  }
}

#' Draws convex hulls around landmark positions
#'
#' A wrapper that uses \link{coo_chull}
#' @param ldk an array (or a list) of landmarks
#' @param col a color for drawing the convex hull
#' @param lty an lty for drawing the convex hulls
#' @seealso \link{coo_chull}, \link{chull}, \link{ldk_confell}, \link{ldk_contour}
#' @examples
#' data(wings)
#' coo_plot(mshapes(wings))
#' ldk_chull(wings$coo)
#' @family ldk functions
#' @export
ldk_chull <- function(ldk, col = "grey40", lty = 1) {
  ldk <- ldk_check(ldk)
  nl <- dim(ldk)[1]
  for (i in 1:nl) {
    ind.i <- chull(ldk[i, 1, ], ldk[i, 2, ])
    coo_draw(coo_close(t(ldk[i, , ind.i])), border = col,
             col = NA, lty = lty, points = FALSE, first.point = FALSE,
             centroid = FALSE)
  }
}

# random stuff ####### to be cleaned someday

#' Draws colored segments from a matrix of coordinates.
#'
#' Given a matrix of (x; y) coordinates, draws segments between every points
#' defined by the row of the matrix and uses a color to display an information.
#'
#' @usage plot_devsegments(coo, cols, lwd = 1)
#' @param coo A matrix of coordinates.
#' @param cols A vector of color of \code{length = nrow(coo)}.
#' @param lwd The \code{lwd} to use for drawing segments.
#' @examples
#'
#' # we load some data
#' data(bot)
#' guinness <- coo_sample(bot[9], 100)
#'
#' # we calculate the diff between 48 harm and one with 6 harm.
#' out.6    <- efourier_i(efourier(guinness, nb.h=6), nb.pts=120)
#'
#' # we calculate deviations, you can also try 'edm'
#' dev <- edm_nearest(out.6, guinness) / coo_centsize(out.6)
#'
#' # we prepare the color scale
#' d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
#' cols  <- paste0(col_summer(20)[d.cut], 'CC')
#'
#' # we draw the results
#' coo_plot(guinness, main='Guiness fitted with 6 harm.', points=FALSE)
#' par(xpd=NA)
#' plot_devsegments(out.6, cols=cols, lwd=4)
#' coo_draw(out.6, lty=2, points=FALSE, col=NA)
#' par(xpd=FALSE)
#' @family ldk functions
#' @export
plot_devsegments <- function(coo, cols, lwd = 1) {
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2],
             coo[i + 1, 1], coo[i + 1, 2],
             col = cols[i], lwd = lwd)
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
#' @return a matrix of (x; y) coordinates to draw the ellipsis
#' @examples
#' x <- rnorm(100, sd=3)
#' y <- rnorm(100)
#' plot(x, y, asp=1)
#' ce095 <- conf_ell(x, y, conf=0.95) # no need for conf arg since it's .95 by default
#' ce090 <- conf_ell(x, y, conf=0.90)
#' ce050 <- conf_ell(x, y, conf=0.50)
#' cols <- col_hot(10)
#' lines(ce050$ell, col=cols[5]) # you can also coo_close(ce050$ell)
#' lines(ce090$ell, col=cols[8])
#' lines(ce095$ell, col=cols[9])
#' segments(ce095$seg[1, 1], ce095$seg[1, 2], ce095$seg[2, 1], ce095$seg[2, 2])
#' segments(ce095$seg[3, 1], ce095$seg[3, 2], ce095$seg[4, 1], ce095$seg[4, 2])
#' @export
conf_ell <- function(x, y, conf = 0.95, nb.pts = 60) {
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
  ell.al <- coo_align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]),
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  return(list(ell = ell, seg = seg))
}

#' Plots confusion matrix of sample sizes within $fac
#'
#' An utility that plots a confusion matrix of sample size (or a barplot)
#' for every object with a $fac. Useful to visually how large are sample sizes,
#' how (un)balanced are designs, etc.
#'
#' @param x any object with a $fac slot (Coo, Coe, PCA, etc.)
#' @param fac1 the name or id of the first factor
#' @param fac2 the name of id of the second factor
#' @param rm0 logical whether to print zeros
#' @return a ggplot2 object
#' @examples
#' data(olea)
#' Ntable(olea, "var")
#' Ntable(olea, "domes", "var")
#' gg <- Ntable(olea, "domes", "var", rm0 = TRUE)
#' gg
#' library(ggplot2)
#' gg + coord_equal()
#' gg + scale_fill_gradient(low="green", high = "red")
#' gg + coord_flip()
#' @family plotting functions
#' @export
Ntable <- function(x, fac1, fac2=fac1, rm0 = FALSE){
  # we check a bit
  if (is.null(x$fac))
    stop(" * Ntable must be called on an object with a $fac slot")
  if (missing(fac1))
    stop(" * 'fac1' must be specified")
  df <- select_(x$fac, fac1, fac2)
  # we return a barplot when a single fac is called (fac1 then)
  if (missing(fac2) | identical(fac1, fac2)) { # | is justified by rm0 after
    gg <- ggplot(df, aes_string(x=fac1)) + geom_bar()
    return(gg)
  }
  # otherwise we prepare a table and a df
  tab <- table(df)
  df <- as.data.frame(tab)
  colnames(df) <- c("fac1", "fac2", "count")
  gg <- ggplot(df, aes(x=fac1, y=fac2, fill=count)) +
    geom_tile()  +
    scale_x_discrete(name=fac1) +
    scale_y_discrete(name=fac2) +
    scale_fill_gradient(low="white") +
    theme_linedraw()
  if (rm0) {
    gg <- gg + geom_text(data=filter(df, count !=0), aes(label=count))
  } else {
    gg <- gg + geom_text(aes(label=count))
  }
  return(gg)}


##### Graphics misc

#' @export
.grid.sample <- function(..., nside = 10, over = 1) {
  wdw <- apply(rbind(...), 2, range)
  wdw <- coo_scale(wdw, scale = 1/over)
  by <- min(apply(wdw, 2, diff))/nside
  xr <- seq(wdw[1, 1], wdw[2, 1], by = by)
  yr <- seq(wdw[1, 2], wdw[2, 2], by = by)
  grid <- expand.grid(xr, yr)
  return(as.matrix(grid))
}

# returns the size of the graphical window
#' @export
.wdw <- function() {
  wdw <- par("usr")
  x <- wdw[2] - wdw[1]
  y <- wdw[4] - wdw[3]
  return(c(x, y))
}

# ggplot2 ######

# # from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   require(grid)
#
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots==1) {
#     print(plots[[1]])
#
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

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
#' @examples
#' data(shapes)
#' coo_oscillo(shapes[4])
#' coo_oscillo(shapes[4], 'efourier')
#' coo_oscillo(shapes[4], 'rfourier')
#' coo_oscillo(shapes[4], 'tfourier')
#' #tfourier is prone to high-frequency noise but smoothing can help
#' coo_oscillo(coo_smooth(shapes[4], 10), 'tfourier')
#' @seealso exemplifying functions
#' @export
coo_oscillo <- function(coo, method = c("efourier", "rfourier",
                                        "tfourier", "all")[4], nb.pts = 24) {
  # we preapre a couple of things for coming graphics
  labels <- 1:nb.pts
  sampled <- round(seq(1, nrow(coo), len = nb.pts + 1)[-(nb.pts +
                                                           1)])
  coo_lite <- coo[sampled, ]  # equivalent to coo_sample
  # we define a layout
  if (method == "all") {
    layout(matrix(1:4, ncol = 2, byrow = TRUE))
  } else {
    layout(matrix(1:2, ncol = 2, byrow = TRUE))
  }

  # the original shape
  coo_plot(coo, first.point = FALSE)
  text(coo_lite, labels = labels, cex = 0.7, font = 2)

  if (any(method == c("all", "efourier"))) {
    # efourier
    dxy <- coo_dxy(coo)
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
    dr <- coo_centdist(coo)
    plot(NA, xlim = c(1, nrow(coo)), ylim = range(dr), main = "Radius variation",
         xlab = "Points along the outline", ylab = "Radius length (pixels)")
    lines(dr, col = "black")
    text(sampled, dr[sampled], labels = labels, col = "black",
         cex = 0.7, font = 2)
  }
  # tfourier
  if (any(method == c("all", "tfourier"))) {
    dt <- coo_tangle(coo)
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

#' Ptolemaic ellipses and illustration of efourier
#'
#' Calculate and display Ptolemaic ellipses which illustrates
#' intuitively the principle behing elliptical Fourier analysis.
#'
#' @param coo a matrix of (x; y) coordinates
#' @param t A \code{vector} af angles (in radians) on which to display ellipses
#' @param nb.h \code{integer}. The number of harmonics to display
#' @param nb.pts \code{integer}. The number of points to use to display shapes
#' @param zoom numeric a zoom factor for \link{coo_plot}
#' @param palette a color palette
#' @param legend \code{logical}. Whether to plot the legend box
#' @param ... additional parameters to feed \link{coo_plot}
#' @references
#' This method has been inspired by the figures found in the followings papers.
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#'  \emph{Computer Graphics and Image Processing} \bold{18}: 236-258.
#' Crampton JS. 1995. Elliptical Fourier shape analysis of fossil bivalves:
#' some practical considerations. \emph{Lethaia} \bold{28}: 179-186.
#' @seealso An intuitive explanation of elliptic Fourier analysis can be found in
#' the \bold{Details} section of the \link{efourier} function.
#' @examples
#' data(shapes)
#' cat <- shapes[4]
#' Ptolemy(cat, main="An EFT cat")
#' @seealso exemplifying functions
#' @export
Ptolemy <- function(coo, t = seq(0, 2 * pi, length = 7)[-1],
                    nb.h = 3, nb.pts = 360, palette = col_heat,
                    zoom=5/4, legend = TRUE, ...) {
  coo <- coo_center(coo)
  # we prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  coo_plot(coo, zoom=zoom, ...)
  par(xpd = NA)
  cols <- palette(nb.h+1)[-1]
  # k <- floor(length(coo$x)/4)

  # now we calculate for every harmonic
  coo_ef <- efourier(coo, nb.h)
  coo_efi <- efourier_i(coo_ef, nb.h, nb.pts)
  vect <- matrix(nrow = nb.h, ncol = 2)
  vect <- rbind(c(0, 0), vect)
  for (i in seq(along = t)) {
    for (j in 1:nb.h) {
      vect[j + 1, 1] <- coo_ef$an[j] * cos(j * t[i]) +
        coo_ef$bn[j] * sin(j * t[i])
      vect[j + 1, 2] <- coo_ef$cn[j] * cos(j * t[i]) +
        coo_ef$dn[j] * sin(j * t[i])
    }
    vs <- apply(vect, 2, cumsum)
    for (j in 1:nb.h) {
      lh <- efourier_shape(coo_ef$an[1:j], coo_ef$bn[1:j],
                           coo_ef$cn[1:j], coo_ef$dn[1:j],
                           nb.h = j, nb.pts = nb.pts,
                           plot = FALSE)
      ellh <- efourier_shape(coo_ef$an[j], coo_ef$bn[j],
                             coo_ef$cn[j], coo_ef$dn[j],
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
