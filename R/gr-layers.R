# layers ------

# create an empty frame

.frame <- function(xy, xlim=NULL, ylim=NULL, center.origin = FALSE, zoom = 1, bg="white") {
  op <- par(bg=bg)
  on.exit(par(op))

  if (!is.null(xlim) | !is.null(xlim)){
    if (is.null(xlim)){
      plot(NA, ylim=ylim, asp=1, axes = FALSE, frame = FALSE)
    }
    if (is.null(ylim)){
      plot(NA, xlim=xlim, asp=1, axes = FALSE, frame = FALSE)
    }
    if (!is.null(xlim) & !is.null(xlim)){
      plot(NA, xlim=xlim, ylim=ylim, asp=1, axes = FALSE, frame = FALSE)
    }
  } else {
    if (center.origin) {
      w <- (1/zoom) * max(abs(xy))
      plot(NA, xlim = c(-w, w), ylim = c(-w, w), asp = 1, axes = FALSE,
           frame = FALSE)
    } else {
      w <- (1/zoom) * apply(abs(xy), 2, max)
      plot(xy, xlim = c(-w[1], w[1]), ylim = c(-w[2], w[2]),
           type = "n", asp = 1, axes = FALSE, frame = FALSE)
    }
  }
}


.chessboard <- function(n=50, col="grey98", w){
  # grabs window parameters if not provided
  if (missing(w)) w <- par("usr")
  # max dimension
  wm <-   max(w[2] - w[1], w[4] - w[3])
  side <- wm/n
  # generates xleft coordinates (1/2 is picked)
  xl <- seq(w[1], w[2], side)[seq(0, n, 2)+1]
  # generates all ybottom coordinates
  yb <- seq(w[3], w[4], side)
  # handles g(ap) through modulus
  g <- c(0, side)[(1:n %% 2)+1]
  # loop and draw rectangles
  for (j in 1:n)
    rect(xl + g[j], yb[j], xl + g[j] + side, yb[j]+side, col=col, border=NA)
}


.axes_corner <- function(bb, w, col="grey20", signif=2, lwd=0.5, cex=0.5){
  # if required we prettify bb coordinates
  if (is.numeric(signif))
    pretty_bb <- sapply(bb, signif, signif)
  else
    pretty_bb <- bb
  # ticks
  segments(bb$x0, bb$y0, bb$x0, bb$y0-w, lwd=lwd, col=col)
  segments(bb$x1, bb$y1, bb$x1, bb$y1+w, lwd=lwd, col=col)
  segments(bb$x0, bb$y0, bb$x0-w, bb$y0, lwd=lwd, col=col)
  segments(bb$x1, bb$y1, bb$x1+w, bb$y1, lwd=lwd, col=col)
  # and their legend
  text(bb$x0,   bb$y0-w, pretty_bb[1], cex=cex, col=col, adj=c(0.5,  5/3))
  text(bb$x1,   bb$y1+w, pretty_bb[2], cex=cex, col=col, adj=c(0.5, -2/3))
  text(bb$x0-w, bb$y0,   pretty_bb[3], cex=cex, col=col, adj=c(5/3,  0.5))
  text(bb$x1+1, bb$y1,   pretty_bb[4], cex=cex, col=col, adj=c(-2/3, 0.5))
}

# grid layer

.grid <- function(nb.grids = 3) {
  m <- max(abs(par("usr")))
  g <- seq(0, m, length = nb.grids)
  g <- c(g[-1] * -1, g[-1])
  abline(h = g, col = "grey80", lty = 3)
  abline(v = g, col = "grey80", lty = 3)
  abline(v = 0, col = "grey80")
  abline(h = 0, col = "grey80")

}

# unit
.unit <- function(nb.grids=3){
  m <- max(abs(par("usr")))
  g <- seq(0, m, length = nb.grids)
  g <- c(g[-1] * -1, g[-1])
  text(min(abs(g)), 0, signif(min(abs(g)), 2), cex=0.5, adj=c(0.5, 0))
}

# rug

.rug <- function(xy, fac, col) {

  xy <- jitter(xy, 5)
  g <- 0.0075
  y0 <- par("usr")[3]
  x0 <- par("usr")[1]
  # gy <- (par('usr')[4] - par('usr')[3])*g gx <-
  # (par('usr')[2] - par('usr')[1])*g #*(.wdw()[2] / .wdw()[1])
  gy <- 0
  gx <- 0
  if (is.null(fac)) {
    rug(xy[, 1], ticksize = g, side = 1, col = "black", lend = 1,
        quiet = TRUE)
    rug(xy[, 2], ticksize = g, side = 2, col = "black", lend = 1,
        quiet = TRUE)
  } else {
    for (i in seq(along = levels(fac))) {
      rug(xy[fac == levels(fac)[i], 1], ticksize = g, pos = y0 +
            (i - 1) * gy, side = 1, col = col_alpha(col[i],
                                                    0), lwd = 1.2, lend = 1, quiet = TRUE)
      rug(xy[fac == levels(fac)[i], 2], ticksize = g, pos = x0 +
            (i - 1) * gx, side = 2, col = col_alpha(col[i],
                                                    0), lwd = 1.2, lend = 1, quiet = TRUE)
    }
  }
}

# confidence ellipses

.ellipses <- function(xy, fac, conf, col) {
  nas <- which(is.na(fac))
  if (length(nas)>0){
    fac <- factor(fac[-nas])
    xy <- xy[-nas,]
    col <- col[nas]
  }
  for (i in seq(along = levels(fac))) {
    pts.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(pts.i)) {
      if (nrow(pts.i) > 2) {
        ell.i <- conf_ell(x = pts.i, conf = conf)$ell
        lines(coo_close(ell.i), col = col[i])
        points(coo_centpos(pts.i)[1], coo_centpos(pts.i)[2],
               pch = 3, cex = 0.3, col = col[i])
      }
    }
  }
}

# confidence ellipses

.ellipsesax <- function(xy, fac, conf, col, lty = 1, lwd = 1) {

  nas <- which(is.na(fac))
  if (length(nas)>0){
    fac <- factor(fac[-nas])
    xy <- xy[-nas,]
  }

  if (length(conf) > 1) {
    conf <- sort(conf, decreasing = FALSE)
    if (missing(lwd) | length(lwd) != length(conf)) {
      lwd <- rev(seq(0, max(lwd), length = (length(conf) + 1))[-1])
      lwd <- lwd^1.8  # just to amplify things
    }
  }
  for (i in seq(along = levels(fac))) {
    pts.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(pts.i)) {
      if (nrow(pts.i) > 2) {
        for (j in seq(along = conf)) {
          seg.i <- conf_ell(x = pts.i, conf = conf[j],
                            nb.pts = 720)$seg
          segments(seg.i[1, 1], seg.i[1, 2],
                   seg.i[2, 1], seg.i[2, 2],
                   lty = lty, col = col[i],
                   lwd = lwd[j])
          segments(seg.i[3, 1], seg.i[3, 2],
                   seg.i[4, 1], seg.i[4, 2],
                   lty = lty, col = col[i],
                   lwd = lwd[j])
        }
      }
    }
  }
}

# convex hulls

.chull <- function(coo, fac, col, lty) {
  nas <- which(is.na(fac))
  if (length(nas)>0){
    fac <- factor(fac[-nas])
    coo <- coo[-nas,]
    col <- col[nas]
    lty <- lty[nas]
  }
  for (i in seq(along = levels(fac))) {
    coo_i <- coo[fac == levels(fac)[i], ]
    if (is.matrix(coo_i)) {
      if (nrow(coo_i) > 1) {
        chull.i <- coo_chull(coo_i)
        lines(coo_close(chull.i), col = col[i], lty = lty)
      }
    }
  }
}

# filled convex hulls

.chullfilled <- function(coo, fac, col) {
  nas <- which(is.na(fac))
  if (length(nas)>0){
    fac <- factor(fac[-nas])
    coo <- coo[-nas,]
    col <- col[nas]
    lty <- lty[nas]
  }
  for (i in seq(along = levels(fac))) {
    coo_i <- coo[fac == levels(fac)[i],, drop=FALSE]
    if (nrow(coo_i) > 1) {
      chull.i <- coo_chull(coo_i)
      polygon(coo_close(chull.i), col = col[i], border=col[i])
    }
  }
}


# add labels

.labelsgroups <- function(xy, fac,
                          col, cex = 0.8, rect = TRUE, abbreviate = FALSE) {
  cent <- matrix(NA, nlevels(fac), 2)
  for (i in seq(along = levels(fac))) {
    cent.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(cent.i)) {
      cent.i <- coo_centpos(xy[fac == levels(fac)[i], ])
    }
    cent[i, ] <- cent.i
  }
  labels <- levels(fac)
  if (abbreviate)
    labels <- abbreviate(labels, minlength = 1)
  p <- strheight(labels[1])
  if (abbreviate)
    labels <- abbreviate(labels, minlength = 1)
  if (rect) {
    w <- strwidth(labels, cex = cex)
    h <- strheight(labels, cex = cex)
    rect(xleft = cent[, 1] - w/2 - p,
         ybottom = cent[, 2] - h/2 + p,
         xright = cent[, 1] + w/2 + p,
         ytop = cent[, 2] + h/2 + p,
         col = col_alpha("#FFFFFF", 0.5), border = NA)
  }
  if (nlevels(fac)>2) {
    text(cent[, 1], cent[, 2] + p,
         labels = labels, col = col,
         cex = cex, font = 2)
  } else {
    text(cent[, 1], cent[, 2] + p,
         labels = labels, col = col,
         cex = cex, font = 2)
  }
}

# add 'stars'

.stars <- function(xy, fac, col) {
  col.i <- col_alpha(col, 0.2)
  for (i in seq(along = levels(fac))) {
    pts.i <- xy[fac == levels(fac)[i], ]
    cent.i <- coo_centpos(pts.i)
    for (j in 1:nrow(pts.i)) {
      segments(cent.i[1], cent.i[2],
               pts.i[j, 1], pts.i[j,2],
               col = col.i[i])
    }
  }
}

# add density

.density <- function(xy, fac, levels, col, transp, n.kde2d = 50) {
  if (missing(fac) | is.null(fac))
    fac <- factor(rep("f", nrow(xy)))
  # z0 <- MASS::kde2d(xy[, 1], xy[, 2], n=n.kde2d, lims =
  # c(par('usr')))$z zmin <- min(z0) zmax <- max(z0)
  xy <- as.matrix(xy)
  for (i in seq(along = levels(fac))) {
    xy.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(xy.i)) {
      ki <- MASS::kde2d(xy.i[, 1], xy.i[, 2], n = n.kde2d, lims = c(par("usr")))
      # ki$z <- .normalize(ki$z, zmin, zmax)
      ki$z <- .normalize(ki$z)
      image(ki$x, ki$y, ki$z, add = TRUE, xlim = range(ki$x),
            ylim = range(ki$y), col = col_transp(levels,
                                                 col[i], transp))
    }
  }
}

# add contour lines

.contour <- function(xy, fac, levels, col, transp, n.kde2d = 50) {
  if (missing(fac) | is.null(fac))
    fac <- factor(rep("f", nrow(xy)))
  # z0 <- MASS::kde2d(xy[, 1], xy[, 2], n=n.kde2d, lims =
  # c(par('usr')))$z zmin <- min(z0) zmax <- max(z0)
  xy <- as.matrix(xy)
  for (i in seq(along = levels(fac))) {
    xy.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(xy.i)) {
      ki <- MASS::kde2d(xy.i[, 1], xy.i[, 2], n = n.kde2d, lims = c(par("usr")))
      # ki$z <- .normalize(ki$z, zmin, zmax)
      ki$z <- .normalize(ki$z)
      contour(ki$x, ki$y, ki$z, add = TRUE, nlevels = levels,
              drawlabels = FALSE, col = col_alpha(rep(col[i],
                                                      levels), transp))
    }
  }
}

# add delaunay triangulation

.delaunay <- function(xy, fac, col) {
  if (missing(fac) | is.null(fac))
    fac <- factor(rep("f", nrow(xy)))
  for (i in seq(along = levels(fac))) {
    xy.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(xy.i)) {
      if (nrow(xy.i) > 3) {
        links.i <- links_delaunay(xy.i)
        ldk_links(xy.i, links.i, col = col_alpha(col[i],
                                                 2/3), lwd = 1.5)
      }
    }
  }
}

# add loading vectors

.loadings <- function(loadings.mat, d = 1, d.lab = 1.2, col = "red") {
  loadings.mat <- loadings.mat * d
  loadings.lab <- loadings.mat * d.lab
  arrows(0, 0, loadings.mat[, 1], loadings.mat[, 2], angle = 20,
         length = 0.1, col = col)
  text(loadings.lab[, 1], loadings.lab[, 2], labels = rownames(loadings.lab),
       cex = 0.8, col = col)
}


# add eigen

.eigen <- function(ev, xax, yax, ratio = 0.08, ev.names="") {
  op <- par(no.readonly = TRUE)
  #on.exit(par(op))
  plt0 <- par("plt")
  #on.exit(par(plt = plt0))
  g <- 0.015
  w <- min(c(plt0[2] - plt0[1]), plt0[4] - plt0[3]) * ratio
  par(plt = c(plt0[2] - w - g,
              plt0[2] - g,
              plt0[3] + g * 1.5,
              plt0[3] + w + g * 1.5),
      xpd = NA, new =  TRUE)
  cols <- rep("grey80", 5)
  cols[c(xax, yax)] <- "grey40"
  var <- ev^2
  cs.var <- cumsum(var)/sum(var)
  k <- ifelse(max(c(xax, yax)) > 5, max(c(xax, yax)), 5)
  barplot(var[1:k], axes = FALSE, col = cols, border = NA)
  gy <- strheight(ev.names, cex = 2/3)
  title(main = ev.names, cex.main = 0.6, outer = FALSE, line = 0.15,
        col.main = "grey40", font.main = 1)
  par(op)
}

# names axes

.axisnames <- function(xax, yax, nax = "PC") {
  cex <- 2/3
  # gx <- strwidth('PCN', cex=cex)/1.5
  gy <- strheight("PCN", cex = cex)/1.5
  gx <- strwidth("00.0%", cex = cex)/1.5  # center / relatively to var %
  text(par("usr")[2] - gx, gy, col = "grey40", cex = cex, labels = paste0(nax,
                                                                          xax))
  text(-gy, par("usr")[4] - gx, col = "grey40", cex = cex,
       labels = paste0(nax, yax), srt = 90)
}

# adds var captured

.axisvar <- function(ev, xax, yax) {
  cex <- 0.7
  var <- ev^2
  var <- signif(100 * var/sum(var), 3)
  gx <- strwidth("00.0%", cex = cex)/1.5
  gy <- strheight("00.0%", cex = cex)/1.5
  text(par("usr")[2] - gx, -gy, col = "grey40", cex = cex,
       labels = paste0(var[xax], "%"))
  text(+gy, par("usr")[4] - gx, col = "grey40", cex = cex,
       labels = paste0(var[yax], "%"), srt = 90)
}

# adds title to plots

.title <- function(title) {
  # for cases where plot(PCA()) are passed
  if (is.call(title)) title <- as.character(title)[1]
  # since substitute return a 'name' not a 'character'
  title <- as.character(title)
  pos <- par("usr")
  gy <- strheight(title, font = 2) * 0.75
  text(pos[1], pos[3] + gy, pos = 4, labels = title, font = 2, cex = 0.8)
}

# misc ------------------------

.center_range <- function(x){
  m <- max(abs(x))
  c(-m, m)
}
.x.range.gg <- function(gg){
  ggplot2::ggplot_build(gg)$panel$ranges[[1]]$x.range
}

.y.range.gg <- function(gg){
  ggplot2::ggplot_build(gg)$panel$ranges[[1]]$y.range
}

.wdw.gg <- function(gg){
  c(diff(gg$layout$panel_scales_x[[1]]$range$range),
    diff(gg$layout$panel_scales_y[[1]]$range$range))
}


