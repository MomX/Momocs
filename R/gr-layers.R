##### Functions for graphical layers The loops for the fac
##### handling are neither the most orthodox nor the fastest
##### option but are pretty readable and easy to write as a first
##### approach.

# create an empty frame
#' @export
.frame <- function(xy, center.origin = FALSE, zoom = 1, bg="white") {
  op <- par(bg=bg)
  on.exit(par(op))
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

# grid layer
#' @export
.grid <- function(nb.grids = 3) {
  m <- max(abs(par("usr")))
  g <- seq(0, m, length = nb.grids)
  g <- c(g[-1] * -1, g[-1])
  abline(h = g, col = "grey80", lty = 3)
  abline(v = g, col = "grey80", lty = 3)
  abline(v = 0, col = "grey80")
  abline(h = 0, col = "grey80")
}

# rug
#' @export
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
#' @export
.ellipses <- function(xy, fac, conf, col) {
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
#' @export
.ellipsesax <- function(xy, fac, conf, col, lty = 1, lwd = 1) {
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
#' @export
.chull <- function(coo, fac, col, lty) {
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
#' @export
.chullfilled <- function(coo, fac, col) {
  for (i in seq(along = levels(fac))) {
    coo_i <- coo[fac == levels(fac)[i], ]
    if (is.matrix(coo_i)) {
      if (nrow(coo_i) > 1) {
        chull.i <- coo_chull(coo_i)
        polygon(coo_close(chull.i), col = col[i], border=col[i])
      }
    }
  }
}


# add labels
#' @export
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
#' @export
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
#' @export
.density <- function(xy, fac, levels, col, transp, n.kde2d = 50) {
  if (missing(fac) | is.null(fac))
    fac <- factor(rep("f", nrow(xy)))
  # z0 <- kde2d(xy[, 1], xy[, 2], n=n.kde2d, lims =
  # c(par('usr')))$z zmin <- min(z0) zmax <- max(z0)
  xy <- as.matrix(xy)
  for (i in seq(along = levels(fac))) {
    xy.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(xy.i)) {
      ki <- kde2d(xy.i[, 1], xy.i[, 2], n = n.kde2d, lims = c(par("usr")))
      # ki$z <- .normalize(ki$z, zmin, zmax)
      ki$z <- .normalize(ki$z)
      image(ki$x, ki$y, ki$z, add = TRUE, xlim = range(ki$x),
            ylim = range(ki$y), col = col_transp(levels,
                                                 col[i], transp))
    }
  }
}

# add contour lines
#' @export
.contour <- function(xy, fac, levels, col, transp, n.kde2d = 50) {
  if (missing(fac) | is.null(fac))
    fac <- factor(rep("f", nrow(xy)))
  # z0 <- kde2d(xy[, 1], xy[, 2], n=n.kde2d, lims =
  # c(par('usr')))$z zmin <- min(z0) zmax <- max(z0)
  xy <- as.matrix(xy)
  for (i in seq(along = levels(fac))) {
    xy.i <- xy[fac == levels(fac)[i], ]
    if (is.matrix(xy.i)) {
      ki <- kde2d(xy.i[, 1], xy.i[, 2], n = n.kde2d, lims = c(par("usr")))
      # ki$z <- .normalize(ki$z, zmin, zmax)
      ki$z <- .normalize(ki$z)
      contour(ki$x, ki$y, ki$z, add = TRUE, nlevels = levels,
              drawlabels = FALSE, col = col_alpha(rep(col[i],
                                                    levels), transp))
    }
  }
}

# add delaunay triangulation
#' @export
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
#' @export
.loadings <- function(loadings.mat, d = 1, d.lab = 1.2, col = "red") {
  loadings.mat <- loadings.mat * d
  loadings.lab <- loadings.mat * d.lab
  arrows(0, 0, loadings.mat[, 1], loadings.mat[, 2], angle = 20,
         length = 0.1, col = col)
  text(loadings.lab[, 1], loadings.lab[, 2], labels = rownames(loadings.lab),
       cex = 0.8, col = col)
}


# add eigen
#' @export
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
#' @export
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
#' @export
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
#' @export
.title <- function(title) {
  # for cases where plot(PCA()) are passed
  if (is.call(title)) title <- as.character(title)[1]
  # since substitute return a 'name' not a 'character'
  title <- as.character(title)
  pos <- par("usr")
  gy <- strheight(title, font = 2) * 0.75
  text(pos[1], pos[3] + gy, pos = 4, labels = title, font = 2, cex = 0.8)
}

# Experimental and maybe useless --------------------
# combine factor (a quick wrapper around interaction)
# for PCA, LDA or any data.frame
#' @export
.combine.fac <- function(x, formula){
  if (any(c("LDA", "PCA")==class(x)[1])) x <- x$fac
  f0 <- x[, attr(terms(formula), "term.labels")]
  return(interaction(f0))}

# smart color grouping provided with a fac after interaction
# data(olea)
# op <- PCA(npoly(olea))
# f <- .combine.fac(op, ~view+domes)
# cols <- .smartpalette(f)
# plot(op, f, col=cols)
#' @export
.smartpalette <- function(fac, gap.intra=1, gap.inter=5, palette=col_qual){
  # determine ranking within a factor
  rank <- function(s){
    s <- as.numeric(factor(s))
    rank <- (1:length(unique(s)))
    rank[match(s, unique(s))]}
  # adds gaps between the levels of a factor
  gap.that <- function(s, gap.intra=1){
    s <- as.numeric(factor(s))
    add <- (1:length(unique(s))) + (0:(length(unique(s))-1))*gap.intra
    add[match(s, unique(s))]}
  # we start here
  fac0 <- factor(unique(fac))
  # we resplit the factors
  df <- t(data.frame(strsplit(as.character(fac0), ".", fixed=TRUE), row.names=NULL))
  #df <- df[, ncol(df):1]
  # we calculate ranks
  df.rank <- apply(df, 2, rank)
  df.rank[, 1] <- gap.that(df.rank[, 1], gap.intra=gap.intra)
  # we gap.that consecutive levels
  if (ncol(df.rank)>1){
    for (i in 2:ncol(df.rank)){
      gap.i <- length(unique(df.rank[, (i-1)])) * (gap.inter*i+gap.intra)
      df.rank[, i] <- gap.that(df.rank[, i], gap.intra=gap.i)}}
  # we add ranks and eventually have our indices
  ids <- apply(df.rank, 1, sum)
  ids <- ids-min(ids)+1
  cols <- palette(max(ids))[ids]
  return(cols)}


# .* ------------------------
.cex <- function(x) {
  3/(log(x + 1) + 1)
}

.pch <- function() {
  c(2, 6, 1, 3, 4, 5, 8, 7, 9, 10)}

.center_range <- function(x){
  m <- max(abs(x))
  c(-m, m)
}
.x.range.gg <- function(gg){
  ggplot_build(gg)$panel$ranges[[1]]$x.range
}

.y.range.gg <- function(gg){
  ggplot_build(gg)$panel$ranges[[1]]$y.range
}

.wdw.gg <- function(gg){
  c(diff(.x.range.gg(gg)),
    diff(.y.range.gg(gg)))
}
