
# 8. Thin plate splines plotters
# -----------------------------------------------
#' Thin Plate Splines for 2D data
#' 
#' \code{tps2d} is the core function for Thin Plate Splines. It is used internally for
#' all TPS graphical functions.\code{tps_apply} is the very same function but with
#' arguments properly named (I maintain tps2d as it is for historical reasons) when
#' we want a apply a trasnformation grid.
#' 
#' @param grid0 a matrix of coordinates on which to calculate deformations
#' @param fr the reference \eqn{(x; y)} coordinates
#' @param to the target \eqn{(x; y)} coordinates
#' @return a matrix of \code{(x; y)} coordinates with TPS-interpolated
#' deformations
#' @seealso \link{tps_grid},\link{tps_iso}, \link{tps_arr} functions use
#' \code{tps2d}.
#' @keywords ThinPlateSplines
#' @rdname tps2d
#' @export
tps2d <- function(grid0, fr, to) {
  if (is_closed(fr)) 
    fr <- coo_unclose(fr)
  if (is_closed(to)) 
    to <- coo_unclose(to)
  if (!is.matrix(grid0)) 
    grid0 <- as.matrix(grid0)
  p <- nrow(fr)
  q <- nrow(grid0)
  P <- matrix(NA, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      r2 <- sum((fr[i, ] - fr[j, ])^2)
      P[i, j] <- r2 * log(r2)
    }
  }
  P[is.na(P)] <- 0
  Q <- cbind(1, fr)
  L <- rbind(cbind(P, Q), cbind(t(Q), matrix(0, 3, 3)))
  m2 <- rbind(to, matrix(0, 3, 2))
  coefx <- solve(L) %*% m2[, 1]
  coefy <- solve(L) %*% m2[, 2]
  fx <- function(fr, grid0, coef) {
    Xn <- numeric(nrow(grid0))
    p <- nrow(fr)
    for (i in 1:q) {
      Z <- apply((fr - matrix(grid0[i, ], p, 2, byrow = TRUE))^2, 
                 1, sum)
      Xn[i] <- coef[p + 1] + coef[p + 2] * grid0[i, 1] + 
        coef[p + 3] * grid0[i, 2] + sum(coef[1:p] * (Z * 
                                                       log(Z)))
    }
    return(Xn)
  }
  grid1 <- cbind(fx(fr, grid0, coefx), fx(fr, grid0, coefy))
  return(grid1)
}

#' @rdname tps2d
#' @export
tps_apply <- function(from, to, new){
  tps2d(from, to, new)
}

#' Vanilla Thin Plate Splines
#' 
#' \code{tps_raw} calculates deformation grids and 
#' returns position of sampled points on it.
#' 
#' @param fr the reference \eqn{(x; y)} coordinates
#' @param to the target \eqn{(x; y)} coordinates
#' @param amp an amplification factor of differences between \code{fr} and
#' \code{to}
#' @param over \code{numeric} that indicates how much the thin plate splines
#' extends over the shapes
#' @param grid.size \code{numeric} to specify the number of grid cells on the
#' longer axis on the outlines
#' @return a list with two components: \code{grid} the xy coordinates of sampled
#' points along the grid; \code{dim} the dimension of the grid.
#' @seealso \link{tps_grid}, \link{tps_iso} and \link{tps_arr},
#' \link{coo_lolli}, \link{coo_arrows}.
#' @keywords ThinPlateSplines
#' @examples
#' \dontrun{ 
#' data(bot)
#' ms <- mshapes(efourier(bot, 10), "type")
#' b <- ms$shp$beer
#' w <- ms$shp$whisky
#' g <- tps_raw(b, w)
#' ldk_plot(g$grid)
#' 
#' # a wavy plot
#' ldk_plot(g$grid, pch=NA)
#' cols_ids <- 1:g$dim[1]
#' for (i in 1:g$dim[2]) lines(g$grid[cols_ids + (i-1)*g$dim[1], ])
#' }
#' @export
tps_raw <- function(fr, to, amp = 1, 
                    over = 1.2, grid.size = 15) {
  fr.n <- substitute(fr)
  to.n <- substitute(to)  # otherwise problems with substitute in legend below
  # simple magnification
  if (!missing(amp)) 
    to <- to + (to - fr) * amp
  grid0 <- .grid.sample(fr, to, nside = round(grid.size), over = over)
  res <- list(grid=tps2d(grid0, fr, to),
              dim=c(length(unique(grid0[, 1])), length(unique(grid0[, 2]))))
  return(res)
}


#' Deformation grids using Thin Plate Splines
#' 
#' \code{tps_grid} calculates and plots deformation grids between two
#' configurations.
#' 
#' @param fr the reference \eqn{(x; y)} coordinates
#' @param to the target \eqn{(x; y)} coordinates
#' @param amp an amplification factor of differences between \code{fr} and
#' \code{to}
#' @param over \code{numeric} that indicates how much the thin plate splines
#' extends over the shapes
#' @param grid.size \code{numeric} to specify the number of grid cells on the
#' longer axis on the outlines
#' @param grid.col color for drawing the grid
#' @param poly whether to draw polygons (for outlines) or points (for landmarks)
#' @param shp \code{logical}. Whether to draw shapes
#' @param shp.col Two colors for filling the shapes
#' @param shp.border Two colors for drawing the borders
#' @param shp.lwd Two \code{lwd} for drawing shapes
#' @param shp.lty Two \code{lty} fro drawing the shapes
#' @param legend logical whether to plot a legend
#' @param legend.text some text for the legend
#' @param ... additional arguments to feed \link{coo_draw}
#' @return Nothing
#' @seealso \link{tps_iso} and \link{tps_arr},
#' \link{coo_lolli}, \link{coo_arrows}.
#' @keywords ThinPlateSplines
#' @examples
#' data(bot)
#' botF <- efourier(bot)
#' x <- mshapes(botF, 'type', nb.pts=80)$shp
#' fr <- x$beer
#' to <- x$whisky
#' tps_grid(fr, to, amp=3, grid.size=10)
#' @export
tps_grid <- function(fr, to, amp = 1, 
                     over = 1.2, grid.size = 15, 
                     grid.col = "grey80", poly = TRUE,
                     shp = TRUE, shp.col = rep(NA, 2), 
                     shp.border = col_qual(2), shp.lwd = c(1, 1), 
                     shp.lty = c(1, 1), legend = TRUE, legend.text, ...) {
  fr.n <- substitute(fr)
  to.n <- substitute(to)  # otherwise problems with substitute in legend below
  # simple magnification
  if (!missing(amp)) { 
    to <- to + (to - fr) * amp
  }
  grid0 <- .grid.sample(fr, to, nside = round(grid.size), over = over)
  grid1 <- tps2d(grid0, fr, to)
  dim.grid <- c(length(unique(grid0[, 1])), length(unique(grid0[, 2])))
  op <- par(mar = rep(0, 4))
  on.exit(par(op))
  plot(NA, xlim = range(grid1[, 1]), ylim = range(grid1[, 2]), 
       asp = 1, ann = FALSE, axes = FALSE, mar = rep(0, 4))
  for (i in 1:dim.grid[2]) {
    lines(grid1[(1:dim.grid[1]) + (i - 1) * dim.grid[1], 
                ], col = grid.col)
  }
  for (i in 1:dim.grid[1]) {
    lines(grid1[(1:dim.grid[2]) * dim.grid[1] - i + 1, ], 
          col = grid.col)
  }
  if (shp) {
    points <- ifelse(poly, FALSE, TRUE)
    coo_draw(fr, border = shp.border[1], col = NA, lwd = shp.lwd[1], 
             lty = shp.lty[1], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    coo_draw(to, border = shp.border[2], col = NA, lwd = shp.lwd[2], 
             lty = shp.lty[2], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    if (legend | !missing(legend.text)) {
      if (missing(legend.text)) legend.text <- c(fr.n, to.n)
      legend("topright", legend = legend.text, col = shp.border, 
             lwd = shp.lwd, bty = "n")
    }
  }
}

#' Deformation 'vector field' using Thin Plate Splines
#' 
#' \code{tps_arr}(ows) calculates deformations between two configurations and
#' illustrate them using arrows.
#' 
#' @param fr the reference \eqn{(x; y)} coordinates
#' @param to the target \eqn{(x; y)} coordinates
#' @param amp an amplification factor of differences between \code{fr} and
#' \code{to}
#' @param grid whether to calculate and plot changes across the graphical window
#' \code{TRUE} or just within the starting shape (\code{FALSE})
#' @param over \code{numeric} that indicates how much the thin plate splines
#' extends over the shapes
#' @param palette a color palette such those included in Momocs or produced
#' with \link{colorRampPalette}
#' @param arr.nb \code{numeric} The number of arrows to calculate
#' @param arr.levels \code{numeric}. The number of levels for the color of
#' arrows
#' @param arr.len \code{numeric} for the length of arrows
#' @param arr.ang \code{numeric} for the angle for arrows' heads
#' @param arr.lwd \code{numeric} for the \code{lwd} for drawing arrows
#' @param arr.col if \code{palette} is not used the color for arrows
#' @param poly whether to draw polygons (for outlines) or points (for landmarks)
#' @param shp \code{logical}. whether to draw shapes
#' @param shp.col two colors for filling the shapes
#' @param shp.border two colors for drawing the borders
#' @param shp.lwd two \code{lwd} for drawing shapes
#' @param shp.lty two \code{lty} fro drawing the shapes
#' @param legend logical whether to plot a legend
#' @param legend.text some text for the legend
#' @param ... additional arguments to feed \link{coo_draw}
#' @return Nothing.
#' @seealso \link{tps_grid} and \link{tps_iso},
#' \link{coo_lolli}, \link{coo_arrows}.
#' @keywords ThinPlateSplines
#' @examples
#' data(bot)
#' botF <- efourier(bot)
#' x <- mshapes(botF, 'type', nb.pts=80)$shp
#' fr <- x$beer
#' to <- x$whisky
#' tps_arr(fr, to, arr.nb=200, palette=col_sari, amp=3)
#' tps_arr(fr, to, arr.nb=200, palette=col_sari, amp=3, grid=FALSE)
#' @export
tps_arr <- function(fr, to, amp = 1, 
                    grid = TRUE, over = 1.2, palette = col_summer, 
                    arr.nb = 200, arr.levels = 100, arr.len = 0.1, arr.ang = 20, 
                    arr.lwd = 0.75, arr.col = "grey50", poly = TRUE, 
                    shp = TRUE, shp.col = rep(NA, 2),
                    shp.border = col_qual(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1),
                    legend = TRUE, legend.text, ...) {
  fr.n <- substitute(fr)
  to.n <- substitute(to)  # otherwise problems with substitute in legend below
  if (!missing(amp)) 
    to <- to + (to - fr) * amp
  if (grid){
    grid0 <- .grid.sample(fr, to, nside = round(sqrt(arr.nb)), over = over)
  }
  else {
    grid0 <- spsample(Polygon(coo_close(fr)), arr.nb, type='regular')@coords
  }
  grid1 <- tps2d(grid0, fr, to)
  # grille simple, on affiche d'abord les deux courbes
  op <- par(mar = rep(0, 4))
  on.exit(par(op))
  plot(NA, xlim = range(grid0[, 1]), ylim = range(grid0[, 2]), 
       asp = 1, axes = FALSE, ann = FALSE, mar = rep(0, 4))
  if (missing(arr.levels)) {
    arr.levels = arr.nb
  }
  if (!missing(palette)) {
    q.lev <- cut(edm(grid0, grid1), breaks = arr.levels, 
                 labels = FALSE)
    arr.cols <- palette(arr.levels)[q.lev]
  } else {
    arr.cols <- rep(arr.col, nrow(grid0))
  }
  arrows(grid0[, 1], grid0[, 2], grid1[, 1], grid1[, 2], length = arr.len, 
         angle = arr.ang, lwd = arr.lwd, col = arr.cols)
  if (shp) {
    points <- ifelse(poly, FALSE, TRUE)
    coo_draw(fr, border = shp.border[1], col = NA, lwd = shp.lwd[1], 
             lty = shp.lty[1], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    coo_draw(to, border = shp.border[2], col = NA, lwd = shp.lwd[2], 
             lty = shp.lty[2], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    if (legend | !missing(legend.text)) {
      if (missing(legend.text)) legend.text <- c(fr.n, to.n)
      legend("topright", legend = legend.text, col = shp.border, 
             lwd = shp.lwd, bty = "n")
    }
  }
}

#' Deformation isolines using Thin Plate Splines.
#' 
#' \code{tps_iso} calculates deformations between two configurations and map
#' them with or without isolines.
#' 
#' @param fr The reference \eqn{(x; y)} coordinates
#' @param to The target \eqn{(x; y)} coordinates
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}
#' @param grid whether to calculate and plot changes across the graphical window
#' \code{TRUE} or just within the starting shape (\code{FALSE})
#' @param over A \code{numeric} that indicates how much the thin plate splines
#' extends over the shapes
#' @param palette A color palette such those included in Momocs or produced
#' with \link{colorRampPalette}
#' @param iso.levels \code{numeric}. The number of levels for mapping the
#' deformations
#' @param iso.nb A \code{numeric}. The number of points to use for the
#' calculation of deformation
#' @param cont \code{logical}. Whether to draw contour lines
#' @param cont.col A color for drawing the contour lines
#' @param poly whether to draw polygons (for outlines) or points (for landmarks)
#' @param shp \code{logical}. Whether to draw shapes
#' @param shp.border Two colors for drawing the borders
#' @param shp.lwd Two \code{lwd} for drawing shapes
#' @param shp.lty Two \code{lty} fro drawing the shapes
#' @param legend logical whether to plot a legend
#' @param legend.text some text for the legend
#' @param ... additional arguments to feed \link{coo_draw}
#' @return No returned value
#' @seealso \link{tps_grid} and \link{tps_arr},
#' \link{coo_lolli}, \link{coo_arrows}.
#' @keywords ThinPlateSplines
#' @examples
#' data(bot)
#' botF <- efourier(bot)
#' x <- mshapes(botF, 'type', nb.pts=80)$shp
#' fr <- x$beer
#' to <- x$whisky
#' tps_iso(fr, to, iso.nb=200, amp=3)
#' tps_iso(fr, to, iso.nb=200, amp=3, grid=TRUE)
#' @export
tps_iso <- function(fr, to, amp = 1, 
                    grid = FALSE, over = 1.2, palette = col_spring, 
                    iso.nb = 1000, iso.levels = 12,
                    cont = TRUE, cont.col = "black", 
                    poly = TRUE, shp = TRUE, shp.border = col_qual(2), 
                    shp.lwd = c(2, 2), shp.lty = c(1, 1),
                    legend = TRUE, legend.text, ...) {
  fr.n <- substitute(fr)
  to.n <- substitute(to)  # otherwise problems with substitute in legend below
  if (!missing(amp)) 
    to <- to + (to - fr) * amp
  if (grid) {
    grid0 <- .grid.sample(fr, to, nside = round(sqrt(iso.nb)), over = over)
  } else {
    grid0 <- spsample(Polygon(coo_close(fr)), iso.nb, type='regular')@coords
  }
  grid1 <- tps2d(grid0, fr, to)
  def <- edm(grid0, grid1)
  x1 <- length(unique(grid0[, 1]))
  y1 <- length(unique(grid0[, 2]))
  im <- matrix(NA, x1, y1)
  xind <- (1:x1)[as.factor(rank(grid0[, 1]))]
  yind <- (1:y1)[as.factor(rank(grid0[, 2]))]
  n <- length(xind)
  for (i in 1:n) im[xind[i], yind[i]] <- def[i]
  iso.cols <- palette(iso.levels)
  x <- sort(unique(grid0[, 1]))
  y <- sort(unique(grid0[, 2]))
  op <- par(mar = rep(1, 4))
  on.exit(par(op))
  image(x, y, im, col = iso.cols, asp = 1,
        xlim = range(grid0[, 1]),
        ylim = range(grid0[, 2]),
        axes = FALSE, frame = FALSE, 
        ann = FALSE)
  if (cont) {
    contour(x, y, im, nlevels = iso.levels,
            add = TRUE, drawlabels = FALSE, 
            col = cont.col, lty = 2)
  }
  if (shp) {
    points <- ifelse(poly, FALSE, TRUE)
    coo_draw(fr, border = shp.border[1], col = NA, lwd = shp.lwd[1], 
             lty = shp.lty[1], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    coo_draw(to, border = shp.border[2], col = NA, lwd = shp.lwd[2], 
             lty = shp.lty[2], points = points, first.point = FALSE, 
             centroid = FALSE, ...)
    if (legend | !missing(legend.text)) {
      if (missing(legend.text)) legend.text <- c(fr.n, to.n)
      legend("topright", legend = legend.text, col = shp.border, 
             lwd = shp.lwd, bty = "n")
    }
  }
} 
