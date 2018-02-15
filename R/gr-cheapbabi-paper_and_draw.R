
# .orange   <- "#ffa500"
# .grey90   <- "#e5e5e5"
# .grey60   <- "#999999"

# _____ paper _____ ----------------------------------------
paper_white <- function(coo){
  # no margins
  old <- par(mar=rep(0, 4))
  # empty plot of correct extent
  w <- coo %>% coo_center %>% abs %>% max
  b <- coo %>% coo_centpos()
  plot(NA,
       xlim=c(b[1] - w, b[1] + w), ylim=c(b[2] - w, b[2] + w), asp=1,
       axes=FALSE, ann=FALSE, mar=rep(0, 4))
  # restore par and propagate
  old <- par(mar=rep(0, 4))
  invisible(coo)
}


paper_grid <- function(coo, grid=c(10, 5), cols=c("#ffa500", "#e5e5e5"), ...){
  # no margins
  old <- par(mar=rep(0, 4), xpd=NA)
  # empty plot of correct extent
  w <- coo %>% coo_center %>% abs %>% max
  b <- coo %>% coo_centpos()
  plot(NA,
       xlim=c(b[1] - w, b[1] + w), ylim=c(b[2] - w, b[2] + w), asp=1,
       axes=FALSE, ann=FALSE, mar=rep(0, 4))
  # prepare the grid paper colors
  cols <- cols %>%
    col_alpha(0.5) %>%
    rep.int(c(1, grid[2]-1)) %>%
    rep(grid[1])
  # draw lines
  p <- par("usr") %>% abs %>% max %>% c(-., .) %>%
    pretty(n=prod(grid), min.n=prod(grid))
  abline(h=p, v=p, col=cols, ...)
  # restore par and propagate
  old <- par(mar=rep(0, 4))
  invisible(coo)
}

# _____ draw _____ -----------------------------------------
# shapes
draw_outline <- draw_polygon <- function(coo, border=par("fg"), col=NA, ...){
  # draw the outline as a polygon
  polygon(coo[, 1], coo[, 2], border=border, col=col, ...)
  # propagate
  invisible(coo)
}

draw_landmarks <- draw_points <- function(coo, col=par("fg"), pch=20, ...){
  # draw landmarks as points
  points(coo[, 1], coo[, 2], col=col, pch=pch, ...)
  # propagate
  invisible(coo)
}

draw_curve <- draw_lines <- function(coo, col=par("fg"), ...){
  # draw curve as line
  lines(coo[, 1], coo[, 2], col=col, ...)
  # propagate
  invisible(coo)
}

# meta
draw_centroid <- function(coo, pch=3, cex=0.5, ...){
  # calculate centroid position
  cxy <- coo_centpos(coo)
  points(cxy[1], cxy[2], pch=pch, cex=cex, ...)
  # propagate
  invisible(coo)
}

draw_firstpoint <- function(coo, cex=1, ...){
  # calculate the tangent angle (in degrees) between the first 2 points
  angle <- atan2(coo[2, 2] - coo[1, 2], coo[2, 1] - coo[1, 1]) * (180/pi) - 90
  # draw it as a little circumflex
  text(coo[1, 1], coo[1, 2], labels = "^", cex = cex, srt = angle)
  # propagate
  invisible(coo)
}

# cosmetics
draw_axes <- function(coo, col="#999999", lwd=1/2,...){
  # add x=0 and y=0 lines for axes
  abline(h=0, v=0, col=col, lwd=lwd, ...)
  # propagate
  invisible(coo)
}

draw_labels <- function(coo, labels=1:nrow(coo), d=1/20, cex=1/2, ...){
  # centrifugate labels positions of d*median(distance centroid)
  # away from centroid
  centpos <- coo_centpos(coo)
  dm <- median(coo_centdist(coo))
  for (i in 1:nrow(coo)) {
    dxy <- ed(centpos, coo[i, ])
    labxy <- edi(centpos, coo[i, ], (dxy + dm * d)/dxy)
    # draw
    text(labxy[1], labxy[2], labels = i, cex = cex, ...)
  }
  # propagate
  invisible(coo)
}

draw_links <- function(coo, links, lwd=1/2, col=par("fg"), ...){
  for (i in 1:nrow(links)) {
    segments(coo[links[i, 1], 1],
             coo[links[i, 1], 2],
             coo[links[i, 2], 1],
             coo[links[i, 2], 2],
             col=col, ...)
  }
  # propagate
  invisible(coo)
}
#'
#' bot[1] %>% paper_grid() %>% add_polygon() %>% add_centroid() %>% add_axes()
#' hearts[240] %>% paper_white() %>% add_polygon() %>%
#'   add_firstpoint(cex=1)
#'
#' hearts[240] %>% paper_white() %>% add_outline() %>%
#'   coo_sample(24) %>% add_landmarks %>% add_labels() %>%
#'   add_links(replicate(2, sample(1:24, 8)))
#'
#' # todo
#' #paper_chessboard
#'
#' #' @export
#' .chessboard <- function(n=50, col="grey98", w){
#'   # grabs window parameters if not provided
#'   if (missing(w)) w <- par("usr")
#'   # max dimension
#'   wm <-   max(w[2] - w[1], w[4] - w[3])
#'   side <- wm/n
#'   # generates xleft coordinates (1/2 is picked)
#'   xl <- seq(w[1], w[2], side)[seq(0, n, 2)+1]
#'   # generates all ybottom coordinates
#'   yb <- seq(w[3], w[4], side)
#'   # handles g(ap) through modulus
#'   g <- c(0, side)[(1:n %% 2)+1]
#'   # loop and draw rectangles
#'   for (j in 1:n)
#'     rect(xl + g[j], yb[j], xl + g[j] + side, yb[j]+side, col=col, border=NA)
#' }
