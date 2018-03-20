# Papers ---------------------------------------------------
#' grindr papers for shape plots
#'
#' Papers on which to use [drawers] for building custom
#' shape plots using the grindr approach. See examples and vignettes.
#'
#' @param coo a single shape or any [Coo] object
#' @param ... more arguments to feed the plotting function within each `paper` function
#' @note This approach will (soon) replace [coo_plot] and friends in further versions.
#' All comments are welcome.
#'
#' @family grindr
#' @name papers
#' @rdname papers
#' @export
paper <- function(coo, ...){
  coo %>% paper_white() %>% draw_axes()
}

#' @rdname papers
#' @export
paper_white <- function(coo){
  # smaller margins
  old <- par(mar=c(1.2, 1.2, 0, 0))
  on.exit(par(old))

  # empty plot of correct extent
  r <- coo %>% coo_range_enlarge(k=1/20)
  plot(NA,
       xlim=c(r[1, 1], r[2, 1]), ylim=c(r[1, 2], r[2, 2]), asp=1,
       axes=FALSE, ann=FALSE)

  # propagate
  invisible(coo)
}

#' @rdname papers
#' @param grid `numeric` of length 2 to (roughly) specify the
#' number of majors lines, and the number of minor lines within two major ones
#' @param cols colors (hexadecimal preferred) to use for grid drawing
#' @export
paper_grid <- function(coo, grid=c(10, 5), cols=c("#ffa500", "#e5e5e5"), ...){
  # smaller margins
  old <- par(mar=c(1.2, 1.2, 0, 0), xpd=NA)
  on.exit(par(old))

  # empty plot of correct extent
  r <- coo %>% coo_range_enlarge(k=1/20)
  plot(NA,
       xlim=c(r[1, 1], r[2, 1]), ylim=c(r[1, 2], r[2, 2]), asp=1,
       axes=FALSE, ann=FALSE)

  # prepare the grid paper colors
  cols <- cols %>%
    pal_alpha(0.5) %>%
    rep.int(c(1, grid[2]-1)) %>%
    rep(grid[1])
  # draw lines
  p <- par("usr") %>% abs %>% max %>% c(-., .) %>%
    pretty(n=prod(grid), min.n=prod(grid))
  abline(h=p, v=p, col=cols, ...)
  # propagate
  invisible(coo)
}

#' @export
#' @rdname papers
#' @param n \code{numeric} number of squares for the chessboard
#' @param col color (hexadecimal) to use for chessboard drawing
#' @export
paper_chess <- function(coo, n=50, col="#E5E5E5"){
  # smaller margins
  old <- par(mar=c(1.2, 1.2, 0, 0), xpd=NA)
  on.exit(par(old))

  # empty plot of correct extent
  r <- coo %>% coo_range_enlarge(k=1/20)
  plot(NA,
       xlim=c(r[1, 1], r[2, 1]), ylim=c(r[1, 2], r[2, 2]), asp=1,
       axes=FALSE, ann=FALSE)

  # grabs window parameters
  w <- par("usr")
  wm <- max(.wdw())
  w <- c(w[1]-wm/10, w[2]+wm/10, w[3]-wm/10, w[4]+wm/10)
  # wm <- max(abs(w))
  # max dimension
  # wm <-   max(w[2] - w[1], w[4] - w[3])
  wm <-   max(.wdw())*1.2
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
  # propagate
  invisible(coo)
}

#' @export
#' @rdname papers
#' @param pch to use for dots
paper_dots <- function(coo, pch=20, n=50, col="#7F7F7F"){
  # smaller margins
  old <- par(mar=c(1.2, 1.2, 0, 0), xpd=NA)
  on.exit(par(old))

  # empty plot of correct extent
  r <- coo %>% coo_range_enlarge(k=1/20)
  plot(NA,
       xlim=c(r[1, 1], r[2, 1]), ylim=c(r[1, 2], r[2, 2]), asp=1,
       axes=FALSE, ann=FALSE)

  # grabs window parameters
  w <- par("usr")
  wm <- max(.wdw())
  w <- c(w[1]-wm/10, w[2]+wm/10, w[3]-wm/10, w[4]+wm/10)
  # wm <- max(abs(w))
  # max dimension
  # wm <-   max(w[2] - w[1], w[4] - w[3])
  wm <-   max(.wdw())*1.2
  side <- wm/n
  # generates xleft coordinates (1/2 is picked)
  xs <- seq(w[1], w[2], side)
  # generates all ybottom coordinates
  ys <- seq(w[3], w[4], side)
  # draw dots
  points(expand.grid(xs, ys), pch=pch, col=col, cex=1/4)
  # propagate
  invisible(coo)
}
