# utils ----------------------------------------------------
is_palette <- function(x){
  any(class(x)=="palette")
}

as_palette <- function(x){
  class(x) <- unique(c(class(x), "palette"))
  x
}


this_dispatch <- function(f, this){
  # non factor case
  if (!is.factor(f) && length(f)==length(this))
    return(this)
  # right length case
  if (length(this)==length(f))
    return(this)
  # one for each level case
  if (length(this)==nlevels(f))
    return(this[f])
  if (is.function(this))
    return(this(nlevels(f))[f])
  # single value case
  if (length(this)==1)
    return(rep(this, length(f)))
  # otherwise
  message("bad length for 'this' argument")
}
# Papers ---------------------------------------------------
#' Cheapbabi papers for shape plots
#'
#' Papers on which to use [drawers] for building custom
#' shape plots using the cheapbabi approach. See examples and vignettes.
#'
#' @param coo a single shape or any [Coo] object
#' @param ... more arguments to feed `plot` function within each `paper` function
#' @note This approach will (soon) replace [coo_plot] and friends in further versions.
#' All comments are welcome.
#'
#' @details `paper` is `paper_white %>% draw_axes`.
#'
#' @family cheapbabi
#' @name papers
#' @rdname papers
#' @export
paper_white <- function(coo, ...){
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
#' @export
paper <- function(coo, ...){
  coo %>% paper_white(...) %>% draw_axes()
}

#' @rdname papers
#' @param grid \code{numeric} of length 2 to (roughly) specify the
#' number of majors lines, and the number of minor lines within two major ones
#' @param cols colors (hexadecimal) to use for grid drawing
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
    col_alpha(0.5) %>%
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
paper_chess <- function(coo, n=50, col="#e5e5e5"){
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
paper_dots <- function(coo, n=50, col="#e5e5e5"){
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
  points(expand.grid(xs, ys), pch=20, col="#E5E5E5", cex=1/4)
  # propagate
  invisible(coo)
}

# Drawers -------------------------------------------------
#' Cheapbabi drawers for shape plots
#'
#' Useful drawers for building custom
#' shape plots using the cheapbabi approach. See examples and vignettes.
#'
#' @note This approach will (soon) replace [coo_plot] and friends in further versions.
#' All comments are welcome.
#'
#' @name drawers
#' @rdname drawers
#' @seealso cheapbabi_layers
#' @family cheapbabi
#'
#' @param coo \code{matrix} of 2 columns for (x, y) coordinates
#' @param f an optionnal factor specification to feed. See examples and vignettes.
#' @param border color (hexadecimal) to draw components
#' @param col color (hexadecimal) to draw components
#' @param pch to draw components
#' @param cex to draw components
#' @param lwd to draw components
#' @param lty to draw components
#' @param label to indicate first point
#' @param labels \code{character} name of labels to draw (defaut to \code{1:nrow(coo)})
#' @param d `numeric` proportion of `d(centroid-each_point)` to add when centrifugating landmarks
#' @param links `matrix` of links to use to draw segments between landmarks. See `wings$ldk` for an example
#' @param ... additional options to feed core functions for each drawer
#'
#' @examples
#' bot[1] %>% paper_grid() %>% draw_polygon()
#' olea %>% paper_chess %>% draw_lines(~var)
#'
#' hearts[240] %>% paper_white() %>% draw_outline() %>%
#'   coo_sample(24) %>% draw_landmarks %>% draw_labels() %>%
#'   draw_links(replicate(2, sample(1:24, 8)))
#' @export
draw_polygon <- function(coo, f, border=par("fg"), col=NA, lwd=1, lty=1, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  borders <- this_dispatch(f, border)
  cols    <- this_dispatch(f, col)
  lwds    <- this_dispatch(f, lwd)
  ltys    <- this_dispatch(f, lty)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the outlines as a polygon
  for (i in seq_along(x))
    polygon(x[[i]][, 1], x[[i]][, 2],
            border=borders[i], col=cols[i],
            lty=ltys[i], lwd=lwds[i], ...)

  # propagate
  invisible(coo)
}


#' @export
#' @rdname drawers
draw_outline <- draw_polygon
#' @export
#' @rdname drawers
draw_outlines <- draw_polygon

#' @export
#' @rdname drawers
draw_points <- function(coo,  f, col=par("fg"), cex=1, pch=20, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  cols    <- this_dispatch(f, col)
  cexs    <- this_dispatch(f, cex)
  pchs    <- this_dispatch(f, pch)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the outlines as a polygon
  for (i in seq_along(x))
    points(x[[i]][, 1], x[[i]][, 2],
           col=cols[i],
           cex=cexs[i], pch=pchs[i], ...)
  # propagate
  invisible(coo)
}

#' @export
#' @rdname drawers
draw_landmarks <- draw_points

#' @export
#' @rdname drawers
draw_lines <- function(coo,  f, col=par("fg"), lwd=1, lty=1, ...){

  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  cols    <- this_dispatch(f, col)
  lwds    <- this_dispatch(f, lwd)
  ltys    <- this_dispatch(f, lty)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the outlines as a polygon
  for (i in seq_along(x))
    lines(x[[i]][, 1], x[[i]][, 2],
          col=cols[i],
          lty=ltys[i], lwd=lwds[i], ...)

  # propagate
  invisible(coo)
}


#' @export
#' @rdname drawers
draw_centroid <- function(coo, f, col=par("fg"), pch=3, cex=0.5, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  cols    <- this_dispatch(f, col)
  cexs    <- this_dispatch(f, cex)
  pchs    <- this_dispatch(f, pch)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # calculate centpos
  x <- lapply(x, coo_centpos)
  # draw the outlines as a polygon
  for (i in seq_along(x))
    points(x[[i]][1], x[[i]][2],
           col=cols[i],
           cex=cexs[i], pch=pchs[i], ...)

  # propagate
  invisible(coo)
}

#' @export
#' @rdname drawers
draw_curve <- draw_lines
#' @export
#' @rdname drawers
draw_curves <- draw_lines

#' @export
#' @rdname drawers
draw_firstpoint <- function(coo, f, label="^", col=par("fg"), cex=1, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  labels  <- this_dispatch(f, label)
  cols    <- this_dispatch(f, col)
  cexs    <- this_dispatch(f, cex)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the outlines as a polygon
  for (i in seq_along(x)){
    # calculate the tangent angle (in degrees) between the first 2 points
    angle <- atan2(x[[i]][2, 2] - x[[i]][1, 2],
                   x[[i]][2, 1] - x[[i]][1, 1]) * (180/pi) - 90
    # draw it as a little circumflex
    text(x[[i]][1, 1], x[[i]][1, 2],
         labels = labels[i], col=cols[i], cex = cexs[i], srt = angle, ...)
  }

  # propagate
  invisible(coo)
}

#' @export
#' @rdname drawers
# cosmetics
draw_axes <- function(coo, col="#333333", cex=3/4, lwd=3/4, ...){
  at <- function(x) signif(c(min(x), mean(x), max(x)), 3)
  # we dont need this here but it preserves
  # parallelism between functions
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo))
    x <- coo$coo

  x <- do.call("rbind", x)

  at_x <- at(x[, 1])
  at_y <- at(x[, 2])

  pos_x <- min(at_y) + max(strheight(at_x, cex=cex)) - .wdw()[2]/50
  pos_y <- min(at_x) + max(strheight(at_y, cex=cex)) - .wdw()[1]/50
  axis(1, at=at_x, pos=pos_x,
       col="#FFFFFF00", # fully transparent
       col.ticks=col, cex.axis=cex,
       lwd=lwd, lwd.ticks=lwd, tcl = -1/5, las=1)
  axis(2,  at=at_y, pos=pos_y,
       col="#FFFFFF00", # fully transparent
       col.ticks=col, cex.axis=cex,
       lwd=lwd, lwd.ticks=lwd, tcl = -1/5, las=1)

  # propagate
  invisible(coo)
}

#' @export
#' @rdname drawers
draw_labels <- function(coo, labels=1:nrow(coo), cex=1/2, d=1/20, ...){
  # this one does not support f and,
  # if a Coo is provided turn it into the mean shape
  # Coo case
  if (is_Coo(coo))
    coo <- mshapes(coo)

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

#' @export
#' @rdname drawers
draw_links <- function(coo, f, links, col="#99999955", lwd=1/2, lty=1, ...){


  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # Coo case
  if (is_Coo(coo)){
    x <- coo$coo
    links <- coo$links
  }
  # handle factor
  if (is_Coo(coo) && !missing(f))
    f <- .fac_dispatcher(coo, f)
  else
    f <- factor(rep(1, length(x)))

  # dispath drawer argument
  cols    <- this_dispatch(f, col)
  lwds    <- this_dispatch(f, lwd)
  ltys    <- this_dispatch(f, lty)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the links
  for (i in seq_along(x)){
    for (j in 1:nrow(links)) {
      segments(x[[i]][links[j, 1], 1],
               x[[i]][links[j, 1], 2],
               x[[i]][links[j, 2], 1],
               x[[i]][links[j, 2], 2],
               col=cols[i], lwd=lwds[i], lty=ltys[i], ...)
    }
  }
  # propagate
  invisible(coo)
}
# wings %>% mshapes %>% paper %>% draw_links(links=wings$links) %>% draw_landmarks %>% draw_labels(d=1/5)
