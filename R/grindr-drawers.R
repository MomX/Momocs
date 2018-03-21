# Drawers -------------------------------------------------
#' grindr drawers for shape plots
#'
#' Useful drawers for building custom
#' shape plots using the grindr approach. See examples and vignettes.
#'
#' @note This approach will (soon) replace [coo_plot] and friends in further versions.
#' All comments are welcome.
#'
#' @name drawers
#' @rdname drawers
#' @seealso grindr_layers
#' @family grindr
#'
#' @param coo \code{matrix} of 2 columns for (x, y) coordinates
#' @param f an optionnal factor specification to feed. See examples and vignettes.
#' @param col color (hexadecimal) to draw components
#' @param fill color (hexadecimal) to draw components
#' @param pal a palette to use if no col/border/etc. are provided. See `[palettes]`
#' @param pch to draw components
#' @param cex to draw components ((`c(2, 1)` by default) for `draw_title`)
#' @param lwd to draw components
#' @param lty to draw components
#' @param transp `numeric` transparency (default:0, min:0, max:1)
#' @param label to indicate first point
#' @param labels \code{character} name of labels to draw (defaut to \code{1:nrow(coo)})
#' @param d `numeric` proportion of `d(centroid-each_point)` to add when centrifugating landmarks
#' @param links `matrix` of links to use to draw segments between landmarks. See `wings$ldk` for an example
#' @param main `character` title (empty by default)
#' @param sub `character` subtitle (empty by default)
#' @param font `numeric` to feed [text] (`c(2, 1)` by default)
#' @param padding `numeric` a fraction of the graphical window (`1/200` by default)
#' @param ... additional options to feed core functions for each drawer
#'
#' @examples
#' bot[1] %>% paper_grid() %>% draw_polygon()
#' olea %>% paper_chess %>% draw_lines(~var)
#'
#' hearts[240] %>% paper_white() %>% draw_outline() %>%
#'   coo_sample(24) %>% draw_landmarks %>% draw_labels() %>%
#'   draw_links(links=replicate(2, sample(1:24, 8)))
#'
#' bot %>%
#'     paper_grid() %>%
#'     draw_outlines() %>%
#'     draw_title("Alcohol abuse \nis dangerous for health", "Drink responsibly")

#' @export
draw_polygon <- function(coo, f, col=par("fg"), fill=NA,  lwd=1, lty=1, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }



  # dispatch drawer argument
  if (missing(fill))
    fills <- this_dispatcher(f, pal_alpha(par("bg"), 1))
  else
    fills <- this_dispatcher(f, fill) %>% pal_alpha(transp)

  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  lwds    <- this_dispatcher(f, lwd)
  ltys    <- this_dispatcher(f, lty)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # draw the outlines as a polygon
  for (i in seq_along(x))
    polygon(x[[i]][, 1], x[[i]][, 2],
            border=cols[i], col=fills[i],
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
draw_points <- function(coo,  f, col=par("fg"), cex=1/2, pch=20, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }

  # dispatch drawer argument
  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  cexs    <- this_dispatcher(f, cex)
  pchs    <- this_dispatcher(f, pch)

  # gr parameters
  old <- par(xpd=NA)
  on.exit(par(old))

  # single shape case (eg for PCA$x)
  if (length(x)==1){
      points(x[[1]][, 1], x[[1]][, 2],
             col=cols,
             cex=cexs, pch=pchs, ...)
  } else {
  # otherwise, draw the points
  for (i in seq_along(x))
    points(x[[i]][, 1], x[[i]][, 2],
           col=cols[i],
           cex=cexs[i], pch=pchs[i], ...)
  }
  # propagate
  invisible(coo)
}

#' @export
#' @rdname drawers
draw_landmarks <- draw_points

#' @export
#' @rdname drawers
draw_lines <- function(coo,  f, col=par("fg"), lwd=1, lty=1, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }

  # dispatch drawer argument
  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  lwds    <- this_dispatcher(f, lwd)
  ltys    <- this_dispatcher(f, lty)

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
draw_centroid <- function(coo, f, col=par("fg"), pch=3, cex=0.5, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }

  # dispatch drawer argument
  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  cexs    <- this_dispatcher(f, cex)
  pchs    <- this_dispatcher(f, pch)

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
draw_firstpoint <- function(coo, f, label="^", col=par("fg"), cex=3/4, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }

  # dispatch drawer argument
  labels  <- this_dispatcher(f, label)
  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  cexs    <- this_dispatcher(f, cex)

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
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

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
draw_links <- function(coo, f, links, col="#99999955", lwd=1/2, lty=1, transp=0, pal=pal_qual, ...){
  # shape case
  if (is_shp(coo))
    x <- list(coo)
  # list and Coo case
  if (is.list(coo)){
    if (is_Coo(coo))
      x <- coo$coo
    else
      x <- coo
  }

  # handle factor
  if (!missing(f)){ # factor case for f is native: if (is.factor(f)) f <- f
    if (is_Coo(coo))
      f <- fac_dispatcher(coo, f)

    # handle palette
    if (missing(col)){
      col <- pal(nlevels(f))
    }

  } else {
    f <- factor(rep(1, length(x)))
  }

  # dispatch drawer argument
  cols    <- this_dispatcher(f, col) %>% pal_alpha(transp)
  lwds    <- this_dispatcher(f, lwd)
  ltys    <- this_dispatcher(f, lty)

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

#' @export
#' @rdname drawers
draw_title <- function(coo, main="", sub="", cex=c(1, 3/4), font=c(2, 1), padding=1/200, ...){
  # preserve the par
  old <- par(xpd=NA)
  on.exit(par(old))
  # deduce coordinates
  u <- par("usr")
  w <- .wdw()
  x_left <- u[1] + w[1]*padding
  y_top_main  <- u[4] - w[2]*padding - strheight(main, cex=cex)
  y_top_sub   <- y_top_main - w[2]*padding*2 - strheight(sub, cex=cex)
  # draw title and sub
  text(x_left, y_top_main, main, cex=cex[1], adj=c(0, 0), font=font[1], ...)
  text(x_left, y_top_sub,  sub,  cex=cex[2], adj=c(0, 0), font=font[2], ...)
  # propagate
  invisible(coo)
}
# draw_text
# draw_radii
# draw_contour
