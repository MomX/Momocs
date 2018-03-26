# .orange   <- "#ffa500"
# .grey90   <- "#e5e5e5"
# .grey60   <- "#999999"

.layerize_PCA <- function(x, f, axes=c(1, 2), palette=pal_qual){
  # grab the selected columns
  xy <- x$x[, axes]
  # prepare a factor
  if (missing(f)){ # no factor provided
    f <- factor(rep(1, nrow(x$x)))
    colors_groups <- rep(par("fg"), nlevels(f))
    colors_rows   <- colors_groups[f]
  } else {         # something provided, handle with fac_dispatcher
    f <- x %>% fac_dispatcher(f)
    if (is.numeric(f)){
      colors_groups <- NA
      colors_rows <- f %>% .normalize()  %>%
        cut(breaks = 1e3)  %>% as.numeric() %>% `[`(palette(1e3), .)
    }
    if (is.factor(f)){
      colors_groups <- palette(nlevels(f))
      colors_rows   <- colors_groups[f]
    }
  }

  # NA handling
  nas <- which(is.na(f))
  if (length(nas)>0){
    xy <- xy[-nas, ]
    f  <- f[-nas]
    colors_groups <- colors_groups[-nas]
    colors_rows   <- colors_rows[-nas]
  }
  # return as a list and
  # add potentially useful other components
  list(xy=xy, f=f,
       colors_groups=colors_groups, colors_rows=colors_rows,
       object="PCA", axes=axes, palette=palette,
       method=x$method, mshape=x$mshape, cuts=x$cuts,
       eig=x$eig, sdev=x$sdev, rotation=x$rotation[, axes],
       baseline1=x$baseline1, baseline2=x$baseline2)
}

### ____plot____ -------------------------------------------
#' Multivariate plots using grindr layers
#'
#' Quickly vizualise [PCA] objects and friends ([LDA]) and build customs plots
#' using the [layers]. See examples.
#'
#' @note This approach will replace \link{plot.PCA} (and `plot.lda` in further versions.
#' This is part of `grindr` approach that may be packaged at some point. All comments are welcome.
#'
#' @param x \code{PCA} object
#' @param f \code{factor}. A column name or number from \code{$fac},
#' or a factor can directly be passed. Accept \code{numeric} as well.
#' @param axes \code{numeric} of length two to select PCs to use
#' (\code{c(1, 2)} by default)
#' @param palette \code{color palette} to use \code{col_summer} by default
#' @param points `logical` whether to draw this with [layer_points]
#' @param points_transp `numeric` to feed [layer_points] (default:0.25)
#' @param morphospace `logical` whether to draw this using [layer_morphospace]
#' @param morphospace_position to feed [layer_morphospace] (default: "range")
#' @param chull `logical` whether to draw this with [layer_chull]
#' @param chullfilled `logical` whether to draw this with [layer_chullfilled]
#' @param labelgroups `logical` whether to draw this with [layer_labelgroups]
#' @param legend `logical` whether to draw this with [layer_legend]
#' @param title `character` if specified, fee [layer_title] (default to `""`)
#' @param center_origin `logical` whether to center origin
#' @param zoom `numeric` zoom level for the frame (default: 0.9)
#' @param eigen `logical` whether to draw this using [layer_eigen]
#' @param box `logical` whether to draw this using [layer_box]
#' @param axesnames `logical` whether to draw this using [layer_axesnames]
#' @param axesvar `logical` whether to draw this using [layer_axesvar]
#' @family grindr
#'
#' @examples
#' ### First prepare two PCA objects.
#'
#' # Some outlines with bot
#' bp <- bot %>% mutate(fake=sample(letters[1:5], 40, replace=TRUE)) %>%
#' efourier(6) %>% PCA
#' plot_PCA(bp)
#' plot_PCA(bp, ~type)
#' plot_PCA(bp, ~fake)
#'
#' # Some curves with olea
#' op <- olea %>%
#' mutate(s=coo_area(.)) %>%
#' filter(var != "Cypre") %>%
#' chop(~view) %>% lapply(opoly, 5, nb.pts=90) %>%
#' combine %>% PCA
#' op$fac$s %<>% as.character() %>% as.numeric()
#'
#' op %>% plot_PCA(title="hi there!")
#'
#' ### Now we can play with layers
#' # and for instance build a custom plot
#' # it should start with plot_PCA()
#'
#' my_plot <- function(x, ...){
#'
#' x %>%
#'     plot_PCA(...) %>%
#'     layer_points %>%
#'     layer_ellipsesaxes %>%
#'     layer_rug
#' }
#'
#' # and even continue after this function
#' op %>% my_plot(~var, axes=c(1, 3)) %>%
#'     layer_title("hi there!") %>%
#'     layer_stars()
#'
#' # You get the idea.
#' @export
plot_PCA <- function(x, f, axes=c(1, 2),
         palette=pal_qual,
         points=TRUE,
         points_transp=1/4,
         # morphospace
         morphospace=TRUE,
         morphospace_position="range",
         # chulls
         chull=TRUE,
         chullfilled=FALSE,
         # legends
         labelgroups=FALSE,
         legend=TRUE,
         # cosmetics (mainly)
         title="",
         center_origin=TRUE, zoom=0.9,
         eigen=TRUE,
         box=TRUE,
         axesnames=TRUE, axesvar=TRUE){

  # prepare ---------------------------
  if (missing(f)){
    x %<>% .layerize_PCA(axes=axes, palette=palette)
    labelgroups <- legend <- FALSE
  } else {
    x %<>% .layerize_PCA(f, axes=axes, palette=palette)
  }

  # frame
  x %<>%
    layer_frame(center_origin=center_origin, zoom = zoom) %>%
    layer_axes()

  # cosmetics
  if (axesnames)
    x %<>% layer_axesnames()

  if (axesvar)
    x %<>% layer_axesvar()

  if (eigen)
    x %<>% layer_eigen()

  if (box)
    x %<>% layer_box()

  # morphospace -----------------------
  if (morphospace)
    x %<>% layer_morphospace(position = morphospace_position)

  # data ------------------------------
  if (points)
    x %<>% layer_points(transp=points_transp)

  # groups dispersion -----------------
  if (chull)
    x %<>% layer_chull()

  if (chullfilled)
    x %<>% layer_chullfilled()

  # legends
  if (legend)
    x %<>% layer_legend()

  if (labelgroups)
    x %<>% layer_labelgroups()

  if (title != "")
    x %<>% layer_title(title)

  # propagate
  invisible(x)
}

# bot %>% efourier(6) %>% PCA %>% plot_PCA
### _____Layers_____ ---------------------------------------
# frame and options ----------------------------------------

#' grindr layers for multivariate plots
#'
#' Useful layers for building custom
#' mutivariate plots using the cheapbabi approach. See examples.
#'
#' @name layers
#' @rdname layers
#' @seealso grindr_drawers
#' @family grindr
#'
#' @param x a list, typically returned by \link{plot_PCA}
#' @param center_origin \code{logical} whether to center the origin (default \code{TRUE})
#' @param zoom \code{numeric} to change the zoom (default \code{0.9})
#' @export
layer_frame <- function(x, center_origin = TRUE, zoom = 0.9){
  # neater par
  old <- par(mar=rep(1/8, 4))
  on.exit(par(old))
  xy <- x$xy
  if (center_origin) {
    w <- (1/zoom) * max(abs(xy))
    plot(NA, xlim = c(-w, w), ylim = c(-w, w), asp = 1,
         axes = FALSE, frame = FALSE, mar=rep(0, 4))
  }
  else {
    w <- (1/zoom) * apply(xy, 2, range)
    plot(xy, xlim = w[, 1], ylim = w[, 2],
         type = "n", asp = 1, axes = FALSE, frame = FALSE)
  }
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param col color (hexadecimal) to use for drawing components
#' @param lwd linewidth for drawing components
#' @param ... additional options to feed core functions for each layer
layer_axes <- function(x, col="#999999", lwd=1/2, ...){
  # neater par
  old <- par(mar=rep(1/8, 4))
  on.exit(par(old))
  # add x=0 and y=0 lines for axes
  abline(h=0, v=0, col=col, lwd=lwd, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param lty linetype for drawing components
#' @param grid \code{numeric} number of grid to draw
layer_grid <- function(x, col="#999999", lty=3, grid = 3, ...) {
  # neater par
  old <- par(mar=rep(1/8, 4))
  on.exit(par(old))
  m <- max(abs(par("usr")))
  g <- seq(0, m, length = grid)
  g <- c(g[-1] * -1, 0, g[-1])
  abline(h=g, v=g, col = col, lty = lty, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param border color (hexadecimal) to use to draw border
layer_box <- function(x, border="#e5e5e5", ...){
  # neater par
  old <- par(mar=rep(1/8, 4))
  on.exit(par(old))
  w <- par("usr")
  # draw the box
  rect(w[1], w[3], w[2], w[4], border=border, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_fullframe <- function(x, ...){
  x %>%
    layer_frame(...) %>%
    layer_grid() %>%
    layer_axes() %>%
    layer_box() %>%
    layer_axesvar() %>%
    layer_axesnames()
}

# shapes ---------------------------------------------------
#' @export
#' @rdname layers
#' @param pch to use for drawing components
#' @param cex to use for drawing components
#' @param transp transparency to use (min: 0 defaut:0 max:1)
layer_points <- function(x, pch=20, cex=4/log1p(nrow(x$xy)), transp=0, ...){
  points(x$xy, col=pal_alpha(x$colors_rows, transp=transp), pch=pch, cex=cex, ...)
  # propagate
  invisible(x)
}

# morphospace ----------------------------------------------
#' @export
#' @rdname layers
#' @param position one of \code{range, full, circle,
#' xy, range_axes, full_axes)} to feed \link{morphospace_positions} (default \code{range})
#' @param nb \code{numeric} total number of shapes when \code{position="circle"} (default \code{12})
#' @param nr \code{numeric} number of rows to position shapes (default \code{6})
#' @param nc \code{numeric} number of columns to position shapes (default \code{5})
#' @param rotate \code{numeric} angle (in radians) to rotate shapes
#' when displayed on the morphospace (default \code{0})
#' @param size \code{numeric} size to use to feed \link{coo_template} (default \code{0.9})
#' @param flipx \code{logical} whether to flip shapes against the x-axis (default \code{FALSE})
#' @param flipy \code{logical} whether to flip shapes against the y-axis (default \code{FALSE})
#' @param draw \code{logical} whether to draw shapes (default \code{TRUE})
layer_morphospace <-
  function(x,
           position=c("range", "full", "circle",
                      "xy", "range_axes", "full_axes")[1],
           nb=12, nr=6, nc=5,
           rotate=0, size=0.9,
           col="#999999",
           flipx=FALSE, flipy=FALSE, draw=TRUE, ...){
    # shortcut for useful components
    xy <- x$xy
    rot <- x$rotation
    mshape <- x$mshape
    method <- x$method

    # number of methods must be <=4
    # message and propagate anyway
    if (is.null(method) || length(method)>4) {
      message("layer_morphospace needs a $method of length <= 4")
      invisible(x)
    }

    # position of shapes to reconstruct
    pos <- morphospace_positions(xy=xy, pos.shp = position,
                      nb.shp = nb, nr.shp = nr, nc.shp = nc)

    # according to the type of morphometric method,
    # switch the inverse method and the way we draw shapes
    # (ie draw_outline, draw_curve, draw_landmarks for Out, Opn, Ldk, respectively).
    #
    # when the object combines different morphometric approaches (up to 4)
    # their size is divided by 2 and the shapes and translated (of d)
    # from the (x; y) coordinates from pos.shp,
    # of a distance d below and towards the appropriate direction

    # deduce the number of methods and
    # and recycle shorter arguments
    ml <- length(method)
    if (length(rotate) != ml)
      rotate <- rep(rotate[1], ml)
    if (length(flipx) != ml)
      flipx <- rep(flipx[1], ml)
    if (length(flipy) != ml)
      flipy <- rep(flipy[1], ml)
    if (length(size) != ml)
      size <- rep(size[1], ml)

    # calculate the final templated size for shapes
    # divide by two if arranged, eg if more than one shape
    wdw <- .wdw() %>% max()
    size <- (size*wdw/14) / ifelse(ml==1, 1, 2)

    # defines the x and y translation distances
    # for every sub-morphoshape
    d <- mean(size) / 2 # gap distance
    if (ml==1){
      dx <- 0
      dy <- 0
    }
    if (ml==2){ #met1 on top of met2 - h center
      dx <- c(0, 0)
      dy <- c(d, -d)
    }
    if (ml==3){ #podium arrangement
      dx <- c(0, -d, d)
      dy <- c(d, -d, -d)
    }
    if (ml==4){ #form top left, clockwise
      dx <- c(-d, d, -d, d)
      dy <- c(d, d, -d, -d)
    }

    # indices of coe partition to successively use
    if (ml==1){
      col.start <- 1
      col.end   <- length(mshape)
    } else {
      col.start <- cumsum(x$cuts) - x$cuts + 1
      col.end   <- cumsum(x$cuts)
    }

    # to store the shapes
    SHP <- vector("list", length(method)) %>% `names<-`(method)

    ### loop over method and calculate reconstructed shapes
    for (i in seq_along(method)){
      # to store local results
      shp <- vector("list", nrow(pos))
      ids <- col.start[i]:col.end[i]
      mi <- method[i]

      # deduce the plotting method
      if (grepl("(e|r|s|t)fourier", mi))
        draw_method <- draw_outline
      if (grepl("(dfourier)|(poly)", mi))
        draw_method <- draw_curve
      if (grepl("procrustes", mi))
        draw_method <- draw_landmarks

      # method dispatch
      # efourier
      if (mi == "efourier")
        shp <- PCA2shp_efourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
      # rfourier
      if (mi == "rfourier")
        shp <- PCA2shp_rfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
      # sfourier
      if (mi == "sfourier")
        shp <- PCA2shp_sfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
      # tfourier
      if (mi == "tfourier")
        shp <- PCA2shp_tfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
      # dfourier
      if (mi == "dfourier")
        shp <- PCA2shp_dfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids])
      # opoly
      if (mi == "opoly")
        shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                   ortho = TRUE,
                                   baseline1 = x$baseline1[1:2 + (i-1)*2],
                                   baseline2 = x$baseline2[1:2 + (i-1)*2])
      # npoly
      if (mi == "npoly")
        shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                   ortho = FALSE,
                                   baseline1 = x$baseline1[1:2 + (i-1)*2],
                                   baseline2 = x$baseline2[1:2 + (i-1)*2])
      # landmarks
      if (mi == "procrustes")
        shp <- PCA2shp_procrustes(pos = pos, rot = rot[ids, ])

      # reconstructed shapes are now waiting in shp
      # for templating, translating and friends
      shp %<>%
        # template to the final size
        lapply(coo_template, size = size[i]) %>%
        # coo_template does not center shapes but the bounding box
        lapply(coo_center) %>%
        # rotate shapes
        lapply(coo_rotate, rotate[i])
      # flip (if required)
      if (flipx[i]) shp %<>% lapply(coo_flipx)
      if (flipy[i]) shp %<>% lapply(coo_flipy)
      # finally translate shapes
      if (draw) { # to translate only for morphospace PCA, not PCcontrib, etc.
        shp %<>%
          seq_along() %>%
          lapply(function(.) coo_trans(shp[[.]], pos[., 1] + dx[i], pos[., 2] + dy[i]))
      }
      SHP[[i]] <- shp

      # finally draw the morphospace
      if (draw)
        lapply(shp, draw_method, col=col)
      # if (!is.null(PCA$links)) lapply(shp, function(x) ldk_links(x, PCA$links, col="grey90"))
    }
    # propagate
    if (draw)
      invisible(x)
    else
      return(SHP)
  }

# ellipses layers ------------------------------------------
#' @export
#' @rdname layers
#' @param conf \code{numeric} between 0 and 1 for confidence ellipses
#' @param alpha \code{numeric} between 0 and 1 for the transparency of components
layer_ellipses <- function(x, conf=0.5, lwd=1, alpha=0, ...) {
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    invisible(x)
  # if conf and lwd lengths dont match, recycle
  if (length(conf)!=length(lwd))
    lwd <- rep(lwd[1], length(conf))
  # if conf and alpha lengths dont match, recycle
  if (length(conf)!=length(alpha))
    alpha <- rep(alpha[1], length(conf))
  # loop along all levels
  for (i in seq_along(levels(x$f))) {
    xy_i <- x$xy[x$f == levels(x$f)[i], ,drop=FALSE]
    # with less than 3 points in a group,
    # conf_ell would fail
    if (nrow(xy_i)>2) {
      # loop along conf levels
      for (j in seq_along(conf)){
        conf_ell(x = xy_i, conf = conf[j])$ell %>%
          coo_close %>%
          draw_lines(col=x$colors_groups[i] %>%
                       pal_alpha(alpha[j]), ...)
      }
    }
  }
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_ellipsesfilled <- function(x, conf=0.5, lwd=1, alpha=0, ...) {
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    invisible(x)
  # if conf and lwd lengths dont match, recycle
  if (length(conf)!=length(lwd))
    lwd <- rep(lwd[1], length(conf))
  # if conf and alpha lengths dont match, recycle
  if (length(conf)!=length(alpha))
    alpha <- rep(alpha[1], length(conf))

  # loop along all levels
  for (i in seq_along(levels(x$f))) {
    xy_i <- x$xy[x$f == levels(x$f)[i], ,drop=FALSE]
    # with less than 3 points in a group,
    # conf_ell would fail
    if (nrow(xy_i)>2) {
      # loop along conf levels
      for (j in seq_along(conf)){
        conf_ell(x = xy_i, conf = conf[j])$ell %>%
          coo_close %>%
          draw_polygon(fill=NA,
                       col=x$colors_groups[i] %>%
                         pal_alpha(alpha[j]), ...)
      }
    }
  }
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_ellipsesaxes <- function(x, conf=0.5, lwd=1, alpha=0, ...) {
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    invisible(x)
  # if conf and lwd lengths dont match, recycle
  if (length(conf)!=length(lwd))
    lwd <- rep(lwd[1], length(conf))
  # if conf and alpha lengths dont match, recycle
  if (length(conf)!=length(alpha))
    alpha <- rep(alpha[1], length(conf))

  # loop along all levels
  for (i in seq_along(levels(x$f))) {
    xy_i <- x$xy[x$f == levels(x$f)[i], ,drop=FALSE]
    # with less than 3 points in a group,
    # conf_ell would fail
    if (nrow(xy_i)>2) {
      # loop along conf levels
      for (j in seq_along(conf)){
        seg_i <- conf_ell(x = xy_i, conf = conf[j], nb.pts = 360)$seg
        segments(seg_i[1, 1], seg_i[1, 2],
                 seg_i[2, 1], seg_i[2, 2],
                 lwd = lwd[j],
                 col=x$colors_groups[i] %>%
                   pal_alpha(alpha[j]), lend=2, ...)
        segments(seg_i[3, 1], seg_i[3, 2],
                 seg_i[4, 1], seg_i[4, 2],
                 lwd = lwd[j],
                 col=x$colors_groups[i] %>%
                   pal_alpha(alpha[j]), lend=2, ...)

      }
    }
  }
  # propagate
  invisible(x)
}

# chull layers ---------------------------------------------
#' @export
#' @rdname layers
layer_chull <- function(x, ...){
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    invisible(x)
  # loop along levels and draw
  for (i in seq_along(levels(x$f))) {
    coo <- x$xy[x$f == levels(x$f)[i],, drop=FALSE]
    # with less than 3 points in a group,
    # coo_chull would fail
    if (nrow(coo)>2) {
      coo %>% coo_chull() %>% coo_close %>%
        draw_lines(col=x$colors_groups[i], ...)
    }
  }
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_chullfilled <- function(x, alpha=0.8, ...){
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    invisible(x)
  # loop along levels and draw
  for (i in seq_along(levels(x$f))) {
    coo <- x$xy[x$f == levels(x$f)[i],, drop=FALSE]
    if (nrow(coo)>2) {
      # with less than 3 points in a group,
      # coo_chull would fail
      coo %>% coo_chull() %>% coo_close %>%
        draw_polygon(fill=x$colors_groups[i], col=x$colors_groups[i], transp = alpha, ...)
    }
  }
  # propagate
  invisible(x)
}

# stars layers ---------------------------------------------
#' @export
#' @rdname layers
layer_stars <- function(x, alpha=0.5, ...) {
  # if numeric, apply with a local trick on f
  if (!is.factor(x$f)){
    f <- factor(rep(1, length(x$f)))
    colors_groups <- "#000000"
  } else {
    f <- x$f
    colors_groups <- x$colors_groups
  }

  # loop along levels and draw
  for (i in seq_along(levels(f))) {
    xy_i <- x$xy[f == levels(f)[i],, drop=FALSE]
    c_i <- coo_centpos(xy_i)
    for (j in 1:nrow(xy_i)) {
      segments(c_i[1], c_i[2],
               xy_i[j, 1], xy_i[j, 2],
               col = pal_alpha(colors_groups[i], alpha), ...)
    }
  }
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_delaunay <- function(x, ...){
  # if numeric, apply with a local trick on f
  if (!is.factor(x$f)){
    f <- factor(rep(1, length(x$f)))
    colors_groups <- "#000000"
  } else {
    f <- x$f
    colors_groups <- x$colors_groups
  }

  # loop along levels and draw
  for (i in seq_along(levels(f))) {
    xy_i <- x$xy[f == levels(f)[i],, drop=FALSE]
    # with less than 3 points in a group,
    # coo_chull would fail
    if (nrow(xy_i)>2) {
      xy_i %>% geometry::delaunayn() %>%
      {rbind(.[, -1], .[, -2], .[, -3])} %>%
        `[`(-duplicated(.), ) %>%
        draw_links(xy_i, ., col=colors_groups[i], ...)
    }
  }
  # propagate
  invisible(x)
}


# density layers -------------------------------------------
#' @export
#' @rdname layers
#' @param levels_density \code{numeric} number of levels to use to feed \code{MASS::kde2d}
#' @param levels_contour \code{numeric} number of levels to use to feed \code{graphics::contour}
#' @param n \code{numeric} number of grid points to feed \code{MASS::kde2d}
#' @param density \code{logical} whether to draw density estimate
#' @param contour \code{logical} whether to draw contour lines
layer_density <- function(x, levels_density=20,
                          levels_contour=4, alpha=1/3,
                          n = 200, density=TRUE, contour=TRUE) {
  # if numeric, apply with a local trick on f
  if (!is.factor(x$f)){
    f <- factor(rep(1, length(x$f)))
    colors_groups <- "#000000"
  } else {
    f <- x$f
    colors_groups <- x$colors_groups
  }

  # neater par
  old <- par(mar=rep(1/8, 4), xpd=NA)
  on.exit(par(old))
  # loop over levels and use MASS::kde2d
  for (i in seq_along(levels(f))) {
    xy_i <- x$xy[f == levels(f)[i],, drop=FALSE]
    ki <- MASS::kde2d(xy_i[, 1], xy_i[, 2], n = n, lims = c(par("usr")))
    ki$z %<>% .normalize()
    if (density)
      graphics::image(ki$x, ki$y, ki$z, add = TRUE,
                      xlim = range(ki$x), ylim = range(ki$y),
                      col = pal_manual(colors_groups[i], transp=alpha)(levels_density))
    if (contour)
      graphics::contour(ki$x, ki$y, ki$z, add = TRUE,
                        nlevels = levels_contour,
                        col=pal_manual(colors_groups[i], transp=alpha)(levels_contour))
  }
  # propagate
  invisible(x)
}

# label layers ---------------------------------------------
#' @export
#' @rdname layers
#' @param font to feed \link{text}
#' @param abbreviate \code{logical} whether to abbreviate names
layer_labelpoints <- function(x, col=par("fg"), cex=2/3,
                              font=1, abbreviate=FALSE, ...){
  if (missing(col))
    col <- x$colors_rows

  # prepare labs
  labs <- rownames(x$xy)
  if (abbreviate)
    labs %<>% abbreviate(minlength = 1)

  # draw labels
  text(x$xy[, 1], x$xy[, 2],
       labels = labs, col = col,
       cex = cex, font = font, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param rect \code{logical} whether to draw a rectangle below names
layer_labelgroups <- function(x, col=par("fg"), cex=3/4, font=2,
                              rect=TRUE, alpha=1/4, abbreviate=FALSE, ...){
  # if numeric, do not apply this layer but still propagate
  if (!is.factor(x$f))
    return(x)
  cxys <- apply(x$xy, 2, function(.) tapply(., x$f, mean))
  # no f or single level fac
  if (!is.matrix(cxys))
    cxys %<>%
    matrix(nrow=1, ncol=2) %>%
    `row.names<-`(levels(x$f))

  # prepare labs
  labs <- rownames(cxys)
  if (abbreviate)
    labs %<>% abbreviate(minlength = 1)

  # if rect, calculate their dimensions and draw them
  p <- strheight(labs[1])
  if (rect) {
    w <- strwidth(labs, cex = cex)
    h <- strheight(labs, cex = cex)
    rect(xleft   = cxys[, 1] - w/2 - p,
         ybottom = cxys[, 2] - h/2 + p,
         xright  = cxys[, 1] + w/2 + p,
         ytop    = cxys[, 2] + h/2 + p,
         col     = pal_alpha("#FFFFFF", alpha), border = NA)
  }
  # draw labels
  text(cxys[, 1], cxys[, 2] + p,
       labels = labs, col = x$colors_groups,
       cex = cex, font = font, ...)
  # propagate
  invisible(x)
}

# meta layers ----------------------------------------------
#' @export
#' @rdname layers
layer_rug <- function(x, size=1/200, ...){
  # if numeric, apply with a local trick on f
  if (!is.factor(x$f)){
    f <- factor(rep(1, length(x$f)))
    colors_groups <- "#000000"
  } else {
    f <- x$f
    colors_groups <- x$colors_groups
  }
  # neater par
  old <- par(mar=rep(1/8, 4))
  on.exit(par(old))
  # tick size
  h <- size*max(.wdw())
  for (i in seq_along(levels(f))){
    rug(x$xy[f == levels(f)[i], 1], ticksize = h, side=1,
        col=colors_groups[i], lend=1, quiet=TRUE)
    rug(x$xy[f == levels(f)[i], 2], ticksize = h, side=2,
        col=colors_groups[i], lend=1, quiet=TRUE)
  }
  #propagate
  invisible(x)
}


# cosmetics layers -----------------------------------------
#' @export
#' @rdname layers
#' @param title to add to the plot (default \code{""})
layer_title <- function(x, title="", cex=3/4, ...) {
  # neater par
  old <- par(mar=rep(1/8, 4), xpd=NA)
  on.exit(par(old))
  text(par("usr")[1], par("usr")[4] - strheight(title),
       pos = 4, labels = title, font = 2, cex = cex, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param name to use on axes (default \code{"Axis"})
layer_axesnames <- function(x, cex=3/4, name="Axis", ...){
  # neater par
  old <- par(mar=rep(1/8, 4), xpd=NA)
  on.exit(par(old))
  # calculate dimensions
  gy <- strheight(paste(name, 1), cex = cex)/1.5
  gx <- strwidth("00.0%", cex = cex)/1.5
  # add axes names
  text(par("usr")[2] - gx, gy,
       col = "grey50", cex = cex,
       labels = paste(name, x$axes[1]), ...)
  text(-gy, par("usr")[4] - gx,
       col = "grey50", cex = cex,
       labels = paste(name, x$axes[2]), srt = 90, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param nb_max \code{numeric} number of eigen values to display (default \code{5})
layer_eigen <- function(x, nb_max=5, cex=1/2, ...){
  # if non pertinent, propagate
  if (is.null(x$sdev))
    invisible(x)
  # neater par
  old <- par(mar=rep(0, 4), xpd=NA)
  on.exit(par(old))

  # default dimensions
  range_padding = 1/50
  range_width   = 1/20
  range_height  = 1/10
  # window dimensions
  u <- par("usr")
  w <- .wdw()
  # deduce subplot bounding box
  x0 <- u[2] - w[1]*(range_width+range_padding)
  x1 <- u[2] - w[1]*range_padding
  y0 <- u[3] + w[2]*range_padding
  y1 <- u[3] + w[2]*(range_height+range_padding)
  xs_left  <- seq(x0, x1, length.out = nb_max+1)[-(nb_max+1)]
  xs_right <- xs_left + diff(xs_left[1:2])
  s_ys <- seq(y0, y1, length.out = 3)

  # handle bars selection, height and colors
  if (max(x$axes)>nb_max)
    nb_max <- max(x$axes)
  var <- x$sdev^2
  ev <- var/sum(var)
  h0 <- ev[1:nb_max]
  hb <- h0*range_height*w[2]*2 # *2 because of 0.5 below
  cols <- rep("grey98", nb_max)
  cols[x$axes] <- "grey60"

  # axis 1 ticks
  segments(x0, s_ys, x0 - range_width*w[2]/6, s_ys, lwd=1/2)
  text(x0, s_ys, seq(0, 0.5, length.out = 3), cex=cex, pos=2, adj=1)
  # draw bars
  rect(xs_left, y0, xs_right, y0+hb, col=cols, lwd=1/2)

  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
layer_axesvar <- function(x, cex=3/4, ...){
  # neater par
  old <- par(mar=rep(1/8, 4), xpd=NA)
  on.exit(par(old))
  # calculate variance
  var <- x$sdev^2
  var <- signif(100 * var/sum(var), 3)
  # and dimensions of labels on window
  gx <- strwidth("00.0%", cex = cex)/1.5
  gy <- strheight("00.0%", cex = cex)/1.5
  # add it below the axes
  text(par("usr")[2] - gx, -gy,
       col = "grey50", cex = cex,
       labels = paste0(var[x$axes[1]], "%"), ...)
  text(gy, par("usr")[4] - gx,
       col = "grey50", cex = cex,
       labels = paste0(var[x$axes[2]], "%"), srt = 90, ...)
  # propagate
  invisible(x)
}

#' @export
#' @rdname layers
#' @param probs \code{numeric} sequence to feed \code{stats::quantile}
#' and to indicate where to draw ticks and legend labels
layer_legend <- function(x, probs=seq(0, 1, 0.25), cex=3/4,  ...){
  # neater par
  old <- par(mar=rep(0, 4), xpd=NA)
  on.exit(par(old))
  # default dimensions
  range_padding = 1/30
  range_width   = 1/60
  range_height  = 1/8
  # window dimensions
  u <- par("usr")
  w <- .wdw()
  # factor case
  # simple legend
  if (is.factor(x$f)){
    wid_leg <- levels(x$f) %>% strwidth(cex=cex) %>% max
    x0 <- u[2] - w[1]*(range_padding+range_width) - wid_leg*1.5
    y0 <- u[4] - w[2]*range_padding
    legend(x0, y0, legend = levels(x$f), fill = x$colors_groups,
           border=par("fg"), bty="n", cex = cex)
  }
  # numeric case
  # redraw continuous scale manually
  if (is.numeric(x$f)){
    qf <- stats::quantile(x$f, probs = probs) %>% signif(2)
    wid_leg <- qf %>% strwidth(cex=cex) %>% max()
    # scale position
    s_y0 <- u[4] - w[2]*(range_padding+range_height)
    s_y1 <- u[4] - w[2]*range_padding
    s_x0 <- u[2] - w[1]*(range_padding+range_width) - wid_leg*1.5
    s_x1 <- u[2] - w[1]*range_padding - wid_leg
    # scale_bars positions
    s_ys <- seq(s_y0, s_y1, length.out = 100)
    s_yg <- diff(s_ys[1:2])
    # legend text position
    t_x0 <- s_x1 + w[2]*(range_padding + range_width) - wid_leg*0.9
    t_ys <- s_y0 + probs*(range_height*w[2])
    rect(s_x0, s_ys,
         s_x1, s_ys+s_yg,
         col=x$palette(100), lwd=0)
    rect(s_x0, s_ys[1],
         s_x1, (s_ys+s_yg)[length(s_ys)],
         lwd=1/2, col=NA, border=par("fg"))
    segments(s_x1, t_ys + s_yg*0.5,
             s_x1 + range_width*w[2]/4, t_ys + s_yg*0.5, lwd=1/2)
    text(t_x0, t_ys,
         qf, cex=cex, pos=4)
  }
  # propagate
  invisible(x)
}


#' # add loading vectors
#' .loadings <- function(loadings.mat, d = 1, d.lab = 1.2, col = "red") {
#'   loadings.mat <- loadings.mat * d
#'   loadings.lab <- loadings.mat * d.lab
#'   arrows(0, 0, loadings.mat[, 1], loadings.mat[, 2], angle = 20,
#'          length = 0.1, col = col)
#'   text(loadings.lab[, 1], loadings.lab[, 2], labels = rownames(loadings.lab),
#'        cex = 0.8, col = col)
#' }
