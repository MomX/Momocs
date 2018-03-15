##### Main Coo (Out/Opn/Ldk) plotters

# plot ----------------------------------------------------
#' Graphical inspection of shapes
#'
#' Allows to plot shapes, individually, for \link{Coo} (\link{Out}, \link{Opn} or \link{Ldk}) objects.
#' @param x the \link{Coo} object
#' @param id the id of the shape to plot, if not provided a
#' random shape is plotted. If passed with \code{'all'} all shapes are plotted,
#' one by one.
#' @param ... further arguments to be passed to \link{coo_plot}
#' @family Coo_graphics
#' @examples
#' \dontrun{
#' inspect(bot, 5)
#' inspect(bot)
#' inspect(bot, 5, pch=3, points=TRUE) # an example of '...' use
#' }
#' @export
inspect <- function(x, id, ...){
  UseMethod("inspect")
}

#' @export
inspect.Coo <- function(x, id, ...) {
  Coo <- x
  if (missing(id)) {
    repeat {
      Coo1 <- sample_n(x, 1)
      Coo1 %>% stack(title=names(Coo1), coo_sample=NULL)
      readline(prompt = "Press <Enter> to continue, <Esc> to quit...")
    }
  }
  if (id[1] == "all") {
    id <- 1:length(Coo)
  }
  if (is.numeric(id)) {
    if (length(id) == 1) {
      slice(Coo, id) %>% stack(main = names(Coo)[id], ...)
    } else {
      for (i in seq(along = id)) {
        slice(Coo, id) %>% stack(main = names(Coo)[id], ...)
        readline(prompt = "Press <Enter> to continue, <Esc> to quit...")
      }
    }
  }
}

# stack ----------------------------------------------------
#' Family picture of shapes
#'
#' Plots all the outlines, on the same graph, from a \link{Coo} (\link{Out}, \link{Opn} or \link{Ldk})
#' object.
#' @param x The \code{Coo} object to plot.
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param fac a factor within the $fac slot for colors
#' @param palette a color palette to use when fac is provided
#' @param coo_sample if not NULL the number of point per shape to display (to plot quickly)
#' @param points \code{logical} whether to draw or not points
#' @param first.point \code{logical} whether to draw or not the first point
#' @param centroid \code{logical} whether to draw or not the centroid
#' @param ldk \code{logical}. Whether to display landmarks (if any).
#' @param ldk_pch \code{pch} for these landmarks
#' @param ldk_col color for these landmarks
#' @param ldk_cex \code{cex} for these landmarks
#' @param meanshape \code{logical} whether to add meanshape related stuff (below)
#' @param meanshape_col a color for everything meanshape
#' @param ldk_links \code{logical} whether to draw links (of the mean shape)
#' @param ldk_confell \code{logical} whether to draw conf ellipses
#' @param ldk_contour \code{logical} whether to draw contour lines
#' @param ldk_chull \code{logical} whether to draw convex hull
#' @param ldk_labels \code{logical} whether to draw landmark labels
#' @param slidings \code{logical} whether to draw slidings semi landmarks
#' @param slidings_pch \code{pch} for semi landmarks
#' @param xy.axis whether to draw or not the x and y axes
#' @param title a title for the plot. The name of the \code{Coo} by default
#' @param ... further arguments to be passed to \link{coo_plot}
#' @family Coo_graphics
#' @examples
#' \dontrun{
#' stack(bot)
#' bot.f <- efourier(bot, 12)
#' stack(bot.f)
#' stack(mosquito, borders='#1A1A1A22', first.point=FALSE)
#' stack(hearts)
#' stack(hearts, ldk=FALSE)
#' stack(hearts, borders='#1A1A1A22', ldk=TRUE, ldk_col=col_summer(4), ldk_pch=20)
#' stack(hearts, fac="aut", palette=col_sari)
#'
#' chaffal <- fgProcrustes(chaff)
#' stack(chaffal, slidings=FALSE)
#' stack(chaffal, meanshape=TRUE, meanshape_col="blue")
#' }
#' @rdname stack.Coo
#' @aliases stack.Coo
#' @aliases stack
#' @name stack
#' @export
stack.Coo <-
  function(x,
           cols, borders,
           fac, palette = col_summer,
           coo_sample=120,
           points = FALSE, first.point = TRUE, centroid = TRUE,
           ldk = TRUE,
           ldk_pch = 3, ldk_col = "#FF000055",
           ldk_cex = 0.5, ldk_links = FALSE,
           ldk_confell = FALSE, ldk_contour = FALSE,
           ldk_chull = FALSE, ldk_labels = FALSE,
           xy.axis = TRUE, title=substitute(x), ...) {
    message("will soon be deprecated, see ?pile")
    Coo <- x
    # downsize
    if (is.numeric(coo_sample)) {
      if (all(coo_nb(Coo) >= coo_sample)) {
        Coo <- suppressMessages(coo_sample(Coo, coo_sample))
      }
    }

    # we handle for missing cols
    if (missing(cols)) {
      cols <- rep(NA, length(Coo))
    }
    # or when provided fro an irregular lenght
    if (length(cols) != length(Coo)) {
      cols <- rep(cols[1], length(Coo))
    }
    # same thing for borders
    if (missing(borders)) {
      borders <- rep("#0000003F", length(Coo))
    }
    if (length(borders) != length(Coo)) {
      borders <- rep(borders[1], length(Coo))
    }
    # but if fac is provided
    if (!missing(fac)){
      fac <- fac_dispatcher(Coo, fac)
      cols <- NA
      borders <- palette(nlevels(fac))[fac]
    }

    # we define local par (margins)
    op <- par(mar = c(3, 3, 2, 1))
    on.exit(par(op))
    # we calculate data range
    wdw <- apply(do.call(rbind, Coo$coo), 2, range)
    plot(NA, type = "n",
         asp = 1, xlim = wdw[, 1], ylim = wdw[, 2],
         las = 1, cex.axis = 2/3,
         ann = TRUE, frame = FALSE,  main=title)
    if (xy.axis) {
      abline(h = 0, v = 0, col = "grey80", lty = 2)
    }
    # should be lapply-ed but how to keep cols/borders ?
    if (ldk & length(Coo$ldk) != 0){
      ldks <- get_ldk(Coo)
    }
    for (i in 1:length(Coo)) {
      coo_draw(Coo$coo[[i]], col = cols[i], border = borders[i],
               points = points, first.point = TRUE, centroid = centroid)
      if (ldk & is_ldk(Coo)) {
        points(ldks[[i]][, 1], ldks[[i]][ ,2], pch = ldk_pch,
               col = ldk_col, cex = ldk_cex)
      }
    }
  }

#' @rdname stack.Coo
#' @export
stack.Ldk <- function(x, cols, borders, first.point = TRUE, centroid = TRUE,
                      ldk = TRUE, ldk_pch = 20, ldk_col=col_alpha("#000000", 0.5), ldk_cex = 0.3,
                      meanshape = FALSE, meanshape_col="#FF0000",
                      ldk_links = FALSE, ldk_confell = FALSE, ldk_contour = FALSE,
                      ldk_chull = FALSE, ldk_labels = FALSE,
                      slidings=TRUE, slidings_pch="", xy.axis = TRUE, title=substitute(x), ...) {
  message("will soon be deprecated, see ?pile")
  Coo <- x
  if (missing(cols)) {
    cols <- rep(NA, length(Coo))
  }
  if (length(cols) != length(Coo)) {
    cols <- rep(cols[1], length(Coo))
  }
  if (missing(borders)) {
    borders <- rep("#33333355", length(Coo))
  }
  if (length(borders) != length(Coo)) {
    borders <- rep(borders[1], length(Coo))
  }
  op <- par(mar = c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Coo$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim = wdw[, 1], ylim = wdw[, 2], asp = 1, las = 1,
       cex.axis = 2/3, ann = FALSE, frame = FALSE)
  title(title)
  if (xy.axis) {
    abline(h = 0, v = 0, col = "grey80", lty = 2)
  }
  # semilandmarks lines
  if (slidings & is_slidings(Coo)){
    sl <- get_slidings(Coo)
    for (i in 1:length(sl)) {
      lapply(sl[[i]], lines, col=col_alpha("#000000", 0.9))
      lapply(sl[[i]], points, col=col_alpha("#000000", 0.5), pch=slidings_pch)
    }
  }
  # points
  # for (i in 1:length(Coo)) {
  #   points(Coo$coo[[i]], pch = ldk_pch, col = ldk_col, cex = ldk_cex)
  # }
  lapply(get_ldk(Coo), points, pch = ldk_pch, col = ldk_col, cex = ldk_cex)
  # semilandmarks
  # if (is_slidings(Coo)){
  #   cur_binded <- get_cur_binded(Coo)
  #   for (i in 1:length(Coo)) {
  #     points(cur_binded[[i]], pch = cur_pch, col = ldk_col, cex = ldk_cex*0.25)
  #   }
  # }
  # Specific to Ldk not very clean below
  # A <- l2a(Coo$coo)
  # mA <- mshapes(A)

  if (meanshape){
    A <- l2a(get_ldk(Coo))
    mA <- mshapes(A)
    if (ldk_confell) {
      ldk_confell(A, conf = 0.9, col=meanshape_col)
    }
    if (ldk_contour) {
      ldk_contour(A, nlevels = 3, col = meanshape_col)
    }
    if (ldk_chull) {
      ldk_chull(A, col = meanshape_col)
    }
    if (ldk_links | missing(ldk_links))  {
      if (is_links(Coo))
        ldk_links(mshapes(A), Coo$links, col=ldk_col)
    }
    if (ldk_labels) {
      ldk_labels(mshapes(A), col=meanshape_col)
    }
    points(mA, pch = ldk_pch,
           cex = ifelse(ldk_cex > 0.5, ldk_cex * 1.5, 0.5), col = meanshape_col)
  }
}

# stack2 ----
# #' Family picture of shapes (ggplot2)
# #'
# #' Will replace stack soon.
# #' @param Coo a Coo object
# #' Family picture of shapes
# #' @return a ggplot2 object
# #' @examples
# #' stack2(bot)
# #' @export
# stack2 <- function(Coo){
#   df <- as_df(Coo)
#   gg <- ggplot(df, aes_string(x="x", y="y", group="id")) +
#     geom_path() +
#     coord_equal()
#   gg
# }

# panel ---------------------------------------------------
#' Family picture of shapes
#'
#' Plots all the outlines, side by side, from
#' a \link{Coo} (\link{Out}, \link{Opn} or \link{Ldk}) objects.
#'
#' @param x The \code{Coo} object to plot.
#' @param  dim for \link{coo_listpanel}: a numeric of length 2
#' specifying the dimensions of the panel
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equal to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param fac a factor within the $fac slot for colors
#' @param palette a color \link{palette}
#' @param coo_sample if not NULL the number of point per shape to display (to plot quickly)
#' @param names whether to plot names or not. If TRUE uses shape names, or something for [fac_dispatcher]
#' @param cex.names a cex for the names
#' @param points \code{logical} (for Ldk) whether to draw points
#' @param points.pch (for Ldk) and a pch for these points
#' @param points.cex (for Ldk) and a cex for these points
#' @param points.col (for Ldk) and a col  for these points
#' @param ... additional arguments to feed generic \code{plot}
#' @note If you want to reorder shapes according to a factor, use \link{arrange}.
#' @family Coo_graphics
#' @examples
#' panel(mosquito, names=TRUE, cex.names=0.5)
#' panel(olea)
#' panel(bot, c(4, 10))
#' # an illustration of the use of fac
#' panel(bot, fac='type', palette=col_spring, names=TRUE)
#' @aliases panel.Coo
#' @rdname panel.Coo
#' @export
panel <- function(x, ...) {
  UseMethod("panel")
}
#' @rdname panel.Coo
#' @export
panel.Out <- function(x, dim, cols, borders, fac,
                      palette = col_summer, coo_sample=120, names = NULL, cex.names = 0.6, points = TRUE,
                      points.pch = 3, points.cex = 0.2, points.col, ...) {
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar = rep(1.2, 4), oma = rep(0.2, 4))

  Coo <- x
  Coo <- coo_template(Coo, size = 0.95)
  if (is.numeric(coo_sample)) {
    if (all(coo_nb(Coo) >= coo_sample)) {
      Coo <- suppressMessages(coo_sample(Coo, coo_sample))
    }
  }

  if (!missing(fac)) {
    f <- fac_dispatcher(Coo, fac)
    if (missing(cols)) {
      cols <- palette(nlevels(f))[f]
    } else {
      cols <- cols[f]
    }
  }
  if (missing(cols)) {
    cols <- rep(NA, length(Coo))
  }
  if (length(cols) != length(Coo)) {
    cols <- rep(cols[1], length(Coo))
  }
  if (missing(borders)) {
    borders <- rep("#333333", length(Coo))
  }
  if (length(borders) != length(Coo)) {
    borders <- rep(borders[1], length(Coo))
  }

  pos <- coo_listpanel(Coo$coo, dim=dim, cols = cols, borders = borders,
                       poly = TRUE, ...)
  if (!is.null(names)) {
    if (is.logical(names)) {
      text(pos[, 1], pos[, 2], labels = names(Coo), cex = cex.names)
    } else {
      names <- fac_dispatcher(Coo, names) %>% as.character()
      text(pos[, 1], pos[, 2],
           labels = names, cex = cex.names)
    }
  }
}

# #' @rdname panel.Coo
# #' @export
# panel.OutCoe <- function(x, nb.pts=120, ...){
#   OutCoe <- x
#   Out <- as.Out(x, nb.pts=nb.pts)
#   panel(Out, title=paste0(substitute(x),".i"),...)}


#' @rdname panel.Coo
#' @export
panel.Opn <- function(x, cols, borders, fac,
                      palette = col_summer, coo_sample=120, names = NULL, cex.names = 0.6, points = TRUE,
                      points.pch = 3, points.cex = 0.2, points.col, ...) {
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar = rep(0, 4), oma = rep(0.2, 4))

  Coo <- x
  Coo <- coo_template(Coo, size = 0.95)
  if (is.numeric(coo_sample)) {
    if (all(coo_nb(Coo) >= coo_sample)) {
      Coo <- suppressMessages(coo_sample(Coo, coo_sample))
    }
  }
  if (!missing(fac)) {
    f <- fac_dispatcher(Coo, fac)
    if (missing(cols)) {
      cols <- palette(nlevels(f))[f]
    } else {
      cols <- cols[f]
    }
  }
  if (missing(cols)) {
    cols <- rep(NA, length(Coo))
  }
  if (length(cols) != length(Coo)) {
    cols <- rep(cols[1], length(Coo))
  }
  if (missing(borders)) {
    borders <- rep("#333333", length(Coo))
  }
  if (length(borders) != length(Coo)) {
    cols <- rep(borders[1], length(Coo))
  }

  pos <- coo_listpanel(Coo$coo, cols = cols, borders = borders, poly = FALSE, ...)
  if (!is.null(names)) {
    if (is.logical(names)) {
      text(pos[, 1], pos[, 2], labels = names(Coo), cex = cex.names)
    } else {
      if (length(names) != length(Coo)) {
        text(pos[, 1], pos[, 2], labels = Coo$fac[,
                                                  names], cex = cex.names)
      } else {
        text(pos[, 1], pos[, 2], labels = names, cex = cex.names)
      }
    }
  }
}

#' @rdname panel.Coo
#' @export
panel.Ldk <- function(x, cols, borders, fac,
                      palette = col_summer, names = NULL, cex.names = 0.6, points = TRUE,
                      points.pch = 3, points.cex = 0.2, points.col = "#333333",
                      ...) {
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar = rep(0, 4), oma = rep(0.2, 4))

  Coo <- x
  Coo <- coo_template(Coo, size = 0.95)

  if (missing(cols) & !missing(borders))
    borders <- cols

  if (!missing(fac)) {
    f <- fac_dispatcher(Coo, fac)
    if (missing(borders)) {
      borders <- palette(nlevels(f))[f]
    } else {
      borders <- borders[f]
    }
  }
  if (missing(borders)) {
    borders <- rep("#000000", length(Coo))
  }
  if (length(borders) != length(Coo)) {
    borders <- rep(borders[1], length(Coo))
  }

  pos <- coo_listpanel(Coo$coo, cols = NULL, borders = NULL,
                       poly = FALSE, points = points, points.pch = "",
                       points.cex = points.cex, points.col = points.col, ...)

  ### quick and dirty patch for slidings, links, etc.

  # links
  if (is_links(Coo)){
    links <- Coo$links
    ldk_all <- get_ldk(Coo)
    for (i in seq_along(Coo)){
      ldk_i <- coo_trans(ldk_all[[i]], pos[i, 1], pos[i, 2])
      for (j in 1:nrow(links))
        segments(ldk_i[links[j, 1], 1], ldk_i[links[j, 1], 2],
                 ldk_i[links[j, 2], 1], ldk_i[links[j, 2], 2],
                 #col=col_alpha("#000000", 0.75))
                 col=col_alpha(borders[i], 0.9))
    }
  }

  # slidings
  if (is_slidings(Coo)){
    slidings_all <- get_slidings(Coo)
    for (i in seq_along(slidings_all))
      for (j in seq_along(slidings_all[[i]]))
        lines(coo_trans(slidings_all[[i]][[j]], pos[j, 1], pos[j, 2]),
              col=col_alpha(borders[j], 0.75))
  }

  # ldk
  ldk_all <- get_ldk(Coo)
  for (i in seq_along(Coo)){
    ldk_i <- coo_trans(ldk_all[[i]], pos[i, 1], pos[i, 2])
    for (j in 1:nrow(ldk_i))
      points(ldk_i[j, 1], ldk_i[j, 2], col=borders[i], pch=points.pch, cex=points.cex)
  }

  if (!is.null(names)) {
    if (is.logical(names)) {
      text(pos[, 1], pos[, 2], labels = names(Coo), cex = cex.names)
    } else {
      if (length(names) != length(Coo)) {
        text(pos[, 1], pos[, 2], labels = Coo$fac[, names], cex = cex.names)
      } else {
        text(pos[, 1], pos[, 2], labels = names, cex = cex.names)
      }
    }
  }
}

# panel2 -----
# #' Family picture of shapes (ggplot2)
# #'
# #' May replace panel one day.
# #' @param Coo a Coo object
# #' @return a ggplot2 object
# #' @examples
# #' panel2(shapes)
# #' @family Coo_graphics
# #' @export
# panel2 <- function(Coo){
#   df <- as_df(Coo)
#   gg <- ggplot(df, aes_string(x="x", y="y", group="id")) +
#     geom_path() +
#     coord_equal() + facet_wrap( ~ id)
#   gg
# }

##### end graphics Coo
