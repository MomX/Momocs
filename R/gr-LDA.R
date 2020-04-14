# plot.LDA ---------
#' Plots Linear Discriminant Analysis
#'
#' The Momocs' \code{\link{LDA}} plotter with many graphical options.
#' @param x an object of class "LDA", typically obtained with \link{LDA}
#' @param fac name or the column id from the $fac slot, or a formula combining colum names
#' from the $fac slot (cf. examples). A factor or a numeric of the same length
#' can also be passed on the fly.
#' @param xax the first PC axis
#' @param yax the second PC axis
#' @param points logical whether to plot points
#' @param col a color for the points (either global, for every level of the fac
#' or for every individual, see examples)
#' @param pch a pch for the points (either global, for every level of the fac
#' or for every individual, see examples)
#' @param cex the size of the points
#' @param palette a \link{palette}
#' @param center.origin logical whether to center the plot onto the origin
#' @param zoom to keep your distances
#' @param xlim numeric of length two ; if provided along with ylim, the x and y lims to use
#' @param ylim numeric of length two ; if provided along with xlim, the x and y lims to use
#' @param bg color for the background
#' @param grid logical whether to draw a grid
#' @param nb.grids and how many of them
#' @param morphospace logical whether to add the morphological space
#' @param pos.shp passed to \link{morphospace_positions}, one of
#' \code{"range", "full", "circle", "xy", "range_axes", "full_axes"}. Or directly
#' a matrix of positions. See \link{morphospace_positions}
#' @param amp.shp amplification factor for shape deformation
#' @param size.shp the size of the shapes
#' @param nb.shp (pos.shp="circle") the number of shapes on the compass
#' @param nc.shp (pos.shp="full" or "range) the number of shapes per column
#' @param nr.shp (pos.shp="full" or "range) the number of shapes per row
#' @param rotate.shp angle in radians to rotate shapes (if several methods, a vector of angles)
#' @param flipx.shp same as above, whether to apply coo_flipx
#' @param flipy.shp same as above, whether to apply coo_flipy
#' @param pts.shp the number of points fro drawing shapes
#' @param border.shp the border color of the shapes
#' @param lwd.shp the line width for these shapes
#' @param col.shp the color of the shapes
#' @param stars logical whether to draw "stars"
#' @param ellipses logical whether to draw confidence ellipses
#' @param conf.ellipses numeric the quantile for the (bivariate gaussian) confidence ellipses
#' @param ellipsesax logical whether to draw ellipse axes
#' @param conf.ellipsesax one or more numeric, the quantiles for the (bivariate gaussian) ellipses axes
#' @param lwd.ellipsesax if yes, one or more numeric for the line widths
#' @param lty.ellipsesax if yes, the lty with which to draw these axes
#' @param chull logical whether to draw a convex hull
#' @param chull.lty if yes, its linetype
#' @param chull.filled logical whether to add filled convex hulls
#' @param chull.filled.alpha numeric alpha transparency
#' @param density whether to add a 2d density kernel estimation (based on \link{kde2d})
#' @param lev.density if yes, the number of levels to plot (through \link{image})
#' @param contour whether to add contour lines based on 2d density kernel
#' @param lev.contour if yes, the (approximate) number of lines to draw
#' @param n.kde2d the number of bins for \link{kde2d}, ie the 'smoothness' of density kernel
#' @param delaunay logical whether to add a delaunay 'mesh' between points
#' @param loadings logical whether to add loadings for every variables
#' @param labelspoints if TRUE rownames are used as labels, a colname from $fac can also be passed
#' @param col.labelspoints a color for these labels, otherwise inherited from fac
#' @param cex.labelspoints a cex for these labels
#' @param abbreviate.labelspoints logical whether to abbreviate
#' @param labelsgroups logical whether to add labels for groups
#' @param cex.labelsgroups ifyes, a numeric for the size of the labels
#' @param rect.labelsgroups logical whether to add a rectangle behind groups names
#' @param abbreviate.labelsgroups logical, whether to abbreviate group names
#' @param color.legend logical whether to add a (cheap) color legend for numeric fac
#' @param axisnames logical whether to add PC names
#' @param axisvar logical whether to draw the variance they explain
#' @param unit logical whether to add plane unit
#' @param eigen logical whether to draw a plot of the eigen values
#' @param rug logical whether to add rug to margins
#' @param title character a name for the plot
#' @param box whether to draw a box around the plotting region
#' @param old.par whether to restore the old \link{par}. Set it to \code{FALSE} if you want to reuse the graphical window.
#' @param ... useless here, just to fit the generic plot
#' @details Widely inspired by the "layers" philosophy behind graphical functions
#' of the ade4 R package.
#' @note Morphospaces are deprecated so far. 99% of the code
#' is shared with \link{plot.PCA} waiting for a general rewriting of a multivariate plotter.
#' See https://github.com/vbonhomme/Momocs/issues/121
#' @seealso \link{LDA}, \link{plot_CV}, \link{plot_CV2}, \link{plot.PCA}.
#' @method plot LDA
#' @export
plot.LDA <- function(x, fac=x$fac, xax=1, yax=2,
                     #points arguments
                     points=TRUE, col="#000000", pch=20, cex=0.5, palette=col_solarized,
                     #.frame
                     center.origin=FALSE, zoom=1, xlim=NULL, ylim=NULL, bg=par("bg"),
                     #.grid
                     grid=TRUE, nb.grids=3,
                     #shapes
                     morphospace=FALSE,
                     pos.shp=c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
                     amp.shp=1,
                     size.shp=1, nb.shp=12, nr.shp=6, nc.shp=5,
                     rotate.shp=0, flipx.shp=FALSE, flipy.shp=FALSE,
                     pts.shp=60, border.shp=col_alpha("#000000", 0.5),
                     lwd.shp=1, col.shp=col_alpha("#000000", 0.95),
                     #stars
                     stars=FALSE,
                     #ellipses
                     ellipses=FALSE, conf.ellipses=0.5,
                     #ellipsesax
                     ellipsesax=TRUE, conf.ellipsesax=c(0.5, 0.9),
                     lty.ellipsesax=1, lwd.ellipsesax=sqrt(2),
                     #convexhulls
                     chull=FALSE, chull.lty=1,
                     #filled convex hulls,
                     chull.filled=FALSE, chull.filled.alpha=0.92,
                     #kde2d
                     density=FALSE, lev.density=20,
                     contour = FALSE, lev.contour=3, n.kde2d=100,
                     #delaunay
                     delaunay=FALSE,
                     #loadings
                     loadings=FALSE,
                     #labelspoint
                     labelspoints=FALSE,
                     col.labelspoints=par("fg"),
                     cex.labelspoints=0.6,
                     abbreviate.labelspoints=TRUE,
                     #labelsgroups
                     labelsgroups=TRUE,
                     cex.labelsgroups=0.8,
                     rect.labelsgroups=FALSE,
                     abbreviate.labelsgroups=FALSE,
                     # legend for numeric fac
                     color.legend=FALSE,
                     #axisnames
                     axisnames=TRUE,
                     #axisvar
                     axisvar=TRUE,
                     # unit
                     unit=FALSE,
                     #eigen
                     eigen=TRUE,
                     # various
                     rug=TRUE,
                     title=substitute(x), box=TRUE, old.par=TRUE, ...
){
  # todo
  message("will be deprecated soon, see ?plot_LDA")
  ##### Preliminaries
  # morphospace deprecated
  morphospace=FALSE
  fac <- x$fac

  fac <- fac_dispatcher(x, fac)

  if (nlevels(fac) <= 2) { # case of 2 levels and a single LD
    xy <- x$mod.pred$x[, 1, drop=FALSE]
  } else {
    xy <- x$mod.pred$x[, c(xax, yax)]
  }

  if (!missing(col)){
    if (length(col)==nlevels(fac)) {
      col.groups <- col
      col <- col.groups[fac]
    } else {
      col.groups <- rep(col[1], nlevels(fac))
      if (length(col) != nrow(xy)){
        col <- rep(col[1], nrow(xy))}}
  } else {
    col.groups <- palette(nlevels(fac))
    if (length(col) != nrow(xy)){
      col <- col.groups[fac]}
  }
  # pch handling
  if (!missing(pch)) {
    if (length(pch)==nlevels(fac)) { pch <- pch[fac] }
  }
  else {
    pch <- 20
  }


  # # if fac is a numeric
  # if (is.numeric(fac)){
  #   if (missing(col)){
  #     if (missing(palette)){
  #       palette <- col_gallus
  #     }
  #     cols_breaks = 1000
  #     cols_all <- palette(cols_breaks)
  #     cols_id <- fac  %>% .normalize()  %>% cut(breaks = cols_breaks)  %>% as.numeric()
  #     col <- cols_all[cols_id]
  #   }
  # }

  if (nlevels(fac) <= 2){
    op <- par(mfrow=c(2, 1), oma=c(0, 0, 0, 0), mar=c(4, 1, 3, 1 ))
    on.exit(op)
    hist.range <- range(xy)
    hist(xy[fac==levels(fac)[1]], xlim=hist.range,
         ylab=NA, xlab="LD1", main=levels(fac)[1],
         col=palette(2)[1], axes=FALSE); axis(1)
    hist(xy[fac==levels(fac)[2]], xlim=hist.range,
         ylab=NA, xlab="LD1", main=levels(fac)[2],
         col=palette(2)[2], axes=FALSE); axis(1)
    par(mfrow=c(1, 1))
    return()
  }

  # cosmetics
  if ((density) & missing(contour)) contour   <- TRUE
  if ((density) & missing(ellipses)) ellipses <- FALSE
  if ((density) & missing(rect.labelsgroups)) rect.labelsgroups <- FALSE
  if (missing(rug) & nlevels(fac)>6) rug      <- FALSE
  if (!missing(chull.lty)) chull              <- TRUE
  if (!missing(chull.filled.alpha)) chull.filled <- TRUE
  if (!missing(labelspoints) & missing(points)) points <- FALSE
  if (missing(col.labelspoints)) col.labelspoints <- col
  if (stars & missing(ellipsesax)) ellipsesax <- FALSE

  ##### Graphics start here
  # we prepare the graphic window
  opar <- par(mar = par("mar"), xpd=FALSE)
  if (old.par) on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  # we initate it
  .frame(xy, xlim=xlim, ylim=ylim, center.origin, zoom=zoom, bg=bg)
  if (grid)     .grid(nb.grids)

  # if numeric fac, we add the (cheap) legend
  if (is.numeric(fac) & color.legend) {
    legend_labels <- round(c(max(fac), mean(range(fac)), min(fac)), 2)
    legend_cols <- col[c(length(col), round(length(col)/2), 1)]
    legend("topright", fill=legend_cols,
           legend=legend_labels, bty="n",
           y.intersp = 0.8, cex=0.8, adj=0, xjust=1)
  }

  # then the layers
  if (density)  .density(xy, fac, levels= lev.density, col=col.groups, transp=0.3, n.kde2d=n.kde2d)
  if (contour)  .contour(xy, fac, levels= lev.contour, col=col.groups, transp=ifelse(density, 0.5, 0.3), n.kde2d=n.kde2d)
  if (delaunay) .delaunay(xy, fac, col.groups)

  if (is.factor(fac)) {
    if (stars)      .stars(xy, fac, col.groups)
    if (ellipsesax) .ellipsesax(xy, fac, conf.ellipsesax, col.groups, lty.ellipsesax, lwd.ellipsesax)
    if (ellipses)   .ellipses(xy, fac, conf.ellipses, col.groups) #+conf
    if (chull.filled) .chullfilled(xy, fac, col_alpha(col.groups, chull.filled.alpha))
    if (chull)      .chull(xy, fac, col.groups, chull.lty)
    if (labelsgroups)     .labelsgroups(xy, fac, col.groups,
                                        cex=cex.labelsgroups, rect=rect.labelsgroups,
                                        abbreviate=abbreviate.labelsgroups)
    if (rug)        .rug(xy, fac, col.groups)
  } else {
    if (rug)        .rug(xy, NULL, col)
  }
  # return(col)

  if (points) points(xy, pch=pch, col=col, cex=cex)
  if (!missing(labelspoints)) {
    if (labelspoints==FALSE) {
      rn <- NULL
    } else {

      if (any(colnames(x$fac)==labelspoints)) {
        rn <- x$fac[, labelspoints]
      } else {
        rn <- rownames(x$x)
      }
    }
    if (!is.null(rn)){
      if (abbreviate.labelspoints) rn <- abbreviate(rn)
      text(xy[, 1], xy[, 2], labels=rn,
           col=col.labelspoints, cex=cex.labelspoints)
    }
  }
  if (loadings)   .loadings(x$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "LD")
  if (axisvar)    .axisvar(x$mod$svd, xax, yax)
  if (unit)       .unit(nb.grids)
  .title(title)
  if (eigen)     .eigen(x$mod$svd, xax, yax, ev.names="Prop. of trace")
  if (box) box()
  # we return a df
  if (is.null(fac))
    invisible(dplyr::data_frame(x=xy[, 1], y=xy[, 2]))
  else
    invisible(dplyr::data_frame(x=xy[, 1], y=xy[, 2], fac=fac))
}


# plot_CV ---------
#' Plots a cross-validation table as an heatmap
#'
#' Either with frequencies (or percentages) plus marginal sums,
#' and values as heatmaps. Used in Momocs for plotting cross-validation tables
#' but may be used for any table (likely with \code{freq=FALSE}).
#'
#' @param x a (cross-validation table) or an LDA object
#' @param freq logical whether to display frequencies (within an actual class) or counts
#' @param rm0 logical whether to remove zeros
#' @param pc logical whether to multiply proportion by 100, ie display percentages
#' @param fill logical whether to fill cell according to count/freq
#' @param labels logical whether to add text labels on cells
#' @param axis.size numeric to adjust axis labels
#' @param axis.x.angle numeric to rotate x-axis labels
#' @param cell.size numeric to adjust text labels on cells
#' @param signif numeric to round frequencies using [signif]
#' @param ... useless here
#' @return a ggplot object
#' @seealso \link{LDA}, \link{plot.LDA}, and (pretty much the same) \link{plot_table}.
#' @examples
#' h <- hearts %>%
#'      fgProcrustes(0.01) %>% coo_slide(ldk=2) %T>% stack %>%
#'      efourier(6, norm=FALSE) %>% LDA(~aut)
#'
#' h %>% plot_CV()
#' h %>% plot_CV(freq=FALSE, rm0=FALSE, fill=FALSE)
#' # you can customize the returned gg with some ggplot2 functions
#' h %>% plot_CV(labels=FALSE, fill=TRUE, axis.size=5) + ggplot2::ggtitle("A confusion matrix")
#'
#' # or build your own using the prepared data_frame:
#' df <- h %>% plot_CV() %$% data
#' df
#'
#' # you can even use it as a cross-table plotter
#' bot$fac %>% table %>% plot_CV()
#'
#' @rdname plot_CV
#' @export
plot_CV <- function(x,
                    freq = FALSE, rm0 = FALSE, pc=FALSE,
                    fill=TRUE, labels=TRUE,
                    axis.size=10,
                    axis.x.angle = 45,
                    cell.size = 2.5,
                    signif = 2,
                    ...){UseMethod("plot_CV")}

#' @rdname plot_CV
#' @export
plot_CV.default <- function(x,
                            freq = FALSE, rm0 = FALSE, pc=FALSE,
                            fill=TRUE, labels=TRUE,
                            axis.size=10,
                            axis.x.angle = 45,
                            cell.size = 2.5,
                            signif = 2, ...){

  # check a bit
  .check(is.matrix(x), "x must be a table or a matrix")

  # mat dimensions and try to adapt a bit
  n <- nrow(x)
  if ( n > 60 && missing(labels))
    labels <- FALSE

  # expect a table with rows for actual, columns for predicted
  df <- x %>%
    dplyr::as_tibble() %>%
    `colnames<-`(c("actual", "predicted", "n")) %>%
    dplyr::mutate(actual=factor(actual), predicted=factor(predicted)) %>%
    dplyr::group_by(predicted) %>%
    dplyr::arrange(actual) %>%
    dplyr::ungroup()

  # if freq, frequencies within actual class
  if (freq){
    df %<>%
      dplyr::group_by(actual) %>%
      dplyr::mutate(n=n/sum(n)) %>%
      dplyr::ungroup()

    # if pc, turn into percentages
    if (pc) {
      df %<>% dplyr::mutate(n=100*n)
    }

  }

  # round, if required
  df %<>% dplyr::mutate(n=signif(n, signif))

  # if required, remove zeros
  if (rm0){
    df %<>% filter(n!=0)
  }

  df$actual2 <- factor(df$actual, levels=rev(levels(df$actual)))

  # initiates the gg
  gg <- df %>%
    ggplot() +
    aes(x=predicted, y=actual2) +
    # cosmetics
    xlab("predicted as") + ylab("") +
    # we all love squares
    coord_equal() +
    # put the x-axis on top
    scale_x_discrete(position = "top") +
    # gg + theme(panel.grid.major = element_line(size = 0.3))
    theme_minimal() +
    theme(panel.background = element_rect(fill = NA, colour=NA),
          panel.grid.major = element_line(size = 0.1),
          axis.text.y = element_text(size=axis.size),
          axis.text.x = element_text(size=axis.size,
                                     angle = axis.x.angle,
                                     hjust = 0))

  # add fill, if required
  if (fill){
    gg <- gg +
      geom_tile(aes(fill=n), alpha=0.5) +
      scale_fill_viridis_c(direction= -1) +
      guides(fill=FALSE)
  }
  # add labels, if required
  if (labels){
    gg <- gg + geom_text(aes(label=n), size=cell.size)
  }

  # return this beauty
  gg
}

#' @rdname plot_CV
#' @export
plot_CV.LDA <- function(x,
                        freq = TRUE, rm0 = TRUE, pc=TRUE,
                        fill=TRUE, labels=TRUE,
                        axis.size=10,
                        axis.x.angle = 45,
                        cell.size = 2.5,
                        signif = 2, ...){
  plot_CV(x$CV.tab,
          freq=freq, rm0=rm0,
          fill=fill, labels=labels,
          axis.size=axis.size,
          axis.x.angle=axis.x.angle,
          cell.size=cell.size,
          signif=signif,  ...)
}

# plot_CV2 ---------
#' Plots a cross-correlation table
#'
#' Or any contingency/confusion table. A simple graphic representation based on variable
#' width and/or color for arrows or segments, based on the relative frequencies.
#'
#' @param x an \link{LDA} object, a table or a squared matrix
#' @param links.FUN a function to draw the links: eg \link{segments} (by default), \link{arrows}, etc.
#' @param col logical whether to vary the color of the links
#' @param col0 a color for the default link (when \code{col = FALSE})
#' @param col.breaks the number of different colors
#' @param palette a color palette, eg \link{col_summer}, \link{col_hot}, etc.
#' @param lwd logical whether to vary the width of the links
#' @param lwd0 a width for the default link (when \code{lwd = FALSE})
#' @param gap.dots numeric to set space between the dots and the links
#' @param pch.dots a pch for the dots
#' @param gap.names numeric to set the space between the dots and the group names
#' @param cex.names a cex for the names
#' @param legend logical whether to add a legend
#' @param ... useless here.
#' @seealso \link{LDA}, \link{plot.LDA}, \link{plot_CV}.
#' @note When `freq=FALSE`, the fill colors are not weighted
#' within actual classes and should not be displayed if classes sizes are not balanced.
#' @examples
#' # Below various table that you can try. We will use the last one for the examples.
#' \dontrun{
#' #pure random
#' a <- sample(rep(letters[1:4], each=10))
#' b <- sample(rep(letters[1:4], each=10))
#' tab <- table(a, b)
#'
#' # veryhuge + some structure
#' a <- sample(rep(letters[1:10], each=10))
#' b <- sample(rep(letters[1:10], each=10))
#' tab <- table(a, b)
#' diag(tab) <- round(runif(10, 10, 20))
#'
# more structure
#' tab <- matrix(c(8, 3, 1, 0, 0,
#'                 2, 7, 1, 2, 3,
#'                 3, 5, 9, 1, 1,
#'                 1, 1, 2, 7, 1,
#'                 0, 9, 1, 4, 5), 5, 5, byrow=TRUE)
#' tab <- as.table(tab)
#' }
#' # good prediction
#' tab <- matrix(c(8, 1, 1, 0, 0,
#'                1, 7, 1, 0, 0,
#'                 1, 2, 9, 1, 0,
#'                 1, 1, 1, 7, 1,
#'                 0, 0, 0, 1, 8), 5, 5, byrow=TRUE)
#' tab <- as.table(tab)
#'
#'
#' plot_CV2(tab)
#' plot_CV2(tab, arrows) # if you prefer arrows
#' plot_CV2(tab, lwd=FALSE, lwd0=1, palette=col_india) # if you like india but not lwds
#' plot_CV2(tab, col=FALSE, col0='pink') # only lwd
#' plot_CV2(tab, col=FALSE, lwd0=10, cex.names=2) # if you're getting old
#' plot_CV2(tab, col=FALSE, lwd=FALSE) # pretty but useless
#' plot_CV2(tab, col.breaks=2) # if you think it's either good or bad
#' plot_CV2(tab, pch=NA) # if you do not like dots
#' plot_CV2(tab, gap.dots=0) # if you want to 'fill the gap'
#' plot_CV2(tab, gap.dots=1) # or not
#'
#' #trilo examples
#' trilo.f <- efourier(trilo, 8)
#' trilo.l <- LDA(PCA(trilo.f), 'onto')
#' trilo.l
#' plot_CV2(trilo.l)
#'
#' # olea example
#' op <- opoly(olea, 5)
#' opl <- LDA(PCA(op), 'var')
#' plot_CV2(opl)
#' @rdname plot_CV2
#' @export
plot_CV2 <- function(x, ...){
  UseMethod("plot_CV2")
}

#' @rdname plot_CV2
#' @export
plot_CV2.LDA <- function(x, ...){
  plot_CV2(x$CV.tab, ...)
}

#' @rdname plot_CV2
#' @export
plot_CV2.table <- function(x, links.FUN = arrows, col = TRUE,
                           col0 = "black", col.breaks = 5, palette = col_heat, lwd = TRUE,
                           lwd0 = 5, gap.dots = 0.2, pch.dots = 20, gap.names = 0.25,
                           cex.names = 1, legend = TRUE, ...) {
  # to maintain the generic
  tab <- x
  # we check a bit
  if (ncol(x) != nrow(x))
    stop("a table or a squared matrix must be passed")
  # we deduce xy positions
  gap.mid <- 3
  n <- nrow(tab)
  x.dots <- c(rep(1, n), rep(1 + gap.mid, n))
  y.dots <- rep(1:n, 2)
  x1.link <- rep(1 + gap.dots, n)
  x2.link <- rep(1 + gap.mid - gap.dots, n)
  y.link <- y.dots
  # we initiate the graphics window: no margins and 'butt'
  # lines end
  op <- par(mar = rep(0, 4), lend = 1)
  leg.y1 <- ifelse(legend, 0, 0.5)
  plot(NA, xlim = c(0.8, gap.mid + 1.2), ylim = c(leg.y1, n +
                                                    0.5))
  # we deduce the 'lwd matrix'
  if (lwd) {
    tab.lwd <- apply(tab, 1, function(x) x/sum(x))
    tab.lwd <- tab.lwd * lwd0
  } else {
    if (missing(lwd0))
      lwd0 <- 1  # to avoid too puffy segments
    tab.lwd <- matrix(lwd0, nrow = n, ncol = n)
  }
  # we decude the 'col matrix'
  if (col) {
    cols <- palette(col.breaks)[as.numeric(cut(tab, breaks = col.breaks))]
  } else {
    cols <- rep(col0, n^2)
  }
  # since cols is not yet a matrix, allows a parallel coding in
  # 'segments' below
  tab.cols <- matrix(cols, n, n, byrow = TRUE)
  # the loop that draws the segments
  for (i in 1:n) {
    for (j in 1:n) {
      links.FUN(x1.link[i], y.link[i], x2.link[j], y.link[j],
                lwd = tab.lwd[i, j], col = tab.cols[i, j])
    }
  }
  # we add dots and classes names
  points(x.dots, y.dots, pch = pch.dots)
  text(x.dots, y.dots + gap.names, labels = unlist(dimnames(tab)),
       cex = cex.names)
  if (legend) {
    text(1, 1/3, labels = names(dimnames(tab))[1], cex = cex.names,
         font = 2)
    text(1 + gap.mid, 1/3, labels = names(dimnames(tab))[2],
         cex = cex.names, font = 2)
  }
  # we restore the graphics parameters
  par(op)
}




