# PCA plotters ------

#' Plots Principal Component Analysis
#'
#' The Momocs' \code{\link{PCA}} plotter with morphospaces and many graphical options.
#'
#' @param x `PCA`, typically obtained with [PCA]
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
#' @seealso \link{plot.LDA}
#' @note  NAs is \code{$fac} are handled quite experimentally.
#' More importantly, as of early 2018, I plan I complete rewrite of
#' \code{plot.PCA} and other multivariate plotters.
#'
#' @examples
#' \dontrun{
#' bot.f <- efourier(bot, 12)
#' bot.p <- PCA(bot.f)
#'
#' ### Morphospace options
#' plot(bot.p, pos.shp="full")
#' plot(bot.p, pos.shp="range")
#' plot(bot.p, pos.shp="xy")
#' plot(bot.p, pos.shp="circle")
#' plot(bot.p, pos.shp="range_axes")
#' plot(bot.p, pos.shp="full_axes")
#'
#' plot(bot.p, morpho=FALSE)
#'
#' ### Passing factors to plot.PCA
#' # 3 equivalent methods
#' plot(bot.p, "type")
#' plot(bot.p, 1)
#' plot(bot.p, ~type)
#'
#' # let's create a dummy factor of the correct length
#' # and another added to the $fac with mutate
#' # and a numeric of the correct length
#' f <- factor(rep(letters[1:2], 20))
#' z <- factor(rep(LETTERS[1:2], 20))
#' bot %<>% mutate(cs=coo_centsize(.), z=z)
#' bp <- bot %>% efourier %>% PCA
#' # so bp contains type, cs (numeric) and z; not f
#' # yet f can be passed on the fly
#' plot(bp, f)
#' # numeric fac are allowed
#' plot(bp, "cs", cex=3, color.legend=TRUE)
#' # formula allows combinations of factors
#' plot(bp, ~type+z)
#'
#' ### other morphometric approaches works the same
#' # open curves
#' op <- npoly(olea, 5)
#' op.p <- PCA(op)
#' op.p
#' plot(op.p, ~ domes + var, morpho=TRUE) # use of formula
#'
#' # landmarks
#' wp <- fgProcrustes(wings, tol=1e-4)
#' wpp <- PCA(wp)
#' wpp
#' plot(wpp, 1)
#'
#' # traditionnal measurements
#' flower %>% PCA %>% plot(1)
#'
#' # plot.PCA can be used after a PCA
#' PCA(iris[, 1:4], fac=iris$Species)  %>% plot(1)
#'
#' ### Cosmetic options
#' # window
#' plot(bp, 1, zoom=2)
#' plot(bp, zoom=0.5)
#' plot(bp, center.origin=FALSE, grid=FALSE)
#'
#' # colors
#' plot(bp, col="red") # globally
#' plot(bp, 1, col=c("#00FF00", "#0000FF")) # for every level
#' # a color vector of the right length
#' plot(bp, 1, col=rep(c("#00FF00", "#0000FF"), each=20))
#' # a color vector of the right length, mixign Rcolor names (not a good idea though)
#' plot(bp, 1, col=rep(c("#00FF00", "forestgreen"), each=20))
#'
#'
#' # ellipses
#' plot(bp, 1, conf.ellipsesax=2/3)
#' plot(bp, 1, ellipsesax=FALSE)
#' plot(bp, 1, ellipsesax=TRUE, ellipses=TRUE)
#'
#' # stars
#' plot(bp, 1, stars=TRUE, ellipsesax=FALSE)
#'
#' # convex hulls
#' plot(bp, 1, chull=TRUE)
#' plot(bp, 1, chull.lty=3)
#'
#' # filled convex hulls
#' plot(bp, 1, chull.filled=TRUE)
#' plot(bp, 1, chull.filled.alpha = 0.8, chull.lty =1) # you can omit chull.filled=TRUE
#'
#' # density kernel
#' plot(bp, 1, density=TRUE, contour=TRUE, lev.contour=10)
#'
#' # delaunay
#' plot(bp, 1, delaunay=TRUE)
#'
#' # loadings
#' flower %>% PCA %>% plot(1, loadings=TRUE)
#'
#' # point/group labelling
#' plot(bp, 1, labelspoint=TRUE) # see options for abbreviations
#' plot(bp, 1, labelsgroup=TRUE) # see options for abbreviations
#'
#' # clean axes, no rug, no border, random title
#' plot(bp, axisvar=FALSE, axisnames=FALSE, rug=FALSE, box=FALSE, title="random")
#'
#' # no eigen
#' plot(bp, eigen=FALSE) # eigen cause troubles to graphical window
#' # eigen may causes troubles to the graphical window. you can try old.par = TRUE
#' }
#' @method plot PCA
#' @name plot.PCA
#' @export
plot.PCA <- function(x, fac, xax=1, yax=2,
                     #points arguments
                     points=TRUE, col="#000000", pch=20, cex=0.5, palette=col_solarized,
                     #.frame
                     center.origin=FALSE, zoom=1, xlim=NULL, ylim=NULL, bg=par("bg"),
                     #.grid
                     grid=TRUE, nb.grids=3,
                     #shapes
                     morphospace=TRUE,
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
                     ellipsesax=FALSE, conf.ellipsesax=c(0.5, 0.9),
                     lty.ellipsesax=1, lwd.ellipsesax=sqrt(2),
                     #convexhulls
                     chull=FALSE, chull.lty=1,
                     #filled convex hulls,
                     chull.filled=TRUE, chull.filled.alpha=0.92,
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
  message("will be deprecated soon, see ?plot_PCA")
  ##### Preliminaries
  PCA <- x
  xy <- PCA$x[, c(xax, yax)]
  ### we check and prepare everything related to groups
  ### fac not provided
  if (missing(fac)) { # mainly for density and contour
    fac <- NULL
    col.groups <- col
  } else {
    # # fac provided ------------------------
    # # fac provided, as formula ============
    # if (class(fac) == "formula") {
    #   column_name <- attr(terms(fac), "term.labels")
    #   # we check for wrong formula
    #   if (any(is.na(match(column_name, colnames(PCA$fac)))))
    #     stop("formula provided must match with $fac column names")
    #   # otherwise we retrive the column(s)
    #   fac <- PCA$fac[, column_name]
    #   # multicolumn/fac case
    #   if (is.data.frame(fac))
    #     fac <- factor(apply(fac, 1, paste, collapse="_"))
    # }
    # # fac provided, as column name or id
    # if (length(fac)==1){
    #   fac <- PCA$fac[, fac]
    # }

    fac <- fac_dispatcher(x, fac)

    # if fac is a factor
    if (is.factor(fac)){


      # handles NAs
      nas <- which(is.na(fac))
      if (length(nas)>0){
        fac <- factor(fac[-nas])
        xy <- xy[-nas,]
      }
      # end NA patch

      if (!missing(col)){
        if (length(col)==nlevels(fac)) {
          col.groups <- col
          col <- col.groups[fac]
        }
        if (length(col)==length(fac)) {
          col.groups <- unique(col)[unique(as.numeric(fac))]
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
    }
  }

  # if fac is a numeric
  if (is.numeric(fac)){
    if (missing(col)){
      if (missing(palette)){
        palette <- pal_qual
      }
      cols_breaks = 1000
      cols_all <- palette(cols_breaks)
      cols_id <- fac  %>% .normalize()  %>% cut(breaks = cols_breaks)  %>% as.numeric()
      col <- cols_all[cols_id]
      col.groups <- pal_qual(1)
    }
  }
  # if Rcolors are passed...
  if (any(col %in% grDevices::colors()) & any(col.groups %in% grDevices::colors())){
    col.groups[which(col.groups %in% grDevices::colors())] %<>% .rcolors2hex()
    col[which(col %in% grDevices::colors())] %<>% .rcolors2hex()
  }

  # cosmetics
  if ((density) & missing(contour)) contour   <- TRUE
  if ((density) & missing(ellipses)) ellipses <- FALSE
  if ((density) & missing(rect.labelsgroups)) rect.labelsgroups <- FALSE
  if (missing(rug) & nlevels(fac)>6) rug      <- FALSE
  if (!missing(chull.lty)) chull              <- TRUE
  if (!missing(chull.filled.alpha) & missing(chull.filled)) chull.filled <- TRUE
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

  # morphospace handling - a big baby
  if (morphospace & !is.null(PCA$method) & length(PCA$method)<=4) {
    morphospacePCA(PCA, xax=xax, yax=yax, pos.shp=pos.shp,
                   nb.shp=nb.shp, nr.shp=nr.shp, nc.shp=nc.shp,
                   rotate.shp=rotate.shp, flipx.shp=flipx.shp, flipy.shp=flipy.shp,
                   amp.shp=amp.shp, size.shp=size.shp, pts.shp=pts.shp,
                   col.shp=col.shp, border.shp=border.shp, lwd.shp=lwd.shp,
                   plot=TRUE)
  }
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

      if (any(colnames(PCA$fac)==labelspoints)) {
        rn <- PCA$fac[, labelspoints]
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
  if (loadings)   .loadings(PCA$rotation[, c(xax, yax)])
  if (axisnames)  .axisnames(xax, yax, "PC")
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  if (unit)       .unit(nb.grids)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax, ev.names="Eigenvalues")
  if (box) box()
  # we return a df
  if (is.null(fac))
    invisible(dplyr::data_frame(x=xy[, 1], y=xy[, 2]))
  else
    invisible(dplyr::data_frame(x=xy[, 1], y=xy[, 2], fac=fac))
}

# Boxplot ---------
#' Boxplot on PCA objects
#'
# @method boxplot PCA
#' @param x `PCA`, typically obtained with [PCA]
#' @param fac factor, or a name or the column id from the $fac slot
#' @param nax the range of PC to plot (1 to 99pc total variance by default)
#' @param ... useless here
#' @return a ggplot object
#' @examples
#' bot.f <- efourier(bot, 12)
#' bot.p <- PCA(bot.f)
#' boxplot(bot.p)
#' p <- boxplot(bot.p, 1)
#' #p +  theme_minimal() + scale_fill_grey()
#' #p + facet_wrap(~PC, scales = "free")
#' @export
boxplot.PCA <- function(x, fac=NULL, nax, ...){
  PCA <- x
  if (missing(nax))
    nax <- 1:scree_min(x)
  if (max(nax) > ncol(PCA$x)) nax <- 1:ncol(PCA$x)
  if (is.null(fac)) {
    df <- data.frame(PCA$x[, nax])
    df <- df %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(df),
                                    Var2=colnames(df)[i],
                                    value=df[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("score", "PC", "value"))
    gg <- ggplot(data=df, aes_string(x="PC", y="value")) +
      geom_boxplot() + labs(x=NULL, y="score")
    return(gg)
  } else {
    ### we check and prepare everything related to groups
    ### fac not provided
    if (missing(fac)) { # mainly for density and contour
      fac <- NULL
      col.groups <- col
    } else {
      ### fac provided
      # fac provided, as formula

      fac <- fac_dispatcher(x, fac)

      df <- data.frame(PCA$x[, nax])
      df <- df %>% seq_along %>%
        lapply(function(i) data.frame(fac=fac,
                                      Var1=rownames(df),
                                      Var2=colnames(df)[i],
                                      value=df[,i])) %>%
        do.call("rbind", .) %>%
        `colnames<-`(c("fac", "name", "PC", "value"))
      gg <- ggplot(data=df, aes_string(x="PC", y="value", fill="fac")) +
        geom_boxplot() + labs(x=NULL, y="score", fill=NULL)
    }
    return(gg)
  }
}


# PCcontrib ----------
#' Shape variation along PC axes
#'
#' Calculates and plots shape variation along Principal Component axes.
#'
#' @param PCA a `PCA` object
#' @param nax the range of PCs to plot (1 to 99pc total variance by default)
#' @param sd.r a single or a range of mean +/- sd values (eg: c(-1, 0, 1))
#' @param gap for combined-Coe, an adjustment variable for gap between shapes. (bug)Default
#' to 1 (whish should never superimpose shapes), reduce it to get a more compact plot.
#' @param ... additional parameter to pass to \code{\link{coo_draw}}
#' @return (invisibly) a list with \code{gg} the ggplot object and \code{shp} the list of shapes.
#' @examples
#' bot.p <- PCA(efourier(bot, 12))
#' PCcontrib(bot.p)
#' \dontrun{
#' library(ggplot2)
#' gg <- PCcontrib(bot.p, nax=1:8, sd.r=c(-5, -3, -2, -1, -0.5, 0, 0.5, 1, 2, 3, 5))
#' gg + geom_polygon(fill="slategrey", col="black") + ggtitle("A nice title")
#' }
#' @rdname PCcontrib
#' @export
PCcontrib <- function(PCA, ...){
  UseMethod("PCcontrib")
}

#' @rdname PCcontrib
#' @export
PCcontrib.PCA <-
  function(PCA,
           nax,
           sd.r=c(-2, -1, -0.5, 0, 0.5, 1, 2),
           gap=1,
           ...){
    x <- PCA
    shp <- list()
    if (missing(nax))
      nax <- 1:scree_min(x)
    for (i in seq(along=nax)){
      sd.i <- sd(x$x[, nax[i]])
      pos.i <- data.frame(x=sd.r*sd.i, y=rep(0, length(sd)))
      shp.i <- morphospace2PCA(x, xax=i, yax=1, pos = pos.i)
      shp[[i]] <- mutate(shp.i, nax=i) }

    shp <- dplyr::bind_rows(shp)
    shp$shp <- sd.r[shp$shp]

    gg <- ggplot(data=shp, aes(x=x_c + x_d, y=y_c + y_d, group=shp1)) +
      geom_polygon(colour="grey50", fill="grey95") + coord_equal() +
      facet_grid(nax ~ shp) + labs(x="Mean + SD", y="PC") +
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank())

    print(gg)

    shp <- gg$data %>%
      # we dont want all columns
      select(x=x_c, y=y_c, shp1, nax) %>%
      # we split for each pair of nax x shp1
      split(f=list(.$shp1, .$nax))

    list(gg=gg, shp=shp) %>% invisible()
  }


#' Methods for PCA eigen values
#'
#' A set of functions around PCA/LDA eigen/trace. \code{scree} calculates their proportion and cumulated proportion;
#' \code{scree_min} returns the minimal number of axis to use to retain a given proportion; \code{scree_plot} displays a screeplot.
#'
#' @param x a \link{PCA} object
#' @param nax numeric range of axis to consider
#' @param prop numeric how many axis are enough this proportion of variance, if too high then number of axis is returned.
#' @return scree returns a data.frame, scree_min a numeric, scree_plot a ggplot.
#' @examples
#' # On PCA
#' bp <- PCA(efourier(bot))
#' scree(bp)
#' scree_min(bp, 0.99)
#' scree_min(bp, 1)
#'
#' scree_plot(bp)
#' scree_plot(bp, 1:5)
#'
#' # on LDA, it uses svd
#' bl <- LDA(PCA(opoly(olea)), "var")
#' scree(bl)
#'
#' @export
#' @rdname scree
scree <- function(x, nax) {
  UseMethod("scree")}

#' @export
#' @rdname scree
scree.PCA <- function(x, nax=1:10){
  eig <- (x$sdev^2)
  eig <- eig / sum(eig)
  if (max(nax)>length(eig)) nax <- 1:length(eig)
  eig <- eig[nax]
  df <-  dplyr::data_frame(axis=ordered(1:length(eig)), proportion=eig, cumsum=cumsum(eig))
  df
}

#' @export
#' @rdname scree
scree.LDA <- function(x, nax=1:10){
  eig <- (x$mod$svd^2)
  eig <- eig / sum(eig)
  if (max(nax)>length(eig)) nax <- 1:length(eig)
  eig <- eig[nax]
  df <-  dplyr::data_frame(axis=ordered(1:length(eig)), proportion=eig, cumsum=cumsum(eig))
  df
}

#' @export
#' @rdname scree
scree_min <- function(x, prop=0.99){
  enough <- scree(x)$cumsum >= prop
  ifelse(any(enough), min(which(enough)), length(enough))
}

#' @export
#' @rdname scree
scree_plot <- function(x, nax=1:10){
  df <- scree(x, nax)
  gg <- ggplot(df, aes_string(x="axis", y="proportion")) +
    geom_hline(yintercept=c(0.5, 0.90, 0.95, 0.99), linetype=2, alpha=0.5) +
    geom_bar(stat="identity") + geom_text(label=round(df$cumsum, 3), vjust=0) +
    labs(x="Components", y="Proportion")
  gg
}


# selected=NULL,
# return(df)
# fills <- rep("black", nrow(df))
# fills[selected] <- "red"
# gg <- ggplot(data=df, aes(x=x, y=y), fill=fill) +
#   geom_bar(fill=fills, stat="identity") +
#   labs(x="Components", y="Variances")
# gg



#### borrowed from ggplot2 by Hadley
calculate_ellipse <- function(data, vars, type, level, segments){
  dfn <- 2
  dfd <- nrow(data) - 1
  if (!type %in% c("t", "norm", "euclid")){
    message("unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else if (dfd < 3){
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else {
    if (type == "t"){
      v <- MASS::cov.trob(data[,vars])
    } else if (type == "norm"){
      v <- cov.wt(data[,vars])
    } else if (type == "euclid"){
      v <- cov.wt(data[,vars])
      v$cov <- diag(rep(min(diag(v$cov)), 2))
    }
    shape <- v$cov
    center <- v$center
    chol_decomp <- chol(shape)
    if (type == "euclid"){
      radius <- level/max(chol_decomp)
    } else {
      radius <- sqrt(dfn * qf(level, dfn, dfd))
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  }
  ellipse <- as.data.frame(ellipse)
  colnames(ellipse) <- vars
  return(ellipse)
}


calculate_ellipseax <- function(ell){
  if (any(is.na(ell))) {
    na <- rep(NA, 2)
    seg <- data.frame(x=na, y=na, xend=na, yend=na)
    return(seg)
  }
  ell.al <- coo_align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]),
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  seg <- dplyr::bind_cols(slice(seg, c(1, 3)), slice(seg, c(2, 4)))
  colnames(seg) <- c("x", "y", "xend", "yend")
  seg
}


##### end PCA plotters

