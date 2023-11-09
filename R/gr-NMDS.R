#' NMDS plot unsing grindr layers
#'
#' Quickly vizualise [MDS] and [NMDS] objects and build customs plots
#' using the [layers]. See examples.
#'
#' @param x the result of [MDS] or [NMDS]
#' @param f factor specification to feed [fac_dispatcher]
#' @param axes \code{numeric} of length two to select PCs to use
#' (\code{c(1, 2)} by default)
#' @param palette \code{color palette} to use \code{col_summer} by default
#' @param points `logical` whether to draw this with [layer_points]
#' @param points_transp `numeric` to feed [layer_points] (default:0.25)
#' @param chull `logical` whether to draw this with [layer_chull]
#' @param chullfilled `logical` whether to draw this with [layer_chullfilled]
#' @param labelgroups `logical` whether to draw this with [layer_labelgroups]
#' @param legend `logical` whether to draw this with [layer_legend]
#' @param title `character` if specified, fee [layer_title] (default to `""`)
#' @param box `logical` whether to draw this using [layer_box]
#' @param axesnames `logical` whether to draw this using [layer_axesnames]
#' @family grindr
#'
#' @examples
#' ### First prepare an NMDS object
#' x <- bot %>% efourier %>% NMDS
#'
#' plot_NMDS(x)
#' plot_NMDS(x, ~type) %>% layer_stars() %>% layer_labelpoints()
#'
#' ### Same on MDS object
#' x <- bot %>% efourier %>% MDS
#'
#' plot_MDS(x)
#' plot_MDS(x, ~type) %>% layer_stars() %>% layer_labelpoints()
#' @rdname plot_NMDS
#' @export
plot_NMDS <- function(x,
                      f=NULL,
                      axes=c(1, 2),
                      # points
                      points=TRUE,
                      points_transp=1/4,
                      # chulls
                      chull=TRUE,
                      chullfilled=FALSE,
                      # legends
                      labelgroups=FALSE,
                      legend=TRUE,
                      # cosmetics (mainly)
                      title="",
                      box=TRUE,
                      axesnames=TRUE,
                      palette=pal_qual){

  # check ---------------
  .check(any(class(x)=="NMDS"),
         "only supported on NMDS objects")

  # prepare -------------
  if (missing(f) | is.null(f)){
    x %<>% .layerize_NMDS(axes=axes, palette=palette)
    labelgroups <- legend <- FALSE
  } else {
    x %<>% .layerize_NMDS(f, axes=axes, palette=palette)
  }

  # frame
  x %<>%
    layer_frame() %>%
    layer_axes()

  # cosmetics
  if (axesnames)
    x %<>% layer_axesnames(name="NMDS")

  if (box)
    x %<>% layer_box()

  # data ------------------------------
  if (points)
    x %<>% layer_points(transp=points_transp)

  # groups dispersion -----------------
  if (chull & nlevels(x$f)>1)
    x %<>% layer_chull()

  if (chullfilled & nlevels(x$f)>1)
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

#' @rdname plot_NMDS
#' @export
plot_MDS <- function(x,
                      f=NULL,
                      axes=c(1, 2),
                      # points
                      points=TRUE,
                      points_transp=1/4,
                      # chulls
                      chull=TRUE,
                      chullfilled=FALSE,
                      # legends
                      labelgroups=FALSE,
                      legend=TRUE,
                      # cosmetics (mainly)
                      title="",
                      box=TRUE,
                      axesnames=TRUE,
                      palette=pal_qual){

  # check ---------------
  .check(any(class(x)=="MDS"),
         "only supported on MDS objects")

  # prepare -------------
  if (missing(f) | is.null(f)){
    x %<>% .layerize_MDS(axes=axes, palette=palette)
    labelgroups <- legend <- FALSE
  } else {
    x %<>% .layerize_MDS(f, axes=axes, palette=palette)
  }

  # frame
  x %<>%
    layer_frame() %>%
    layer_axes()

  # cosmetics
  if (axesnames)
    x %<>% layer_axesnames(name="MDS")

  if (box)
    x %<>% layer_box()

  # data ------------------------------
  if (points)
    x %<>% layer_points(transp=points_transp)

  # groups dispersion -----------------
  if (chull & nlevels(x$f)>1)
    x %<>% layer_chull()

  if (chullfilled & nlevels(x$f)>1)
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
