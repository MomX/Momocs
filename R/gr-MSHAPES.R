
#' Pairwise comparison of a list of shapes
#'
#' "Confusion matrix" of a list of shapes. See examples.
#' @note Directly inspired by Chitwood et al. (2016) in _New Phytologist_
#' @param x a list of shapes (eg as returned by [MSHAPES])
#' @param draw_fun one of [draw_outline], [draw_curves], [draw_landmarks].
#' When the result of [MSHAPES] is passed, detected based on `$Coe`, otherwise default to `draw_curves`.
#' @param size numeric shrinking factor for shapes (and \link{coo_template};
#' 3/4 by default)
#' @param palette on of [palettes]
#' @return a plot
#' @examples
#' x <- bot %>% efourier(6) %>% MSHAPES(~type)
#'
#' # custom colors
#' x %>% plot_MSHAPES(palette=pal_manual(c("darkgreen", "orange")))
#'
#' # also works on list of shapes, eg:
#' leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo
#' class(leaves)
#' leaves %>% plot_MSHAPES()
#'
#' # or
#' shapes %>%
#' # subset and degrade
#' slice(1:12) %>% coo_sample(60) %$%  # grab the coo
#'     coo %>%
#'     plot_MSHAPES()
#' @export
plot_MSHAPES <- function(x, draw_fun, size, palette){
  UseMethod("plot_MSHAPES")
}

#' @export
plot_MSHAPES.list <- function(x, draw_fun=draw_curves, size=3/4, palette=pal_div){
  # neater par
  op <- par(mar=c(4, 4, 0, 0))
  on.exit(par(op))

  ms <- x
  cols <- palette(3)[-2]

  # template shapes
  ms %<>% lapply(coo_template, size)

  # initializes the plot
  n <- length(ms)
  plot(NA, xlim=c(0.5, n+0.5), ylim=c(0.5, n+0.5), axes=F, ann=F, asp=1)
  axis(1, at=1:n, tick = FALSE, labels = names(ms), las=2, col.axis=cols[2])
  axis(2, at=1:n, tick = FALSE, labels = names(ms), las=2, col.axis=cols[1])
  abline(h=1:n, v=1:n, col="grey90", lty=3)

  # draw shapes
  for (i in 1:n){
    for (x in 1:n){
      if (x == i) # diagonal
        ms[[i]] %>%
        coo_trans(x=x, y=i) %>%
        draw_fun(col=par("fg"), transp=0.5)
      else
        ms[[i]] %>%
        coo_trans(x=x, y=i) %>%
        draw_fun(col=cols[1], transp=0.25)
      if (x != i) # not diagonal
        ms[[i]] %>%
        coo_trans(x=i, y=x) %>%
        draw_fun(col=cols[2], transp = 0.25)
    }
  }
}

#' @export
plot_MSHAPES.matrix <- function(x, draw_fun=draw_curves, size=3/4, palette=pal_div){
  x %>% coo_template(size=size) %>% paper_white() %>% draw_fun
}


#' @export
plot_MSHAPES.MSHAPES <- function(x, draw_fun, size=3/4, palette=pal_div){

  if (missing(draw_fun)){

    if (is_OutCoe(x$Coe))
      draw_fun <- draw_outlines

    if (is_OpnCoe(x$Coe))
      draw_fun <- draw_curves


    if (is_LdkCoe(x$Coe) | is_Ldk(x$Coe))
      draw_fun <- draw_landmarks
  }

  plot_MSHAPES(x$shp, draw_fun=draw_fun, size=size, palette=palette)

}


