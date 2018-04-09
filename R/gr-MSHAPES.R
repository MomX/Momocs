
#' Pairwise comparison of a list of shapes
#'
#' "Confusion matrix" of a list of shapes. See examples.
#' @note Directly inspired by Chitwood et al. (2016) in _New Phytologist_
#' @param x a list of shapes (eg as returned by \link{mshapes})
#' @param size numeric shrinking factor for shapes (and \link{coo_template};
#' 3/4 by default)
#' @param palette on of [palettes]
#' @examples
#' bot %>% efourier(6) %>% mshapes("type") %>% plot_mshapes
#' # above, a shortcut for working with the result of mshapes
#' # but works on list of shapes, eg:
#' leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo
#' class(leaves)
#' leaves %>% plot_mshapes()
#'
#' # or
#' shapes %>%
#' # subset and degrade
#' slice(1:12) %>% coo_sample(60) %$%  # grab the coo
#' coo %>%
#' plot_mshapes()
#' @export
plot_mshapes <- function(x, size=3/4, palette=pal_div){
  # neater par
  op <- par(mar=c(4, 4, 0, 0))
  on.exit(par(op))

  ms <- x
  cols <- pal_div(3)[-2]
  # so that the result of mshapes can directly be passed
  if (all(names(ms) %in% c("Coe", "shp")))
    ms <- ms$shp
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
        draw_outline(col=par("fg"), transp=0.5)
      else
        ms[[i]] %>%
        coo_trans(x=x, y=i) %>%
        draw_outline(col=cols[1], transp=0.25)
      if (x != i) # not diagonal
        ms[[i]] %>%
        coo_trans(x=i, y=x) %>%
        draw_outline(col=cols[2], transp = 0.25)
    }
  }
}
