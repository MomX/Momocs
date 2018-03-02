
#' Pairwise comparison of a list of shapes
#'
#' "Confusion matrix" of a list of shapes. See examples.
#' @note Inspired by Chitwood et al. (2016) New Phytologist
#' @param x a list of shapes (eg as returned by \link{mshapes})
#' @param size numeric shrinking factor for shapes (and \link{coo_template};
#' 3/4 by default)
#' @param col2 color as hexadecimal ("#FF0000", eg red, by default)
#' @examples
#' bot %>% efourier(6) %>% mshapes("type") %>% plot_mshapes
#' # above, a shortcut for working with the result of mshapes
#' # but works on list of shapes, eg:
#' leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo
#' class(leaves)
#' leaves %>% plot_mshapes(col2="#0000FF")
#' @export
plot_mshapes <- function(x, size=3/4, col2="#FF0000"){
  ms <- x
  # so that the result of mshapes can directly be passed
  if (all(names(ms) %in% c("Coe", "shp")))
    ms <- ms$shp
  # template shapes
  ms %<>% lapply(coo_template, size)
  # initializes the plot
  n <- length(ms)
  plot(NA, xlim=c(0.5, n+0.5), ylim=c(0.5, n+0.5), axes=F, ann=F, asp=1)
  axis(1, at=1:n, tick = FALSE, labels = names(ms), col.axis=col2)
  axis(2, at=1:n, tick = FALSE, labels = names(ms), las=2)
  abline(h=0:n, v=0:n, col="grey90", lty=2)
  # draw shapes
  for (i in 1:n){
    for (x in 1:n){
      ms[[i]] %>% coo_trans(x=x, y=i) %>% coo_draw(border="#00000055", centroid=FALSE, first.point=FALSE)
      if (x == i) next() # diagonal
      ms[[i]] %>% coo_trans(x=i, y=x) %>% coo_draw(border=paste0(col2, "88"), centroid=FALSE, first.point=FALSE)
    }
  }
}
