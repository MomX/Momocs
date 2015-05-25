#' KMEANS on PCA objects
#'
#' A very basic and prototypic implementation of k-means.
#' Beware that morphospaces are calculated so far for the 1st and 2nd
#' component.
#' @param x PCA object
#' @param centers numeric number of centers
#' @param nax numeric the range of PC components to use (1:2 by default)
#' @param pch to draw the points
#' @param cex to draw the points
#' @param ... additional arguments to be passed to \link{kmeans}
#' @return the same thing as \link{kmeans}
#' @examples
#' data(bot)
#' bp <- PCA(efourier(bot, 10))
#' KMEANS(bp, 2)
#' @rdname KMEANS
#' @export
KMEANS <- function(x, ...){
  UseMethod("KMEANS")
}

#' @rdname KMEANS
#' @export
KMEANS.PCA <- function(x, centers, nax=1:2, pch=20, cex=0.5, ...){
  xy <- x$x[, nax]
  km <- kmeans(xy, centers, ...)
  clusts <- factor(km$cluster)
  cols <- col_qual(nlevels(clusts))
  on.exit(par(opar))
  opar <- par(mar = par("mar"), xpd = FALSE)
  par(mar = rep(0.1, 4))
  .frame(xy, center.origin = TRUE, zoom = 1)
  .grid(5)
  .chull(xy, as.factor(km$cluster), col = cols, lty=2)
  points(xy, pch=pch, cex=cex, col=cols[clusts])
  points(km$centers, pch=3, col=cols)
  morphospacePCA(x, 1, 2, pos.shp=km$centers,
                          col.shp = "#00000011", border.shp="#000000")
  box()
  return(km)
}
