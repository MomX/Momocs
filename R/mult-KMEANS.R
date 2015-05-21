#' k-means
#'
#' A very basic and prototypic implementation of k-means
#' @param x PCA object
#' @param centers numeric number of centers
#' @param nax numeric the range of PC components to use (1:2 by default)
#' @param ... additional arguments to be passed to \link{kmeans}
#' @return the same thing as \link{kmeans}
#' @example
#' data(bot)
#' bp <- PCA(efourier(bot, 10))
#' KMEANS(bp, 2)
#' @export
KMEANS <- function(x, ...){
  UseMethod("KMEANS")
}
#' @export
KMEANS.PCA <- function(x, centers, nax=1:2, ...){
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
  points(xy, pch=20, cex=0.5, col=cols[clusts])
  points(km$centers, pch=3, col=cols)
  Momocs:::morphospacePCA(bp, 1, 2, pos.shp=km$centers)
  box()
  return(km)
}
