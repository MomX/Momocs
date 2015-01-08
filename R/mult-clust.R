##### Hierarchical methods for Coe objects.  todo (to a certain
##### exten)

#' Hierarchical clustering
#' 
#' Performs hierarchical clustering through \link{dist} and \link{hclust}. So far it is mainly
#' a wrapper around these two functions, plus plotting using \link{plot.phylo} from the
#' package ape.
#' @param x an Coe object
#' @param fac a column or an id from $fac
#' @param method the method to use among \code{'euclidean'}, \code{'maximum'}, 
#' \code{'manhattan'}, \code{'canberra'}, \code{'binary'} or \code{'minkowski'} to feed \link{dist}
#' @param type the type of plot among \code{'phylogram'} (the default), \code{'cladogram'},
#' \code{'fan'}, \code{'unrooted'} and \code{'radial'} to feed \link{plot.phylo}
#' @param palette a color palette to use
#' @param ... additional arguments to fed \link{plot.phylo}
#' @return a (invisible) list with the following components:
#' \itemize{
#' \item dist.mat the distance matrix
#' \item hclust the hclust object
#' }
#' @keywords Multivariate Graphics
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 12)
#' CLUST(bot.f)
#' bot.c <- CLUST(bot.f, 'type', cex=0.7)
#' #bot.c
#' 
#' data(olea)
#' olea.VL <- subset(olea, view=='VL')
#' op <- npoly(olea.VL, 5)
#' CLUST(op, 1, type='cladogram', palette=col_autumn, cex=0.5)
#' 
#' data(wings)
#' wp <- fgProcrustes(wings)
#' CLUST(wp, 1, cex=0.5)

#' @export
CLUST <- function(x, fac, method, type, palette, ...) {
    UseMethod("CLUST")
}
#' @export
CLUST.Coe <- function(x, fac, method = "euclidean", type = "unrooted", 
    palette = col_summer, ...) {
    Coe <- x
    if (missing(fac)) {
        cols <- rep("black", nrow(Coe$coe))
    } else {
        facs <- Coe$fac[, fac]
        cols <- palette(nlevels(facs))[facs]
    }
    dist.mat <- dist(Coe$coe, method = method)
    Coe.hc <- hclust(dist.mat)
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(oma = rep(0, 4), mar = rep(0, 4))
    phy <- as.phylo.hclust(Coe.hc)
    plot(phy, tip.color = cols, type = type, ...)
    invisible(list(dist.mat = dist.mat, hclust = Coe.hc))
}

#' @export
CLUST.PCA <- function(x, fac, method = "euclidean", type = "unrooted", 
                      palette = col_summer, ...) {
  if (missing(fac)) {
    cols <- rep("black", nrow(x$x))
  } else {
    facs <- x$fac[, fac]
    cols <- palette(nlevels(facs))[facs]
  }
  dist.mat <- dist(x$x, method = method)
  Coe.hc <- hclust(dist.mat)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(oma = rep(0, 4), mar = rep(0, 4))
  phy <- as.phylo.hclust(Coe.hc)
  plot(phy, tip.color = cols, type = type, 
       ...)
  invisible(list(dist.mat = dist.mat, hclust = Coe.hc))
}

##### end clust 



