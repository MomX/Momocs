
#' Hierarchical clustering #todo
#' 
#' @param OutCoe an OutCoe object
#' @param fac a colum or an id from $fac
#' @param method the method to use
#' @param type the type of plot
#' @param palette a color palette to use
#' @param ... additional arguments to fed plot.phylo
#' @keywords Multivariate
#' @export
clust <- function(OutCoe, fac, method, type, palette, ...){UseMethod("clust")}
#' @export
clust.OutCoe <- function(OutCoe, fac,
                         method = "euclidean", type="unrooted", palette=col.summer, ...){
  if (missing(fac)) {
    cols <- rep("black", nrow(OutCoe$coe))
  } else {
    facs <- OutCoe$fac[, fac]
    cols <- palette(nlevels(facs))[facs]
  }
  dist.mat <- dist(OutCoe$coe, method=method)
  OutCoe.hc <- hclust(dist.mat)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(oma=rep(0, 4), mar=rep(0,4))
  plot(as.phylo.hclust(OutCoe.hc), tip.color=cols, type=type, ...)
  return(list(dist.mat=dist.mat, hclust=OutCoe.hc))}
