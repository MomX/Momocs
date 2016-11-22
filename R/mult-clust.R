##### Hierarchical methods for Coe objects.  todo (to a certain
##### exten)

#' Hierarchical clustering
#'
#' Performs hierarchical clustering through \link{dist} and \link{hclust}. So far it is mainly
#' a wrapper around these two functions, plus plotting using \link{plot.phylo} from the
#' package ape.
#' @param x a PCA object (Coe method deprecated so far)
#' @param fac the id or column name or formula for columns to use from $fac.
#' @param type to pass to \code{ape::plot.phylo}'s \code{type} argument, one of
#' "cladogram", "phylogram", "radial", "unrooted" or "fan" (by default)
#' @param dist_method to feed \link{dist}'s \code{method} argument, one of
#' "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param hclust_method to feed \link{hclust}'s method argument, one of
#' "ward.D", "ward.D2", "single", "complete" (default), "average", "mcquitty", "median" or "centroid".
# #' @param mono logical whether to color branches according to their /fac mononophyly status
# #' @param abbreviate numeric, if specified passed as minlength argument to \link{abbreviate}
#' @param retain number of axis to retain from the PCA as a range of number eg \code{1:5} to retain
#' the first 5 PCs. If a number <= 1 is passed, then the number of PCs retained
#' will be enough to capture this proportion of variance.
#' @param tip_labels the id or column name in $fac to use as tip_labels rather than rownames. Note that
#' you can also pass a character (or a factor) with the same number of rows of x$x
#' @param palette a color palette to use (\code{col_qual} by default).
#' If \code{NULL}, \code{par("fg")} is used
#' @param ... additional parameters to feed \code{plot.phylo}
#' @return the \code{phylo} object, invisibly
#' @family multivariate
#' @examples
#' \dontrun{
#'
#' # we prepare a PCA with shorter names
#' olea_lite <- olea
#' names(olea_lite) <- as.character(olea$fac$var)
#' x <- olea_lite %>% opoly(5) %>% PCA()
#'
#' # By default
#' CLUST(x)
#'
#' # With a fac
#' CLUST(x, 1)
#'
#' # plot.phylo types
#' CLUST(x, "var", type="cladogram")
#' CLUST(x, "var", type="phylogram")
#' CLUST(x, "var", type="radial")
#' CLUST(x, "var", type="unrooted")
#'
#' # other dist/hclust methods
#' CLUST(x, "var", layout="cladogram", dist_method="minkowski", hclust_method="average")
#'
#' # With another
#' CLUST(x, "domes", tip_labels="var", palette=col_india)
#'
#' # Alternative ways to pass a factor
#' CLUST(x, 1)
#' CLUST(x, "var")
#' CLUST(x, ~var)
#' # Strict equivalent before but formula allows this:
#' CLUST(x, ~ domes + var, tip_labels = ~ domes + var)
#'
#' # More arguments to plot.phylo
#' CLUST(x, cex=0.5)
#'
#' }
#' @rdname CLUST
#' @export
CLUST <- function(x,
                  fac,
                  type="fan",
                  dist_method="euclidean",
                  hclust_method="complete",
                  retain=0.99,
                  tip_labels,
                  palette=col_qual, ...) {
  UseMethod("CLUST")
}

#' @rdname CLUST
#' @export
CLUST.default <- function(x,
                      ...) {
  stop("only available on PCA objects so far")}

#' @export
CLUST.PCA <- function(x,
                      fac,
                      type="fan",
                      dist_method="euclidean",
                      hclust_method="complete",
                      retain=0.99,
                      tip_labels,
                      palette=col_qual, ...){

  # if retain is a proportion of the total variance, we got capture it
  if (length(retain)==1 && retain <= 1){
    retain <- scree_min(x, retain)
  }
  # we build the phylo object
  phylo <- x$x[, retain] %>%
    dist(method = dist_method) %>%
    hclust(method = hclust_method) %>%
    ape::as.phylo()

  # we grab the fac
  if (!missing(fac))
    fac <- prepare_fac(x, fac)

  # and color it if palette is not null
  if (is.null(palette) | missing(fac)){
    tip.colors <- rep(par("fg"), nrow(x$x))
  } else {
  tip.colors <- palette(nlevels(fac))[fac]
  }

  # prepares tip.label
  if (!missing(tip_labels))
    phylo$tip.label <- as.character(.prep.fac(x, tip_labels))

  # plot the phylo
  plot(phylo, type=type, tip.color=tip.colors, ...)

  # returns it, invisibly
  invisible(phylo)
}
##### end clust



