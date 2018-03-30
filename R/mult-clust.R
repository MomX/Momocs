##### Hierarchical methods for Coe objects.

#' Hierarchical clustering
#'
#' Performs hierarchical clustering through [dist] and [hclust]. So far it is mainly
#' a wrapper around these two functions, plus plotting using the `dendextend` package facilities.
#'
#' @param x a [Coe] or [PCA] object
#' @param fac factor specification for [fac_dispatcher]
#' @param type `character` one of `c("horizontal", "vertical", "fan")` (default: `horizontal`)
#' @param k `numeric` if provided, cut the tree into this number of groups
#' @param dist_method to feed [dist]'s `method` argument, that is one of
#' `euclidean` (default), `maximum`, `manhattan`, `canberra`, `binary` or `minkowski`.
#' @param hclust_method to feed [hclust]'s `method` argument, one of
#' `ward.D`, `ward.D2`, `single`, `complete` (default), `average`, `mcquitty`, `median` or `centroid`.
#' @param retain number of axis to retain if a [PCA] object is passed. If a number < 1 is passed, then the number of PCs retained
#' will be enough to capture this proportion of variance via [scree_min]
#' @param labels factor specification for labelling tips and to feed [fac_dispatcher]
#' @param lwd for branches (default: `0.25`)
#' @param cex for labels (default: `1`)
#' @param palette one of available [palettes]
#' @param ... useless here
#' @return a `ggplot` plot
#' @family multivariate
#' @examples
#' # On Coe
#' bf <- bot %>% efourier(6)
#' CLUST(bf)
#' # with a factor and vertical
#' CLUST(bf, ~type, "v")
#' # with some cutting and different dist/hclust methods
#' CLUST(bf,
#'       dist_method="maximum", hclust_method="average",
#'       labels=~type, k=3, lwd=1, cex=1, palette=pal_manual(c("green", "yellow", "red")))
#'
#' # On PCA
#' bf %>% PCA %>% CLUST
#'
#' @rdname CLUST
#' @export
CLUST <- function(x, ...) {
  UseMethod("CLUST")
}

#' @rdname CLUST
#' @export
CLUST.default <- function(x, ...) {
  message("only supported for Coe and PCA")
}

#' @rdname CLUST
#' @export
CLUST.Coe <- function(x,
                      fac,
                      type=c("horizontal", "vertical", "fan")[1],
                      k,
                      # handle k and manage precedence over fac k,
                      dist_method="euclidean",
                      hclust_method="complete",
                      retain=0.99,
                      labels,
                      # cosmetic param
                      lwd=1/4,
                      cex=1/2,
                      palette=pal_qual,
                      ...
){

  # some checks
  types <- c("vertical", "horizontal", "fan")
  .check(!is.na(pmatch(type, types)),
         "type must be one of 'vertical', 'horizontal', 'fan'")
  type <- types[pmatch(type, types)]

  if (type == "fan")
    message("labels not (yet) supported when type='fan'")

  # take the coe, calculate a dist, then a hclust
  # with appropriate methods
  # then turn it into a dendrogram
  d <- x$coe %>%
    stats::dist(method = dist_method) %>%
    stats::hclust(method = hclust_method) %>%
    stats::as.dendrogram()

  # handle labels (could be shortened)
  dendextend::`labels<-`(d, names(x))
  if (!missing(fac))          # if fac is provided, use it
    dendextend::`labels<-`(d, as.character(fac_dispatcher(x, fac)))
  if (!missing(labels))      # but if labels is provided, overwrite it
    dendextend::`labels<-`(d, as.character(fac_dispatcher(x, labels)))

  # # handles abbreviation
  # if (!missing(abbreviate_n)){ # abbreviate if required
  #   .check(is.numeric(abbreviate_n),
  #          "abbreviate, when provided, must be an integer")
  #   labels(d) %<>% abbreviate(minlength = abbreviate_n)
  # }
  # return(d)
  #
  # lab_f <- factor(lab_f)
  # leaves_cols <- palette(nlevels(lab_f))[lab_f]
  #see ?set
  d %<>%
    dendextend::set("branches_lwd", lwd) %>%
    dendextend::set("labels_cex", cex)

  # cut, if required
  if (!missing(k)){
    d %<>% dendextend::set("branches_k_color", palette(k), k=k)
  }

  # color labels
  if (!missing(fac)){
    f <- fac_dispatcher(x, fac)
    d %<>% dendextend::set("labels_colors",
                           palette(nlevels(f))[f])
  }

  # handle type
  if (type=="horizontal")
    horiz <- TRUE
  else
    horiz <- FALSE

  if (type=="fan")
    labels=FALSE
  else
    labels=TRUE

  # finally ggplot it
  gg <- d %>% dendextend::as.ggdend() %>% ggplot(horiz=horiz)

  # lastly handle fan
  if (type=="fan")
    gg <- gg + coord_polar(theta="x") + scale_y_reverse(expand = c(0.2, 0))

  # return this beauty
  gg
}

#' @export
CLUST.PCA <- function(x,
                      fac,
                      type=c("horizontal", "vertical", "fan")[1],
                      k,
                      # handle k and manage precedence over fac k,
                      dist_method="euclidean",
                      hclust_method="complete",
                      retain=0.99,
                      labels,
                      # cosmetic param
                      lwd=1/4,
                      cex=1/2,
                      palette=pal_qual,
                      ...){

  # if retain is a proportion of the total variance, we got capture it
  if (length(retain)==1 && retain < 1){
    retain <- scree_min(x, retain)
  }

  # Build a vanilla Coe and send it to CLUST
  x <- TraCoe(x$x[, 1:retain, drop=FALSE], fac = x$fac) %>% `names<-`(rownames(x$x))

  # we build the phylo object
  CLUST(x,
        fac=fac,
        type=type,
        k=k,
        # handle k and manage precedence over fac k,
        dist_method=dist_method,
        hclust_method=hclust_method,
        labels=labels,
        # cosmetic param
        lwd=lwd,
        cex=cex,
        palette=palette,
        ...)
}

##### end clust



