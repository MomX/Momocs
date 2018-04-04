#' KMEDOIDS
#'
#' A basic implementation of kmedoids on top of [cluster::pam]
#' Beware that morphospaces are calculated so far for the 1st and 2nd
#' component.
#' @param x a [Coe] or [PCA] object
#' @param k numeric number of centers
#' @param metric one of `euclidean` (default) or `manhattan`, to feed [cluster::pam]
#' @param retain when passing a [PCA] how many PCs to retain, or a proportion of total variance, see [LDA]
#' @param ... additional arguments to feed [cluster::pam]
#' @return  see [cluster::pam]. Other components are returned (`fac`, etc.)
#' @examples
#' data(bot)
#' bp <- PCA(efourier(bot, 10))
#' KMEANS(bp, 2)
#' @rdname KMEDOIDS
#' @family multivariate
#' @examples
#'
#' set.seed(123) # for reproducibility on a dummy matrix
#' matrix(rnorm(100, 10, 10)) %>%
#' KMEDOIDS(5)
#'
#' # On a Coe
#' bot_f <- bot %>% efourier()
#'
#' bot_k <- bot_f %>% KMEDOIDS(2)
#' # confusion matrix
#' table(bot_k$fac$type, bot_k$clustering)
#'
#' # on a PCA
#' bot_k2 <- bot_f %>% PCA() %>% KMEDOIDS(12, retain=0.9)
#'
#' # confusion matrix
#' with(bot_k, table(fac$type, clustering))
#' # silhouette plot
#' bot_k %>% plot_silhouette()
#'
#' # average width as a function of k
#' k_range <- 2:12
#' widths <- sapply(k_range, function(k) KMEDOIDS(bot_f, k=k)$silinfo$avg.width)
#' plot(k_range, widths, type="b")
#' @export
KMEDOIDS <- function(x,
                     k,
                     metric="euclidean",
                     ...){
  UseMethod("KMEDOIDS")
}

#' @rdname KMEDOIDS
#' @export
KMEDOIDS.default <- function(x,
                             k,
                             metric="euclidean",
                             ...){

  .check(!missing(k),
         "k must be provided to KMEDOIDS")
  # copy the original
  x0 <- x

  # some checks
  if (!is.matrix(x))
    x <- as.matrix(x)

  # handles constant and collinear variables
  ids_constant  <- which_constant(x)
  if (length(ids_constant)>0){
    ids_collinear <- which_collinear(x[, -ids_constant])
  } else {
    ids_collinear <- which_collinear(x)
  }

  # message about them, if verbose
  if (.is_verbose()){
    if (length(ids_constant)>0){
      if (is.null(colnames(x))){
        message("removed", length(ids_constant), "collinear columns")
      } else {
        message("removed these collinear columns:", paste(colnames(x)[ids_constant], collapse=", "))
      }
    }
    if (length(ids_collinear)>0){
      if (is.null(colnames(x))) {
        message("removed", length(ids_collinear), "collinear columns")
      } else {
        message("removed these collinear columns:", paste(colnames(x)[ids_collinear], collapse=", "))
      }
    }
  }

  # drop concerned columns
  ids_drop <- c(ids_constant, ids_collinear)
  if (length(ids_drop)>0)
    x <- x[, -c(ids_constant, ids_collinear)]

  # finally calculate kmedoids using cluster::pam
  res <- cluster::pam(x, k=k, metric=metric, ...)

  # add infos
  res$k <- k
  res$ids_constant <- ids_constant
  res$ids_collinear <- ids_collinear

  res
}

#' @rdname KMEDOIDS
#' @export
KMEDOIDS.Coe <- function(x,
                         k,
                         metric="euclidean",
                         ...){

  res <- KMEDOIDS(x$coe, k=k, metric=metric, ...)

  if (!is.null(x$method))
    res$method <- x$method
  if (!is.null(x$cuts))
    res$cuts <- x$cuts
  if (!is.null(x$fac))
    res$fac <- x$fac
  # if (!is.null(x$mshape))
  #   res$mshape <- x$mshape

  res$mshape <- apply(x$coe, 2, mean)

  res
}

#' @rdname KMEDOIDS
#' @export
KMEDOIDS.PCA <- function(x,
                         k,
                         metric="euclidean",
                         retain,
                         ...){

  # if missing retain take all PCs
  if (missing(retain))
    retain <- ncol(x$x)

  # if provided and below 1, use scree_min to capture the
  # right number of PCS
  if (length(retain)==1 && retain < 1)
    retain <- scree_min(x, retain)

  # then send it to KMEDOIDS default
  res <- KMEDOIDS(x$x[, 1:retain], k=k, metric=metric, ...)

  # add important informations
  if (!is.null(x$method))
    res$method <- x$method
  if (!is.null(x$cuts))
    res$cuts <- x$cuts
  if (!is.null(x$fac))
    res$fac <- x$fac
  # if (!is.null(x$mshape))
  #   res$mshape <- x$mshape

  # res$mshape <- apply(x$coe, 2, mean)

  res
}


## Silhouette ------

#' Silhouette plot
#'
#' Only used, so far, after [KMEDOIDS].
#'
#' @param x object returned by [KMEDOIDS]
#' @param palette one of [palettes]
#'
#' @return a `ggplot` plot
#' @examples
#'
#' olea %>% opoly(5) %>%
#'     KMEDOIDS(4) %>%
#'     plot_silhouette(pal_qual_solarized)
#' @export
plot_silhouette <- function(x, palette=pal_qual){
  x <- x$silinfo$widths
  x %>%
    dplyr::as_data_frame() %>%
    dplyr::mutate(x=1:nrow(x),
                  cluster=factor(cluster),
                  neighbor=factor(neighbor)) %>%
    dplyr::select(x, cluster, neighbor, width=sil_width) %>%
    dplyr::group_by(cluster) %>%
    dplyr::arrange(desc(width), .by_group=TRUE) %>%
    dplyr::ungroup() %>%
    arrange(desc(width))-> df

  ggplot(df) +
    aes(y=width, x=x, fill=cluster) +
    geom_col(width=1) +
    scale_fill_manual(values=palette(nlevels(df$cluster))) +
    coord_flip() +
    # xlim(min(df$width), 1) +
    # geom_hline(yintercept = seq(0.5, 1, 0.25), col=pal_seq_OrRd(3), alpha=0.5) +
    geom_hline(yintercept = mean(df$width), col=par("fg"), alpha=0.5) +
    labs(y="Width") +
    theme_empty() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}



