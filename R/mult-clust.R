##### Hierarchical methods for Coe objects.  todo (to a certain
##### exten)

#' Hierarchical clustering
#'
#' Performs hierarchical clustering through \link{dist} and \link{hclust}. So far it is mainly
#' a wrapper around these two functions, plus plotting using \link{plot.phylo} from the
#' package ape.
#' @param x a PCA object (Coe method deprecated so far)
#' @param fac the id or column name in $fac to use for colors and mono
#' @param layout to pass to ggtree, one of 
#' "cladogram", "phylogram", "dendrogram", "radial", "unrooted", "fan" (by default) 
#' @param dist_method to feed \link{dist}'s method argument, one of 
#' "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param hclust_method to feed \link{hclust}'s method argument, one of 
#' "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
#' @param mono logical whether to color branches according to their /fac mononophyly status
#' @param abbreviate numeric, if specified passed as minlength argument to \link{abbreviate}
#' @param tip_fac the id or column name in $fac to use as tip_labels rather than rownames 
#' @param ... useless here
#' @return a ggplot object
#' @keywords Multivariate Graphics
#' @examples 
#' data(bot)
#' bp <- PCA(efourier(bot, 10))
#' CLUST(bp)
#' CLUST(bp, layout="phylogram")
#' CLUST(bp, layout="cladogram")
#' CLUST(bp, layout="dendrogram")
#' CLUST(bp, layout="unrooted")
#' CLUST(bp, layout="radial") 
#' CLUST(bp, "type")
#' CLUST(bp, 1, layout="cladogram", mono=FALSE) #no monophyly coloring
#' 
#' # styling with ggplot2 grammar
#' library(ggplot2)
#' CLUST(bp, 1) + scale_color_discrete(h=c(120, 240))
#' 
#' # tip_fac is useful !
#' data(olea)
#' op <- PCA(opoly(olea, 5))
#' CLUST(op, "var", tip_fac="var")
#' @rdname CLUST
#' @export
CLUST <- function(x,  ...) {
    UseMethod("CLUST")
}

#' @rdname CLUST
#' @export
CLUST.Coe <- function(x,  ...) {
  stop(" * deprecated for the moment")}
#     Coe <- x
#     if (missing(fac)) {
#         cols <- rep("black", nrow(Coe$coe))
#     } else {
#         facs <- Coe$fac[, fac]
#         cols <- palette(nlevels(facs))[facs]
#     }
#     dist.mat <- dist(Coe$coe, method = method)
#     Coe.hc <- hclust(dist.mat)
#     op <- par(no.readonly = TRUE)
#     on.exit(par(op))
#     par(oma = rep(0, 4), mar = rep(0, 4))
#     phy <- as.phylo.hclust(Coe.hc)
#     plot(phy, tip.color = cols, type = type, ...)
#     invisible(list(dist.mat = dist.mat, hclust = Coe.hc))

#' @rdname CLUST
#' @export
CLUST.PCA <- function(x, fac, layout="fan",
                     dist_method="euclidean",
                     hclust_method="complete",
                     mono=TRUE, abbreviate=NULL, tip_fac = NULL, ...){
    
  # This methods owes a lot to phytools and, more importantly, to ggtree package.
  # Unfortunately, it's internally a mess. Due to R CMD CHECKs and others joys,
  # I copied inside this method bits of code that I have adapted just to make it work.
  # Will review/clean it when I'll have some time.

    # from Revell
    getDescendants<-function(tree,node,curr=NULL){
      if(is.null(curr)) curr<-vector()
      daughters<-tree$edge[which(tree$edge[,1]==node),2]
      curr<-c(curr,daughters)
      w<-which(daughters>=length(tree$tip))
      if(length(w)>0) for(i in 1:length(w))
        curr<-getDescendants(tree,daughters[w[i]],curr)
      return(curr)
    }
    
    geom_tree2 <-
      function (layout = "phylogram", color = "black", linetype = "solid",
                size = 0.5, ...)
      {
        x <- y <- parent <- NULL
        lineend = "round"
        if (layout == "phylogram" || layout == "fan") {
          if (length(color) != 1) {
            color <- rep(color, 2)
          }
          if (length(linetype) != 1) {
            linetype <- rep(linetype, 2)
          }
          if (length(size) != 1) {
            size <- rep(size, 2)
          }
          geom_segment(aes(x = c(x[parent], x[parent]),
                           xend = c(x, x[parent]),
                           y = c(y, y[parent]),
                           yend = c(y, y), col=rep(mono, 2)),
                       linetype = linetype, size = size,
                       lineend = lineend, ...)
        }
        else if (layout == "cladogram" || layout == "unrooted") {
          geom_segment(aes(x = x[parent], xend = x, y = y[parent],
                           yend = y, col=mono), linetype = linetype, size = size,
                       lineend = lineend, ...)
        }
      }
    
    ggtree2 <-
      function (tr, showDistance = FALSE, layout = "phylogram", yscale = "none",
                ladderize = TRUE, right = FALSE, branch.length = "branch.length",
                ndigits = NULL, mono=FALSE,...)
      {
        d <- x <- y <- NULL
        if (layout == "fan") {
          type <- "fan"
        }
        else if (layout == "radial") {
          layout <- "cladogram"
          type <- "radial"
        }
        else if (layout == "dendrogram") {
          layout <- "phylogram"
          type <- "dendrogram"
        }
        else {
          type <- "none"
        }
        p <- ggplot(tr, aes(x, y), layout = layout, yscale = yscale,
                    ladderize = ladderize, right = right, branch.length = branch.length,
                    ndigits = ndigits, ...)
        if (mono){
          p <- p + geom_tree2(layout, ...) + xlab("") + ylab("") + theme_tree()
        } else {
          p <- p + geom_tree(layout, ...) + xlab("") + ylab("") + theme_tree()
        }
        if (type == "dendrogram") {
          p <- p + scale_x_reverse() + coord_flip()
        }
        else if (type == "fan" || type == "radial") {
          p <- p + coord_polar(theta = "y")
          p <- p + scale_y_continuous(limits=c(0, sum(p$data$isTip)))
        }
        if (showDistance == FALSE) {
          p <- p + theme_tree()
        }
        attr(p, "param") <- list(layout = layout, yscale = yscale,
                                 ladderize = ladderize, right = right, branch.length = branch.length,
                                 ndigits = ndigits)
        return(p)
      }
    if (!is.null(tip_fac))
      rownames(x$x) <- as.character(x$fac[, tip_fac])
    if (!is.null(abbreviate))
      rownames(x$x) <- abbreviate(rownames(x$x), minlength = abbreviate)
    
    
    fortify_phylo <- 
    function (model, data, layout = "phylogram", ladderize = TRUE, 
              right = FALSE, ...) 
    {
      if (ladderize == TRUE) {
        tree <- ladderize(model, right = right)
      }
      else {
        tree <- model
      }
      df <- as.data.frame(tree, layout = layout, ...)
      idx <- is.na(df$parent)
      df$parent[idx] <- df$node[idx]
      rownames(df) <- df$node
      cn <- colnames(df)
      colnames(df)[grep("length", cn)] <- "branch.length"
      if (layout == "cladogram") {
        df <- add_angle_cladogram(df)
      }
      return(df)
    }
    
    add_angle_cladogram <-
    function (res) 
    {
      dy <- (res[, "y"] - res[res$parent, "y"])/diff(range(res[, 
                                                               "y"]))
      dx <- (res[, "x"] - res[res$parent, "x"])/diff(range(res[, 
                                                               "x"]))
      theta <- atan(dy/dx)
      theta[is.na(theta)] <- 0
      res$angle <- theta/pi * 180
      branch.y <- (res[res$parent, "y"] + res[, "y"])/2
      idx <- is.na(branch.y)
      branch.y[idx] <- res[idx, "y"]
      res[, "branch.y"] <- branch.y
      return(res)
    }
    
    # clust <- function(x, fac, layout){
    phylo <- dist(x$x, method = dist_method) %>% hclust(method = hclust_method) %>% ape::as.phylo()
    if (missing(fac)){
      fac <- factor(rep("a", nrow(x$x)))
    } else {
      fac <- x$fac[, fac]
    }
    phylo_df <- fortify_phylo(phylo, layout=layout)
    fac_long <- fac
    fac_long[(length(fac)+1):nrow(phylo_df)] <- NA
    phylo_df <- cbind(phylo_df, fac=fac_long)
    
    if (mono){
      N <- fac
      for (i in phylo_df$node){
        children <-  getDescendants(phylo, i)
        if (length(children)==0){
          N[i] <- NA
          next
        }
        ids <- phylo_df[children, ] %>% na.omit()
        ids <- ids$node
        tips <- fac[ids] %>% unique
        
        if (length(tips)>1) {
          res <- NA
        } else {
          # res <- match(tips, levels(fac))
          res <- tips
        }
        N[i] <- res
      }
      N[1:length(fac)] <- fac
      phylo_df$mono <- N
      gg <- ggtree2(phylo_df, layout=layout, mono=TRUE)
    } else if (layout=="unrooted") {
      gg <- ggtree(phylo_df, layout=layout)
    } else {
      gg <- ggtree2(phylo_df, layout=layout, mono=FALSE)
    }
    
    if (any(layout=="unrooted", layout=="fan")) {
      # gg$data$angle <- abs(gg$data$angle)
      ang <- gg$data$angle
      ang <- (ang -270) %% 360
      # gg$data$angle[gg$data$angle < -180] <- gg$data$angle[gg$data$angle < -90] +180
      upsidedown <- (ang > 90 & ang < 270)
      ang[upsidedown] <- ang[upsidedown] + 180
      gg$data$angle <- ang
      gg$data <- mutate(gg$data, hjust=c(-0.2, 1.2)[upsidedown+1])
      gg <- gg + geom_text(aes(label=label, col=fac, angle=angle, hjust=hjust), size=4, na.rm=TRUE)
    } else if (layout=="dendrogram"){
      gg <- gg + geom_text(aes(label=label, col=fac), angle=90, hjust=1, size=4,na.rm=TRUE)
    } else {
      # if (any(layout=="phylogram")) {
      gg <- gg + geom_text(aes(label=label, col=fac), hjust=0, size=4, na.rm=TRUE)
    }
    if (nlevels(fac)==1){
      gg <- gg + scale_color_discrete(l=0)
    }
    return(gg)
  }


##### end clust



