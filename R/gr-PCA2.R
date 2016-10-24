#' Plots Principal Component Analysis ala ggplot2
#'
#' Displays a \link{PCA} object with many useful layers in morphometrics,
#' notably morphological space.
#' @param x a PCA object
#' @param fac (optionnal)
#' @param xax the name of the component to plot on the x-axis, eg "PC1"
#' @param yax the name of the component to plot on the y-axis, eg "PC2"
#' @param points logical whether to draw points
#' @param shapes logical whether to draw shapes
#' @param shapes_pos a position parameter xxx either "axes", "full", "range", "circle", "xy"
#' or a custom data.frame of positions
#' @param ellipse logical whether to draw confidence ellipses
#' @param ellipseax logical whether to draw confidence ellipses axes
#' @param ellipse_type character one of the available type in ggplot2::stat_ellipse
#' @param ellipse_level numeric the confidence level for ellipses
#' @param stars logical whether to draw segments between every point and their group centroid
#' @param chull logical whether to draw convex hull
#' @param text numeric : 1 is for labelling every shape,
#' 2 for group centroids, 3 is for evry shape using group names
#' @param text_abbreviatemin numeric if not missing, the min.length sensu \link{abbreviate}
#' @param text_size numeric to adjust the size of labels
#' @param center logical whether to center the plot
#' @param legend_position character for theme(legend.position),
#' either "none", "top", "bottom", "left", "right"
#' @param title character a title for the plot
#' @param return_df logical whether to return ggplot or the data_frames behind
#' @param ... more arguments to be passed to xxx.
#' @return a ggplot object or a list of data.frames :
#' \itemize{
#' \item df0
#' \item df_shp
#' \item df_ellipseax
#' \item df_stars
#' \item df_chull
#' }
#' @details Detail the df_s. Detail the calculations.
#' @examples
#' #data(bot)
#' #bp <- PCA(efourier(bot, 8))
#' #plot2(bp)
#' #plot2(bp, "type")
#' #plot2(bp, "type", ellipse=TRUE)
#' # data(bot)
#'
#' # bot$fac$fake <- factor(rep(letters[1:4], 10))
#' # bot$fac$fake2 <- c(runif(20), runif(20, 5, 10))
#' # xx + stat_density2d(aes(fill = ..level..), geom="polygon", alpha=0.1)
# # xx + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE, alpha=0.2) +geom_point()
#  #legend.position = c(0.14, 0.80),


#' @rdname plot2.PCA
# #' @export
plot2 <- function(x, ...){ UseMethod("plot2")}
#' @rdname plot2.PCA
# #' @export
plot2.PCA <- function(x,
                      fac=NULL, xax="PC1", yax="PC2",
                      points = TRUE,
                      shapes = TRUE, shapes_pos="full",
                      ellipse = FALSE, ellipseax = FALSE,
                      ellipse_type="t", ellipse_level=0.50,
                      stars = FALSE,
                      chull = FALSE,
                      text,
                      text_abbreviatemin = 1, text_size = 5,
                      center = TRUE,
                      legend_position = "bottom",
                      title = substitute(x),
                      return_df = FALSE,
                      ...){
  ### Preliminaries
  df <- as_df(x) #%>% as.tbl()
  # initialize the gg
  if (!is.null(fac)){
    .f <- TRUE
    df0 <- select_(df, x=xax, y=yax, f=fac, .id=quote(.id))
    gg <- ggplot(data=df, aes_string(x=xax, y=yax, col=fac))
  } else {
    .f <- FALSE
    df0 <- select_(df, x=xax, y=yax, .id=quote(.id))
    gg <- ggplot(data=df, aes_string(x=xax, y=yax))
  }
  # we prepare for return_df
  df_shp <- df_ellipseax <- df_stars <- df_chull <- df_text <- NULL

  if (.f){
    if (missing(ellipseax)) ellipseax <- TRUE
    if (missing(text)) text <- 2
  }

  ###### LAYERS
  # +points --------------------------------------------------------------------
  if (points) gg <- gg + geom_point()
  # +shapes --------------------------------------------------------------------
  if (shapes) {
    gg0 <- gg + geom_point()
    if (center){
      wdw <- max(abs(.x.range.gg(gg0)), abs(.y.range.gg(gg0)))
      gg0 <- gg0 + coord_equal(xlim=c(-wdw, wdw), ylim=c(-wdw, wdw))
    }
    pos <- morphospace.pos(select_(df, x=xax, y=yax),
                           pos.shp=shapes_pos, gg=gg0, ...)
    df_shp <- morphospace2PCA(x, xax, yax, pos, wdw=max(.wdw.gg(gg0)))
    gg <- gg + geom_path(data=df_shp,
                         aes_string(x="x", y="y", group="shp1"), col="black", alpha=0.5)
  } else df_shp <- NULL
  ### factor-dependent methods
  if (.f) {
    #+ellipse -------------------------------------------------------------------
    if (ellipse) {
      suppressWarnings(gg <- gg + stat_ellipse(type=ellipse_type, level = ellipse_level))
      }
    #+ellipseax -----------------------------------------------------------------
    if (ellipseax){
      df_ellipseax <- ddply(df0, ~f, function(x)
       calculate_ellipse(x, 1:2, type =ellipse_type, level=ellipse_level, 360) %>%
          calculate_ellipseax())
      suppressWarnings(gg <- gg + geom_segment(data=df_ellipseax,
                              aes(x=x, xend=xend, y=y, yend=yend, col=f)))
    } else df_ellipseax <- NULL
    #+ stars ---------------------------------------------------------------------
    if (stars) {
      df_stars <-  mutate(group_by(df0, f), mx=mean(x), my=mean(y))
      gg <- gg +  geom_segment(data=df_stars,
                               aes_string(x="x", y="y", xend="mx", yend="my", col="f"), alpha=0.25)
    } else df_stars <- NULL
    #+ chull
    if (chull){
      df_chull <- ddply(df0, ~f, function(x) coo_close(coo_chull(x[, 1:2])))
      gg  <- gg + geom_path(data=df_chull, aes(x=x, y=y, col=f))
    }
  }
  #+ text ---------------------------------------------------------------------
  if (!missing(text)){
    if (!.f){ # case with no factor
      df_text <- df0
      if (!missing(text_abbreviatemin)) {
        df_text <- mutate(df_text,
                          .id = abbreviate(.id, minlength = text_abbreviatemin))
      }
      gg <- gg + geom_text(data=df_text, aes(x=x, y=y, label=.id),
                           col="black", size=text_size, show_guide=FALSE)
    } else { # case with a factor
      if (text == 1){
        df_text <- df0}
      if (text == 2) { # a label on group centroids
        df_text <- summarise(group_by(df0, f), x=mean(x), y=mean(y)) %>% mutate(.id=f)}
      if (text == 3) { # a group label on every point
        df_text <- mutate(df0, .id=f)}
      if (!missing(text_abbreviatemin)) {
        df_text <- mutate(df_text,
                          .id = abbreviate(.id, minlength = text_abbreviatemin))
      }
      gg <- gg + geom_text(data=df_text, aes(x=x, y=y, label=.id, col=f),
                           size=text_size, show_guide=FALSE)
    }
  }
  ## Cosmetics
  # because shapes and points may be FALSE
  if (center){
    gg0 <- gg + geom_point()
    wdw <- max(abs(.x.range.gg(gg0)), abs(.y.range.gg(gg0)))
    gg <- gg + coord_equal(xlim=c(-wdw, wdw), ylim=c(-wdw, wdw))
  }

 if (missing(legend_position)) legend_position <- "none"
  gg <- gg + ggtitle(title) +
    theme_minimal() +
    theme(legend.position=legend_position)
  # here is the baby
  if (return_df) {
    return(list(df0=df0,
                df_shp=df_shp,
                df_ellipseax = df_ellipseax,
                df_stars=df_stars,
                df_chull=df_chull))
  } else {
    return(gg)
  }
}



#
#
# #+ eigen -------
# if (eigen) {
#   xr <- .x.range.gg(gg)
#   yr <- .y.range.gg(gg)
#   # axis as ids
#   axi <- as.numeric(substr(c(xax, yax), 3, 3))
#   axi_len <- ifelse(abs(diff(axi)) < 5, 5, abs(diff(axi))+1)
#   # we retrieve the var and their proportion
#   eig <- (x$sdev^2)[1:ifelse(max(axi)<5, 5, max(axi))]
#   eig <- (eig - min(eig))
#   eig <- eig / max(eig)
#   eig <- eig * (diff(yr)*0.1)
#   xp <- seq(xr[2]*0.90, xr[2]*0.99, length=axi_len)
#   yp <- yr[1] + 0.01*diff(yr)
#   cols <- rep("black", axi_len)
#   cols[axi] <- "red" # damn dirty
#   df_eig <- data.frame(x=xp, xend=xp, y=yp, yend=yp+eig)
#   gg <- gg + geom_segment(data=df_eig, aes(x=x, xend=xend, y=y, yend=yend),
#                           colour=cols,
#                           size=2, alpha=0.8)
# }

#' Methods for PCA eigen values
#'
#' A set of functions around PCA/LDA eigen/trace. \code{scree} calculates their proportion and cumulated proportion;
#' \code{scree_min} returns the minimal number of axis to use to retain a given proportion; \code{scree_plot} displays a screeplot.
#'
#' @param x a \link{PCA} object
#' @param nax numeric range of axis to consider
#' @param prop numeric how many axis are enough this proportion of variance, if too high then number of axis is returned.
#' @return scree returns a data.frame, scree_min a numeric, scree_plot a ggplot.
#' @examples
#' data(bot)
#' # On PCA
#' bp <- PCA(efourier(bot))
#' scree(bp)
#' scree_min(bp, 0.99)
#' scree_min(bp, 1)
#'
#' scree_plot(bp)
#' scree_plot(bp, 1:5)
#'
#' # on LDA, it uses svd
#' data(olea)
#' bl <- LDA(PCA(opoly(olea)), "var")
#' scree(bl)
#'
#' @export
#' @rdname scree
scree <- function(x, nax) {
  UseMethod("scree")}

#' @export
#' @rdname scree
scree.PCA <- function(x, nax=1:10){
  eig <- (x$sdev^2)
  eig <- eig / sum(eig)
  if (max(nax)>length(eig)) nax <- 1:length(eig)
  eig <- eig[nax]
  df <-  data_frame(axis=ordered(1:length(eig)), proportion=eig, cumsum=cumsum(eig))
df
}

#' @export
#' @rdname scree
scree.LDA <- function(x, nax=1:10){
  eig <- (x$mod$svd^2)
  eig <- eig / sum(eig)
  if (max(nax)>length(eig)) nax <- 1:length(eig)
  eig <- eig[nax]
  df <-  data_frame(axis=ordered(1:length(eig)), proportion=eig, cumsum=cumsum(eig))
  df
}

#' @export
#' @rdname scree
scree_min <- function(x, prop=0.99){
  enough <- scree(x)$cumsum >= prop
  ifelse(any(enough), min(which(enough)), length(enough))
}

#' @export
#' @rdname scree
scree_plot <- function(x, nax=1:10){
  df <- scree(x, nax)
  gg <- ggplot(df, aes_string(x="axis", y="proportion")) +
    geom_hline(yintercept=c(0.5, 0.90, 0.95, 0.99), linetype=2, alpha=0.5) +
    geom_bar(stat="identity") + geom_text(label=round(df$cumsum, 3), vjust=0) +
    labs(x="Components", y="Proportion")
  gg
}


# selected=NULL,
# return(df)
# fills <- rep("black", nrow(df))
# fills[selected] <- "red"
# gg <- ggplot(data=df, aes(x=x, y=y), fill=fill) +
#   geom_bar(fill=fills, stat="identity") +
#   labs(x="Components", y="Variances")
# gg



#### borrowed from ggplot2 by Hadley
calculate_ellipse <- function(data, vars, type, level, segments){
  dfn <- 2
  dfd <- nrow(data) - 1
  if (!type %in% c("t", "norm", "euclid")){
    message("Unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else if (dfd < 3){
    #message("Too few points to calculate an ellipse")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else {
    if (type == "t"){
      v <- cov.trob(data[,vars])
    } else if (type == "norm"){
      v <- cov.wt(data[,vars])
    } else if (type == "euclid"){
      v <- cov.wt(data[,vars])
      v$cov <- diag(rep(min(diag(v$cov)), 2))
    }
    shape <- v$cov
    center <- v$center
    chol_decomp <- chol(shape)
    if (type == "euclid"){
      radius <- level/max(chol_decomp)
    } else {
      radius <- sqrt(dfn * qf(level, dfn, dfd))
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  }
  ellipse <- as.data.frame(ellipse)
  colnames(ellipse) <- vars
  return(ellipse)
}


calculate_ellipseax <- function(ell){
  if (any(is.na(ell))) {
    na <- rep(NA, 2)
    seg <- data.frame(x=na, y=na, xend=na, yend=na)
    return(seg)
  }
  ell.al <- coo_align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]),
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  seg <- bind_cols(slice(seg, c(1, 3)), slice(seg, c(2, 4)))
  colnames(seg) <- c("x", "y", "xend", "yend")
  seg
}


