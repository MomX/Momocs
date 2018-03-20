
# 1. calibrate_reconstructions -----------
#' Calibrate using reconstructed shapes
#'
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number. Compare them visually with the maximal fit.
#' This explicitely demonstrates how robust efourier is compared to tfourier and rfourier.
#'
#' @param x the \code{Coo} object on which to calibrate_reconstructions
#' @param id the shape on which to perform calibrate_reconstructions
#' @param range vector of harmonics on which to perform calibrate_reconstructions
#' @param baseline1 \eqn{(x; y)} coordinates for the first point of the baseline
#' @param baseline2 \eqn{(x; y)} coordinates for the second point of the baseline
#' @param ... only used for the generic
#' @return a ggplot object and the full list of intermediate results. See examples.
#' @family calibration
#' @name calibrate_reconstructions
#' @examples
#'
#' ### On Out
#' shapes %>%
#'     calibrate_reconstructions_efourier(id=1, range=1:6)
#'
#' # you may prefer efourier...
#' shapes %>%
#'     calibrate_reconstructions_tfourier(id=1, range=1:6)
#'
#' #' you may prefer efourier...
#' shapes %>%
#'     calibrate_reconstructions_rfourier(id=1, range=1:6)
#'
#' #' you may prefer efourier... # todo
#' #shapes %>%
#' #     calibrate_reconstructions_sfourier(id=5, range=1:6)
#'
#' ### On Opn
#' olea %>%
#'     calibrate_reconstructions_opoly(id=1)
#'
#' olea %>%
#'     calibrate_reconstructions_npoly(id=1)
#'
#' olea %>%
#'     calibrate_reconstructions_dfourier(id=1)
#'
#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions <- function(){
  message("Deprecated, see ?calibrate_reconstructions")
}

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_efourier <-
  function(x,
           id,
           range = 1:9) {
    # we detect the method
    # Out dispatcher
    .check(is_Out(x),
           "only defined on Out")
    Out <- x
    method <- efourier
    method_i <- efourier_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Out$coo), 1)
    coo <- Out$coo[[id]]
    coo <- coo_center(coo)
    # check for too ambitious harm.range
    max.h <- nrow(coo)/2 - 1
    if (max(range) > max.h) {
      range <- floor(seq(1, max.h, length = 9))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we calculate all shapes
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, nb.h = max(range)), nb.h = range[i])
    }
    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- method_i(method(coo, nb.h = max.h))
    best <- coo_close(best)
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmetics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_polygon(aes(fill=id), alpha=0.5) +
      geom_path(data=best, aes_string(x="x", y="y")) +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_rfourier <-
  function(x,
           id,
           range = 1:9) {

    .check(is_Out(x),
           "only defined on Out")

    # Out dispatcher
    Out <- x
    method <- rfourier
    method_i <- rfourier_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Out$coo), 1)
    coo <- Out$coo[[id]]
    coo <- coo_center(coo)
    # check for too ambitious harm.range
    max.h <- nrow(coo)/2 - 1
    if (max(range) > max.h) {
      range <- floor(seq(1, max.h, length = 9))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we calculate all shapes
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, nb.h = max(range)), nb.h = range[i])
    }
    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- method_i(method(coo, nb.h = max.h))
    best <- coo_close(best)
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmetics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_polygon(aes(fill=id), alpha=0.5) +
      geom_path(data=best, aes_string(x="x", y="y")) +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_tfourier <-
  function(x,
           id,
           range = 1:9) {

    .check(is_Out(x),
           "only defined on Out")

    # Out dispatcher
    Out <- x
    method <- tfourier
    method_i <- tfourier_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Out$coo), 1)
    coo <- Out$coo[[id]]
    coo <- coo_center(coo)
    # check for too ambitious harm.range
    max.h <- nrow(coo)/2 - 1
    if (max(range) > max.h) {
      range <- floor(seq(1, max.h, length = 9))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we calculate all shapes
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, nb.h = max(range)), nb.h = range[i])
    }
    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- method_i(method(coo, nb.h = max.h))
    best <- coo_close(best)
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmetics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_polygon(aes(fill=id), alpha=0.5) +
      geom_path(data=best, aes_string(x="x", y="y")) +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_sfourier <-
  function(x,
           id,
           range = 1:9) {

    .check(is_Out(x),
           "only defined on Out")

    # Out dispatcher
    Out <- x
    method <- sfourier
    method_i <- sfourier_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Out$coo), 1)
    coo <- Out$coo[[id]]
    coo <- coo_center(coo)
    # check for too ambitious harm.range
    max.h <- nrow(coo)/2 - 1
    if (max(range) > max.h) {
      range <- floor(seq(1, max.h, length = 9))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we calculate all shapes
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, nb.h = max(range)), nb.h = range[i])
    }
    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- method_i(method(coo, nb.h = max.h))
    best <- coo_close(best)
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmetics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_polygon(aes(fill=id), alpha=0.5) +
      geom_path(data=best, aes_string(x="x", y="y")) +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_npoly <-
  function(x,
           id,
           range = 2:10,
           baseline1 = c(-1, 0),
           baseline2 = c(1, 0)) {


    # Opn dispatcher
    Opn <- x

    .check(is_Opn(x),
           "only defined on Opn")

    method   <- npoly
    method_i <- npoly_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Opn$coo), 1)
    coo <- Opn$coo[[id]]
    coo <- coo_baseline(coo,
                        ldk1 = 1, ldk2 = nrow(coo),
                        t1 = baseline1, t2 = baseline2)

    # we check for too ambitious range
    # special case for opoly

    if (max(range) > (nrow(coo) - 1)) {
      range <- 2:10
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we loop
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, range[i]))
    }

    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- res[[length(res)]]
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmectics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_path(data=best, aes_string(x="x", y="y"), alpha=0.5) +
      geom_path(aes(col=id)) +
      # scale_color_gradient2() +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Opn)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_opoly <-
  function(x,
           id,
           range = 2:10,
           baseline1 = c(-1, 0),
           baseline2 = c(1, 0)) {

    # Opn dispatcher
    Opn <- x

    .check(is_Opn(x),
           "only defined on Opn")

    method   <- opoly
    method_i <- opoly_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Opn$coo), 1)
    coo <- Opn$coo[[id]]
    coo <- coo_baseline(coo,
                        ldk1 = 1, ldk2 = nrow(coo),
                        t1 = baseline1, t2 = baseline2)

    # we check for too ambitious range
    # special case for opoly

    if (max(range) > 20) range <- 2:20

    if (max(range) > (nrow(coo) - 1)) {
      range <- 2:10
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we loop
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, range[i]))
    }

    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- res[[length(res)]]
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmectics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_path(data=best, aes_string(x="x", y="y"), alpha=0.5) +
      geom_path(aes(col=id)) +
      # scale_color_gradient2() +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Opn)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions_dfourier <-
  function(x,
           id,
           range = 2:10,
           baseline1 = c(-1, 0),
           baseline2 = c(1, 0)) {

    # Opn dispatcher
    Opn <- x

    .check(is_Opn(x),
           "only defined on Opn")

    method   <- dfourier
    method_i <- dfourier_i

    # we sample a shape
    if (missing(id))
      id <- sample(length(Opn$coo), 1)
    coo <- Opn$coo[[id]]
    coo <- coo_baseline(coo,
                        ldk1 = 1, ldk2 = nrow(coo),
                        t1 = baseline1, t2 = baseline2)

    if (max(range) > (nrow(coo) - 1)) {
      range <- 2:10
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }

    # we loop
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, range[i]))
    }

    # we prepare the plot
    names(res) <- range
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>%
      do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- res[[length(res)]]
    best <- dplyr::data_frame(x=best[, 1], y=best[, 2])
    # cosmectics
    theme_empty <- theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank(),
                         strip.background=element_rect(fill="grey95"),
                         strip.text=element_text(colour = "grey10"),
                         strip.text.x=element_text(colour = "grey10"),
                         strip.text.y=element_text(colour = "grey10"))

    gg <- ggplot(data=coos, aes_string(x="x", y="y")) +
      coord_equal() + geom_path(data=best, aes_string(x="x", y="y"), alpha=0.5) +
      geom_path(aes(col=id)) +
      # scale_color_gradient2() +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Opn)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

# 2. calibrate_deviations -------------------------
#' Quantitative calibration, through deviations, for Out and Opn objects
#'
#' Calculate deviations from original and reconstructed shapes using a
#' range of harmonic number.
#'
#' Note that from version 1.1, the calculation changed and fixed a problem. Before,
#' the 'best' possible shape was calculated using the highest possible number of harmonics.
#' This worked well for efourier but not for others (eg rfourier, tfourier) as they
#' are known to be unstable with high number of harmonics. From now on, Momocs uses
#' the 'real' shape, as it is (so it must be centered) and uses \link{coo_interpolate}
#' to produce \code{interpolate.factor} times more coordinates as the shape
#' has and using the default \code{dist.method}, eg \link{edm_nearest},
#' the latter finds the euclidean distance, for each point on the reconstructed shape,
#' the closest point on this interpolated shape. \code{interpolate.factor} being set
#' to 1 by default, no interpolation will be made in you do not ask for it. Note,
#' that interpolation to decrease artefactual errors may also be done outside
#' \code{calibrate_deviations} and will be probably be removed from it
#' in further versions.
#'
#' Note also that this code is quite old now and would need a good review,
#' planned for 2018.
#'
#' @param x and \code{Out} or \code{Opn} object on which to calibrate_deviations
#' @param id the shape on which to perform calibrate_deviations
#' @param range vector of harmonics (or degree for opoly and npoly on Opn) on which to perform calibrate_deviations.
#' If not provided, the harmonics corresponding to 0.9, 0.95 and 0.99% of harmonic power
#' are used.
#' @param norm.centsize logical whether to normalize deviation by the centroid size
#' @param dist.method a method such as \link{edm_nearest} to calculate deviations
#' @param interpolate.factor a numeric to increase the number of points on the original shape (1 by default)
#' @param dist.nbpts numeric the number of points to use for deviations calculations
#' @param plot logical whether to print the graph (FALSE is you just want the calculations)
#' @details For *poly methods on Opn objects, the deviations are calculated from a degree 12 polynom.
#' @return a ggplot object and the full list of intermediate results. See examples.
#' @family calibration
#' @examples
#' b5 <- slice(bot, 1:5) #for the sake of speed
#' b5 %>% calibrate_deviations_efourier()
#' b5 %>% calibrate_deviations_rfourier()
#' b5 %>% calibrate_deviations_tfourier()
#' b5 %>% calibrate_deviations_sfourier()
#'
#' o5 <- slice(olea, 1:5) #for the sake of speed
#' o5 %>% calibrate_deviations_opoly()
#' o5 %>% calibrate_deviations_npoly()
#' o5 %>% calibrate_deviations_dfourier()
#'
#' @rdname calibrate_deviations
#' @export
calibrate_deviations <- function() {
  message("Deprecated, see ?calibrate_deviations")
}

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_efourier <-
  function(x,
           id = 1, range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Coo <- x
    # Out dispatcher

    # missing lineat.y
    if (missing(range)) {
      hr <- calibrate_harmonicpower_efourier(Coo, plot=FALSE)
      range <- unique(hr$minh)
    }

    method <- efourier
    method.i <- efourier_i

    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(coo_nb(Coo))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious range
    if (max(range) > nb.h.best) {
      range <- floor(seq(4, nb.h.best, length = 6))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("h", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))
    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq_along(id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h = nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, nb.h = range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% dplyr::as_data_frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      xx <- m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)
      colnames(xx) <- c("harm", "pt", "med")
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    # we plot the ggplot
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_tfourier <-
  function(x,
           id = 1, range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Coo <- x
    # Out dispatcher

    # missing lineat.y
    if (missing(range)) {
      hr <- calibrate_harmonicpower_tfourier(Coo, plot=FALSE)
      range <- unique(hr$minh)
    }

    method <- tfourier
    method.i <- tfourier_i

    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(coo_nb(Coo))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious range
    if (max(range) > nb.h.best) {
      range <- floor(seq(4, nb.h.best, length = 6))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("h", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))
    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq_along(id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h = nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, nb.h = range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% dplyr::as_data_frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      xx <- m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)
      colnames(xx) <- c("harm", "pt", "med")
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    # we plot the ggplot
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_rfourier <-
  function(x,
           id = 1, range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Coo <- x
    # Out dispatcher

    # missing lineat.y
    if (missing(range)) {
      hr <- calibrate_harmonicpower_rfourier(Coo, plot=FALSE)
      range <- unique(hr$minh)
    }

    method <- rfourier
    method.i <- rfourier_i

    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(coo_nb(Coo))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious range
    if (max(range) > nb.h.best) {
      range <- floor(seq(4, nb.h.best, length = 6))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("h", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))
    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq_along(id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h = nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, nb.h = range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% dplyr::as_data_frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      xx <- m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)
      colnames(xx) <- c("harm", "pt", "med")
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    # we plot the ggplot
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_sfourier <-
  function(x,
           id = 1, range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Coo <- x
    # Out dispatcher

    # missing lineat.y
    if (missing(range)) {
      hr <- calibrate_harmonicpower_sfourier(Coo, plot=FALSE)
      range <- unique(hr$minh)
    }

    method <- sfourier
    method.i <- sfourier_i

    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(coo_nb(Coo))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious range
    if (max(range) > nb.h.best) {
      range <- floor(seq(4, nb.h.best, length = 6))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("h", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))
    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq_along(id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h = nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, nb.h = range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% dplyr::as_data_frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      xx <- m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)
      colnames(xx) <- c("harm", "pt", "med")
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    # we plot the ggplot
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_npoly <-
  function(x,
           id = 1,
           range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Opn(x),
           "only defined on Opn")
    Coo <- x
    # missing lineat.y
    if (missing(range)) {
      message("'range' was missing and set to 1:8")
      range <- 1:8
    }

    method <- npoly
    method.i <- npoly_i


    nb.pts <- min.nb.pts <- min(sapply(Coo$coo[id], function(x) nrow(unique(x))))
    nb.h.best <- 12
    message("deviations calculated from a degree 12 polynom")

    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)

    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("d", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))

    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq(along = id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the open outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)  -> xx
      colnames(xx) <- c("harm", "pt", "med")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med
      xx$mpsd <- xx$med
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the open outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_opoly <-
  function(x,
           id = 1,
           range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Opn(x),
           "only defined on Opn")
    Coo <- x
    # missing lineat.y
    if (missing(range)) {
      message("'range' was missing and set to 1:8")
      range <- 1:8
    }

    method <- opoly
    method.i <- opoly_i


    nb.pts <- min.nb.pts <- min(sapply(Coo$coo[id], function(x) nrow(unique(x))))
    nb.h.best <- 12
    message("deviations calculated from a degree 12 polynom")

    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)

    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("d", range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))

    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq(along = id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the open outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)  -> xx
      colnames(xx) <- c("harm", "pt", "med")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med
      xx$mpsd <- xx$med
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the open outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thresh)) {
    #       gg <- gg + geom_hline(aes(yintercept=thresh))
    #     }
    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

#' @rdname calibrate_deviations
#' @export
calibrate_deviations_dfourier <-
  function(x,
           id = 1,
           range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {

    .check(is_Opn(x),
           "only defined on Opn")
    Coo <- x
    # missing lineat.y
    if (missing(range)) {
      message("'range' was missing and set to 1:8")
      range <- 1:8
    }

    method <- dfourier
    method.i <- dfourier_i

    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(coo_nb(Coo))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious range
    if (max(range) > nb.h.best) {
      range <- floor(seq(4, nb.h.best, length = 6))
      message("'range' was too high and set to ", paste(range, collapse=" "))
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)

    res <- array(NA, dim = c(nr, nb.pts, nk),
                 dimnames = list(paste0("h", range),
                                 paste("pt", 1:nb.pts),
                                 names(Coo)[id]))

    # progressbar
    if (nk > 5) {
      pb <- progress::progress_bar$new(total = nk)
      t <- TRUE
    } else {
      t <- FALSE
    }
    # the core loops that will calculate deviations
    for (ind in seq(along = id)) {
      coo <- Coo$coo[[id[ind]]] #Coo[id]?
      # below, the best possible fit
      # coo_best <- method.i(method(coo, nb.h.best), nb.pts = nb.pts)
      coo_best <- coo_interpolate(coo, coo_nb(coo)*interpolate.factor)
      for (i in seq(along = range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_i, coo_best)
      }
      # we normalize by the centroid size and prepare the y.title
      if (norm.centsize) {
        res[, , ind] <- res[, , ind]/coo_centsize(coo)
        y.title <- "Deviation (in % of the centroid size)"
      } else {
        y.title <- "Deviation (in original units)"
      }
      if (t)
        pb$tick()
    }
    # below we manage for single/several individuals if more than
    # 1, we calculate median and sd
    if (nk > 1) {
      m <- apply(res, 1:2, median)
      d <- apply(res, 1:2, sd)
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .) %>%
        cbind(as.numeric(d)) -> xx
      colnames(xx) <- c("harm", "pt", "med")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med
      xx$mpsd <- xx$med
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      # hideous but avoid the aes_string problem fro ribbon todo
      xx$mmsd <- xx$med
      xx$mpsd <- xx$med
      # we ggplot
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_ribbon(aes_string(ymin="mmsd", ymax="mpsd",
                               fill="harm"), linetype=0,  alpha=0.1) +
        geom_line(aes_string(x="pt", y="med", col="harm")) +
        labs(x="Points along the open outline", y=y.title,
             col=NULL, fill=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$mpsd)))
    } else {
      m <- res[, , 1]
      d <- NULL
      # we prepare a df
      # we prepare a df
      m %>% as.data.frame() %>% seq_along %>%
        lapply(function(i) data.frame(Var1=rownames(m),
                                      Var2=colnames(m)[i],
                                      value=m[,i])) %>%
        do.call("rbind", .)  -> xx
      colnames(xx) <- c("harm", "pt", "med")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med
      xx$mpsd <- xx$med
      # hideous - todo
      xx$pt <- xx$pt %>% gsub("pt ", "", .) %>% as.numeric
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the open outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }

    if (plot){
      print(gg)
      invisible(list(gg=gg, res = res, m = m, d = d))
    } else {
      return(list(gg=gg, res = res, m = m, d = d))
    }
  }

# 3. calibrate_harmonicpower ----------------
#' Quantitative calibration, through harmonic power, for Out and Opn objects
#'
#' Estimates the number of harmonics required for the four Fourier methods
#' implemented in Momocs: elliptical Fourier analysis
#' (see \link{efourier}), radii variation analysis (see \link{rfourier})
#' and tangent angle analysis (see \link{tfourier}) and
#' discrete Fourier transform (see \link{dfourier}).
#' It returns and can plot cumulated harmonic power whether dropping
#' the first harmonic or not, and based and the maximum possible number
#' of harmonics on the \code{Coo} object.
#'
#' @param x a \code{Coo} of \code{Opn} object
#' @param id the shapes on which to perform calibrate_harmonicpower. All of them by default
#' @param nb.h numeric the maximum number of harmonic, on which to base the cumsum
#' @param drop numeric the number of harmonics to drop for the cumulative sum
#' @param thresh vector of numeric for drawing horizontal lines, and also used for
#' \code{minh} below
#' @param plot logical whether to plot the result or simply return the matrix
#' Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#' @return returns a list with component:
#' \itemize{
#' \item \code{gg} a ggplot object, \code{q} the quantile matrix
#' \item \code{minh} a quick summary that returns the number of harmonics required to achieve
#' a certain proportion of the total harmonic power.
#' }
#' @details
#' The power of a given harmonic \eqn{n} is calculated as follows for
#' elliptical Fourier analysis and the n-th harmonic:
#' \eqn{HarmonicPower_n \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}}
#' and as follows for radii variation and tangent angle:
#' \eqn{HarmonicPower_n= \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}}
#' @family calibration
#' @examples
#' b5 <- bot %>% slice(1:5)
#' b5  %>% calibrate_harmonicpower_efourier(nb.h=12)
#' b5  %>% calibrate_harmonicpower_rfourier(nb.h=12)
#' b5  %>% calibrate_harmonicpower_tfourier(nb.h=12)
#' b5  %>% calibrate_harmonicpower_sfourier(nb.h=12)
#'
#' # on Opn
#' olea %>% slice(1:5) %>%
#'     calibrate_harmonicpower_dfourier(nb.h=12)
#' \dontrun{
#' # let customize the ggplot
#' library(ggplot2)
#' cal <- b5  %>% calibrate_harmonicpower_efourier(nb.h=12)
#' cal$gg + theme_minimal() +
#' coord_cartesian(xlim=c(3.5, 12.5), ylim=c(90, 100)) +
#' ggtitle("Harmonic power calibration")
#' }
#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower <- function(){
  message("Deprecated, see ?calibrate_harmonicpower")
}

#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower_efourier <-
  function(x, id = 1:length(x),
           nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
           plot=TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Out <- x
    # we swith among methods, with a messsage
    method <- efourier

    # here we define the maximum nb.h, if missing
    if (missing(nb.h)){
      nb.h <- floor(min(sapply(Out$coo, nrow))/2)
    }
    # we prepare the result matrix
    res <- matrix(nrow = length(id), ncol = (nb.h - drop))
    x <- (drop + 1):nb.h
    for (i in seq(along = id)) {
      xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
      res[i, ] <- harm_pow(xf)[x]
    }
    rownames(res) <- names(Out)[id]
    colnames(res) <- paste0("h", 1:ncol(res))
    # we remove dropped harmonics
    #res <- res[, -drop]
    # we calculte cumsum and percentages
    res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
    # we ggplot
    h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
    res %>% as.data.frame() %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(res),
                                    Var2=colnames(res)[i],
                                    value=res[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("shp", "harm", "hp")) -> xx
    if (length(id) > 2) {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    } else {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_point() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    }
    if (plot) print(gg)
    # we calculate quantiles and add nice rowcolnames
    # also the median (independently of probs [0.5, etc]) since
    # thresh may change
    med.res <- apply(res, 2, median)
    minh <- numeric(length(thresh))
    names(minh) <- paste0(thresh, "%")
    for (i in seq(along=thresh)){
      wi <- which(med.res > thresh[i])
      minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
    minh <- minh+drop
    # talk to me
    if (.is_verbose()){
      #     cat("\n$minh:\n")
      print(minh)
    }
    # we return the full matrix, the ggplot and the thresholds
    invisible(list(gg=gg, q=res, minh=minh))
  }

#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower_rfourier <-
  function(x, id = 1:length(x),
           nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
           plot=TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Out <- x
    # we swith among methods, with a messsage
    method <- rfourier

    # here we define the maximum nb.h, if missing
    if (missing(nb.h)){
      nb.h <- floor(min(sapply(Out$coo, nrow))/2)
    }
    # we prepare the result matrix
    res <- matrix(nrow = length(id), ncol = (nb.h - drop))
    x <- (drop + 1):nb.h
    for (i in seq(along = id)) {
      xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
      res[i, ] <- harm_pow(xf)[x]
    }
    rownames(res) <- names(Out)[id]
    colnames(res) <- paste0("h", 1:ncol(res))
    # we remove dropped harmonics
    #res <- res[, -drop]
    # we calculte cumsum and percentages
    res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
    # we ggplot
    h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
    res %>% as.data.frame() %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(res),
                                    Var2=colnames(res)[i],
                                    value=res[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("shp", "harm", "hp")) -> xx
    if (length(id) > 2) {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    } else {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_point() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    }
    if (plot) print(gg)
    # we calculate quantiles and add nice rowcolnames
    # also the median (independently of probs [0.5, etc]) since
    # thresh may change
    med.res <- apply(res, 2, median)
    minh <- numeric(length(thresh))
    names(minh) <- paste0(thresh, "%")
    for (i in seq(along=thresh)){
      wi <- which(med.res > thresh[i])
      minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
    minh <- minh+drop
    # talk to me
    if (.is_verbose()){
      #     cat("\n$minh:\n")
      print(minh)}
    # we return the full matrix, the ggplot and the thresholds
    invisible(list(gg=gg, q=res, minh=minh))
  }

#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower_tfourier <-
  function(x, id = 1:length(x),
           nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
           plot=TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Out <- x
    # we swith among methods, with a messsage
    method <- tfourier

    # here we define the maximum nb.h, if missing
    if (missing(nb.h)){
      nb.h <- floor(min(sapply(Out$coo, nrow))/2)
    }
    # we prepare the result matrix
    res <- matrix(nrow = length(id), ncol = (nb.h - drop))
    x <- (drop + 1):nb.h
    for (i in seq(along = id)) {
      xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
      res[i, ] <- harm_pow(xf)[x]
    }
    rownames(res) <- names(Out)[id]
    colnames(res) <- paste0("h", 1:ncol(res))
    # we remove dropped harmonics
    #res <- res[, -drop]
    # we calculte cumsum and percentages
    res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
    # we ggplot
    h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
    res %>% as.data.frame() %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(res),
                                    Var2=colnames(res)[i],
                                    value=res[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("shp", "harm", "hp")) -> xx
    if (length(id) > 2) {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    } else {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_point() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    }
    if (plot) print(gg)
    # we calculate quantiles and add nice rowcolnames
    # also the median (independently of probs [0.5, etc]) since
    # thresh may change
    med.res <- apply(res, 2, median)
    minh <- numeric(length(thresh))
    names(minh) <- paste0(thresh, "%")
    for (i in seq(along=thresh)){
      wi <- which(med.res > thresh[i])
      minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
    minh <- minh+drop
    # talk to me
    if (.is_verbose()){
      #     cat("\n$minh:\n")
      print(minh)}
    # we return the full matrix, the ggplot and the thresholds
    invisible(list(gg=gg, q=res, minh=minh))
  }

#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower_sfourier <-
  function(x, id = 1:length(x),
           nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
           plot=TRUE) {

    .check(is_Out(x),
           "only defined on Out")

    Out <- x
    # we swith among methods, with a messsage
    method <- sfourier

    # here we define the maximum nb.h, if missing
    if (missing(nb.h)){
      nb.h <- floor(min(sapply(Out$coo, nrow))/2)
    }
    # we prepare the result matrix
    res <- matrix(nrow = length(id), ncol = (nb.h - drop))
    x <- (drop + 1):nb.h
    for (i in seq(along = id)) {
      xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
      res[i, ] <- harm_pow(xf)[x]
    }
    rownames(res) <- names(Out)[id]
    colnames(res) <- paste0("h", 1:ncol(res))
    # we remove dropped harmonics
    #res <- res[, -drop]
    # we calculte cumsum and percentages
    res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
    # we ggplot
    h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
    res %>% as.data.frame() %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(res),
                                    Var2=colnames(res)[i],
                                    value=res[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("shp", "harm", "hp")) -> xx
    if (length(id) > 2) {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    } else {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_point() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    }
    if (plot) print(gg)
    # we calculate quantiles and add nice rowcolnames
    # also the median (independently of probs [0.5, etc]) since
    # thresh may change
    med.res <- apply(res, 2, median)
    minh <- numeric(length(thresh))
    names(minh) <- paste0(thresh, "%")
    for (i in seq(along=thresh)){
      wi <- which(med.res > thresh[i])
      minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
    minh <- minh+drop
    # talk to me
    if (.is_verbose()){
      #     cat("\n$minh:\n")
      print(minh)}
    # we return the full matrix, the ggplot and the thresholds
    invisible(list(gg=gg, q=res, minh=minh))
  }


#' @rdname calibrate_harmonicpower
#' @export
calibrate_harmonicpower_dfourier <-
  function(x, id = 1:length(x),
           nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
           plot=TRUE) {

    .check(is_Opn(x),
           "only defined on Out")

    Out <- x
    # we swith among methods, with a messsage
    method <- dfourier

    # here we define the maximum nb.h, if missing
    if (missing(nb.h)){
      nb.h <- floor(min(sapply(Out$coo, nrow))/2)
    }
    # we prepare the result matrix
    res <- matrix(nrow = length(id), ncol = (nb.h - drop))
    x <- (drop + 1):nb.h
    for (i in seq(along = id)) {
      xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
      res[i, ] <- harm_pow(xf)[x]
    }
    rownames(res) <- names(Out)[id]
    colnames(res) <- paste0("h", 1:ncol(res))
    # we remove dropped harmonics
    #res <- res[, -drop]
    # we calculte cumsum and percentages
    res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
    # we ggplot
    h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
    res %>% as.data.frame() %>% seq_along %>%
      lapply(function(i) data.frame(Var1=rownames(res),
                                    Var2=colnames(res)[i],
                                    value=res[,i])) %>%
      do.call("rbind", .) %>%
      `colnames<-`(c("shp", "harm", "hp")) -> xx
    if (length(id) > 2) {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    } else {
      gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_point() +
        labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
        coord_cartesian(xlim=c(0.5, h_display+0.5))
    }
    if (plot) print(gg)
    # we calculate quantiles and add nice rowcolnames
    # also the median (independently of probs [0.5, etc]) since
    # thresh may change
    med.res <- apply(res, 2, median)
    minh <- numeric(length(thresh))
    names(minh) <- paste0(thresh, "%")
    for (i in seq(along=thresh)){
      wi <- which(med.res > thresh[i])
      minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
    minh <- minh+drop
    # talk to me
    if (.is_verbose()){
      #     cat("\n$minh:\n")
      print(minh)}
    # we return the full matrix, the ggplot and the thresholds
    invisible(list(gg=gg, q=res, minh=minh))
  }

# 4. calibrate_r2 ----------------
#' Quantitative r2 calibration for Opn objects
#'
#' Estimates the r2 to calibrate the degree for \link{npoly} and \link{opoly} methods.
#' Also returns a plot
#'
#' @param Opn an Opn object
#' @param id the ids of shapes on which to calculate r2 (all by default)
#' @param degree.range on which to calculate r2
#' @param thresh the threshold to return diagnostic
#' @param plot logical whether to print the plot
#' @param ... useless here
#' @details May be long, so you can estimate it on a sample either with id here, or one of
#' \link{sample_n} or \link{sample_frac}
#' @note Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#' @family calibration
#' @examples
#' \dontrun{
#' olea %>% slice(1:5) %>% #for the sake of spped
#'     calibrate_r2_opoly(degree.range=1:5, thresh=c(0.9, 0.99))
#'
#' olea %>% slice(1:5) %>% #for the sake of spped
#'     calibrate_r2_npoly(degree.range=1:5, thresh=c(0.9, 0.99))
#' }
#'
#' @rdname calibrate_r2
#' @export
calibrate_r2 <- function(){
  message("Deprecated, see ?calibrate_r2")
}

#' @rdname calibrate_r2
#' @export
calibrate_r2_opoly <- function(Opn, id = 1:length(Opn),
                               degree.range=1:8, thresh = c(0.90, 0.95, 0.99, 0.999),
                               plot=TRUE, ...) {
  .check(is_Opn(Opn),
         "only defined on Opn objects")
  method <- opoly

  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = length(degree.range))
  for (i in id) {
    for (j in degree.range) {
      res[i, j] <- method(Opn$coo[[i]], degree = j)$r2
    }
  }
  rownames(res) <- names(Opn)
  colnames(res) <- paste0("degree", degree.range)

  # we ggplot
  h_display <- which(apply(res, 2, median) >= 0.99)[1] + 2 # cosmectics
  res %>% as.data.frame() %>% seq_along %>%
    lapply(function(i) data.frame(Var1=rownames(res),
                                  Var2=colnames(res)[i],
                                  value=res[,i])) %>%
    do.call("rbind", .) %>%
    `colnames<-`(c("shp", "degree", "r2")) -> xx
  gg <- ggplot(xx, aes_string(x="degree", y="r2")) + geom_boxplot() +
    labs(x="Degree", y="r2") +
    coord_cartesian(xlim=c(0.5, h_display+0.5))
  if (plot) print(gg)
  # we calculate quantiles and add nice rowcolnames
  # also the median (independently of probs [0.5, etc]) since
  # thresh may change
  med.res <- apply(res, 2, median)
  minh <- numeric(length(thresh))
  names(minh) <- thresh
  for (i in seq(along=thresh)){
    wi <- which(med.res > thresh[i])
    minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
  mind <- minh
  # talk to me
  if (.is_verbose()){
    #     cat("\n$minh:\n")
    print(mind)}
  # we return the full matrix, the ggplot and the thresholds
  invisible(list(gg=gg, q=res, mind=mind))
}

#' @rdname calibrate_r2
#' @export
calibrate_r2_npoly <- function(Opn, id = 1:length(Opn),
                               degree.range=1:8, thresh = c(0.90, 0.95, 0.99, 0.999),
                               plot=TRUE, ...) {
  .check(is_Opn(Opn),
         "only defined on Opn objects")
  method <- npoly

  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = length(degree.range))
  for (i in id) {
    for (j in degree.range) {
      res[i, j] <- method(Opn$coo[[i]], degree = j)$r2
    }
  }
  rownames(res) <- names(Opn)
  colnames(res) <- paste0("degree", degree.range)

  # we ggplot
  h_display <- which(apply(res, 2, median) >= 0.99)[1] + 2 # cosmectics
  res %>% as.data.frame() %>% seq_along %>%
    lapply(function(i) data.frame(Var1=rownames(res),
                                  Var2=colnames(res)[i],
                                  value=res[,i])) %>%
    do.call("rbind", .) %>%
    `colnames<-`(c("shp", "degree", "r2")) -> xx
  gg <- ggplot(xx, aes_string(x="degree", y="r2")) + geom_boxplot() +
    labs(x="Degree", y="r2") +
    coord_cartesian(xlim=c(0.5, h_display+0.5))
  if (plot) print(gg)
  # we calculate quantiles and add nice rowcolnames
  # also the median (independently of probs [0.5, etc]) since
  # thresh may change
  med.res <- apply(res, 2, median)
  minh <- numeric(length(thresh))
  names(minh) <- thresh
  for (i in seq(along=thresh)){
    wi <- which(med.res > thresh[i])
    minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
  mind <- minh
  # talk to me
  if (.is_verbose()){
    #     cat("\n$minh:\n")
    print(mind)}
  # we return the full matrix, the ggplot and the thresholds
  invisible(list(gg=gg, q=res, mind=mind))
}
