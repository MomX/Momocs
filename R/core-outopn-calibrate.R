
# 1. calibrate_reconstructions -----------
#' Calibrate using reconstructed shapes
#'
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number. Compare them visually with the maximal fit.
#'
#' @param x the \code{Coo} object on which to calibrate_reconstructions
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#'  for \code{Out}, or from \code{c('opoly', 'npoly', 'dfourier')} for \code{Opn}
#' @param id the shape on which to perform calibrate_reconstructions
#' @param range vector of harmonics on which to perform calibrate_reconstructions
#' @param baseline1 \eqn{(x; y)} coordinates for the first point of the baseline
#' @param baseline2 \eqn{(x; y)} coordinates for the second point of the baseline
#' @param ... only used for the generic
#' @return a ggplot object and the full list of intermediate results. See examples.
#' @family calibration
#' @examples
#' calibrate_reconstructions(bot, "efourier", range=1:12)
#'
#' calibrate_reconstructions(olea, "dfourier")
#' @export
calibrate_reconstructions <-
  function(x, method, id, range, baseline1, baseline2) {
    UseMethod("calibrate_reconstructions")
  }

#' @export
calibrate_reconstructions.Out <-
  function(x,
           method = c("efourier", "rfourier", "tfourier"),
           id,
           range = 1:9, baseline1=NULL, baseline2=NULL) {
    # we detect the method
    # Out dispatcher
    Out <- x
    if (missing(method)) {
      message("method not provided. efourier is used")
      method <- efourier
      method_i <- efourier_i
    } else {
      p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
      if (is.na(p)) {
        warning("unvalid method. efourier is used")
      } else {
        method   <- switch(p, efourier, rfourier, tfourier)
        method_i <- switch(p, efourier_i, rfourier_i, tfourier_i)
      }
    }
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
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>% do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- method_i(method(coo, nb.h = max.h))
    best <- coo_close(best)
    best <- data.frame(x=best[, 1], y=best[, 2])
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
      coord_equal() + geom_polygon(aes(fill=id), alpha=0.5) +
      geom_path(data=best, aes_string(x="x", y="y")) +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @export
calibrate_reconstructions.Opn <-
  function(x,
           method = c("npoly", "opoly", "dfourier"),
           id,
           range = 2:10,
           baseline1 = c(-1, 0),
           baseline2 = c(1, 0)) {
    # Opn dispatcher
    Opn <- x
    if (missing(method)) {
      message("method not provided. opoly is used")
      method   <- opoly
      method_i <- opoly_i
      p <- 2
    } else {
      p <- pmatch(tolower(method), c("npoly", "opoly",  "dfourier"))
      if (is.na(p)) {
        warning("unvalid method. opoly is used.\n")
      } else {
        method   <- switch(p, npoly, opoly, dfourier)
        method_i <- switch(p, npoly_i, opoly_i, dfourier_i)
      }
    }

    # we sample a shape
    if (missing(id))
      id <- sample(length(Opn$coo), 1)
    coo <- Opn$coo[[id]]
    coo <- coo_baseline(coo,
                        ldk1 = 1, ldk2 = nrow(coo),
                        t1 = baseline1, t2 = baseline2)

    # we check for too ambitious range
    # special case for opoly # todo
    if (p == 2) {
      if (max(range) > 20) range <- 2:20
    }
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
    coos <- lapply(seq_along(res), function(i) data.frame(i, res[[i]])) %>% do.call("rbind", .)
    colnames(coos) <- c("id", "x", "y")
    coos$id <- as.numeric(coos$id)
    best <- res[[length(res)]]
    best <- data.frame(x=best[, 1], y=best[, 2])
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
#' @param x and \code{Out} or \code{Opn} object on which to calibrate_deviations
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')} and
#' \code{'dfourier'}.
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
#' calibrate_deviations(bot)
#'
#' \dontrun{
#'
#' # on Opn
#' calibrate_deviations(olea)
#'
#' # lets customize the ggplot
#' library(ggplot2)
#' gg <- calibrate_deviations(bot, id=1:20)$gg
#' gg + geom_hline(yintercept=c(0.001, 0.005), linetype=3)
#' gg + labs(col="Number of harmonics", fill="Number of harmonics",
#'            title="Harmonic power") + theme_bw()
#' gg + coord_polar()
#'
#' ### intermediate results can be accessed eg with:
#' shp <- hearts[1] %>% coo_interpolate(360) %>% coo_samplerr(60) %>% Out()
#' calibrate_deviations(shp, id=1, range=1:24, method="efourier") %$%
#'    res %>% apply(1, mean) %>% plot(type="b")
#' calibrate_deviations(shp, id=1, range=1:24, method="rfourier") %$%
#'    res %>% apply(1, mean) %>% plot(type="b")
#' calibrate_deviations(shp, id=1, range=1:24, method="tfourier") %$%
#'    res %>% apply(1, mean) %>% plot(type="b")
#'
#' # ... providing an illustration of the e vs r/t fourier approaches developped in the help page.
#'
#' ### illustration of interpolate.factor
#' interp <- c(1, 5, 25)
#' res <- list()
#' for (i in seq_along(interp))
#' calibrate_deviations(shp, id=1, range=1:24,
#'    method="tfourier", interpolate.factor=interp[i], plot=FALSE) %$%
#'    res %>% apply(1, mean) -> res[[i]]
#'
#'  ### int_5 is more accurate than no inteprolation
#'  sign(res[[2]] - res[[1]])
#'  ### int 25 is more accurate than int_5, etc.
#'  sign(res[[3]] - res[[2]])
#' }
#' @export
calibrate_deviations <- function(x,
                                 method,
                                 id,
                                 range,
                                 norm.centsize,
                                 dist.method,
                                 interpolate.factor,
                                 dist.nbpts,
                                 plot) {
  UseMethod("calibrate_deviations")
}

#' @export
calibrate_deviations.Out <-
  function(x, method = c("efourier", "rfourier", "tfourier"),
           id = 1, range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {
    Coo <- x
    # missing lineat.y
    if (missing(range)) {
      hr <- calibrate_harmonicpower(Coo, plot=FALSE, verbose=FALSE)
      range <- unique(hr$minh)
    }
    if (missing(method)) {
      message("method not provided. efourier is used")
      method <- efourier
      method.i <- efourier_i
    } else {
      p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
      if (is.na(p)) {
        warning("unvalid method. efourier is used")
      } else {
        method <- switch(p, efourier, rfourier, tfourier)
        method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)
      }
    }
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
      pb <- txtProgressBar(1, nk)
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
        setTxtProgressBar(pb, ind)
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

#' @export
calibrate_deviations.Opn<-
  function(x,
           method = c("npoly", "opoly", "dfourier"),
           id = 1,
           range,
           norm.centsize = TRUE,
           dist.method = edm_nearest,
           interpolate.factor = 1,
           dist.nbpts = 120,
           plot = TRUE) {
    Coo <- x
    # missing lineat.y
    if (missing(range)) {
      #       hr <- calibrate_harmonicpower(Coo, plot=FALSE, verbose=FALSE,
      #                                     lineat.y = c(95, 99, 99.9))
      #       range <- unique(hr$minh)
      #
      message("'range' was missing and set to 1:8")
      range <- 1:8
    }

    if (missing(method)) {
      message("method not provided. dfourier is used")
      method <- dfourier
      method.i <- dfourier_i
      p <- 3
    } else {
      p <- pmatch(tolower(method), c("npoly", "opoly", "dfourier"))
      if (is.na(p)) {
        warning("unvalid method. dfourier is used.\n")
        method <- dfourier
        method.i <- dfourier_i
        p <- 3
      } else {
        method <- switch(p, npoly, opoly, dfourier)
        method.i <- switch(p, npoly_i, opoly_i, dfourier_i)
      }
    }
    if (p==3){ # dfourier
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
    } else { #poly methods
      nb.pts <- min.nb.pts <- min(sapply(Coo$coo[id], function(x) nrow(unique(x))))
      nb.h.best <- 12
      message("deviations calculated from a degree 12 polynom")
    }
    nr <- length(range)
    nc <- nb.pts
    nk <- length(id)
    if (p==3){

      res <- array(NA, dim = c(nr, nc, nk),
                   dimnames = list(paste0("h", range),
                                   paste("pt", 1:nb.pts), names(Coo)[id]))
    } else {
      res <- array(NA, dim = c(nr, nc, nk),
                   dimnames = list(paste0("d", range),
                                   paste("pt", 1:nb.pts), names(Coo)[id]))
    }
    # progressbar
    if (nk > 5) {
      pb <- txtProgressBar(1, nk)
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
        setTxtProgressBar(pb, ind)
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
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
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
#' @param method any method from \code{c('efourier', 'rfourier', 'sfourier', 'tfourier')} for \code{Out}s and
#' \code{dfourier} for \code{Out}s.
#' @param id the shapes on which to perform calibrate_harmonicpower. All of them by default
#' @param nb.h numeric the maximum number of harmonic, on which to base the cumsum
#' @param drop numeric the number of harmonics to drop for the cumulative sum
#' @param thresh vector of numeric for drawing horizontal lines, and also used for
#' \code{minh} below
#' @param plot logical whether to plot the result or simply return the matrix
#' @param verbose whether to print results
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
#' cal <- calibrate_harmonicpower(bot)
#' \dontrun{
#' # for Opn objects
#' calibrate_harmonicpower(olea, "dfourier")
#'
#' # let customize the ggplot
#' library(ggplot2)
#' cal$gg + theme_minimal() +
#' coord_cartesian(xlim=c(3.5, 12.5), ylim=c(90, 100)) +
#' ggtitle("Harmonic power calibration")
#' }
#' # if you want to do efourier with 99% calibrate_harmonicpower in one step
#' # efourier(bot, nb.h=calibrate_harmonicpower(bot, "efourier", plot=FALSE)$minh["99%"])
#'
#' @export
calibrate_harmonicpower <- function(x, method, id, nb.h, drop, thresh, plot, verbose) {
  UseMethod("calibrate_harmonicpower")
}

#' @export
calibrate_harmonicpower.Out <- function(x, method = "efourier", id = 1:length(x),
                                        nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
                                        plot=TRUE, verbose=TRUE) {
  Out <- x
  # we swith among methods, with a messsage
  if (missing(method)) {
    if (verbose) message("method not provided. efourier is used")
    method <- efourier
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier", "sfourier", "tfourier"))
    if (is.na(p)) {
      warning("unvalid method. efourier is used")
    } else {
      method <- switch(p, efourier, rfourier, sfourier, tfourier)
    }
  }
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
    `rownames<-`(NULL) %>%
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
  if (verbose){
    #     cat("\n$minh:\n")
    print(minh)}
  # we return the full matrix, the ggplot and the thresholds
  invisible(list(gg=gg, q=res, minh=minh))
}

#' @export
calibrate_harmonicpower.Opn <- function(x, method = "dfourier", id = 1:length(x),
                                        nb.h, drop = 1, thresh = c(90, 95, 99, 99.9),
                                        plot=TRUE, verbose=TRUE) {
  Opn <- x
  # we swith among methods, with a messsage
  if (missing(method)) {
    if (verbose) message("method not provided. dfourier is used")
    method <- dfourier
  } else if (method != "dfourier"){
    if (verbose) message("only available for dfourier. dfourier is used")
    method <- dfourier
  } else {
    method <- dfourier
  }
  #   } else {
  #     p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
  #     if (is.na(p)) {
  #       warning("Unvalid method. efourier is used.")
  #     } else {
  #       method <- switch(p, efourier, rfourier, tfourier)
  #     }
  #   }
  # here we define the maximum nb.h, if missing
  if (missing(nb.h)){
    nb.h <- floor(min(sapply(Opn$coo, nrow))/2)
  }
  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = (nb.h - drop))
  x <- (drop + 1):nb.h
  for (i in seq(along = id)) {
    xf <- method(Opn$coo[[id[i]]], nb.h = nb.h)
    res[i, ] <- harm_pow(xf)[x]}
  rownames(res) <- names(Opn)
  colnames(res) <- paste0("h", 1:ncol(res))
  # we remove dropped harmonics
  #res <- res[, -drop]
  # we calculte cumsum and percentages
  res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
  # we ggplot
  h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmetics
  res %>% as.data.frame() %>% seq_along %>%
    lapply(function(i) data.frame(Var1=rownames(res),
                                  Var2=colnames(res)[i],
                                  value=res[,i])) %>%
    do.call("rbind", .) %>%
    `rownames<-`(NULL) %>%
    `colnames<-`(c("shp", "harm", "hp")) -> xx
  gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
    labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
    coord_cartesian(xlim=c(0.5, h_display+0.5))
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
  if (verbose){
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
#' @param method one of 'npoly' or 'opoly'
#' @param id the ids of shapes on which to calculate r2 (all by default)
#' @param degree.range on which to calculate r2
#' @param thresh the threshold to return diagnostic
#' @param plot logical whether to print the plot
#' @param verbose logical whether to print messages
#' @param ... useless here
#' @details May be long, so you can estimate it on a sample either with id here, or one of
#' \link{sample_n} or \link{sample_frac}
#' @family calibration
#' @examples
#' \dontrun{
#' calibrate_r2(olea, "opoly", degree.range=1:5, thresh=c(0.9, 0.99))
#' }
#'
#' @export
calibrate_r2 <- function(Opn, method = "opoly", id = 1:length(Opn),
                         degree.range=1:8, thresh = c(0.90, 0.95, 0.99, 0.999),
                         plot=TRUE, verbose=TRUE, ...) {
  if (!is.Opn(Opn))
    stop("only defined on Opn objects")
  # we swith among methods, with a messsage
  if (missing(method)) {
    if (verbose) message("method not provided. opoly is used")
    method <- opoly
  } else {
    p <- pmatch(tolower(method), c("npoly", "opoly"))
    if (is.na(p)) {
      warning("unvalid method. opoly is used.\n")
    } else {
      method <- switch(p, npoly, opoly)
    }
  }

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
    `rownames<-`(NULL) %>%
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
  if (verbose){
    #     cat("\n$minh:\n")
    print(mind)}
  # we return the full matrix, the ggplot and the thresholds
  invisible(list(gg=gg, q=res, mind=mind))
}

