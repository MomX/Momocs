
# 1. calibrate_reconstructions -----------
#' Calibrate using reconstructed shapes
#'
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number. Compare them visually with the maximal fit.
#'
#' @aliases calibrate_reconstructions
#' @param x the \code{Coo} object on which to calibrate_reconstructions
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#'  for \code{Out}, or from \code{c('opoly', 'npoly')} for \code{Opn}
#' @param id the shape on which to perform calibrate_reconstructions
#' @param range vector of harmonics on which to perform calibrate_reconstructions
#' @param baseline1 \eqn{(x; y)} coordinates for the first point of the baseline
#' @param baseline2 \eqn{(x; y)} coordinates for the second point of the baseline
#' @param ... only used for the generic
#' @return a ggplot object
#' @keywords Out
#' @examples
#' data(bot)
#' calibrate_reconstructions(bot)
#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions <-
  function(x, ...) {UseMethod("calibrate_reconstructions")}

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions.Out <-
  function(x,
           method = c("efourier", "rfourier", "tfourier"),
           id,
           range = 1:9,
           ...) {
    # we detect the method
    # Out dispatcher
    Out <- x
    if (missing(method)) {
      cat(" * Method not provided. efourier is used.\n")
      method <- efourier
      method_i <- efourier_i
    } else {
      p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
      if (is.na(p)) {
        warning(" * Unvalid method. efourier is used.\n")
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
      cat(" * range was too high and set to: ", range, ".\n")
    }

    # we calculate all shapes
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, nb.h = max(range)), nb.h = range[i])
    }
    # we prepare the plot
    names(res) <- range
    coos <- ldply(res, data.frame)
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
      scale_fill_gradient2() +
      facet_wrap(~ id) +
      labs(x=NULL, y=NULL, title=names(Out)[id]) +
      theme_light() + theme_empty
    return(gg)
  }

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions.Opn <-
  function(x,
           method = c("npoly", "opoly"),
           id,
           range = 2:10,
           baseline1 = c(-1, 0),
           baseline2 = c(1, 0),
           ...) {
    # Opn dispatcher
    Opn <- x
    if (missing(method)) {
      cat(" * Method not provided. opoly is used.\n")
      method   <- opoly
      method_i <- opoly_i
      p <- 2
    } else {
      p <- pmatch(tolower(method), c("npoly", "opoly"))
      if (is.na(p)) {
        warning(" * Unvalid method. opoly is used.\n")
      } else {
        method   <- switch(p, npoly, opoly)
        method_i <- switch(p, npoly_i, opoly_i)
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
      cat(" * range was too high and set to: ", range, ".\n")
    }

    # we loop
    res <- list()
    for (i in seq(along = range)) {
      res[[i]] <- method_i(method(coo, degree = range[i]))
    }

    # we prepare the plot
    names(res) <- range
    coos <- ldply(res, data.frame)
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
#' Quantitative calibration, through deviations, for Out objects
#'
#' Calculate deviations from original and reconstructed shapes using a
#' range of harmonic number.
#'
#' @param Coo the \code{Out} object on which to calibrate_deviations
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform calibrate_deviations
#' @param harm.range vector of harmonics on which to perform calibrate_deviations.
#' If not provided, the harmonics corresponding to 0.9, 0.95 and 0.99% of harmonic power
#' are used.
#' @param norm.centsize logical whether to normalize deviation by the centroid size
#' @param dist.method a method such as \link{edm_nearest} to calculate deviations
#' @param dist.nbpts numeric the number of points to use for deviations calculations
#' @param ... only used for the generic
#' @return a ggplot object
#' @keywords Out
#' @examples
#' data(bot)
#' calibrate_deviations(bot)
#' \dontrun{
#' library(ggplot2)
#' gg <- calibrate_deviations(bot, id=1:20)$gg
#' gg + geom_hline(yintercept=c(0.001, 0.005), linetype=3)
#' gg + labs(col="Number of harmonics", fill="Number of harmonics",
#'            title="Harmonic power") + theme_bw()
#' gg + coord_polar()
#' }
#' @aliases calibrate_deviations
#' @rdname calibrate_deviations
#' @export
calibrate_deviations <- function(Coo, ...) {
  UseMethod("calibrate_deviations")
}

#' @rdname calibrate_deviations
#' @export
calibrate_deviations.Out <-
  function(Coo, method = c("efourier", "rfourier", "tfourier"),
           id = 1, harm.range,
           norm.centsize = TRUE,
           dist.method = edm_nearest, dist.nbpts = 120,
           ...) {
    # missing lineat.y
    if (missing(harm.range)) {
      hr <- calibrate_harmonicpower(Coo, plot=FALSE, verbose=FALSE,
                                    lineat.y = c(95, 99, 99.9))
      harm.range <- unique(hr$minh)
    }
    if (missing(method)) {
      cat(" * Method not provided. efourier is used.\n")
      method <- efourier
      method.i <- efourier_i
    } else {
      p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
      if (is.na(p)) {
        warning("Unvalid method. efourier is used.")
      } else {
        method <- switch(p, efourier, rfourier, tfourier)
        method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)
      }
    }
    # We define the highest possible nb.h along Coo@coo[id]
    min.nb.pts <- min(sapply(Coo$coo[id], nrow))
    nb.h.best <- floor(min.nb.pts/2) - 1
    # we handle too ambitious harm.range
    if (max(harm.range) > nb.h.best) {
      harm.range <- floor(seq(4, nb.h.best, length = 6))
      cat("  * 'harm.range' was too high and set to: ", harm.range,
          ".\n")
    }
    # we prepare the results array
    nb.pts <- ifelse(dist.nbpts == "max", 2 * nb.h.best, dist.nbpts)
    nr <- length(harm.range)
    nc <- nb.pts
    nk <- length(id)
    res <- array(NA, dim = c(nr, nc, nk),
                 dimnames = list(paste0("h", harm.range),
                                 paste("pt", 1:nb.pts), names(Coo)[id]))
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
      coo_best <- method.i(method(coo, nb.h = nb.h.best), nb.pts = nb.pts)
      for (i in seq(along = harm.range)) {
        # for each number of harmonics we calculate deviation with
        # the FUN=method
        coo_i <- method.i(method(coo, nb.h = harm.range[i]), nb.pts = nb.pts)
        res[i, , ind] <- dist.method(coo_best, coo_i)
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
      xx <- melt(m)
      xx <- cbind(xx, melt(d)$value)
      xx$Var2 <- as.numeric(xx$Var2)
      colnames(xx) <- c("harm", "pt", "med", "sd")
      # hideous but avoid the aes_string problem fro ribbon
      xx$mmsd <- xx$med - xx$sd
      xx$mpsd <- xx$med + xx$sd
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
      xx <- melt(m)
      xx$Var2 <- as.numeric(xx$Var2)
      colnames(xx) <- c("harm", "pt", "med")
      gg <- ggplot(xx, aes_string(x="pt", y="med", col="harm")) +
        geom_line() +
        labs(x="Points along the outline", y=y.title, col=NULL) +
        coord_cartesian(xlim=range(xx$pt), ylim=c(0, max(xx$med)))
    }
    #     # horizontal lines
    #     if (!is.null(thres.h)) {
    #       gg <- gg + geom_hline(aes(yintercept=thres.h))
    #     }
    # we plot the ggplot
    print(gg)
    ####
    invisible(list(gg=gg, res = res, m = m, d = d))
  }


# 3. calibrate_harmonicpower ----------------
#' Quantitative calibration, through harmonic power, for Out objects
#'
#' Estimates the number of harmonics required for the three Fourier methods
#' implemented in Momocs: elliptical Fourier analysis
#' (see \link{efourier}), radii variation analysis (see \link{rfourier})
#' and tangent angle analysis (see \link{tfourier}).
#' It returns and can plot cumulated harmonic power whether dropping
#' the first harmonic or not, and based and the maximum possible number
#' of harmonics on the \code{Out} object.
#'
#' @param Out the \code{Out} object on which to calibrate_harmonicpower
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform calibrate_harmonicpower. All of them by default
#' @param nb.h numeric the maximum number of harmonic, on which to base the cumsum
#' @param drop numeric the number of harmonics to drop for the cumulative sum
#' @param thres.h vector of numeric for drawing horizontal lines, and also used for
#' \code{minh} below
#' @param plot logical whether to plot the result or simply return the matrix
#' @param verbose whether to print results
#' @param ... just for the generic
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
#' @keywords Out
#' @examples
#' data(bot)
#' cal <- calibrate_harmonicpower(bot)
#' \dontrun{
#' library(ggplot2)
#' cal$gg + theme_minimal() +
#' coord_cartesian(xlim=c(3.5, 12.5), ylim=c(90, 100)) +
#' ggtitle("Harmonic power calibration")
#' }
#' # if you want to do efourier with 99% calibrate_harmonicpower in one step
#' # efourier(bot, nb.h=calibrate_harmonicpower(bot, "efourier", plot=FALSE)$minh["99%"])
#' @export
calibrate_harmonicpower <- function(Out, ...) {
  UseMethod("calibrate_harmonicpower")
}

#' @export
calibrate_harmonicpower.Out <- function(Out, method = "efourier", id = 1:length(Out),
                                        nb.h, drop = 1, thres.h = c(90, 95, 99, 99.9),
                                        plot=TRUE, verbose=TRUE, ...) {
  # we swith among methods, with a messsage
  if (missing(method)) {
    if (verbose) cat(" * Method not provided. calibrate_harmonicpower | efourier is used.\n")
    method <- efourier
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
    if (is.na(p)) {
      warning("Unvalid method. efourier is used.")
    } else {
      method <- switch(p, efourier, rfourier, tfourier)
    }
  }
  # here we define the maximum nb.h, if missing
  if (missing(nb.h)){
    nb.h <- floor(min(sapply(Out$coo, nrow))/2)}
  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = (nb.h - drop))
  x <- (drop + 1):nb.h
  for (i in seq(along = id)) {
    xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
    res[i, ] <- harm_pow(xf)[x]}
  rownames(res) <- names(Out)
  colnames(res) <- paste0("h", 1:ncol(res))
  # we remove dropped harmonics
  #res <- res[, -drop]
  # we calculte cumsum and percentages
  res <- t(apply(res, 1, function(x) cumsum(x) / sum(x))) * 100
  # we ggplot
  h_display <- which(apply(res, 2, median) >= 99)[1] + 2 # cosmectics
  xx <- melt(res)
  colnames(xx) <- c("shp", "harm", "hp")
  gg <- ggplot(xx, aes_string(x="harm", y="hp")) + geom_boxplot() +
    labs(x="Harmonic rank", y="Cumulative sum harmonic power") +
    coord_cartesian(xlim=c(0.5, h_display+0.5))
  if (plot) print(gg)
  # we calculate quantiles and add nice rowcolnames
  # also the median (independently of probs [0.5, etc]) since
  # thres.h may change
  med.res <- apply(res, 2, median)
  minh <- numeric(length(thres.h))
  names(minh) <- paste0(thres.h, "%")
  for (i in seq(along=thres.h)){
    wi <- which(med.res > thres.h[i])
    minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
  minh <- minh+drop
  # talk to me
  if (verbose){
    #     cat("\n$minh:\n")
    print(minh)}
  # we return the full matrix, the ggplot and the thresholds
  invisible(list(gg=gg, q=res, minh=minh))
}

# nquant npow

