
# 1. calibrate_reconstructions -----------
#' Calibrate using reconstructed shapes
#'
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number.
#'
#' @aliases calibrate_reconstructions
#' @param x the \code{Coo} object on which to calibrate_reconstructions
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform calibrate_reconstructions
#' @param range vector of harmonics on which to perform calibrate_reconstructions
#' @param baseline1 \eqn{(x; y)} coordinates for the first point of the baseline
#' @param baseline2 \eqn{(x; y)} coordinates for the second point of the baseline
#' @param plot.method either \code{'\link{panel}'} or \code{'\link{stack}'}
#' @param shp.col a color for the shape (\code{NA} by default)
#' @param shp.border a color for the border of the shape
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param ... additional parameters to fed \link{coo_plot}
#' @keywords Out
#' @examples
#' data(bot)
#' calibrate_reconstructions(bot)
#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions <- function(x, ...) {UseMethod("calibrate_reconstructions")}

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions.Out <- function(x,
                                          method = c("efourier", "rfourier", "tfourier"),
                                          id,
                                          range = 1:9,
                                          plot.method = c("panel", "stack")[1],
                                          legend = TRUE,
                                          legend.title = "Nb of harmonics",
                                          palette = col_heat,
                                          shp.col = NA,
                                          shp.border = "#1A1A1A",
                                          ...) {
  # we detect the method
  Out <- x
  if (missing(id))
    id <- sample(length(Out$coo), 1)
  if (missing(method)) {
    cat(" * Method not provided. efourier is used.\n")
    method <- efourier
    method.i <- efourier_i
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
    if (is.na(p)) {
      warning(" * Unvalid method. efourier is used.\n")
    } else {
      method <- switch(p, efourier, rfourier, tfourier)
      method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)
    }
  }
  
  # check for too ambitious harm.range
  if (max(range) > (min(sapply(Out$coo, nrow))/2 + 1)) {
    range <- floor(seq(1, q/2 - 1, length = 6))
    cat(" * range was too high and set to: ", range, ".\n")
  }
  coo <- Out$coo[[id]]
  coo <- coo_center(coo)
  res <- list()
  for (i in seq(along = range)) {
    res[[i]] <- method.i(method(coo, nb.h = max(range)), nb.h = range[i])
  }
  # plotting
  op <- par(mar = c(3, 3, 4, 1))
  on.exit(par(op))
  cols <- paste0(palette(length(range)), "EE")
  if (plot.method == "stack") {
    coo_plot(coo, border = shp.border, col = shp.col, lwd = 2,
             points = FALSE, main = names(Out)[id], ...)
    for (i in seq(along = range)) {
      lines(res[[i]], col = cols[i], lwd = 1)
    }
    if (legend) {
      legend("topright", legend = as.character(range),
             bty = "n", col = cols, lty = 1, lwd = 1, cex = 0.7,
             title = legend.title)
    }
  } else {
    if (plot.method == "panel") {
      op <- par(mar=c(1, 1, 4, 1))
      on.exit(par(op))
      pos <- coo_listpanel(res, cols = cols)
      if (legend) {
        text(x = pos[, 1], y = pos[, 2], as.character(range))
      }
      title(names(Out)[id], cex = 1.3, outer = FALSE)
    }
  }
  # we return res
  names(res) <- range
  invisible(res)
}

#' @rdname calibrate_reconstructions
#' @export
calibrate_reconstructions.Opn <- function(x,
                                          method = c("npoly", "opoly"),
                                          id,
                                          range = c(2, 3, 4, 6, 8, 10),
                                          baseline1 = c(-1, 0),
                                          baseline2 = c(1, 0),
                                          plot.method = c("panel", "stack")[1],
                                          legend = TRUE,
                                          legend.title = "Degree",
                                          palette = col_india,
                                          shp.border = "#1A1A1A",
                                          ...) {
  
  Opn <- x
  if (missing(id))
    id <- sample(length(Opn$coo), 1)
  if (missing(method)) {
    cat(" * Method not provided. opoly is used.\n")
    ortho <- TRUE
  } else {
    p <- pmatch(tolower(method), c("npoly", "opoly"))
    if (is.na(p)) {
      warning(" * Unvalid method. opoly is used.\n")
    } else {
      method <- switch(p, npoly, opoly)
      method_i <- switch(p, npoly_i, opoly_i)
    }
  }
  # check for too ambitious range
  if (max(degree.range) > (min(sapply(Opn$coo, nrow)) - 1)) {
    degree.range <- (min(sapply(Opn$coo, nrow)) - 1)
    cat(" * degree.range was too high and set to: ", degree.range, ".\n")
  }
  coo <- Opn$coo[[id]]
  coo <- coo_baseline(coo, ldk1 = 1, ldk2 = nrow(coo), t1 = baseline1, t2 = baseline2)
  res <- list()
  for (i in seq(along = degree.range)) {
    res[[i]] <- method_i(method(coo, degree = degree.range[i]))
  }
  # plotting
  #   op <- par(mar = c(3, 3, 2, 1))
  #   on.exit(par(op))
  cols <- paste0(palette(length(degree.range)), "EE")
  if (plot.method == "stack") {
    # to initiate the plot but stack may be a better option for
    # that part
    coo_plot(coo, border = shp.border, lwd = 1)
    for (i in seq(along = degree.range)) {
      lines(res[[i]], col = cols[i])
    }
    if (legend) {
      legend("topright", legend = as.character(degree.range),
             bty = "n", col = cols, lty = 1, lwd = 1, cex = 0.7,
             title = legend.title)
    }
  } else {
    if (plot.method == "panel") {
      # par(oma=c(1, 1, 3, 0))
      pos <- coo_listpanel(res, borders = cols, cols = par("bg"),
                           poly = FALSE)
      if (legend) {
        text(x = pos[, 1], y = pos[, 2], as.character(degree.range))
      }
      title(names(Opn)[id], cex = 1.3)
    }
  }
  # we return res
  names(res) <- range
  invisible(res)
}

# 2. calibrate_deviations -------------------------
#' Quantitative calibration, through deviations, for Out objects
#'
#' Calculate deviations from original and reconstructed shapes using a
#' range of harmonic number.
#'
#' @aliases calibrate_deviations
#' @param Coo the \code{Out} object on which to calibrate_deviations
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform calibrate_deviations
#' @param harm.range vector of harmonics on which to perform calibrate_deviations
#' @param smooth.it numeric, number of smoothing iterations
#' @param norm.centsize logical whether to normalize deviation by the centroid size
#' @param dist.method a method such as \link{edm_nearest} to calculate deviations
#' @param dist.nbpts numeric the number of points to use for deviations calculations
#' @param plot logical whether to plot the results
#' @param plot_dev logical whether to plot deviations
#' @param title a title for the plot
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param lineat.y vector of numeric for drawing horizontal lines
#' @keywords Out
#' @examples
#' data(bot)
#' calibrate_reconstructions(bot)
#' @export
calibrate_deviations <- function(Coo,
                                 method = c("efourier", "rfourier", "tfourier"),
                                 id = 1,
                                 harm.range = seq(4, 20, 4),
                                 smooth.it = 0,
                                 norm.centsize = TRUE,
                                 dist.method = edm_nearest,
                                 dist.nbpts = 120,
                                 plot = TRUE,
                                 plot_dev = TRUE,
                                 title = "Deviations along the outline",
                                 legend = TRUE,
                                 legend.title = "Nb of harmonics",
                                 palette = col_summer,
                                 lineat.y = c(0.5, 0.1, 0.01)) {
  UseMethod("calibrate_deviations")
}

#' @export
calibrate_deviations.Out <- function(Coo, method = c("efourier", "rfourier",
                                                     "tfourier"), id = 1, harm.range = seq(4, 20, 4), smooth.it = 0,
                                     norm.centsize = TRUE, dist.method = edm_nearest, dist.nbpts = 120,
                                     plot = TRUE, plot_dev = TRUE, title = "Deviations along the outline",
                                     legend = TRUE, legend.title = "Nb of harmonics", palette = col_summer,
                                     lineat.y = c(0.5, 0.1, 0.01)) {
  if (missing(method)) {
    cat("  * Method not provided. efourier is used.\n")
    method <- efourier
    method.i <- efourier_i
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier",
                                   "tfourier"))
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
    coo_best <- method.i(method(coo, nb.h = nb.h.best, smooth.it = smooth.it),
                         nb.pts = nb.pts)
    for (i in seq(along = harm.range)) {
      # for each number of harmonics we calculate deviation with
      # the FUN=method
      coo_i <- method.i(method(coo, nb.h = harm.range[i],
                               smooth.it = smooth.it), nb.pts = nb.pts)
      res[i, , ind] <- dist.method(coo_best, coo_i)
    }
    # we normalize by the centroid size
    if (norm.centsize) {
      res[, , ind] <- res[, , ind]/coo_centsize(coo)
    }
    if (t)
      setTxtProgressBar(pb, ind)
  }
  # below we manage for single/several individuals if more than
  # 1, we calculate median and sd
  if (nk > 1) {
    m <- apply(res, 1:2, median)
    d <- apply(res, 1:2, sd)
  } else {
    m <- res[, , 1]
    d <- NULL
  }
  # plotting stuff
  if (plot) {
    cols <- palette(nr)
    if (nk > 1) {
      ylim <- c(0, max(m + d, na.rm = TRUE))
    } else {
      ylim <- range(m)
    }
    if (norm.centsize) {
      ylab = "Deviation (in % of the centroid size)"
    } else {
      ylab = "Deviation (in original units)"
    }
    plot(NA, xlim = c(1, nc), ylim = ylim, xlab = "Points sampled along the outline",
         ylab = ylab, main = title, xaxs = "i", yaxs = "i",
         axes = FALSE)
    axis(1, at = seq(0, dist.nbpts, length = 5))
    axis(2)
    abline(h = lineat.y, lty = 2, col = "grey90")
    # if you want deviations, here they are
    if (plot_dev) {
      if (nk > 1) {
        plot_dev(m, d, cols = cols)
      } else {
        for (i in 1:nr) {
          lines(1:ncol(m), m[i, ], col = cols[i])
        }
      }
    }
    # same for legend
    if (legend) {
      legend("topright", legend = as.character(harm.range),
             bty = "n", col = cols, lty = 1, lwd = 1, bg = "#FFFFFFCC",
             inset = 0.005, cex = 0.7, title = legend.title)
    }
    box()
  }
  invisible(list(res = res, m = m, d = d))
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
#' @aliases calibrate_harmonicpower
#' @param Out the \code{Out} object on which to calibrate_harmonicpower
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform calibrate_harmonicpower. All of them by default
#' @param nb.h numeric the maximum number of harmonic
#' @param drop numeric the number of harmonics to drop for the cumulative sum
#' @param probs a vector of quantiles to calculate, to be passed to \link{quantile}
#' @param smooth.it numeric, number of smoothing iterations
#' @param plot logical whether to plot the result or simply return the matrix
#' @param xlim an optional xlim to refine the plot
#' @param ylim an optional xlim to refine the plot
#' @param title a title for the plot
#' @param lineat.y vector of numeric for drawing horizontal lines, and also used for
#' \code{minh} below
#' @param verbose whether to print results
#' @return a matrix containing cumulated harmonic power for each harmonic.
#' @return returns a list with component \code{q} the quantile matrix
#' and \code{minh} a quick summary that returns the number of harmonics required to achieve
#' a certain proportion of the total harmonic power.
#' @details
#' The power of a given harmonic \eqn{n} is calculated as follows for
#' elliptical Fourier analysis and the n-th harmonic:
#' \deqn{HarmonicPower_n \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}}
#' and as follows for radii variation and tangent angle:
#' \deqn{
#' HarmonicPower_n= \frac{A^2_n+B^2_n+C^2_n+D^2_n}{2}
#' }
#' @keywords Out
#' @examples
#' data(bot)
#' calibrate_harmonicpower(bot)
#' # if you want to do efourier with 99% calibrate_harmonicpower in one step
#' efourier(bot, nb.h=calibrate_harmonicpower(bot, "efourier", plot=FALSE)$minh["99%"])
#' @export
calibrate_harmonicpower <- function(Out, method = "efourier", id = 1:length(Out),
                                    nb.h = 24, drop = 1, probs=seq(0, 1, 0.25),
                                    smooth.it = 0, plot = TRUE,
                                    xlim=c(drop+1, nb.h), ylim=c(0, 100),
                                    title = "Fourier coefficients power spectrum",
                                    lineat.y = c(90, 95, 99, 99.9), verbose=TRUE) {
  UseMethod("calibrate_harmonicpower")
}

#' @export
calibrate_harmonicpower.Out <- function(Out, method = "efourier", id = 1:length(Out),
                                        nb.h, drop = 1, probs=seq(0, 1, 0.25),
                                        smooth.it = 0, plot = TRUE,
                                        xlim=c(drop+1, nb.h), ylim=c(0, 100),
                                        title = "Harmonic power of coefficients",
                                        lineat.y = c(90, 95, 99, 99.9), verbose=TRUE) {
  
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
  # if required, we smooth
  if (!missing(smooth.it)){
    Out <- coo_smooth(Out, smooth.it)}
  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = (nb.h - drop))
  x <- (drop + 1):nb.h
  for (i in seq(along = id)) {
    xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
    pow <- harm_pow(xf)[x]
    res[i, ] <- cumsum(pow)/sum(pow)}
  # we turn in to precentages
  res <- res * 100
  # we calculate quantiles and add nice rowcolnames
  res <- apply(res, 2, quantile, probs = probs)
  rownames(res) <- paste0("q", probs)
  colnames(res) <- paste0("h", x)
  # we calculate quantiles and add nice rowcolnames
  # also the median (independently of probs [0.5, etc]) since
  # lineat.y may change
  med.res <- apply(res, 2, median)
  minh <- numeric(length(lineat.y))
  names(minh) <- paste0(lineat.y, "%")
  for (i in seq(along=lineat.y)){
    wi <- which(med.res > lineat.y[i])
    minh[i] <- ifelse(length(wi)==0, NA, min(wi))}
  minh <- minh+drop
  #   return(q)
  # plot section
  if (plot) {
    if (missing(xlim)) xlim <- c(1+drop, minh[length(minh)])
    plot(NA, xlim = xlim, ylim = ylim, yaxs = "i", xaxs="i",
         xlab = "Number of harmonics",
         ylab = paste0("Cumulative harmonic power / h", nb.h, " (%)"),
         main = title, sub = paste0("(", length(id), " outlines)"),
         axes = FALSE)
    axis(1, at = x, line=NA)
    axis(2, las=2, line=NA)
    #abline(h = lineat.y, lty = 1, col = col_heat(length(lineat.y)))
    #         lines(x, res[2, ], col="grey50")
    #         segments(x, res[1, ], x, res[3, ])
    
    devmat.plot <- function(q, x, cols.poly, med.col, opacity.max=0.5, ...){
      if (missing(x)) x <- 1:ncol(q)
      nq <- floor(nrow(q)/2)
      if (missing(cols.poly)) {
        cols <- col_grey(nq*2)
        cols.poly <- cols[1:nq]
        med.col   <- "#000000"}
      xi <- c(x, rev(x))
      for (i in 1:nq){
        yi <- c(q[i, ], rev(q[nrow(q)+1-i, ]))
        polygon(xi, yi, col=cols.poly[i], border = NA)}
      if (nrow(q) %% 2) {
        lines(x, q[nq+1, ], col=med.col, ...)}
    }
    
    devmat.plot(res, x, type="o", cex=2/3, pch=20, lty=3)
    
    box()
  }
  if (verbose){
    cat("$q:\n")
    print(round(res[, 1:(max(minh, na.rm=TRUE)-drop)], 3))
    cat("\n$minh:\n")
    print(minh)}
  invisible(list(q=res, minh=minh))
}

# nquant npow

