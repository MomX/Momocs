# 1. Out
# -----------------------------------------------------------------------

#' Builds an Out object
#'
#' In Momocs, \code{Out} classes objects are lists of \bold{closed} outlines,
#' on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. \link{eFourier} can be applied.
#'  \code{Out} objects are primarily \code{\link{Coo}} objects.
#'
#' @param x a \code{list} of matrices of \eqn{(x; y)} coordinates,
#' or an array or an Out object or an Ldk object
#' @param ldk (optionnal) \code{list} of landmarks as row number indices
#' @param fac (optionnal) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @return an \code{Out} object
#' @seealso \link{Coo}, \link{Opn}, link{Ldk}.
#' @keywords Out
#' @aliases Out
#' @examples
#' methods(class=Out)
#' @export
Out <- function(x, ldk = list(), fac = data.frame) {
  UseMethod("Out")
}

#' @export
Out.default <- function(x, ldk = list(), fac = data.frame()) {
  cat(" * an Ldk object can only be build from a list, an array or an Opn object")
}

#' @export
Out.list <- function(x, ldk = list(), fac = data.frame()) {
  coo.list <- x
  Out <- list(coo = coo.list, ldk = ldk, fac = fac)
  if (!is.null(Out$fac))
    Out$fac <- .refactor(Out$fac)
  class(Out) <- c("Out", "Coo")
  return(Out)
}

#' @export
Out.array <- function(x, ldk = list(), fac = data.frame()) {
  x <- a2l(x)
  Out(x, fac = fac)
}

# experimental below
#' @export
Out.Coo <- function(x, ldk = list(), fac = data.frame()) {
  Out(x = x$coo, ldk = x$ldk, fac = x$fac)
}

# The print method for Out objects
#' @export
print.Out <- function(x, ...) {
  Out <- x
  ### Header
  cat("An Out object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo.nb <- length(Out)
  coo.len <- sapply(Out$coo, nrow)
  coo.closed <- sapply(Out$coo, is.closed)
  #     # one random outline
  #     eg <- sample(length(Out$coo), 1)
  #     coo.eg <- Out$coo[[eg]]
  #     colnames(coo.eg) <- c("x", "y")
  #     cat(" - One random outline in $coo: '", names(Out$coo)[eg],
  #         "':\n", sep = "")
  #     if (nrow(coo.eg) > 5) {
  #       print(coo.eg[1:5, ], print.gap = 2)
  #       cat("etc.\n")
  #     } else {
  #       print(coo.eg, print.gap = 2)
  #       cat("\n\n")
  #     }
  # number of outlines
  cat(" - $coo:", coo.nb, "outlines")
  
  # number of coordinates
  cat(" (", round(mean(coo.len)), " +/- ", round(sd(coo.len)), " coordinates, ", sep="")
  # outlines closed or not
  if (all(coo.closed)) {
    cat("all closed)\n")
  } else {
    if (any(!coo.closed)) {
      cat("all unclosed)\n")
    } else {
      cat(sum(coo.closed), " closed\n")
    }
  }
  # number of landmarks
  if (length(Out$ldk) != 0) {
    cat(" -", length(Out$ldk[[1]]), "landmark(s) defined\n")
  } else {
    #cat(" - No landmark defined\n")
  }
  # number of grouping factors
  df <- Out$fac
  nf <- ncol(df)
  if (nf == 0) {
    #cat(" - $fac: No groups defined in $fac\n")
  } else {
    if (nf<2) {
      cat(" - $fac:", nf, "grouping factor:\n")
    } else {
      cat(" - $fac:", nf, "grouping factors:\n")}
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      # cosmectics below
      if (sum(nchar(lev.i))>60){
        maxprint <- which(cumsum(nchar(lev.i))>30)[1]
        cat("     '", colnames(df)[i], "': ", paste(lev.i[1:maxprint], collapse=", "),
            " ... + ", length(lev.i) - maxprint, " more.\n", sep="")
      } else {
        cat("     '", colnames(df)[i], "': ", paste(lev.i, collapse=", "), ".\n", sep="")
      }
    }
  }
}


# 2. Out calibration
# -----------------------------------------------------------
#' Graphical calibration for Out objects
#'
#' Calculate and displays reconstructed shapes using a
#' range of harmonic number.
#'
#' @aliases hqual
#' @param Out the \code{Out} object on which to hqual
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform hqual
#' @param harm.range vector of harmonics on which to perform hqual
#' @param smooth.it numeric, number of smoothing iterations
#' @param scale logical whether to scale the shape
#' @param center logical whether to center the shape
#' @param align logical whether to align th shape
#' @param plot.method either \code{'\link{panel}'} or \code{'\link{stack}'}
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param shp.col a color for the shape (\code{NA} by default)
#' @param shp.border a color for the border of the shape
#' @param ... additional parameters to fed \link{coo.plot}
#' @keywords Out
#' @examples
#' data(bot)
#' hqual(bot)
#' @export
hqual <- function(Out, method = c("efourier", "rfourier", "tfourier"),
                  id, harm.range = c(1, 2, 4, 8, 16, 32), smooth.it = 0, scale = TRUE,
                  center = TRUE, align = TRUE, plot.method = c("panel", "stack")[1],
                  legend = TRUE, legend.title = "Nb of harmonics", palette = col.india,
                  shp.col = NA, shp.border = "#1A1A1A", ...) {
  UseMethod("hqual")
}
#' @export
hqual.Out <- function(Out, method = c("efourier", "rfourier",
                                      "tfourier"), id, harm.range = c(1, 2, 4, 8, 16, 32), smooth.it = 0,
                      scale = TRUE, center = TRUE, align = TRUE, plot.method = c("panel",
                                                                                 "stack")[1], legend = TRUE, legend.title = "Nb of harmonics",
                      palette = col.india, shp.col = NA, shp.border = "#1A1A1A",
                      ...) {
  if (missing(id))
    id <- sample(length(Out$coo), 1)
  if (missing(method)) {
    cat(" * Method not provided. efourier is used.\n")
    method <- efourier
    method.i <- efourier.i
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier",
                                   "tfourier"))
    if (is.na(p)) {
      warning(" * Unvalid method. efourier is used.\n")
    } else {
      method <- switch(p, efourier, rfourier, tfourier)
      method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)
    }
  }
  
  # check for too ambitious harm.range
  if (max(harm.range) > (min(sapply(Out$coo, nrow))/2 + 1)) {
    harm.range <- floor(seq(1, q/2 - 1, length = 6))
    cat(" * harm.range was too high and set to: ", harm.range,
        ".\n")
  }
  coo <- Out$coo[[id]]
  if (scale)
    coo <- coo.scale(coo)
  if (center)
    coo <- coo.center(coo)
  if (align)
    coo <- coo.align(coo)
  res <- list()
  for (i in seq(along = harm.range)) {
    res[[i]] <- method.i(method(coo, nb.h = max(harm.range),
                                smooth.it = smooth.it), nb.h = harm.range[i])
  }
  # plotting
  op <- par(mar = c(3, 3, 2, 1))
  on.exit(par(op))
  cols <- paste0(palette(length(harm.range)), "EE")
  if (plot.method == "stack") {
    coo <- coo.smooth(coo, smooth.it)
    coo.plot(coo, border = shp.border, col = shp.col, lwd = 2,
             points = FALSE, main = names(Out)[id], ...)
    for (i in seq(along = harm.range)) {
      lines(res[[i]], col = cols[i], lwd = 1)
    }
    if (legend) {
      legend("topright", legend = as.character(harm.range),
             bty = "n", col = cols, lty = 1, lwd = 1, cex = 0.7,
             title = legend.title)
    }
  } else {
    if (plot.method == "panel") {
      # par(oma=c(1, 1, 3, 0))
      pos <- coo.list.panel(res, cols = cols)
      if (legend) {
        text(x = pos[, 1], y = pos[, 2], as.character(harm.range))
      }
      title(names(Out)[id], cex = 1.3)
    }
  }
}

#' Quantitative calibration, through deviations, for Out objects
#'
#' Calculate deviations from original and reconstructed shapes using a
#' range of harmonic number.
#'
#' @aliases hquant
#' @param Coo the \code{Out} object on which to hquant
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform hquant
#' @param harm.range vector of harmonics on which to perform hquant
#' @param smooth.it numeric, number of smoothing iterations
#' @param norm.centsize logical whether to normalize deviation by the centroid size
#' @param dist.method a method such as \link{edm.nearest} to calculate deviations
#' @param dist.nbpts numeric the number of points to use for deviations calculations
#' @param plot logical whether to plot the results
#' @param dev.plot logical whether to plot deviations
#' @param title a title for the plot
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param lineat.y vector of numeric for drawing horizontal lines
#' @keywords Out
#' @examples
#' data(bot)
#' hqual(bot)
#' @export
hquant <- function(Coo, method = c("efourier", "rfourier", "tfourier"),
                   id = 1, harm.range = seq(4, 20, 4), smooth.it = 0, norm.centsize = TRUE,
                   dist.method = edm.nearest, dist.nbpts = 120, plot = TRUE,
                   dev.plot = TRUE, title = "Deviations along the outline",
                   legend = TRUE, legend.title = "Nb of harmonics", palette = col.summer,
                   lineat.y = c(0.5, 0.1, 0.01)) {
  UseMethod("hquant")
}

#' @export
hquant.Out <- function(Coo, method = c("efourier", "rfourier",
                                       "tfourier"), id = 1, harm.range = seq(4, 20, 4), smooth.it = 0,
                       norm.centsize = TRUE, dist.method = edm.nearest, dist.nbpts = 120,
                       plot = TRUE, dev.plot = TRUE, title = "Deviations along the outline",
                       legend = TRUE, legend.title = "Nb of harmonics", palette = col.summer,
                       lineat.y = c(0.5, 0.1, 0.01)) {
  if (missing(method)) {
    cat("  * Method not provided. efourier is used.\n")
    method <- efourier
    method.i <- efourier.i
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier",
                                   "tfourier"))
    if (is.na(p)) {
      warning("Unvalid method. efourier is used.")
    } else {
      method <- switch(p, efourier, rfourier, tfourier)
      method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)
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
  res <- array(NA, dim = c(nr, nc, nk), dimnames = list(paste0("h",
                                                               harm.range), paste("pt", 1:nb.pts), names(Coo)[id]))
  # progressbar
  if (nk > 5) {
    pb <- txtProgressBar(1, nk)
    t <- TRUE
  } else {
    t <- FALSE
  }
  # the core loops that will calculate deviations
  for (ind in seq(along = id)) {
    coo <- Coo$coo[[id[ind]]]
    # below, the best possible fit
    coo.best <- method.i(method(coo, nb.h = nb.h.best, smooth.it = smooth.it),
                         nb.pts = nb.pts)
    for (i in seq(along = harm.range)) {
      # for each number of harmonics we calculate deviation with
      # the FUN=method
      coo.i <- method.i(method(coo, nb.h = harm.range[i],
                               smooth.it = smooth.it), nb.pts = nb.pts)
      res[i, , ind] <- dist.method(coo.best, coo.i)
    }
    # we normalize by the centroid size
    if (norm.centsize) {
      res[, , ind] <- res[, , ind]/coo.centsize(coo)
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
    if (dev.plot) {
      if (nk > 1) {
        dev.plot(m, d, cols = cols)
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
#' @aliases hpow
#' @param Out the \code{Out} object on which to hpow
#' @param method any method from \code{c('efourier', 'rfourier', 'tfourier')}
#' @param id the shape on which to perform hpow. All of them by default
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
#' hpow(bot)
#' # if you want to do eFourier with 99% hpow in one step
#' eFourier(bot, nb.h=hpow(bot, "efourier", plot=FALSE)$minh["99%"])
#' @export
hpow <- function(Out, method = "efourier", id = 1:length(Out),
                 nb.h = 24, drop = 1, probs=seq(0, 1, 0.25),
                 smooth.it = 0, plot = TRUE,
                 xlim=c(drop+1, nb.h), ylim=c(0, 100),
                 title = "Fourier coefficients power spectrum",
                 lineat.y = c(90, 95, 99, 99.9), verbose=TRUE) {
  UseMethod("hpow")
}
#' @export
hpow.Out <- function(Out, method = "efourier", id = 1:length(Out),
                     nb.h, drop = 1, probs=seq(0, 1, 0.25),
                     smooth.it = 0, plot = TRUE,
                     xlim=c(drop+1, nb.h), ylim=c(0, 100),
                     title = "Harmonic power of coefficients",
                     lineat.y = c(90, 95, 99, 99.9), verbose=TRUE) {
  
  # we swith among methods, with a messsage
  if (missing(method)) {
    if (verbose) cat(" * Method not provided. hpow | efourier is used.\n")
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
    Out <- coo.smooth(Out, smooth.it)}
  # we prepare the result matrix
  res <- matrix(nrow = length(id), ncol = (nb.h - drop))
  x <- (drop + 1):nb.h
  for (i in seq(along = id)) {
    xf <- method(Out$coo[[id[i]]], nb.h = nb.h)
    pow <- harm.pow(xf)[x]
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
    #abline(h = lineat.y, lty = 1, col = col.heat(length(lineat.y)))
    #         lines(x, res[2, ], col="grey50")
    #         segments(x, res[1, ], x, res[3, ])
    
    devmat.plot <- function(q, x, cols.poly, med.col, opacity.max=0.5, ...){
      if (missing(x)) x <- 1:ncol(q)
      nq <- floor(nrow(q)/2)
      if (missing(cols.poly)) {
        cols <- col.grey(nq*2)
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


# 3. OutCoe definition
# -------------------------------------------------------
#' Builds an OutCoe object
#'
#' In Momocs, \code{OutCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot})
#' and specific methods can be applied.
#'  \code{OutCoe} objects are primarily \code{\link{Coe}} objects.
#'
#' @param coe \code{matrix} of harmonic coefficients
#' @param fac (optionnal) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param norm the normalisation used to obtain these coefficients
#' @return an \code{OutCoe} object
#' @details These methods can be applied on \code{Out} objects:
#' @seealso \link{Coe}, \link{OpnCoe}
#' @keywords Out
#' @examples
#' # all OutCoe methods
#' methods(class='OutCoe')
#' @export
OutCoe <- function(coe = matrix(), fac = data.frame(), method,
                   norm) {
  if (missing(method))
    stop("a method must be provided to OutCoe")
  OutCoe <- list(coe = coe, fac = fac, method = method, norm = norm)
  class(OutCoe) <- c("OutCoe", "Coe")
  return(OutCoe)
}

##### TO FIX FOR Combined OutCoe The print method for Out objects
#' @export
print.OutCoe <- function(x, ...) {
  OutCoe <- x
  p <- pmatch(OutCoe$method[1], c("eFourier", "rFourier", "tFourier"))
  met <- switch(p, "elliptical Fourier", "radii variation",
                "tangent angle")
  ### Header
  cat("An OutCoe object [", met, "analysis ]\n")
  cat(rep("-", 20), "\n", sep = "")
  coo.nb <- nrow(OutCoe$coe)  #nrow method ?
  harm.nb <- ncol(OutCoe$coe)/ifelse(p == 1, 4, 2)
  # number of outlines and harmonics
  cat(" - $coe:", coo.nb, "outlines described, ")
  cat(harm.nb, "harmonics\n")
  # lets show some of them for a quick inspection
  cat(" - $coe: 1st harmonic coefficients from random individuals: \n")
  row.eg <- sort(sample(coo.nb, ifelse(coo.nb < 5, coo.nb, 5), replace = FALSE))
  col.eg <- coeff.sel(retain = ifelse(harm.nb > 3, 3, harm.nb), drop = 0, nb.h = harm.nb, cph = ifelse(p == 1, 4, 2))
  print(round(OutCoe$coe[row.eg, col.eg], 3))
  cat("etc.\n")
  # number of grouping factors
  df <- OutCoe$fac
  nf <- ncol(df)
  if (nf == 0) {
    #cat(" - $fac: No groups defined in $fac\n")
  } else {
    if (nf<2) {
      cat(" - $fac:", nf, "grouping factor:\n")
    } else {
      cat(" - $fac:", nf, "grouping factors:\n")}
    for (i in 1:nf) {
      lev.i <- levels(df[, i])
      # cosmectics below
      if (sum(nchar(lev.i))>60){
        maxprint <- which(cumsum(nchar(lev.i))>30)[1]
        cat("     '", colnames(df)[i], "': ", paste(lev.i[1:maxprint], collapse=", "),
            " ... + ", length(lev.i) - maxprint, " more.\n", sep="")
      } else {
        cat("     '", colnames(df)[i], "': ", paste(lev.i, collapse=", "), ".\n", sep="")
      }
    }
  }
}

# 4. Out morphometrics
# ---------------------------------------------------------
#' Calculates elliptical Fourier transforms on Out objects
#'
#' A wrapper for \link{efourier} to be applied on Out objects.
#' @rdname eFourier-Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the coefficients using \link{efourier.norm}
#' @param start logical whether to consider the first point as homologous
#' @seealso \link{efourier}, \link{efourier.norm}
#' @examples
#' data(bot)
#' eFourier(bot, 12)
#' @export
eFourier <- function(Out, nb.h, smooth.it, norm, start) {
  UseMethod("eFourier")
}
#' @export
eFourier.Out <- function(Out, nb.h, smooth.it = 0, norm = TRUE,
                         start = FALSE) {
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")
  }
  if (nb.h > q) {
    nb.h <- q  # should not be 1 #todo
    cat(" * at least one outline has no more than", q * 2,
        "coordinates.\n", "* 'nb.h' has been set to", q,
        "harmonics.\n")
  }
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:4], each = nb.h), rep(1:nb.h,
                                                      times = 4))
  coe <- matrix(ncol = 4 * nb.h, nrow = length(coo), dimnames = list(names(coo),
                                                                     col.n))
  for (i in seq(along = coo)) {
    # todo: vectorize ?
    ef <- efourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it,
                   verbose = TRUE)
    if (norm) {
      ef <- efourier.norm(ef, start = start)
      if (ef$A[1] < 0) {
        ef$A <- (-ef$A)
        ef$B <- (-ef$B)
        ef$C <- (-ef$C)
        ef$D <- (-ef$D)
        ef$lnef <- (-ef$lnef)
      }
      coe[i, ] <- c(ef$A, ef$B, ef$C, ef$D)
    } else {
      coe[i, ] <- c(ef$an, ef$bn, ef$cn, ef$dn)
    }
  }
  coe[abs(coe) < 1e-12] <- 0  #not elegant but round normalized values to 0
  return(OutCoe(coe = coe, fac = Out$fac, method = "eFourier",
                norm = norm))
}

#' Calculates radius lengths Fourier analysis on Out objects
#'
#' A wrapper for \link{rfourier} to be applied on Out objects.
#' @rdname rFourier-Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients.
#' @seealso \link{rfourier}
#' @examples
#' data(bot)
#' rFourier(bot, 12)
#' @export
rFourier <- function(Out, nb.h, smooth.it, norm) {
  UseMethod("rFourier")
}
#' @export
rFourier.Out <- function(Out, nb.h = 40, smooth.it = 0, norm = TRUE) {
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * nb.h not provided and set to", nb.h, "\n")
  }
  if (nb.h > q) {
    nb.h <- q  # should not be 1 #todo
    cat(" * at least one outline has no more than", q * 2,
        "coordinates.\n", "* 'nb.h' has been set to", q,
        "harmonics.\n")
  }
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h,
                                                      times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo),
                                                                     col.n))
  for (i in seq(along = coo)) {
    rf <- rfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it,
                   norm = norm, verbose = TRUE)  #todo: vectorize
    coe[i, ] <- c(rf$an, rf$bn)
  }
  return(OutCoe(coe = coe, fac = Out$fac, method = "rFourier",
                norm = norm))
}

#' Calculates tangent angle Fourier analysis on Out objects
#'
#' A wrapper for \link{tfourier} to be applied on Out objects.
#' @rdname tFourier-Out
#' @param Out the Out object on which to calculate eft
#' @param nb.h the number of harmonics to calculate
#' @param smooth.it the number of smoothing iterations to perform
#' @param norm whether to normalize the matrix of coefficients. See Details.
#' @seealso \link{tfourier}
#' @examples
#' data(bot)
#' tFourier(bot, 12)
#' @export
tFourier <- function(Out, nb.h, smooth.it, norm) {
  UseMethod("tFourier")
}
#' @export
tFourier.Out <- function(Out, nb.h = 40, smooth.it = 0, norm = TRUE) {
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    nb.h <- if (q >= 32) {
      32
    } else {
      q
    }
    cat(paste("  * nb.h not provided and set to", nb.h, "\n"))
  }
  if (nb.h > q) {
    nb.h <- q  # should not be 1
    cat(" * At least one outline has no more than", q * 2,
        "coordinates.\n", "* 'nb.h' has been set to", q,
        "harmonics.\n")
  }
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h,
                                                      times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo),
                                                                     col.n))
  for (i in seq(along = coo)) {
    tf <- tfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it,
                   norm = norm, verbose = TRUE)
    coe[i, ] <- c(tf$an, tf$bn)
  }
  return(OutCoe(coe = coe, fac = Out$fac, method = "tFourier",
                norm = norm))
}

# 5. Out + landmarks
# ---------------------------------------------------------

#' Define landmarks on Out and Opn objects
#'
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @keywords Out Opn Ldk
#' @examples
#' \dontrun{
#' data(bot)
#' bot <- bot[1:5] # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by defLandmarks...
#' bot2 <- defLandmarks(bot, 3)
#' stack(bot2)
#' bot2$ldk
#' }
#' @export
defLandmarks <- function(Coo, nb.ldk) {
  UseMethod("defLandmarks")
}
#' @export
defLandmarks.Coo <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop(" * 'nb.ldk' must be specified.")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    Coo$ldk[[i]] <- coo.ldk(Coo$coo[[i]], nb.ldk = nb.ldk)
  }
  return(Coo)
}

#' Retrieve landmarks coordinates from Opn and Out objects
#'
#' In \link{Out} and \link{Opn} classes, landmarks (if any) are stored as
#' row indices. This methods allows to retrieve the corresponding (x; y) coordinates.
#' @param Coo a Coo object, either Out or Opn
#' @return an array of coordinates X (x; y) coordinates X number of shapes.
#' @seealso \link{defLandmarks}, \link{fgProcrustes}
#' @keywords Out Opn Ldk
#' @examples
#' data(hearts)
#' ldk.h <- getLandmarks(hearts)
#' stack(Ldk(a2l(ldk.h)))
#' ldk.h
#' @export
getLandmarks <- function(Coo) {
  UseMethod("getLandmarks")
}
#' @export
getLandmarks.Out <- function(Coo) {
  coo <- Coo$coo
  ldk <- Coo$ldk
  ref <- array(NA, dim = c(length(ldk[[1]]), ncol(coo[[1]]),
                           length(coo)))
  for (i in seq(along = coo)) {
    ref[, , i] <- coo[[i]][ldk[[1]], ]
  }
  return(ref)
}
#' @export
getLandmarks.Opn <- getLandmarks.Out

# Symmetry -----------------

#' Calcuates symmetry indices on OutCoe objects
#'
#' For \link{OutCoe} objects obtained with \link{eFourier}, calculates several
#' indices on the matrix of coefficients: \code{AD}, the sum of absolute values of
#' harmonic coefficients A and D; \code{BC} same thing for B and C; \code{amp} the
#' sum of the absolute value of all harmonic coefficients and \code{sym} which is the ratio
#' of \code{AD} over \code{amp}. See references below for more details.
#' @param OutCoe [efourier] objects
#' @return a matrix with 4 colums described above.
#' @references Below: the first mention, and two applications.
#' \itemize{
#' #' \item Iwata, H., Niikura, S., Matsuura, S., Takano, Y., & Ukai, Y. (1998).
#' Evaluation of variation of root shape of Japanese radish (Raphanus sativus L.)
#' based on image analysis using elliptic Fourier descriptors. Euphytica, 102, 143-149.
#' \item Iwata, H., Nesumi, H., Ninomiya, S., Takano, Y., & Ukai, Y. (2002).
#' The Evaluation of Genotype x Environment Interactions of Citrus Leaf Morphology
#' Using Image Analysis and Elliptic Fourier Descriptors. Breeding Science, 52(2),
#' 89-94. doi:10.1270/jsbbs.52.89
#' \item Yoshioka, Y., Iwata, H., Ohsawa, R., & Ninomiya, S. (2004).
#' Analysis of petal shape variation of Primula sieboldii by elliptic fourier descriptors
#' and principal component analysis. Annals of Botany, 94(5), 657-64. doi:10.1093/aob/mch190
#' }
#' @seealso \link{removeAsymmetric} and \link{removeSymmetric}.
#' @keywords OutCoe
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' res <- symmetry(bot.f)
#' hist(res[, 'sym'])
#' @export
symmetry <- function(OutCoe) {
  UseMethod("symmetry")
}
#' @export
symmetry.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "eFourier")
    stop(" * Can only be applied on OutCoe [eFourier] objects.")
  x <- OutCoe$coe
  nb.h <- ncol(x)/4
  AD.ids <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
  BC.ids <- (nb.h + 1):(nb.h * 3)
  AD <- apply(abs(x[, AD.ids]), 1, sum)
  BC <- apply(abs(x[, BC.ids]), 1, sum)
  amp <- apply(abs(x), 1, sum)
  sym <- AD/amp
  res <- cbind(AD, BC, amp, sym)
  return(res)
}


#' Removes asymmetric and symmetric variation on OutCoe objects
#'
#' Only for those obtained with \link{eFourier}, otherwise a message is returned.
#' \code{removeAsymmetric} sets all B and C coefficients to 0; \code{removeSymmetric} sets
#' all A and D coefficients to 0.
#' @param OutCoe an OutCoe object
#' @return an OutCoe object
#' @references Below: the first mention, and two applications.
#' \itemize{
#' #' \item Iwata, H., Niikura, S., Matsuura, S., Takano, Y., & Ukai, Y. (1998).
#' Evaluation of variation of root shape of Japanese radish (Raphanus sativus L.)
#' based on image analysis using elliptic Fourier descriptors. Euphytica, 102, 143-149.
#' \item Iwata, H., Nesumi, H., Ninomiya, S., Takano, Y., & Ukai, Y. (2002).
#' The Evaluation of Genotype x Environment Interactions of Citrus Leaf Morphology
#' Using Image Analysis and Elliptic Fourier Descriptors. Breeding Science, 52(2),
#' 89-94. doi:10.1270/jsbbs.52.89
#' \item Yoshioka, Y., Iwata, H., Ohsawa, R., & Ninomiya, S. (2004).
#' Analysis of petal shape variation of Primula sieboldii by elliptic fourier descriptors
#' and principal component analysis. Annals of Botany, 94(5), 657-64. doi:10.1093/aob/mch190
#' }
#' @seealso \link{symmetry}.
#' @examples
#' data(bot)
#' botf <- eFourier(bot, 12)
#' botSym <- removeAsymmetric(botf)
#' boxplot(botSym)
#' botSymp <- PCA(botSym)
#' plot(botSymp)
#' plot(botSymp, amp.shp=5)
#'
#' # Asymmetric only
#' botAsym <- removeSymmetric(botf)
#' boxplot(botAsym)
#' botAsymp <- PCA(botAsym)
#' plot(botAsymp)
#' # strange shapes because the original shape was mainly symmetric and would need its
#' # symmetric (eg its average) for a proper reconstruction. Should only be used like that:
#' plot(botAsymp, morpho=FALSE)
#' @rdname removeAsymmetric
#' @aliases removeSymmetric
#' @export
removeAsymmetric <- function(OutCoe) {
  UseMethod("removeAsymmetric")
}
#' @rdname removeAsymmetric
#' @export
removeAsymmetric.default <- function(OutCoe) {
  cat(" * Can only be applied on OutCoe objects.")
}
#' @rdname removeAsymmetric
#' @export
removeAsymmetric.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "eFourier")
    stop(" * Can only be applied on OutCoe [eFourier] objects.")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- (nb.h + 1):(nb.h * 3)
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

#' @rdname removeAsymmetric
#' @export
removeSymmetric <- function(OutCoe) {
  UseMethod("removeSymmetric")
}
#' @rdname removeAsymmetric
#' @export
removeSymmetric.default <- function(OutCoe) {
  cat(" * Can only be applied on OutCoe objects.")
}
#' @rdname removeAsymmetric
#' @export
removeSymmetric.OutCoe <- function(OutCoe) {
  if (OutCoe$method != "eFourier")
    stop(" * Can only be applied on OutCoe [eFourier] objects.")
  x <- OutCoe$coe
  nb.h <- ncol(OutCoe$coe)/4
  zeros <- c(1:nb.h, ((nb.h * 3 + 1):(nb.h * 4)))
  OutCoe$coe[, zeros] <- 0
  return(OutCoe)
}

# Bonus
# ------------------------------------------------------------------

#' Ptolemaic ellipses and illustration of eFourier
#'
#' Calculate and display Ptolemaic ellipses which illustrates
#' intuitively the principle behing elliptical Fourier analysis.
#'
#' @param Out The \code{Out} object on which to display Ptolemaic ellipses.
#' @param id The \code{id} on which to display Ptolemaic ellipses.
#' @param t A \code{vector} af angles (in radians) on which to display ellipses.
#' @param nb.h \code{integer}. The number of harmonics to display.
#' @param nb.pts \code{integer}. The number of points to use to display shapes.
#' @param palette A color palette.
#' @param legend \code{logical}. Whether to plot the legend box.
#' @references
#' This method has been inspired by the figures found in the followings papers.
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#'  \emph{Computer Graphics and Image Processing} \bold{18}: 236-258.
#' Crampton JS. 1995. Elliptical Fourier shape analysis of fossil bivalves:
#' some practical considerations. \emph{Lethaia} \bold{28}: 179-186.
#' @seealso \link{efourier}.
#' An intuitive explanation of elliptic Fourier analysis can be found in
#' the \bold{Details} section of the \link{efourier} function.
#' @examples
#' data(hearts)
#' Ptolemy(hearts, 1)
#' @export
Ptolemy <- function(Out, id, t, nb.h, nb.pts, palette, legend) {
  UseMethod("Ptolemy")
}
#' @export
Ptolemy.Out <- function(Out, id = 1, t = seq(0, 2 * pi, length = 7)[-1],
                        nb.h = 3, nb.pts = 360, palette = col.sari, legend = FALSE) {
  # we prepare and deduce
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd = NA)
  cols <- palette(nb.h)
  coo <- coo.center(Out$coo[[id]])
  # k <- floor(length(coo$x)/4)
  coo.plot(coo, main = names(Out)[id])
  # now we calculate for every harmonic
  coo.ef <- efourier(coo, nb.h)
  coo.efi <- efourier.i(coo.ef, nb.h, nb.pts)
  vect <- matrix(nrow = nb.h, ncol = 2)
  vect <- rbind(c(0, 0), vect)
  for (i in seq(along = t)) {
    for (j in 1:nb.h) {
      vect[j + 1, 1] <- coo.ef$an[j] * cos(j * t[i]) +
        coo.ef$bn[j] * sin(j * t[i])
      vect[j + 1, 2] <- coo.ef$cn[j] * cos(j * t[i]) +
        coo.ef$dn[j] * sin(j * t[i])
    }
    vs <- apply(vect, 2, cumsum)
    for (j in 1:nb.h) {
      lh <- efourier.shape(coo.ef$an[1:j], coo.ef$bn[1:j],
                           coo.ef$cn[1:j], coo.ef$dn[1:j], nb.h = j, nb.pts = nb.pts,
                           plot = FALSE)
      ellh <- efourier.shape(coo.ef$an[j], coo.ef$bn[j],
                             coo.ef$cn[j], coo.ef$dn[j], nb.h = 1, nb.pts = nb.pts,
                             plot = FALSE)
      lines(lh, col = paste(cols[j], "22", sep = ""), lwd = 0.8)
      lines(ellh[, 1] + vs[j, 1], ellh[, 2] + vs[j, 2],
            col = cols[j], lwd = 1)
      points(vs[j + 1, 1], vs[j + 1, 2], col = cols[j],
             cex = 0.8)
      arrows(vs[j, 1], vs[j, 2], vs[j + 1, 1], vs[j + 1,
                                                  2], col = cols[j], angle = 10, length = 0.05,
             lwd = 1.2)
    }
  }
  points(0, 0, pch = 20, col = cols[1])
  if (legend) {
    legend("topright", legend = as.character(1:nb.h), bty = "o",
           col = cols, lty = 1, lwd = 1, bg = "#FFFFFFCC", cex = 0.7,
           title = "Number of harmonics")
  }
}
