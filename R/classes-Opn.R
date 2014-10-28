
# 1. Opn builder and domestic functions
# -------------------------------------------

#' Builds an Opn object
#'
#' In Momocs, \code{Opn} classes objects are
#' lists of \bold{op}e\bold{n} outlines, with optionnal components,
#' on which generic methods such as plotting methods (e.g. \link{stack})
#' and specific methods (e.g. \link{rawPolynomials} can be applied.
#'  \code{\link{Opn}} objects are primarily \code{\link{Coo}} objects.
#'
#' @param x \code{list} of matrices of (x; y) coordinates
#' @param fac (optionnal) a \code{data.frame} of factors and/or numerics
#' specifying the grouping structure
#' @param ldk (optionnal) \code{list} of landmarks as row number indices
#' @return an \code{Opn} object
#' @seealso \link{Coo}, \link{Out}, \link{Ldk}.
#' @keywords Opn
#' @aliases Opn
#' @examples
#' methods(class=Opn)
#' # we load some open outlines. See ?olea for credits
#' data(olea)
#' olea
#' panel(olea)
#' nqual(olea)
#' # orthogonal polynomials
#' op <- orthoPolynomials(olea, degree=5)
#' # we print the Coe
#' op
#' # Let's do a PCA on it
#' op.p <- PCA(op)
#' plot(op.p, 'domes')
#' plot(op.p, 'cep')
#' # and now an LDA
#' olda <- LDA(op, 'cep')
#' # for CV table
#' olda
#' plot(olda)
#' @export
Opn <- function(x, fac = data.frame(), ldk = list()) {
  UseMethod("Opn")
}

#' @export
Opn.default <- function(x, fac = data.frame(), ldk = list()) {
  cat(" * an Opn object can only be build from a list, an array or a Coo object")
}

#' @export
Opn.list <- function(x, fac = data.frame(), ldk = list()) {
  Opn <- structure(list(coo = x, fac = fac, ldk = ldk), class=c("Opn", "Coo"))
  if (!is.null(Opn$fac))
    Opn$fac <- as.data.frame(Opn$fac, stringsAsFactors = FALSE)
  class(Opn) <- c("Opn", "Coo")
  return(Opn)
}

#' @export
Opn.array <- function(x, fac = data.frame(), ldk = list()) {
  x <- a2l(x)
  Opn(x, fac = fac, ldk = ldk)
}

#' @export
Opn.Coo <- function(x, fac = data.frame(), ldk = list()) {
  Opn(x = x$coo, fac = x$fac, ldk = x$ldk)
}

# The print method for Out objects
#' @export
print.Opn <- function(x, ...) {
  Opn <- x
  ### Header
  cat("An Opn object with: \n")
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- length(Opn)
  coo_len <- sapply(Opn$coo, nrow)
  coo_closed <- sapply(Opn$coo, is.closed)
  #     # one random outline
  #     eg <- sample(length(Opn$coo), 1)
  #     coo_eg <- Opn$coo[[eg]]
  #     colnames(coo_eg) <- c("x", "y")
  #     cat(" - One random open outline in $coo: '", names(Opn$coo)[eg],
  #         "':\n", sep = "")
  #     if (nrow(coo_eg) > 5) {
  #         print(coo_eg[1:5, ], print.gap = 2)
  #         cat("etc.\n")
  #     } else {
  #         print(coo_eg, print.gap = 2)
  #         cat("\n\n")
  #     }
  # number of outlines
  cat(" - $coo:", coo_nb, "open outlines")
  
  # number of coordinates
  cat(" (", round(mean(coo_len)), " +/- ", round(sd(coo_len)), " coordinates)\n", sep="")
  # number of landmarks
  if (length(Opn$ldk) != 0) {
    cat(" - $ldk", length(Opn$ldk[[1]]), "landmark(s) defined\n")
  } else {
    #     cat(" - No landmark defined\n")
  }
  # we print the fac
  .print.fac(Opn$fac)
}

# 2. Opn calibration
# -----------------------------------------------------------
#' Graphical calibration for Opn objects
#'
#' Calculate and displays reconstructed shapes using a
#' range of polynomial degrees.
#'
#' @aliases nqual
#' @param Opn the \code{Opn} object on which to nqual
#' @param method any method from \code{c('rawPolynomials', 'orthoPolynomials')}
#' @param id the shape on which to perform nqual
#' @param degree.range vector of polynomial degrees on which to perform nqual
#' @param smooth.it numeric, number of smoothing iterations
#' @param baseline1 \eqn{(x; y)} coordinates for the first point of the baseline
#' @param baseline2 \eqn{(x; y)} coordinates for the second point of the baseline
#' @param plot.method either \code{'\link{panel}'} or \code{'\link{stack}'}
#' @param legend logical whether to plot a legend
#' @param legend.title if TRUE above, its title
#' @param palette a color \link{palette}
#' @param shp.border a color for the border of the shape
#' @keywords Opn
#' @examples
#' data(olea)
#' nqual(olea, degree.range=1:9)
#' @export
nqual <- function(Opn, method = c("rawPolynomials", "orthoPolynomials"),
                  id, degree.range = c(2, 3, 4, 6, 8, 10), smooth.it = 0, baseline1 = c(-1,
                                                                                        0), baseline2 = c(1, 0), plot.method = c("panel", "stack")[1],
                  legend = TRUE, legend.title = "Degree", palette = col.india,
                  shp.border = "#1A1A1A") {
  UseMethod("nqual")
}
#' @export
nqual.Opn <- function(Opn, method = c("rawPolynomials", "orthoPolynomials"),
                      id, degree.range = c(2, 3, 4, 6, 8, 10), smooth.it = 0, baseline1 = c(-1,
                                                                                            0), baseline2 = c(1, 0), plot.method = c("panel", "stack")[1],
                      legend = TRUE, legend.title = "Degree", palette = col.india,
                      shp.border = "#1A1A1A") {
  if (missing(id))
    id <- sample(length(Opn$coo), 1)
  if (missing(method)) {
    cat(" * Method not provided. orthoPolynomials is used.\n")
    ortho <- TRUE
  } else {
    p <- pmatch(tolower(method), c("rawpolynomials", "orthopolynomials"))
    if (is.na(p)) {
      warning(" * Unvalid method. orthoPolynomials is used.\n")
    } else {
      ortho <- switch(p, TRUE, FALSE)
    }
  }
  # check for too ambitious harm.range
  if (max(degree.range) > (min(sapply(Opn$coo, nrow)) - 1)) {
    degree.range <- (min(sapply(Opn$coo, nrow)) - 1)
    cat(" * degree.range was too high and set to: ", degree.range,
        ".\n")
  }
  coo <- Opn$coo[[id]]
  if (smooth.it != 0)
    coo <- coo_smoothcurve(coo, smooth.it)
  coo <- coo_baseline(coo, ldk1 = 1, ldk2 = nrow(coo), t1 = baseline1,
                      t2 = baseline2)
  res <- list()
  for (i in seq(along = degree.range)) {
    res[[i]] <- polynomials.i(polynomials(coo, degree = degree.range[i],
                                          ortho = ortho))
  }
  # plotting
  op <- par(mar = c(3, 3, 2, 1))
  on.exit(par(op))
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
      pos <- coo_list.panel(res, borders = cols, cols = par("bg"),
                            poly = FALSE)
      if (legend) {
        text(x = pos[, 1], y = pos[, 2], as.character(degree.range))
      }
      title(names(Opn)[id], cex = 1.3)
    }
  }
}

# nquant npow

# 3. OpnCoe definition
# ---------------------------------------------------------
#' Builds an OpnCoe object
#'
#' In Momocs, \code{OpnCoe} classes objects are wrapping around
#' lists of morphometric coefficients, along with other informations,
#' on which generic methods such as plotting methods (e.g. \link{boxplot})
#' and specific methods can be applied.
#'  \code{OpnCoe} objects are primarily \code{\link{Coe}} objects.
#'
#' @param coe \code{matrix} of morphometric coefficients
#' @param fac (optionnal) a \code{data.frame} of factors,
#' specifying the grouping structure
#' @param method used to obtain these coefficients
#' @param baseline1 \eqn{(x; y)} coordinates of the first baseline point
#' @param baseline2 \eqn{(x; y)} coordinates of the second baseline point
#' @param mod an R \link{lm} object, used to reconstruct shapes
#' @param r2 numeric, the r-squared from every model
#' @return an \code{OpnCoe} object
#' @seealso \link{Coe}, \link{OutCoe}
#' @keywords Opn
#' @examples
#' # all OpnCoe classes
#' methods(class='OpnCoe')
#' @export
OpnCoe <- function(coe = matrix(), fac = data.frame(), method = character(),
                   baseline1 = numeric(), baseline2 = numeric(), mod = list(),
                   r2 = numeric()) {
  if (missing(method))
    stop("a method must be provided to OpnCoe")
  OpnCoe <- list(coe = coe, fac = fac, method = method, baseline1 = baseline1,
                 baseline2 = baseline2, mod = mod, r2 = r2)
  class(OpnCoe) <- c("OpnCoe", "Coe")
  return(OpnCoe)
}

# The print method for Out objects
#' @export
print.OpnCoe <- function(x, ...) {
  OpnCoe <- x
  if (length(OpnCoe$method)>1) {
    met <- c("combined:", paste0(OpnCoe$method, collapse=" + "))
    met <- c(met, "analyses ]\n")
    combined <- TRUE
  } else {
    p <- pmatch(OpnCoe$method, c("rawPolynomials", "orthoPolynomials", "dct"))
    met <- switch(p, "raw Polynomials", "orthogonal Polynomials", "discret cosine tansform")
    met <- c(met, "analysis ]\n")
    combined <- FALSE
  }
  ### Header
  cat("An OpnCoe object [", met)
  cat(rep("-", 20), "\n", sep = "")
  coo_nb <- nrow(OpnCoe$coe)  #nrow method ?
  if (!combined) {
    degree <- ncol(OpnCoe$coe)
    # p==3 is the case for dct all along the method
    if (p==3) degree <- degree/2 
    # number of outlines and harmonics
    cat(" - $coe:", coo_nb, "open outlines described, ")
    if (p==3){
      cat(degree, " harmonics\n", sep="")
    } else {
      cat(degree, "th degree (+Intercept)\n", sep="")
    }
    # we print the baselines
    cat(" - $baseline1: (", paste(x$baseline1, collapse="; "), ")\n", sep="")
    cat(" - $baseline2: (", paste(x$baseline2, collapse="; "), ")\n", sep="")
    # lets show some of the coefficients for a quick inspection
    if (p==3){
      cat(" - $coe: 1st harmonic coefficients from random open outlines: \n")
    } else {
      cat(" - $coe: 1st polynomial coefficients from random open outlines: \n")}
    row.eg <- sort(sample(coo_nb, ifelse(coo_nb < 5, coo_nb, 5), replace = FALSE))
    nc <- ncol(OpnCoe$coe)
    if (nc > 10) {
      if (p==3) { col.eg <- c(1:5, (nc/2)+1:5)
      } else {
        col.eg <- 1:nc}
    } else {
      col.eg <- 1:nc
    }
    print(round(OpnCoe$coe[row.eg, col.eg], 3))
    cat("etc.\n")
  } else {
    cat(" - $baseline1, $baseline2: baselines registrations\n")
    cat(" - $coe: coefficients\n")}
  if (p != 3) {
    # r2 quick summary
    r2  <- OpnCoe$r2
    cat(" - $r2: min=", signif(min(r2), 3),
        ", median=",    signif(median(r2), 3),
        ", mean=",      signif(mean(r2), 3),
        ", sd=",        signif(mean(r2), 3),
        ", max=",       signif(max(r2), 3), "\n", sep="")}
  # we print the fac
  .print.fac(OpnCoe$fac)
}

# 3. Opn morphometrics
# ---------------------------------------------------------
#' Calculates raw (natural) polynomials on Opn
#'
#' @aliases rawPolynomials
#' @param Opn an \link{Opn} object
#' @param degree of the polynomial
#' @param baseline1 numeric the (x; y) coordinates of the first baseline
#' by default (x= -0.5; y=0)
#' @param baseline2 numeric the (x; y) coordinates of the second baseline
#' by default (x= 0.5; y=0)
#' @param nb.pts number of points to sample and on which to calculate polynomials
#' @return a \code{OpnCoe} object.
#' @keywords Opn
#' @examples
#' data(olea)
#' op <- rawPolynomials(olea, 5)
#' op
#' # a summary of the r2 (fit)
#' hist(op$r2)
#' # and the standard deviation
#' sd(op$r2)
#' @export
rawPolynomials <- function(Opn, degree, baseline1, baseline2, nb.pts) {
  UseMethod("rawPolynomials")
}
#' @export
rawPolynomials.Opn <- function(Opn, degree,
                               baseline1 = c(-0.5, 0), baseline2 = c(0.5, 0), nb.pts = 120) {
  # we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if (missing(nb.pts)) {
      nb.pts <- min.pts
      cat(" * 'nb.pts' missing and set to: ", nb.pts, "\n")
    } else {
      nb.pts <- min.pts
      cat(" * at least one outline has less coordinates than 'nb.pts':",
          nb.pts, "\n")
    }
  }
  if (missing(degree)) {
    degree <- 5
    cat(" * 'degree' missing and set to: ", degree, "\n")
  }
  # we normalize
  Opn <- coo_sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo_baseline, ldk1 = 1, ldk2 = nb.pts,
                t1 = baseline1, t2 = baseline2)
  # we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow = length(Opn), ncol = degree + 1,
                dimnames = list(rn, cn))
  r2 <- numeric(length(Opn))
  mod <- list()
  # the loop
  for (i in seq(along = coo)) {
    mod <- polynomials(coo[[i]], degree = degree, ortho = FALSE)
    # mod[[i]] <- pol
    coe[i, ] <- mod$coeff
    r2[i] <- mod$r2
  }
  # mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "rawPolynomials"
  return(OpnCoe(coe = coe, fac = Opn$fac, method = method,
                baseline1 = baseline1, baseline2 = baseline2, r2 = r2,
                mod = mod))
}

#' Calculates orthogonal polynomials on Opn
#'
#' @aliases orthoPolynomials
#' @param Opn an \link{Opn} object
#' @param degree of the polynomial
#' @param baseline1 numeric the \eqn{(x; y)} coordinates of the first baseline
#' by default \eqn{(x= -0.5; y=0)}
#' @param baseline2 numeric the \eqn{(x; y)} coordinates of the second baseline
#' by default \eqn{(x= 0.5; y=0)}
#' @param nb.pts number of points to sample and on which to calculate polynomials
#' @return a \code{OpnCoe} object.
#' @keywords Opn
#' @examples
#' data(olea)
#' op <- orthoPolynomials(olea, 5)
#' op
#' # a summary of the r2 (fit)
#' hist(op$r2)
#' summary(op$r2)
#' # and the standard deviation
#' sd(op$r2)
#' @export
orthoPolynomials <- function(Opn, degree, baseline1, baseline2,
                             nb.pts) {
  UseMethod("orthoPolynomials")
}
#' @export
orthoPolynomials.Opn <- function(Opn, degree, baseline1 = c(-0.5, 0), baseline2 = c(0.5, 0), nb.pts = 120) {
  # we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if (missing(nb.pts)) {
      nb.pts <- min.pts
      cat(" * 'nb.pts' missing and set to: ", nb.pts, "\n")
    } else {
      nb.pts <- min.pts
      cat(" * at least one outline has less coordinates than 'nb.pts':",
          nb.pts, "\n")
    }
  }
  if (missing(degree)) {
    degree <- 5
    cat(" * 'degree' missing and set to: ", degree, "\n")
  }
  # we normalize
  Opn <- coo_sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo_baseline, ldk1 = 1, ldk2 = nb.pts,
                t1 = baseline1, t2 = baseline2)
  # we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow = length(Opn), ncol = degree + 1,
                dimnames = list(rn, cn))
  r2 <- numeric(length(Opn))
  mod <- list()
  # the loop
  for (i in seq(along = coo)) {
    mod <- polynomials(coo[[i]], degree = degree, ortho = TRUE)
    # mod[[i]] <- pol
    coe[i, ] <- mod$coeff
    r2[i] <- mod$r2
  }
  # mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "orthoPolynomials"
  return(OpnCoe(coe = coe, fac = Opn$fac, method = method,
                baseline1 = baseline1, baseline2 = baseline2, r2 = r2,
                mod = mod))
}

# nquant (n no longer pertinent) calib.xxx everywhere ?? npow
# / nr2 natSplines cubicSplines Bezier

###### end Opn
