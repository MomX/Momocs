##### Core function for elliptical Fourier analyses

#' Elliptical Fourier transform (and its normalization)
#'
#' `efourier` computes Elliptical Fourier Analysis (or Transforms or EFT)
#' from a matrix (or a list) of (x; y) coordinates. `efourier_norm` normalizes Fourier coefficients.
#' Read Details carefully.
#'
#' @param x A \code{list} or a \code{matrix} of coordinates or a \code{Out} object
#' @param nb.h \code{integer}. The number of harmonics to use. If missing, 12 is used on shapes;
#' 99 percent of harmonic power on Out objects, both with messages.
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform.
#' @param norm whether to normalize the coefficients using \link{efourier_norm}
#' @param start `logical`. For `efourier` whether to consider the first point as homologous;
#' for `efourier_norm` whether to conserve the position of the first
#' point of the outline.
#' @param ef `list` with `a_n`, `b_n`, `c_n` and
#' `d_n` Fourier coefficients, typically returned by [efourier]
#' @param ... useless here
#' @return For `efourier`, a list with components: `an`, `bn`, `cn`, `dn` harmonic coefficients, plus `ao` and `co`.
#' The latter should have been named `a0` and `c0` in Claude (2008) but I (intentionnaly) propagated the error.
#'
#' For `efourier_norm`, a list with components: `A`, `B`, `C`, `D`
#' for harmonic coefficients, plus `size`, the magnitude of the semi-major axis of the first
#' fitting ellipse, `theta` angle, in radians, between the starting and the semi-major axis
#' of the first fitting ellipse, `psi` orientation of the first fitting ellipse, `ao` and `do`, same as above,
#' and `lnef` that is the concatenation of coefficients.
#'
#' @note Directly borrowed for Claude (2008).
#'
#' Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#'
#' @details For the maths behind see the paper in JSS.
#'
#' Normalization of coefficients has long been a matter of trouble,
#' and not only for newcomers. There are two ways of normalizing outlines: the first,
#' and by far the most used, is to use a "numerical" alignment, directly on the
#' matrix of coefficients. The coefficients of the first harmonic are consumed
#' by this process but harmonics of higher rank are normalized in terms of size
#' and rotation. This is sometimes referred as using the "first ellipse", as the
#' harmonics define an ellipse in the plane, and the first one is the mother of all
#' ellipses, on which all others "roll" along. This approach is really convenient
#' as it is done easily by most software (if not the only option) and by Momocs too.
#' It is the default option of \code{efourier}.
#'
#' But here is the pitfall: if your shapes are prone to bad aligments among all
#' the first ellipses, this will result in poorly (or even not at all) "homologous" coefficients.
#' The shapes particularly prone to this are either (at least roughly) circular and/or with a strong
#' bilateral symmetry. You can try to use \code{\link{stack}} on the \code{\link{Coe}} object
#'  returned by \code{efourier}. Also, and perhaps more explicitely, morphospace usually show a mirroring symmetry,
#'  typically visible when calculated in some couple of components (usually the first two).
#'
#'  If you see these  upside-down (or 180 degrees rotated) shapes on the morphospace,
#'  you should seriously consider aligning your shapes __before__ the [efourier] step,
#' and performing the latter with `norm = FALSE`.
#'
#' Such a pitfall explains the (quite annoying) message when passing `efourier` with just the `Out`.
#'
#' You have several options to align your shapes, using control points (or landmarks),
#' by far the most time consuming (and less reproducible) but possibly the best one too
#' when alignment is too tricky to automate.
#' You can also try Procrustes alignment (see \code{\link{fgProcrustes}}) through their calliper
#' length (see \code{\link{coo_aligncalliper}}), etc. You should also make the first
#' point homologous either with \code{\link{coo_slide}} or \code{\link{coo_slidedirection}}
#' to minimize any subsequent problems.
#'
#' I will dedicate (some day) a vignette or a paper to this problem.
#' @family efourier
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @examples
#' # single shape
#' coo <- bot[1]
#' coo_plot(coo)
#' ef <- efourier(coo, 12)
#' ef
#' efi <- efourier_i(ef)
#' coo_draw(efi, border='red', col=NA)
#'
#' # on Out
#' bot %>% slice(1:5) %>% efourier
#' @rdname efourier
#' @export
efourier <- function(x, ...){UseMethod("efourier")}
eFourier <- efourier

#' @rdname efourier
#' @export
efourier.default <- function(x, nb.h, smooth.it = 0, ...) {
  coo <- x
  coo <- coo_check(coo)
  if (coo_is_closed(coo))
    coo <- coo_unclose(coo)
  nr <- nrow(coo)
  if (missing(nb.h)) {
    nb.h <- 12
    message("'nb.h' not provided and set to ", nb.h)
  }
  if (nb.h * 2 > nr) {
    nb.h = floor(nr/2)
    if (.is_verbose()) {
      message("'nb.h' must be lower than half the number of points, and has been set to ", nb.h, "harmonics")
    }
  }
  if (nb.h == -1) {
    nb.h = floor(nr/2)
    if (.is_verbose()) {
      message("the number of harmonics used has been set to: ", nb.h)
    }
  }
  if (smooth.it != 0) {
    coo <- coo_smooth(coo, smooth.it)
  }
  Dx <- coo[, 1] - coo[, 1][c(nr, (1:(nr - 1)))]
  Dy <- coo[, 2] - coo[, 2][c(nr, (1:(nr - 1)))]
  Dt <- sqrt(Dx^2 + Dy^2)
  Dt[Dt < 1e-10] <- 1e-10  # to avoid Nan
  t1 <- cumsum(Dt)
  t1m1 <- c(0, t1[-nr])
  T <- sum(Dt)
  an <- bn <- cn <- dn <- numeric(nb.h)
  for (i in 1:nb.h) {
    Ti <- (T/(2 * pi^2 * i^2))
    r <- 2 * i * pi
    an[i] <- Ti * sum((Dx/Dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    bn[i] <- Ti * sum((Dx/Dt) * (sin(r * t1/T) - sin(r * t1m1/T)))
    cn[i] <- Ti * sum((Dy/Dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    dn[i] <- Ti * sum((Dy/Dt) * (sin(r * t1/T) - sin(r * t1m1/T)))
  }
  ao <- 2 * sum(coo[, 1] * Dt/T)
  co <- 2 * sum(coo[, 2] * Dt/T)
  return(list(an = an, bn = bn, cn = cn, dn = dn, ao = ao,
              co = co))
}

#' @rdname efourier
#' @export
efourier.Out <- function(x, nb.h, smooth.it = 0, norm = TRUE, start = FALSE, ...) {
  if (norm)
    message("you selected `norm=TRUE`, which is not recommended. See ?efourier")
  Out <- x
  # validates
  Out %<>% validate()
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    #nb.h <- ifelse(q >= 32, 32, q)
    nb.h <- calibrate_harmonicpower_efourier(Out, thresh = 99, plot=FALSE)$minh
    if (.is_verbose()) message("'nb.h' not provided and set to ", nb.h, " (99% harmonic power)")
  }
  if (nb.h > q) {
    nb.h <- q
    message("at least one outline has no more than ", q * 2,
            " coordinates. 'nb.h' has been set to ", q, " harmonics")
  }
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:4], each = nb.h), rep(1:nb.h,
                                                      times = 4))
  coe <- matrix(ncol = 4 * nb.h, nrow = length(coo), dimnames = list(names(coo),
                                                                     col.n))
  for (i in seq(along = coo)) {
    # todo: vectorize
    ef <- efourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it)
    if (norm) {
      ef <- efourier_norm(ef, start = start)
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
  res <- OutCoe(coe = coe, fac = Out$fac, method = "efourier", norm = norm)
  res$cuts <- ncol(res$coe)
  return(res)
}


#' @rdname efourier
#' @export
efourier_norm <- function(ef, start = FALSE) {
  A1 <- ef$an[1]
  B1 <- ef$bn[1]
  C1 <- ef$cn[1]
  D1 <- ef$dn[1]
  nb.h <- length(ef$an)
  theta <- 0.5 * atan(2 * (A1 * B1 + C1 * D1)/(A1^2 + C1^2 -
                                                 B1^2 - D1^2))%%pi
  phaseshift <- matrix(c(cos(theta), sin(theta), -sin(theta),
                         cos(theta)), 2, 2)
  M2 <- matrix(c(A1, C1, B1, D1), 2, 2) %*% phaseshift
  v <- apply(M2^2, 2, sum)
  if (v[1] < v[2]) {
    theta <- theta + pi/2
  }
  theta <- (theta + pi/2)%%pi - pi/2
  Aa <- A1 * cos(theta) + B1 * sin(theta)
  Cc <- C1 * cos(theta) + D1 * sin(theta)
  scale <- sqrt(Aa^2 + Cc^2)
  psi <- atan(Cc/Aa)%%pi
  if (Aa < 0) {
    psi <- psi + pi
  }
  size <- 1/scale
  rotation <- matrix(c(cos(psi), -sin(psi), sin(psi), cos(psi)),
                     2, 2)
  A <- B <- C <- D <- numeric(nb.h)
  if (start) {
    theta <- 0
  }
  for (i in 1:nb.h) {
    mat <- size * rotation %*%
      matrix(c(ef$an[i], ef$cn[i],
               ef$bn[i], ef$dn[i]), 2, 2) %*%
      matrix(c(cos(i * theta), sin(i * theta),
               -sin(i * theta), cos(i * theta)), 2, 2)
    A[i] <- mat[1, 1]
    B[i] <- mat[1, 2]
    C[i] <- mat[2, 1]
    D[i] <- mat[2, 2]
    lnef <- c(A[i], B[i], C[i], D[i])
  }
  list(A = A, B = B, C = C, D = D, size = scale, theta = theta,
       psi = psi, ao = ef$ao, co = ef$co, lnef = lnef)
}


#' Inverse elliptical Fourier transform
#'
#' \code{efourier_i} uses the inverse elliptical Fourier transformation to
#' calculate a shape, when given a list with Fourier coefficients, typically
#' obtained computed with \link{efourier}.
#'
#' See \link{efourier} for the mathematical background.
#'
#' @param ef \code{list}. A list containing \eqn{a_n}, \eqn{b_n}, \eqn{c_n} and
#' \eqn{d_n} Fourier coefficients, such as returned by \code{efourier}.
#' @param nb.h \code{integer}. The number of harmonics to use. If not
#' specified, \code{length(ef$an)} is used.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @return A matrix of (x; y) coordinates.
#' @note Directly borrowed for Claude (2008), and also called \code{iefourier} there.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @family efourier
#' @examples
#' coo <- bot[1]
#' coo_plot(coo)
#' ef  <- efourier(coo, 12)
#' ef
#' efi <- efourier_i(ef)
#' coo_draw(efi, border='red', col=NA)
#' @export
efourier_i <- function(ef, nb.h, nb.pts = 120) {
  if (is.null(ef$ao))
    ef$ao <- 0
  if (is.null(ef$co))
    ef$co <- 0
  an <- ef$an
  bn <- ef$bn
  cn <- ef$cn
  dn <- ef$dn
  ao <- ef$ao
  co <- ef$co
  if (missing(nb.h)) {
    nb.h <- length(an)
  }
  theta <- seq(0, 2 * pi, length = nb.pts + 1)[-(nb.pts + 1)]
  hx <- matrix(NA, nb.h, nb.pts)
  hy <- matrix(NA, nb.h, nb.pts)
  for (i in 1:nb.h) {
    hx[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i * theta)
    hy[i, ] <- cn[i] * cos(i * theta) + dn[i] * sin(i * theta)
  }
  x <- (ao/2) + apply(hx, 2, sum)
  y <- (co/2) + apply(hy, 2, sum)
  coo <- cbind(x, y)
  colnames(coo) <- c("x", "y")
  return(coo)
}

#' Calculates and draw 'efourier' shapes.
#'
#' \code{efourier_shape} calculates a 'Fourier elliptical shape' given Fourier
#' coefficients (see \code{Details}) or can generate some 'efourier' shapes.
#' Mainly intended to generate shapes and/or to understand how efourier works.
#'
#' \code{efourier_shape} can be used by specifying \code{nb.h} and
#' \code{alpha}. The coefficients are then sampled in an uniform distribution
#' \eqn{(-\pi ; \pi)} and this amplitude is then divided by
#' \eqn{harmonicrank^alpha}. If \code{alpha} is lower than 1, consecutive
#' coefficients will thus increase. See \link{efourier} for the mathematical
#' background.
#'
#' @param an \code{numeric}. The \eqn{a_n} Fourier coefficients on which to
#' calculate a shape.
#' @param bn \code{numeric}. The \eqn{b_n} Fourier coefficients on which to
#' calculate a shape.
#' @param cn \code{numeric}. The \eqn{c_n} Fourier coefficients on which to
#' calculate a shape.
#' @param dn \code{numeric}. The \eqn{d_n} Fourier coefficients on which to
#' calculate a shape.
#' @param nb.h \code{integer}. The number of harmonics to use.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @param alpha \code{numeric}. The power coefficient associated with the
#' (usually decreasing) amplitude of the Fourier coefficients (see
#' \bold{Details}).
#' @param plot \code{logical}. Whether to plot or not the shape.
#' @return A list with components:
#' \itemize{
#'  \item \code{x} \code{vector} of x-coordinates
#'  \item \code{y} \code{vector} of y-coordinates.
#'  }
#' @family efourier
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#'
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @examples
#' ef <- efourier(bot[1], 24)
#' efourier_shape(ef$an, ef$bn, ef$cn, ef$dn) # equivalent to efourier_i(ef)
#' efourier_shape() # is autonomous
#'
#' panel(Out(a2l(replicate(100,
#' efourier_shape(nb.h=6, alpha=2.5, plot=FALSE))))) # Bubble family
#' @export
efourier_shape <- function(an, bn, cn, dn, nb.h, nb.pts = 60,
                           alpha = 2, plot = TRUE) {
  if (missing(nb.h) & missing(an))
    nb.h <- 3
  if (missing(nb.h) & !missing(an))
    nb.h <- length(an)
  if (missing(an))
    an <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  if (missing(bn))
    bn <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  if (missing(cn))
    cn <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  if (missing(dn))
    dn <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  ef <- list(an = an, bn = bn, cn = cn, dn = dn, ao = 0, co = 0)
  shp <- efourier_i(ef, nb.h = nb.h, nb.pts = nb.pts)
  if (plot)
    coo_plot(shp)
  return(shp)
}

##### end efourier
