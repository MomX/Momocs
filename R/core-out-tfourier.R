##### Core function for tangent angle Fourier analyses

#' Tangent angle Fourier transform
#'
#' \code{tfourier} computes tangent angle Fourier analysis from a matrix or a
#' list of coordinates.
#'
#' Given a closed outline which the outline has been scaled to \eqn{2\pi},
#' \eqn{\phi(t)} can be expressed as follows: \eqn{ \phi(t) = \theta(t) -
#' \theta(0) - t } where \eqn{t} is the distance along the outline,
#' \eqn{\theta(t)} the angle of the tangent vector at \eqn{t} and
#' \eqn{\theta(0)} the angle of the tangent vector taken for the first point.
#' It can be removed for normalizing the coefficients obtained. Two
#' coefficients per harmonics can be estimated as follow:
#'
#' \eqn{ a_n = \frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\cos n \theta_i } \eqn{
#' b_n = \frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\sin n \theta_i } with \eqn{
#' a_0 = \sqrt{\frac{2}{p}}\sum\limits_{n=1}^{p}\phi(t) }
#'
#' @param x A list or matrix of coordinates or an \code{Out}
#' @param nb.h \code{integer}. The number of harmonics to use. If missing 99pc harmonic power is used.
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform
#' @param norm \code{logical}. Whether to scale and register new coordinates so
#' that the first point used is sent on the origin.
#' @param verbose \code{logical}. Whether to display diagnosis messages.
#' @param ... useless here
#' @return A list with the following components:
#' \itemize{
#' \item \code{ao} ao harmonic coefficient
#' \item \code{an} vector of \eqn{a_{1->n}} harmonic coefficients
#' \item \code{bn} vector of \eqn{b_{1->n}} harmonic coefficients
#' \item \code{phi} vector of variation of the tangent angle
#' \item \code{t} vector of distance along the perimeter expressed in radians
#' \item \code{perimeter} numeric. The perimeter of the outline
#' \item \code{thetao} numeric. The first tangent angle
#' \item \code{x1} The x-coordinate of the first point
#' \item \code{y1} The y-coordinate of the first point.
#' }
#' @seealso \link{tfourier} for analysis on \link{Out} objects.
#' \link{efourier}, \link{rfourier} for the other members of the
#' Fourier's family. \link{tfourier_shape} to play around with this approach.
#' @note Directly borrowed for Claude (2008), and called \code{fourier2} there.
#' @references Zahn CT, Roskies RZ. 1972. Fourier Descriptors for Plane Closed
#' Curves. \emph{IEEE Transactions on Computers} \bold{C-21}: 269-281.
#'
#' Claude, J. (2008) \emph{Morphometrics with R}, Use R! series, Springer 316
#' pp.
#' @examples
#' data(bot)
#' coo <- bot[1]
#' coo_plot(coo)
#' tf  <- tfourier(coo, 12)
#' tf
#' tfi <- tfourier_i(tf)
#' coo_draw(tfi, border='red', col=NA) # the outline is not closed...
#' coo_draw(tfourier_i(tf, force2close=TRUE), border='blue', col=NA) # we force it to close.
#' @rdname tfourier
#' @export
tfourier <- function(x, ...) {UseMethod("tfourier")}
tFourier <- tfourier

#' @rdname tfourier
#' @export
tfourier.default <- function(x, nb.h, smooth.it = 0, norm = FALSE, verbose = TRUE, ...) {
  coo <- x
  if (missing(nb.h)) {
        nb.h <- 12
        message("'nb.h' not provided and set to ", nb.h)
    }
    if (is.list(coo)) {
        coo <- l2m(coo)
    }
    if (is_closed(coo)) {
        coo <- coo_unclose(coo)
    }
    if (nb.h * 2 > nrow(coo)) {
        nb.h = floor(nrow(coo)/2)
        if (verbose) {
            message("'nb.h' must be lower than half the number of points and has been set to ",
                nb.h)
        }
    }
    if (nb.h == -1) {
        nb.h = floor(nrow(coo)/2)
        if (verbose) {
            message("'nb.h' must be lower than half the number of points. It has been set to ", nb.h, " harmonics")
        }
    }
    if (smooth.it != 0) {
        coo <- coo_smooth(coo, smooth.it)
    }
    if (norm) {
        coo <- coo_scale(coo_center(coo))
        coo <- coo_trans(coo, -coo[1, 1], -coo[1, 2])
    }
    p <- nrow(coo)
    an <- bn <- numeric(nb.h)
    tangvect <- coo - rbind(coo[p, ], coo[-p, ])
    perim <- sum(sqrt(apply((tangvect)^2, 1, sum)))
    v0 <- coo[1, ] - coo[p, ]
    tet1 <- Arg(complex(real = tangvect[, 1], imaginary = tangvect[,
        2]))
    tet0 <- tet1[1]
    t1 <- seq(0, 2 * pi, length = (p + 1))[1:p]
    phi <- (tet1 - tet0 - t1)%%(2 * pi)
    ao <- 2 * sum(phi)/p
    for (i in 1:nb.h) {
        an[i] <- (2/p) * sum(phi * cos(i * t1))
        bn[i] <- (2/p) * sum(phi * sin(i * t1))
    }
    list(ao = ao, an = an, bn = bn, phi = phi, t = t1, perimeter = perim,
        thetao = tet0, x1 = coo[1, 1], y1 = coo[1, 2])
}

#' @rdname tfourier
#' @export
tfourier.Out <- function(x, nb.h = 40, smooth.it = 0, norm = TRUE, verbose=TRUE, ...) {
  Out <- x
  # validates
  Out %<>% validate()
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    nb.h <- calibrate_harmonicpower(Out, method="tfourier",
                                    thresh = 99, verbose=FALSE, plot=FALSE)$minh
    if (verbose) message("'nb.h' not provided and set to ", nb.h, " (99% harmonic power)")
  }
  if (nb.h > q) {
    nb.h <- q  # should not be 1
    message("At least one outline has no more than ", q * 2,
        " coordinates. 'nb.h' has been set to ", q,
        " harmonics")
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
  res <- OutCoe(coe = coe, fac = Out$fac, method = "tfourier",norm = norm)
  res$cuts <- ncol(res$coe)
  return(res)
}

#' Inverse tangent angle Fourier transform
#'
#' \code{tfourier_i} uses the inverse tangent angle Fourier transformation to
#' calculate a shape, when given a list with Fourier coefficients, typically
#' obtained computed with \link{tfourier}.
#'
#' See \link{tfourier} for the mathematical background.
#'
#' @param tf a list with ao, an and bn components, typically as returned by
#' tfourier
#' @param nb.h \code{integer}. The number of harmonics to calculate/use
#' @param nb.pts \code{integer}. The number of points to calculate
#' @param force2close \code{logical}. Whether to force the outlines calculated
#' to close (see \link{coo_force2close}).
#' @param rescale \code{logical}. Whether to rescale the points calculated so
#' that their perimeter equals \code{perim}.
#' @param perim The perimeter length to rescale shapes.
#' @param thetao \code{numeric}. Radius angle to the reference (in radians)
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' \item{phi }{\code{vector} of interpolated changes on the tangent angle.}
#' \item{angle }{\code{vector} of position on the perimeter (in radians).}
#' @seealso \link{tfourier} for the reverse operation and also
#' \code{tfourier_shape}. \link{tfourier}. \link{l2m}, \link{coeff_split} may be useful.
#' @note Directly borrowed for Claude (2008), and called \code{ifourier2} there.
#' @references Zahn CT, Roskies RZ. 1972. Fourier Descriptors for Plane Closed
#' Curves. \emph{IEEE Transactions on Computers} \bold{C-21}: 269-281.
#'
#' Claude, J. (2008) \emph{Morphometrics with R}, Use R! series, Springer 316
#' pp.
#' @examples
#' data(bot)
#' tfourier(bot[1], 24)
#' tfourier_shape()
#' @export
tfourier_i <- function(tf, nb.h, nb.pts = 120, force2close = FALSE,
    rescale = TRUE, perim = 2 * pi, thetao = 0) {
    if (!all(c("an", "bn") %in% names(tf))) {
        stop("a list containing 'an' and 'bn' harmonic coefficients must be provided")
    }
    ao <- ifelse(is.null(tf$ao), 0, tf$ao)
    if (missing(thetao)) {
        thetao <- ifelse(is.null(tf$thetao), 0, tf$thetao)
    }
    an <- tf$an
    bn <- tf$bn
    if (missing(nb.h)) {
        nb.h <- length(an)
    }
    if (nb.h > length(an)) {
        nb.h <- length(an)
    }
    theta <- seq(0, 2 * pi, length = nb.pts)
    harm <- matrix(NA, nrow = nb.h, ncol = nb.pts)
    for (i in 1:nb.h) {
        harm[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i *
            theta)
    }
    phi <- (ao/2) + apply(harm, 2, sum)
    vect <- matrix(NA, 2, nb.pts)
    Z <- complex(modulus = (2 * pi)/nb.pts, argument = phi +
        theta + thetao)
    Z1 <- cumsum(Z)
    coo <- cbind(Re(Z1), Im(Z1))
    if (force2close) {
        coo <- coo_force2close(coo)
    }
    if (rescale) {
        if (missing(perim)) {
            perim <- ifelse(is.null(tf$perim), 2 * pi, tf$perim)
        }
        coo <- coo_scale(coo, coo_perim(coo)/perim)
    }
    if (!all(is.null(tf$x1) & is.null(tf$x1))) {
        coo <- coo_trans(coo, tf$x1, tf$y1)
    }
    # return(list(x=coo[, 1], y=coo[, 2], angle=theta, phi=phi))}
    colnames(coo) <- c("x", "y")
    return(coo)
}


#' Calculates and draws 'tfourier' shapes.
#'
#' \code{tfourier_shape} calculates a 'Fourier tangent angle shape' given
#' Fourier coefficients (see \code{Details}) or can generate some 'tfourier'
#' shapes.
#'
#' \code{tfourier_shape} can be used by specifying \code{nb.h} and
#' \code{alpha}. The coefficients are then sampled in an uniform distribution
#' \eqn{(-\pi ; \pi)} and this amplitude is then divided by
#' \eqn{harmonicrank^alpha}. If \code{alpha} is lower than 1, consecutive
#' coefficients will thus increase. See \link{tfourier} for the mathematical
#' background.
#'
#' @param an \code{numeric}. The \eqn{a_n} Fourier coefficients on which to
#' calculate a shape.
#' @param bn \code{numeric}. The \eqn{b_n} Fourier coefficients on which to
#' calculate a shape.
#' @param ao \code{ao} Harmonic coefficient.
#' @param nb.h \code{integer}. The number of harmonics to use.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @param alpha \code{numeric}. The power coefficient associated with the
#' (usually decreasing) amplitude of the Fourier coefficients (see
#' \bold{Details}).
#' @param plot \code{logical}. Whether to plot or not the shape.
#' @return A matrix of (x; y) coordinates.
#' @seealso \link{tfourier_i}, \link{tfourier}, link{tfourier}.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @examples
#' data(bot)
#' tf <- tfourier(bot[1], 24)
#' tfourier_shape(tf$an, tf$bn) # equivalent to rfourier_i(rf)
#' tfourier_shape()
#' tfourier_shape(nb.h=6, alpha=0.4, nb.pts=500)
#' panel(Out(a2l(replicate(100,
#' coo_force2close(tfourier_shape(nb.h=6, alpha=2, nb.pts=200, plot=FALSE)))))) # biological shapes
#' @export
tfourier_shape <- function(an, bn, ao = 0, nb.h, nb.pts = 80,
    alpha = 2, plot = TRUE) {
    if (missing(nb.h) & missing(an))
        nb.h <- 1
    if (missing(nb.h) & !missing(an))
        nb.h <- length(an)
    if (missing(an))
        an <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
    if (missing(bn))
        bn <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
    tf <- list(an = an, bn = bn, ao = ao)
    shp <- tfourier_i(tf, nb.h = nb.h, nb.pts = nb.pts)
    if (plot)
        coo_plot(shp)
    return(shp)
}

##### end tfourier
