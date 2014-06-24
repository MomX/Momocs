##todo: ventiler dans Out/Opn?
# Core functions for modern morphometrics
# 1. Outline functions ---------------------------------------------------------

# Mainly due to Julien Claude with some wrapping
# 1.1 Elliptical Fourier Analysis ==============================================
# Core functions for modern morphometrics
# 1. Outline functions ---------------------------------------------------------

# Mainly due to Julien Claude with some wrapping
# 1.1 Elliptical Fourier Analysis ==============================================

#' Calculates elliptical Fourier analysis.
#'
#' \code{efourier} computes elliptical Fourier analysis from a matrix or a list
#' of coordinates.
#'
#' These functions and their mathematical background detailed below are here
#' detailed to ease their use in new methods but are used internally by methods
#' on \code{Out}-objects.
#'
#' Elliptic Fourier analysis and normalization are calculated as follows. Let
#' \eqn{T} be the perimeter of a given closed outline, here considered as the
#' period of the signal. One sets \eqn{\omega = 2\pi/T} to be the pulse. Then,
#' the curvilinear abscissa, \eqn{t} varies from \eqn{0} to \eqn{T}. One can
#' express \eqn{x(t)} and \eqn{y(t)} as: \deqn{ x(t) =
#' \frac{a_0}{2}+\sum\limits_{n=1}^{+\infty} a_n\cos n\omega t + b_n\sin
#' n\omega t } with \deqn{ a_n = \frac{2}{T}+ \int\limits_{0}^{T} x(t)\cos
#' (n\omega t) \mathrm{d} t } \deqn{ b_n = \frac{2}{T}+ \int\limits_{0}^{T}
#' x(t)\sin (n\omega t) \mathrm{d} t }
#'
#' similarly, \deqn{ y(t) = \frac{c_0}{2}+\sum\limits_{n=1}^{+\infty} c_n\cos
#' n\omega t + d_n\sin n\omega t } with \deqn{ c_n = \frac{2}{T}+
#' \int\limits_{0}^{T} y(t)\cos (n\omega t) \mathrm{d} t } \deqn{d_n =
#' \frac{2}{T}+ \int\limits_{0}^{T} y(t)\sin (n\omega t) \mathrm{d} t }
#'
#' Since the outline contains a \eqn{k} finite number of points, one can
#' therefore calculate discrete estimators for every harmonic coefficient of
#' the \eqn{n^{th}} harmonics: \deqn{
#' a_n=\frac{T}{2\pi^2n^2}\sum\limits_{p=1}^k \frac{\Delta x_p}{\Delta
#' t_p}(\cos\frac{2\pi nt_p}{T}-\cos\frac{2\pi nt_{p-1}}{T}) } \deqn{
#' b_n=\frac{T}{2\pi^2n^2}\sum\limits_{p=1}^k \frac{\Delta x_p}{\Delta
#' t_p}(\sin\frac{2\pi nt_p}{T}-\sin\frac{2\pi nt_{p-1}}{T}) }
#'
#' \eqn{\Delta x_1=x_1-x_k} and \eqn{c_n} and \eqn{d_n} are calculated
#' similarly. \eqn{a_0} and \eqn{c_0} correspond to the estimate of the
#' coordinates of the centroid of original outline and are estimated by: \deqn{
#' a_0=\frac{2}{T}\sum\limits_{i=1}^p x_i } and \deqn{
#' c_0=\frac{2}{T}\sum\limits_{i=1}^p y_i }
#'
#' Intuitively, for all positive integers \eqn{n}, the sum of a cosine curve
#' and a sine curve represent the \eqn{n^{th}} harmonic content of the \eqn{x}
#' and \eqn{y} projections of the \eqn{k}-edged polygon, and for any \eqn{n},
#' these two curves define an ellipse in the plane. Ferson and colleagues
#' noticed that in the "time" it takes the \eqn{n^{th}} harmonic to traverse
#' its ellipse \eqn{n} times, the \eqn{(n+1)^{th}} harmonic has traversed its
#' own ellipse \eqn{n+1} times. The reconstruction of the original polygon is
#' done by vector adding these ellipses for all harmonics, which echoes
#' astronomical Ptolemy's epicycles (see \link{Ptolemy}), and the
#' reconstruction obtained from \eqn{N} harmonics is the best possible fit in a
#' least-squares sense.
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param nb.h \code{integer}. The number of harmonics to use
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform.
#' @param verbose \code{logical}. Whether to print or not diagnosis messages.
#' @return A list with these components: \item{an }{\code{vector} of
#' \eqn{a_{1->n}} harmonic coefficients.} \item{bn }{\code{vector} of
#' \eqn{b_{1->n}} harmonic coefficients.} \item{cn }{\code{vector} of
#' \eqn{c_{1->n}} harmonic coefficients.} \item{dn }{\code{vector} of
#' \eqn{d_{1->n}} harmonic coefficients.} \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{co }{\code{co} Harmonic coefficient.}
#' @seealso \link{efourier.i} for the reverse operation and \link{eFourier} the
#' method for \code{Out} objects. \link{Ptolemy} for an implementation of the
#' Ptolemaic ellipses. \link{rfourier}, \link{tfourier} for the other members
#' of the Fourier's family.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#'
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords coreMorpho
#' @examples
#'
#' data(bot)
#' coo <- bot[1]
#' coo.plot(coo)
#' ef <- efourier(coo, 12)
#' ef
#' efi <- efourier.i(ef)
#' coo.draw(efi, border="red", col=NA)
#' @export
efourier <- function (coo, nb.h, smooth.it = 0, verbose = TRUE) {
  coo <- coo.check(coo)
  if (is.closed(coo)) coo <- coo.unclose(coo)
  nr <- nrow(coo)
  if (missing(nb.h)) {
    nb.h <- 32
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if(nb.h * 2 > nr) {
    nb.h = floor(nr/2)
    if (verbose){
      cat(" * 'nb.h' must be lower than half the number of points.\n",
          "* It has been set to", nb.h, "harmonics.\n")}}
  if (nb.h == -1) {
    nb.h = floor(nr/2)
    if (verbose){
      cat(" * The number of harmonics used has been set to: ", nb.h)}}
  if (smooth.it != 0) { coo <- coo.smooth(coo, smooth.it) }
  Dx <- coo[, 1] - coo[, 1][c(nr, (1:(nr - 1)))] # there was a bug there. check from claude? #todo
  Dy <- coo[, 2] - coo[, 2][c(nr, (1:(nr - 1)))]
  Dt <- sqrt(Dx^2 + Dy^2)
  Dt[Dt<1e-10] <- 1e-10 # to avoid Nan
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
  return(list(an = an, bn = bn, cn = cn, dn = dn, ao = ao, co = co))}

#' Calculates inverse elliptical Fourier analysis.
#' 
#' \code{efourier.i} uses the inverse elliptical Fourier transformation to
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
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' @seealso \link{efourier} for the reverse operation. \link{l2m},
#' \link{coeff.split} may be useful.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' 
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' coo <- bot[1]
#' coo.plot(coo)
#' ef  <- efourier(coo, 12)
#' ef
#' efi <- efourier.i(ef)
#' coo.draw(efi, border="red", col=NA)
#' @export
efourier.i <- function(ef, nb.h, nb.pts = 120) {
  #if (any(names(ef) != c("an", "bn", "cn", "dn"))) {
  #  stop("a list containing 'an', 'bn', 'cn' and 'dn' harmonic coefficients must be provided")}
  if (is.null(ef$ao)) ef$ao <- 0
  if (is.null(ef$co)) ef$co <- 0
  an <- ef$an
  bn <- ef$bn
  cn <- ef$cn
  dn <- ef$dn
  ao <- ef$ao
  co <- ef$co
  if (missing(nb.h)) { nb.h <- length(an) }
  theta <- seq(0, 2 * pi, length = nb.pts + 1)[-(nb.pts + 1)]
  hx <- matrix(NA, nb.h, nb.pts)
  hy <- matrix(NA, nb.h, nb.pts)
  for (i in 1:nb.h) {
    hx[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i * theta)
    hy[i, ] <- cn[i] * cos(i * theta) + dn[i] * sin(i * theta)}
  x <- (ao/2) + apply(hx, 2, sum)
  y <- (co/2) + apply(hy, 2, sum)
  coo <- cbind(x, y)
  colnames(coo) <- c("x", "y")
  return(coo)}

#' Normalizes harmonic coefficients.
#' 
#' \code{efourier.norm} normalizes Fourier coefficients for rotation,
#' tranlation, size and orientation of the first ellipse.
#' 
#' See \link{efourier} for the mathematical background of the normalization.
#' Other approaches implemented in SHAPE are possible such as manually editing
#' or using the longest radius. They will be implemented in further Momocs
#' versions.
#' 
#' @param ef \code{list}. A list containing \eqn{a_n}, \eqn{b_n}, \eqn{c_n} and
#' \eqn{d_n} Fourier coefficients, such as returned by \code{efourier}.
#' @param start \code{logical}. Whether to conserve the position of the first
#' point of the outline.
#' @return A list with following components: \item{A }{\code{vector} of
#' \code{numeric} \eqn{A_{1->n}} \emph{normalized} harmonic coefficients.}
#' \item{B }{\code{vector} of \code{numeric} \eqn{B_{1->n}} \emph{normalized}
#' harmonic coefficients.} \item{C }{\code{vector} of \code{numeric}
#' \eqn{C_{1->n}} \emph{normalized} harmonic coefficients.} \item{D
#' }{\code{vector} of \code{numeric} \eqn{D_{1->n}} \emph{normalized} harmonic
#' coefficients.} \item{size }{Magnitude of the semi-major axis of the first
#' fitting ellipse.} \item{theta }{Angle, in radians, between the starting
#' point and the semi-major axis of the first fitting ellipse.} \item{psi
#' }{Orientation of the first fitting ellipse.} \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{co }{\code{co} Harmonic coefficient.} \item{lnef }{A
#' \code{list} with A, B, C and D concatenated in a \code{vector} that may be
#' convenient for some uses.}
#' @seealso \link{efourier} and \link{efourier.i}. Also \link{eFourier} for
#' normalizing harmonic coefficients when calculating it for \code{Out}
#' objects.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' 
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' q <- efourier(bot[1], 24)
#' efourier.i(q) # equivalent to efourier.shape(q$an, q$bn, q$cn, q$dn)
#' efourier.norm(q)
#' efourier.shape(nb.h=5, alpha=1.2)
#' efourier.shape(nb.h=12, alpha=0.9)
#' @export
efourier.norm <- function(ef, start = FALSE) {
  A1 <- ef$an[1]
  B1 <- ef$bn[1]
  C1 <- ef$cn[1]
  D1 <- ef$dn[1]
  nb.h <- length(ef$an)
  theta      <- 0.5 * atan(2 * (A1 * B1 + C1 * D1)/(A1^2 + C1^2 - B1^2 - D1^2)) %% pi
  phaseshift <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  M2 <- matrix(c(A1, C1, B1, D1), 2, 2) %*% phaseshift
  v <- apply(M2^2, 2, sum)
  if (v[1] < v[2]) {theta <- theta + pi/2}
  theta <- (theta + pi/2)%%pi - pi/2
  Aa <- A1*cos(theta) + B1*sin(theta)
  Cc <- C1*cos(theta) + D1*sin(theta)
  scale <- sqrt(Aa^2 + Cc^2)
  psi   <- atan(Cc/Aa)%%pi
  if (Aa<0){psi<-psi+pi}
  size  <- 1/scale
  rotation <- matrix(c(cos(psi), -sin(psi), sin(psi), cos(psi)), 2, 2)
  A <- B <- C <- D <- numeric(nb.h)
  if (start) {theta <- 0}
  for (i in 1:nb.h) {
    mat <- size * rotation %*%
      matrix(c(ef$an[i], ef$cn[i], ef$bn[i], ef$dn[i]), 2, 2) %*%
      matrix(c(cos(i*theta), sin(i*theta), -sin(i*theta), cos(i*theta)), 2, 2)
    A[i] <- mat[1, 1]
    B[i] <- mat[1, 2]
    C[i] <- mat[2, 1]
    D[i] <- mat[2, 2]
    lnef <- c(A[i], B[i], C[i], D[i])}
  list(A = A, B = B, C = C, D = D, size = scale, theta = theta, 
       psi = psi, ao = ef$ao, co = ef$co, lnef = lnef)}

#' Calculates and draw "efourier" shapes.
#' 
#' \code{efourier.shape} calculates a "Fourier elliptical shape" given Fourier
#' coefficients (see \code{Details}) or can generate some "efourier" shapes.
#' 
#' \code{efourier.shape} can be used by specifying \code{nb.h} and
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
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' @seealso \link{efourier.i}.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' 
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' ef <- efourier(bot[1], 24)
#' efourier.shape(ef$an, ef$bn, ef$cn, ef$dn) # equivalent to efourier.i(ef)
#' efourier.shape() # is autonomous
#' 
#' panel(Out(a2l(replicate(100, 
#' efourier.shape(nb.h=6, alpha=2.5, plot=FALSE))))) # Bubble family
#' @export
efourier.shape <- function(an, bn, cn, dn, nb.h, nb.pts=60, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 3
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(cn)) cn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(dn)) dn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  ef  <- list(an=an, bn=bn, cn=cn, dn=dn, ao=0, co=0)
  shp <- efourier.i(ef, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

#' Dilates shapes based on elliptical Fourier decomposition.
#' 
#'  Calculates dilated and eroded shapes based on elliptical
#' Fourier decomposition \emph{i.e.} taking into account the shape as a whole.
#' Lists created by \code{efourier} objects can be passed to \code{ef.amplify}
#' @export ef.amplify 
#' @param ef \code{list}. A list containing \eqn{a_n}, \eqn{b_n}, \eqn{c_n} and
#' \eqn{d_n} Fourier coefficients, such as returned by \code{efourier}.
#' @param amp A vector of \code{numeric}. If \code{amp} is of length 4, the
#' value specify the multiplication factor for \eqn{a_1}, \eqn{b_1}, \eqn{c_1}
#' and \eqn{d_1} ; if only one value is provided, then the multiplication
#' factor will be the same for the four coefficients \eqn{abcd_1}.
#' @return \code{ef.amplify} returns the \code{ef} provided but with
#' "amplified" coefficients for the first harmonics.
#' @seealso \link{efourier} for a description of the elliptical Fourier
#' analysis and \link{Ptolemy} for an illustration of the first
#' ellipse/harmonic defining the shape "amplitude".
#' @keywords coreMorpho
#' @export
ef.amplify <- function(ef, amp=rep(0.5, 4)){
  ef$an <- ef$an*amp[1]
  ef$bn <- ef$bn*amp[2]
  ef$cn <- ef$cn*amp[3]
  ef$dn <- ef$dn*amp[4]
  return(ef)}