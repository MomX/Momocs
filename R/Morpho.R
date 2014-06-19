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
# 1.2 Radius lengths Fourier analysis ==========================================
#' Calculates radii variation Fourier analysis.
#' 
#' \code{rfourier} computes radii variation Fourier analysis from a matrix or a
#' list of coordinates.
#' 
#' Given a closed outline, the radius \eqn{r}, taken as the distance from the
#' outline barycentre and a given point of the outline, can be expressed as a
#' periodic function of the angle \eqn{\theta}. Harmonics from \eqn{0} to
#' \eqn{k} approximate the function \eqn{r(\theta)}:
#' 
#' \deqn{r(\theta)= \frac{1}{2}a_0 + \sum\limits_{n=1}^{k}a_k\cos(w_k\theta +
#' b_k\sin(w_k\theta)} with: \deqn{ a_n =
#' \frac{2}{p}\sum\limits_{n=1}^{p}r_i\cos(n\theta_i) } \deqn{ b_n =
#' \frac{2}{p}\sum\limits_{n=1}^{p}r_i\sin(n\theta_i) } with \deqn{ a_0 =
#' \sqrt{\frac{2}{p}}\sum\limits_{n=1}^{p}r_i }
#' 
#' The \eqn{a_n} and \eqn{b_n} harmonic coefficients, extracted for every
#' individual shape, are then used for multivariate analyses.
#' @param coo A \code{list} or \code{matrix} of coordinates.
#' @param nb.h \code{integer}. The number of harmonics to calculate/use.
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform.
#' @param norm \code{logical}. Whether to scale the outlines so that the mean
#' length of the radii used equals 1.
#' @param verbose \code{logical}. Whether to display diagnosis messages.
#' @return A list with these components: \item{an }{\code{vector} of
#' \eqn{a_{1->n}} harmonic coefficients.} \item{bn }{\code{vector} of
#' \eqn{b_{1->n}} harmonic coefficients.} \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{r }{\code{vector} of radii lengths.}
#' @seealso \link{rfourier.i} for the inverse operation, \link{rfourier.shape}.
#' \link{efourier}, \link{tfourier} for the other members of the Fourier's
#' family.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @examples
#' data(bot)
#' coo <- coo.center(bot[1]) # centering is almost mandatory for rfourier family
#' coo.plot(coo)
#' rf  <- rfourier(coo, 12)
#' rf
#' rfi <- rfourier.i(rf)
#' coo.draw(rfi, border="red", col=NA)
#' @export
rfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, verbose=TRUE){
  coo <- coo.check(coo)
  if (missing(nb.h)) {
    nb.h <- 12
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo) | missing(nb.h)) {
    nb.h = floor(nrow(coo)/2)
    if (verbose){
      cat(" * 'nb.h' must be lower than half the number of points and has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)
    if (verbose){
      cat(" * 'nb.h' must be lower than half the number of points.\n",
          "* It has been set to", nb.h, "harmonics.\n")}}
  if (smooth.it!=0) { coo <- coo.smooth(coo, smooth.it)}
  if (norm) {
    coo   <- coo.scale(coo.center(coo))
    rsize <- mean(apply(coo, 1, function(x) sqrt(sum(x^2))))
    coo   <- coo.scale(coo, 1/rsize)}
  
  # from Claude
  p     <- nrow(coo)
  an    <- bn <- numeric(nb.h)
  Z     <- complex(real=coo[, 1], imaginary=coo[, 2])
  r     <- Mod(Z)
  angle <- Arg(Z)
  ao    <- 2*sum(r)/p
  for (i in 1:nb.h){
    an[i]<-(2/p)*sum(r * cos(i*angle))
    bn[i]<-(2/p)*sum(r * sin(i*angle))}
  list(an=an, bn=bn, ao=ao, r=r)}

#' Calculates inverse radii variation analysis.
#' 
#' \code{rfourier.i} uses the inverse radii variation transformation to
#' calculate a shape, when given a list with Fourier coefficients, typically
#' obtained computed with \link{rfourier}.
#' 
#' See \link{efourier} for the mathematical background.
#' 
#' @param rf A \code{list} with \code{ao}, \code{an} and \code{bn} components,
#' typically as returned by \code{rfourier}.
#' @param nb.h \code{integer}. The number of harmonics to calculate/use.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' \item{angle}{\code{vector} of angles used.} \item{r}{\code{vector} of radii
#' calculated.}
#' @seealso \link{efourier} for the reverse operation and also
#' \code{rfourier.shape}. \link{l2m}, \link{coeff.split} may be useful.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' coo <- coo.center(bot[1]) # centering is almost mandatory for rfourier family
#' coo.plot(coo)
#' rf  <- rfourier(coo, 12)
#' rf
#' rfi <- rfourier.i(rf)
#' coo.draw(rfi, border="red", col=NA)
#' 
#' @export
rfourier.i <- function(rf, nb.h, nb.pts=120) {
  if (!all(c("an", "bn") %in% names(rf))) {
    stop("a list containing 'an' and 'bn' harmonic coefficients 
         must be provided")}
  ao <- ifelse(is.null(rf$ao), 1, rf$ao)
  an <- rf$an
  bn <- rf$bn
  if (missing(nb.h)) {nb.h <- length(an)}
  if (nb.h > length(an)) {
    nb.h <- length(an)
    cat(" * nb.h cannot be higher than length(rf$an) and has been set to: ", nb.h)}
  theta <- seq(0, 2*pi, length=nb.pts)
  harm  <- matrix(NA, nrow=nb.h, ncol=nb.pts)
  for (i in 1:nb.h){
    harm[i, ]<- an[i]*cos(i*theta) + bn[i]*sin(i*theta)}
  r <- (ao/2) + apply(harm, 2, sum)
  Z <- complex(modulus=r, argument=theta)
  #list(x=Re(Z), y=Im(Z), angle=theta, r=r)}
  x <- Re(Z)
  y <- Im(Z)
  coo <- cbind(x, y)
  colnames(coo) <- c("x", "y")
  return(coo)}

#' Calculates and draw "rfourier" shapes.
#' 
#' \code{rfourier.shape} calculates a "Fourier radii variation shape" given
#' Fourier coefficients (see \code{Details}) or can generate some "rfourier"
#' shapes.
#' 
#' \code{rfourier.shape} can be used by specifying \code{nb.h} and
#' \code{alpha}. The coefficients are then sampled in an uniform distribution
#' \eqn{(-\pi ; \pi)} and this amplitude is then divided by
#' \eqn{harmonicrank^alpha}. If \code{alpha} is lower than 1, consecutive
#' coefficients will thus increase. See \link{rfourier} for the mathematical
#' background.
#' 
#' @param an \code{numeric}. The \eqn{a_n} Fourier coefficients on which to
#' calculate a shape.
#' @param bn \code{numeric}. The \eqn{b_n} Fourier coefficients on which to
#' calculate a shape.
#' @param nb.h \code{integer}. The number of harmonics to use.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @param alpha \code{numeric}. The power coefficient associated with the
#' (usually decreasing) amplitude of the Fourier coefficients (see
#' \bold{Details}).
#' @param plot \code{logical}. Whether to plot or not the shape.
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' @seealso \link{rfourier.i}.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords coreMorpho
#' @examples
#' data(bot)
#' rf <- rfourier(bot[1], 24)
#' rfourier.shape(rf$an, rf$bn) # equivalent to rfourier.i(rf)
#' rfourier.shape() # not very interesting
#' 
#' rfourier.shape(nb.h=12) # better
#' rfourier.shape(nb.h=6, alpha=0.4, nb.pts=500)
#' 
#' # Butterflies of the vignette' cover
#' panel(Out(a2l(replicate(100,
#' rfourier.shape(nb.h=6, alpha=0.4, nb.pts=200, plot=FALSE)))))
#' @export
rfourier.shape <- function(an, bn, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  rf  <- list(an=an, bn=bn, ao=0)
  shp <- rfourier.i(rf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

# 1.3 Tangent angle ============================================================
#' Calculates tangent angle Fourier analysis.
#' 
#' \code{tfourier} computes tangent angle Fourier analysis from a matrix or a
#' list of coordinates.
#' 
#' Given a closed outline which the outline has been scaled to \eqn{2\pi},
#' \eqn{\phi(t)} can be expressed as follows: \deqn{ \phi(t) = \theta(t) -
#' \theta(0) - t } where \eqn{t} is the distance along the outline,
#' \eqn{\theta(t)} the angle of the tangent vector at \eqn{t} and
#' \eqn{\theta(0)} the angle of the tangent vector taken for the first point.
#' It can be removed for normalizing the coefficients obtained. Two
#' coefficients per harmonics can be estimated as follow:
#' 
#' \deqn{ a_n = \frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\cos n \theta_i } \deqn{
#' b_n = \frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\sin n \theta_i } with \deqn{
#' a_0 = \sqrt{\frac{2}{p}}\sum\limits_{n=1}^{p}\phi(t) }
#' 
#' @param coo A list or matrix of coordinates
#' @param nb.h \code{integer}. The number of harmonics to calculate/use
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform
#' @param norm \code{logical}. Whether to scale and register new coordinates so
#' that the first point used is sent on the origin.
#' @param verbose \code{logical}. Whether to display diagnosis messages.
#' @return A list with these components: \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{an }{\code{vector} of \eqn{a_{1->n}} harmonic
#' coefficients.} \item{bn }{\code{vector} of \eqn{b_{1->n}} harmonic
#' coefficients.} \item{phi }{\code{vector} of variation of the tangent angle.}
#' \item{t }{\code{vector} of distance along the perimeter expressed in
#' radians.} \item{perimeter }{\code{numeric}. The perimeter of the outline.}
#' \item{thetao }{\code{numeric}. The first tangent angle.} \item{x1 }{The
#' x-coordinate of the first point.} \item{y1 }{The y-coordinate of the first
#' point.}
#' @seealso \link{efourier}, \link{rfourier} for the other members of the
#' Fourier's family.
#' @references Zahn CT, Roskies RZ. 1972. Fourier Descriptors for Plane Closed
#' Curves. \emph{IEEE Transactions on Computers} \bold{C-21}: 269-281.
#' 
#' Claude, J. (2008) \emph{Morphometrics with R}, Use R! series, Springer 316
#' pp.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' coo <- bot[1]
#' coo.plot(coo)
#' tf  <- tfourier(coo, 12)
#' tf
#' tfi <- tfourier.i(tf)
#' coo.draw(tfi, border="red", col=NA) # the outline is not closed...
#' coo.draw(tfourier.i(tf, force2close=TRUE), border="blue", col=NA) # we force it to close.
#' @export
tfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, verbose=TRUE){
  if (missing(nb.h)) {
    nb.h <- 12
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if (is.list(coo))   {coo <- l2m(coo)}
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo)) {
    nb.h = floor(nrow(coo)/2)
    if (verbose){
      cat(" * 'nb.h' must be lower than half the number of points and has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)
    if (verbose){
      cat(" * 'nb.h' must be lower than half the number of points.\n",
          "* It has been set to", nb.h, "harmonics.\n")}}
  if (smooth.it!=0) { coo <- coo.smooth(coo, smooth.it)}
  if (norm) {
    coo <- coo.scale(coo.center(coo))
    coo <- coo.trans(coo, -coo[1, 1], -coo[1, 2])}
  p <- nrow(coo)
  an <- bn <- numeric(nb.h)
  tangvect <- coo - rbind(coo[p,], coo[-p,])
  perim <- sum(sqrt(apply((tangvect)^2, 1, sum)))
  v0    <- coo[1,]-coo[p,]
  tet1   <- Arg(complex(real=tangvect[,1], imaginary = tangvect[,2]))
  tet0   <- tet1[1]
  t1     <- seq(0, 2*pi, length= (p+1))[1:p]
  phi    <- (tet1-tet0-t1)%%(2*pi)
  ao     <- 2*sum(phi)/p
  for (i in 1:nb.h){
    an[i]<- (2/p) * sum( phi * cos (i*t1))
    bn[i]<- (2/p) * sum( phi * sin (i*t1))}
  list(ao=ao, an=an, bn=bn, phi=phi, t=t1, perimeter=perim,
       thetao=tet0, x1=coo[1, 1], y1=coo[1, 2])}

#' Calculates inverse tangent angle Fourier analysis.
#' 
#' \code{tfourier.i} uses the inverse tangent angle Fourier transformation to
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
#' to close (see \link{coo.force2close}).
#' @param rescale \code{logical}. Whether to rescale the points calculated so
#' that their perimeter equals \code{perim}.
#' @param perim The perimeter length to rescale shapes.
#' @param thetao \code{numeric}. Radius angle to the reference (in radians)
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' \item{phi }{\code{vector} of interpolated changes on the tangent angle.}
#' \item{angle }{\code{vector} of position on the perimeter (in radians).}
#' @seealso \link{tfourier} for the reverse operation and also
#' \code{tfourier.shape}. \link{l2m}, \link{coeff.split} may be useful.
#' @references Zahn CT, Roskies RZ. 1972. Fourier Descriptors for Plane Closed
#' Curves. \emph{IEEE Transactions on Computers} \bold{C-21}: 269-281.
#' 
#' Claude, J. (2008) \emph{Morphometrics with R}, Use R! series, Springer 316
#' pp.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' tfourier(bot[1], 24)
#' tfourier.shape()
#' @export
tfourier.i<-function(tf, nb.h, nb.pts=120,
                     force2close=FALSE, rescale=TRUE, perim=2*pi, thetao=0){
  if (!all(c("an", "bn") %in% names(tf))) {
    stop("a list containing 'an' and 'bn' harmonic coefficients must be provided")}
  ao <- ifelse(is.null(tf$ao), 0, tf$ao)
  if (missing(thetao)) { thetao <- ifelse(is.null(tf$thetao), 0, tf$thetao) }
  an <- tf$an
  bn <- tf$bn
  if (missing(nb.h)) {nb.h <- length(an)}
  if (nb.h > length(an)) {
    nb.h <- length(an)
    #cat(" * nb.h cannot be higher than length(rf$an) and has been set to: ", nb.h)}
  }
    #if (missing(nb.pts)) {nb.pts=nb.h*2}
  theta <- seq(0, 2*pi, length=nb.pts)
  harm  <- matrix(NA, nrow=nb.h, ncol=nb.pts)
  for (i in 1:nb.h){
    harm[i,] <- an[i]*cos(i*theta) + bn[i]*sin(i*theta)}
  phi  <- (ao/2) + apply(harm, 2, sum)
  vect <- matrix(NA, 2, nb.pts)
  Z    <- complex(modulus=(2*pi)/nb.pts, argument=phi+theta+thetao)
  Z1   <- cumsum(Z)
  coo <- cbind(Re(Z1), Im(Z1))
  if (force2close) { coo <- coo.force2close(coo)}
  if (rescale)     {
    if(missing(perim)) {
      perim <- ifelse(is.null(tf$perim), 2*pi, tf$perim)}
    coo <- coo.scale(coo, coo.perim(coo)/perim) }
  if (!all(is.null(tf$x1) & is.null(tf$x1))) {
    coo <- coo.trans(coo, tf$x1, tf$y1)}
  #return(list(x=coo[, 1], y=coo[, 2], angle=theta, phi=phi))}
  colnames(coo) <- c("x", "y")
  return(coo)}
  
  
#' Calculates and draw "tfourier" shapes.
#' 
#' \code{tfourier.shape} calculates a "Fourier tangent angle shape" given
#' Fourier coefficients (see \code{Details}) or can generate some "tfourier"
#' shapes.
#' 
#' \code{tfourier.shape} can be used by specifying \code{nb.h} and
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
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' @seealso \link{tfourier.i}.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords coreMorpho
#' @examples
#' 
#' data(bot)
#' tf <- tfourier(bot[1], 24)
#' tfourier.shape(tf$an, tf$bn) # equivalent to rfourier.i(rf)
#' tfourier.shape()
#' tfourier.shape(nb.h=6, alpha=0.4, nb.pts=500)
#' panel(Out(a2l(replicate(100,
#' coo.force2close(tfourier.shape(nb.h=6, alpha=2, nb.pts=200, plot=FALSE)))))) # biological shapes
#' @export
tfourier.shape <- function(an, bn, ao=0, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  tf  <- list(an=an, bn=bn, ao=ao)
  shp <- tfourier.i(tf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

#' Calculates harmonic power given a list from e/t/rfourier
#' 
#' Given a list with \code{an, bn (and eventually cn and dn)}, returns the
#' harmonic power.
#' 
#' @param xf A list with an, bn (and cn, dn) components, typically from a
#' e/r/tfourier passed on coo.
#' @return Returns a \code{vector} of harmonic power
#' @keywords calibration
#' @examples
#' 
#' data(bot)
#' ef <- efourier(bot[1], 24)
#' rf <- efourier(bot[1], 24)
#' harm.pow(ef)
#' harm.pow(rf)
#' 
#' plot(cumsum(harm.pow(ef)[-1]), type="o",
#'   main="Cumulated harmonic power without the first harmonic",
#'   ylab="Cumulated harmonic power", xlab="Harmonic rank")
#' 
#' @export
harm.pow <- function(xf){
  if (is.list(xf)) {
    if (all(c("an", "bn", "cn", "dn") %in% names(xf))) {
      return((xf$an^2 + xf$bn^2 + xf$cn^2 + xf$dn^2)/2)
    } else {
      if (all(c("an", "bn") %in% names(xf))) {
        return((xf$an^2 + xf$bn^2)/2)}
    }
  } else {
    stop("a list containing 'an', 'bn' ('cn', 'dn') harmonic coefficients must be provided")}}
# 1.4 Utilities ================================================================

#' Helps to select a given number of harmonics from a numerical vector.
#' 
#' \code{coeff.sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{$coe} slot of \code{Coe}-objects in
#' that way: \deqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{eFourier} and
#' \link{efourier}) while \deqn{C_n and D_n} harmonic are absent for radii
#' variation and tangent angle approaches (see \link{rfourier} and
#' \link{tfourier} respectively). . This function is used internally but might
#' be of interest elwewhere.
#' 
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff.sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff.split} returns a named list
#' of coordinates.
#' @export
coeff.sel <- function(retain=8, drop=0, nb.h=32, cph=4){
  cs <- numeric()
  for (i in 1:cph) {
    cs <- c(cs, (1+drop):retain + nb.h*(i-1))}
  return(cs)}

#' Converts a numerical description of harmonic coefficients to a named list.
#' 
#' \code{coeff.split} returns a named list of coordinates from a vector of
#' harmonic coefficients. For instance, harmonic coefficients are arranged in
#' the \code{$coe} slot of \code{Coe}-objects in that way: \deqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{eFourier} and \link{efourier}) while \deqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#' 
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @examples
#' coeff.split(1:128, nb.h=32, cph=4) # efourier
#' coeff.split(1:64, nb.h=32, cph=2)  # t/r fourier
#' @export
coeff.split <- function(cs, nb.h=8, cph=4){
  if (missing(nb.h)) {nb.h <- length(cs)/cph }
  cp <- list()
  for (i in 1:cph) {
    cp[[i]] <- cs[1:nb.h + (i-1)*nb.h]
  }
  names(cp) <- paste(letters[1:cph], "n", sep="")
  return(cp)}
# 2. Open outlines -------------------------------------------------------------
# 2.1 Polynomials --------------------------------------------------------------
#' Calculate polynomial fit on open outlines
#' 
#' Returns a model obtained with a lm with polynomial coefficients, orthogonal 
#' or not, from a matrix of (x; y) coordinates (assumed to be normalized).
#' @param coo a matrix or a list of (x; y) coefficients
#' @param n the degree of the polynomial to fit (the intercept is returned)
#' @param ortho logical wheter to calculate orthogonal coefficients
#' @return a lm model object
#' @keywords morphoCore
#' @export
polynomials <- function(coo, n, ortho=TRUE){
  coo <- coo.check(coo)
  if (missing(n)) {
    n <- 5
    cat(" * 'n' not provided and set to", n, ".\n")}
  x <- poly(coo[, 1], degree=n, raw=!ortho)
  mod <- lm(coo[, 2] ~ x)
  r2 <- summary(mod)$r.squared
  return(list(coeff=mod$coefficients, ortho=ortho, 
              baseline1=coo[1, ], baseline2=coo[nrow(coo), ], r2=r2, mod=mod))}

#' Calculate shape from a polynomial model
#' 
#' Returns a matrix of (x; y) coordinates when passed with a model obtained with
#' \link{polynomials}.
#' @param pol a pol list such as created by \link{polynomials}.
#' @param nb.pts the number of points to predict. By default (and can't be higher)
#' the number of points in the original shape.
#' @param reregister logical whether to reregister the shape with the original baseline.
#' @return a matrix of (x; y) coordinates.
#' @keywords morphoCore
#' @examples
#' data(olea)
#' o <- olea[5]
#' coo.plot(o)
#' for (i in 2:7){
#' x <- polynomials.i(polynomials(o, i, ortho=TRUE))
#' coo.draw(x, border=col.summer(7)[i], points=FALSE)  }
#' @export
polynomials.i <- function(pol, nb.pts=120, reregister=TRUE){
  x.new  <- seq(pol$baseline1[1], pol$baseline2[1], length=nb.pts)
  degree <- length(pol$coeff)-1
  if (pol$ortho) {
    x.poly <- poly(x.new, degree=degree)
    y.pred <- predict(x.poly, x.new)
    y.new  <- pol$coeff[1] + apply(.mprod(m=y.pred, s=pol$coeff[-1]), 1, sum) 
  } else {
    y.pred <- numeric(nb.pts)
    for (i in 1:degree){
      y.pred <- y.pred + (x.new^i * pol$coeff[i+1]) }
    y.new <- y.pred + pol$coeff[1]}
  coo <- cbind(x.new, y.new)
  if (reregister) {
    coo <- coo.baseline(coo, 1, nrow(coo), t1=pol$baseline1, t2=pol$baseline2)}
  colnames(coo) <- c("x", "y")
  return(coo)}
# 2.2 Cubic splines -------------------------------------------------------
# 2.3 Bezier splines -------------------------------------------------------
#' Calculates Bezier coefficients from a shape
#' #todo
#' @param coo a matrix or a list of (x; y) coordinates
#' @param n the degree
#' @return a list with J and B
#' @keywords morphoCore
#' @export
bezier <- function(coo, n){
  coo <- coo.check(coo)
  if (missing(n)) n <- nrow(coo)
  p <- nrow(coo)
  if (n != p) { n <- n + 1 }
  coo1 <- coo / coo.perim.cum(coo)[p]
  t1   <- 1 - coo.perim.cum(coo1)
  J <- matrix(NA, p, p)
  for (i in 1:p){
    for (j in 1:p){
      J[i, j] <- (factorial(p-1) / (factorial(j-1) * factorial(p-j))) *
        (((1-t1[i])^(j-1)) * t1[i]^(p-j))}}
  B <- ginv(t(J[, 1:n]) %*% J[,1:n])%*%(t(J[,1:n])) %*% coo
  coo <- J[, 1:n] %*% B
  B <- ginv(t(J[,1:n])%*%J[,1:n])%*%(t(J[,1:n])) %*% coo
  list(J=J, B=B)}

#' Calculates a shape from Bezier coefficients
#' 
#' todo
#' @param B a matrix
#' @param nb.pts the number of points to return for drawing he shape
#' @return a matrix of (x; y) coordinates
#' @keywords morphoCore
#' @export
bezier.i <-function(B, nb.pts=120){
  x   <- y <- numeric(nb.pts)
  n    <- nrow(B)-1
  t1   <- seq(0, 1, length=nb.pts)
  coef <- choose(n, k=0:n)
  b1   <- 0:n
  b2   <- n:0
  for (j in 1:nb.pts){
    vectx <- vecty <- NA
    for (i in 1:(n+1)){
      vectx[i] <- B[i,1] * coef[i]*t1[j]^b1[i]*(1-t1[j])^b2[i]
      vecty[i] <- B[i,2] * coef[i]*t1[j]^b1[i]*(1-t1[j])^b2[i] }
    x[j] <- sum(vectx)
    y[j] <- sum(vecty)}
  coo <- cbind(x, y)
  return(coo)}
# 3. Landmarks ------------------------------------------------------------

#' Full Procrustes alignment between two shapes
#' 
#' Directly borrowed from Claude 2008 (fPsup)
#' @param coo1 Configuration matrix to be superimposed onto the centered preshape of coo2.
#' @param coo2 Reference configuration matrix.
#' @return a list with components
#' \enumerate{
#' \item coo1: Superimposed centered preshape of coo1 onto the centered preshape of coo2
#' \item coo2: Centered preshape of coo2
#' \item rotation: Rotation matrix
#' \item scale: Scale parameter
#' \item DF: Full Procrustes distance between coo1 and coo2.
#' }
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @export
fProcrustes <- function(coo1, coo2){
  # directly borrowed from Claude
  k <- ncol(coo1)
  Z1      <- coo.center(coo.scale(coo1))  
  Z2      <- coo.center(coo.scale(coo2))  
  sv      <- svd(t(Z2) %*% Z1)
  U       <- sv$v
  V       <- sv$u
  Delt    <- sv$d
  sig     <- sign(det(t(Z2)%*%Z1))
  Delt[k] <- sig*abs(Delt[k])
  V[,k]   <- sig * V[,k]
  Gam<-U%*%t(V)
  beta<-sum(Delt)
  DF <- ifelse((1-beta^2)>0, sqrt(1-beta^2), NA)
  list(coo1=beta*Z1%*%Gam, coo2=Z2,
       rotation=Gam, scale=beta, DF=DF)}

#' Full Generalized Procrustes alignment between shapes
#' 
#' Directly borrowed from Claude 2008 (fgpa2).
#' @param x an array, a list of configurations, or an Out or on Ldk object
#' @param tol numeric when to stop iterations
#' @param verbose logical whether to print outputs
#' @return a list with components
#' \enumerate{
#' \item rotated: Array of superimposed configurations.
#' \item iterationnumber: Number of iterations.
#' \item Q: Convergence criterion.
#' \item Qi: Full list of Q.
#' \item Qd: Difference between succesive Q.
#' \item interproc.dist: Minimal sum of squared norms of pairwise differences between
#' all shapes in the superimposed sample.
#' \item mshape: Mean shape configuration.
#' \item cent.size: Vector of centroid sizes.
#' } or and Ldk or and Out object.
#' @note Slightly less optimized than procGPA in the shapes package (~20% on my machine).
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @export
fgProcrustes <- function(x, tol, verbose){UseMethod("fgProcrustes")}

fgProcrustes.default <- function(x, tol=1e-5, verbose=TRUE){
  A <- x
  A <- ldk.check(A)
  # directly borrowed from Claude
  p <- dim(A)[1]
  k <- dim(A)[2]
  n <- dim(A)[3]
  temp2 <- temp1 <- array(NA, dim=c(p, k, n))
  Siz   <- numeric(n)
  for (i in 1:n){
    Siz[i]      <- coo.centsize(A[,,i])
    temp1[,,i]  <- coo.center(coo.scale(A[,,i]))}
  iter<-0; sf<-NA
  M   <- temp1[,,1]
  for (i in 1:n){ temp1[,,i]<-fProcrustes(temp1[,,i], M)$coo1 }
  M   <- mshape(temp1)
  Qm1 <- dist(t(matrix(temp1,k*p,n)))
  Qd <- Qi <- Q   <- sum(Qm1); iter<-0
  sc  <- rep(1,n)
  while (abs(Q) > tol){
    for (i in 1:n){
      Z1         <- temp1[,,i]
      sv         <- svd(t(M)%*%Z1)
      U          <- sv$v
      V          <- sv$u
      Delt       <- sv$d
      sig        <- sign(det(t(Z1)%*%M))
      Delt[k]    <- sig*abs(Delt[k])
      V[,k]      <- sig*V[,k]
      phi        <- U%*%t(V)
      beta       <- sum(Delt)
      temp1[,,i] <- X <- sc[i]*Z1%*%phi}
    M <- mshape(temp1)
    for (i in 1:n){
      sf[i] <- sqrt(sum(diag(temp1[,,i]%*%t(M)))
                    / (sum(diag(M%*%t(M)))
                       *  sum(diag(temp1[,,i]%*%t(temp1[,,i])))))
      temp2[,,i] <- sf[i]*temp1[,,i]}
    M     <- mshape(temp2)
    sc    <- sf*sc
    Qm2   <- dist(t(matrix(temp2,k*p,n)))
    Qd[iter] <- Q     <- sum(Qm1)-sum(Qm2)
    Qm1   <- Qm2
    Qi[iter] <- sum(Qm2)
    iter  <- iter+1
    if (verbose) { 
      cat("iteration: ", iter, "\tgain:", signif(abs(Q), 5), "\n")}
    temp1 <- temp2 }
  list(rotated=temp2, iterationnumber=iter,
       Q=Q, Qi=Qi, Qd=Qd, intereuclidean.dist=Qm2,
       mshape=coo.centsize(mshape(temp2)), cent.size=Siz)}

#' @export
fgProcrustes.Out <- function(x, tol=1e-10, verbose=TRUE){
  Coo <- x
  # if no $ldk defined, we convert Out into a Ldk and then perform the fgProcrustes
  # and return back an Out object.
  if (length(Coo$ldk)==0) {
    cat(" * No landmarks defined in $ldk, so trying to work on $coo directly.\n")
    Coo2 <- Ldk(Coo)
    Coo2 <- fgProcrustes(Coo2, tol=tol, verbose=verbose)
    Coo2 <- Out(Coo2)
    return(Coo2)}
  Coo2 <- coo.center(coo.scale(Coo))
  ref  <- getLandmarks(Coo2)
  tar <- fgProcrustes(ref, tol=tol, verbose=verbose)$rotated
  # would benefit to be handled by coo.baseline ?
  for (i in 1:length(Coo2)) {
    tari <- tar[, , i]
    refi <- ref[, , i]
    t1x <- tari[1, 1]
    t1y <- tari[1, 2]
    t2x <- tari[2, 1]
    t2y <- tari[2, 2]
    r1x <- refi[1, 1]
    r1y <- refi[1, 2]
    r2x <- refi[2, 1]
    r2y <- refi[2, 2]
    # translation
    t <- tari[1, ] - refi[1, ]
    refi <- coo.trans(refi, t[1], t[2])
    # rotation  
    tx <- t2x - t1x
    ty <- t2y - t1y
    rx <- r2x - r1x
    ry <- r2y - r1y
    vi <- vecs.param(rx, ry, tx, ty)
    coo.i <- Coo2$coo[[i]]
    coo.i <- coo.trans(coo.i, t[1]-t1x, t[2]-t1y)
    coo.i <- coo.i / vi$r.norms
    coo.i <- coo.rotate(coo.i, -vi$d.angle)
    coo.i <- coo.trans(coo.i, t1x, t1y)
    Coo2$coo[[i]] <- coo.i
  }
  return(Coo2)}

#' @export
fgProcrustes.Opn <- fgProcrustes.Out

#' @export
fgProcrustes.Ldk <- function(x, tol=1e-10, verbose=TRUE){
  Coo <- x
  Coo2 <- Coo
  ref <- l2a(Coo2$coo)
  tar <- fgProcrustes(ref, tol=tol, verbose=verbose)$rotated
  Coo2$coo <- a2l(tar)
  Coo2$coe <- a2m(l2a(Coo2$coo))
  Coo2$method <- "gProcrustes"
  names(Coo2$coo) <- names(Coo$coo)
  class(Coo2) <- c(class(Coo2), "LdkCoe", "Coe")
  return(Coo2)}

#' Partial Procrustes alignment between two shapes
#' 
#' Directly borrowed from Claude 2008 (pPsup).
#' @param coo1 Configuration matrix to be superimposed onto the centered preshape of coo2.
#' @param coo2 Reference configuration matrix.
#' @return a list with components
#' \enumerate{
#' \item coo1: Superimposed centered preshape of coo1 onto the centered preshape of coo2
#' \item coo2: Centered preshape of coo2
#' \item rotation: Rotation matrix
#' \item DP: Partial Procrustes distance between coo1 and coo2.
#' \item rho: Trigonometric Procrustes distance.
#' }
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @export
pProcrustes <- function(coo1, coo2){
  # directly borrowed from Claude
  k <- ncol(coo1)
  Z1      <- coo.center(coo.scale(coo1))  
  Z2      <- coo.center(coo.scale(coo2))  
  sv      <- svd(t(Z2) %*% Z1)
  U       <- sv$v
  V       <- sv$u
  Delt    <- sv$d
  sig     <- sign(det(t(Z2)%*%Z1))
  Delt[k] <- sig*abs(Delt[k])
  V[,k]   <- sig * V[,k]
  Gam<-U%*%t(V)
  beta<-sum(Delt)
  list(coo1=Z1%*%Gam, coo2=Z2,
       rotation=Gam, DP=sqrt(sum(edm(Z1%*%Gam, Z2)^2)), rho=acos(beta))}


#' Performs a general Procrustes alignment on Opn and Out objects
#' 
#' It relies on the $ldk slot and on the procGPA function in the shapes package.
#' @param Coo a Coo object (only Out so far)
#' @param ... additional arguments to be passed to procGPA
#' @return a procGPA-aligned Coo object
#' @seealso \link{defLandmarks} to define landmarks on your shapes and 
#' \link{coo.bookstein} for another "alignment" approach. \link{procGPA} for the
#' core function. Thanks to Dryden et al. !
#' @keywords Out Opn Ldk
#' @examples
#' \dontrun{
#' data(hearts)
#' stack(hearts) # raw hearts
#' hearts2 <- gProcrustes(hearts)
#' stack(hearts2) #procGPA-aligned hearts
#' hearts3 <- coo.bookstein(hearts, 2, 4)
#' stack(hearts3) # bookstein baseline
#' hearts4 <- coo.slide(hearts3, 2)
#' stack(hearts4) #bookstein baseline + first point on the second landmarks
#' }
#' @export

