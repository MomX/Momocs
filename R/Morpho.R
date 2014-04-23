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
#' @export 
#' @usage efourier(coo, nb.h, smooth.it = 0, verbose = TRUE)
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
efourier <- function (coo, nb.h, smooth.it = 0, verbose = TRUE) {
  coo <- coo.check(coo)
  if (is.closed(coo)) coo <- coo.unclose(coo)
  nr <- nrow(coo)
  if (missing(nb.h)) {
    nb.h <- 32
    warning(paste(" * 'nb.h' not provided and set to", nb.h))}
  if(nb.h * 2 > nr) {
    nb.h = floor(nr/2) - 1
    if (verbose){
      warning(" * 'nb.h' must be lower than half the number of points and has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nr/2) - 1 # should not be -1 #todo
    if (verbose){
      cat(" * The number of harmonics used has been set to: ", nb.h)}}
  if (smooth.it != 0) { coo <- coo.smooth(coo, smooth.it) }
  Dx <- coo[, 1] - coo[, 1][c(nr, (1:(nr - 1)))] # there was a bug there. check from claude? #todo
  Dy <- coo[, 2] - coo[, 2][c(nr, (1:(nr - 1)))]
  Dt <- sqrt(Dx^2 + Dy^2)
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
#' @export efourier.i
#' @usage efourier.i(ef, nb.h, nb.pts = 120)
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
#' @export efourier.norm
#' @usage efourier.norm(ef, start = FALSE)
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
#' @export efourier.shape
#' @usage efourier.shape(an, bn, cn, dn, nb.h, nb.pts=60, alpha=2, plot=TRUE)
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
#' @usage ef.amplify(ef, amp=rep(0.5, 4)) 
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
#' @export rfourier
#' @usage rfourier(coo, nb.h, smooth.it = 0, norm = FALSE, verbose=TRUE)
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
rfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, verbose=TRUE){
  coo <- coo.check(coo)
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo) | missing(nb.h)) {
    nb.h = floor(nrow(coo)/2) - 1 # should not be -1 but 0 #todo
    if (verbose){
      warning("'nb.h' must be lower than half the number of 
              points and has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (verbose){
      cat("The number of harmonics used has been set to: ", nb.h)}}
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
#' @usage rfourier.i(rf, nb.h, nb.pts=300)
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
#'     # it works since coo.draw and coo.plot retrieve "x"
#'     # and "y" components (through l2m when passed with a list.
#' 
#' @export rfourier.i
rfourier.i <- function(rf, nb.h, nb.pts=300) {
  if (!all(c("an", "bn") %in% names(rf))) {
    stop("a list containing 'an' and 'bn' harmonic coefficients 
         must be provided")}
  ao <- ifelse(is.null(rf$ao), 1, rf$ao)
  an <- rf$an
  bn <- rf$bn
  if (missing(nb.h)) {nb.h <- length(an)}
  if (nb.h > length(an)) {
    nb.h <- length(an)
    warning("nb.h cannot be higher than length(rf$an) and 
            has been set to: ", nb.h)}
  theta <- seq(0, 2*pi, length=nb.pts)
  harm  <- matrix(NA, nrow=nb.h, ncol=nb.pts)
  for (i in 1:nb.h){
    harm[i, ]<- an[i]*cos(i*theta) + bn[i]*sin(i*theta)}
  r <- (ao/2) + apply(harm, 2, sum)
  Z <- complex(modulus=r, argument=theta)
  list(x=Re(Z), y=Im(Z), angle=theta, r=r)}

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
#' @export rfourier.shape
#' @usage rfourier.shape(an, bn, nb.h, nb.pts=80, alpha=2, plot=TRUE)
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
#' panel(Out(a2l(replicate(100,
#' rfourier.shape(nb.h=6, alpha=0.4, nb.pts=200, plot=FALSE))))) # Butterflies
rfourier.shape <- function(an, bn, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  rf  <- list(an=an, bn=bn, ao=0)
  shp <- rfourier.i(rf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(cbind(shp$x, shp$y))}
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
#' @export tfourier
#' @usage tfourier(coo, nb.h, smooth.it=0, norm = FALSE, verbose = TRUE)
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
tfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, verbose=TRUE){
  if (missing(nb.h))  {stop("nb.h must be provided")}
  if (is.list(coo))   {coo <- l2m(coo)}
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo)) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (verbose){
      warning("'nb.h' must be lower than half the number of points and has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (verbose){
      cat("The number of harmonics used has been set to: ", nb.h)}}
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
#' @export tfourier.i
#' @usage tfourier.i(tf, nb.h, nb.pts = 300, force2close = FALSE, rescale
#' =TRUE, perim = 2 * pi, thetao = 0)
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

tfourier.i<-function(tf, nb.h, nb.pts=300, #nb.h -> 180 everywhere ?#todo
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
    warning("nb.h cannot be higher than length(rf$an) and has been set to: ", nb.h)}
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
  return(list(x=coo[, 1], y=coo[, 2], angle=theta, phi=phi))}

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
#' @export tfourier.shape
#' @usage tfourier.shape(an, bn, ao = 0, nb.h, nb.pts=80, alpha=2, plot=TRUE)
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
#' 
#' @usage harm.pow(xf)
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
#' 
#' @export harm.pow
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
#' @export coeff.sel
#' @usage coeff.sel(retain = 8, drop = 0, nb.h = 32, cph = 4)
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff.sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff.split} returns a named list
#' of coordinates.
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
#' @export coeff.split
#' @usage coeff.split(cs, nb.h = 8, cph = 4)
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @examples
#' coeff.split(1:128, nb.h=32, cph=4) # efourier
#' coeff.split(1:64, nb.h=32, cph=2)  # t/r fourier
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
#' @export polynomials
#' @param coo a matrix or a list of (x; y) coefficients
#' @param n the degree of the polynomial to fit (the intercept is returned)
#' @param orthogonal logical wheter to calculate orthogonal coefficients
#' @return a lm model object
#' @keywords morphoCore

polynomials <- function(coo, n, orthogonal=TRUE){
  coo <- coo.check(coo)
  if (missing(n)) {
    n <- 5
    warning(paste(" * 'n' not provided and set to", n, "\n"))}
  x <- poly(coo[, 1], degree=n, raw=!orthogonal)
  mod <- lm(coo[, 2] ~ x)
  return(mod)}

#' Calculate shape from a polynomial model
#' 
#' Returns a matrix of (x; y) coordinates when passed with a model obtained with
#' \link{polynomials}.
#' @export polynomials.i
#' @param mod a lm model (see \link{polynomials}).
#' @param x.pred the x-range on which to predict y-values (basically, a sequence
#' of points along the the baseline range).
#' @param nb.pts the number of points to predict. By default (and can't be higher)
#' the number of points in the original shape.
#' @return a matrix of (x; y) coordinates.
#' @keywords morphoCore
polynomials.i <- function(mod, x.pred, nb.pts=nrow(mod$model),
                          baseline1x=-1, baseline2x=1){
  if (missing(x.pred)) {
    x.pred <- seq(baseline1x, baseline2x, length=nb.pts)}
  x.poly <- poly(x.pred, degree=mod$rank-1)
  y.pred <- predict(mod, newdata=data.frame(x=x.poly))
  coo <- cbind(x.pred, y.pred)
  colnames(coo) <- c("x", "y")
  return(coo)}
# 2.2 Cubic splines -------------------------------------------------------
# 2.3 Bezier splines -------------------------------------------------------
#' Calculates Bezier coefficients from a shape
#' 
#' todo
#' @export bezier
#' @param coo a matrix or a list of (x; y) coordinates
#' @param n the degree
#' @return a list with J and B
#' @keywords morphoCore
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
#' @export bezier.i
#' @param B a matrix
#' @param nb.pts the number of points to return for drawing he shape
#' @return a matrix of (x; y) coordinates
#' @keywords morphoCore
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