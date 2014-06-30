##### Core function for radii variation Fourier analyses

#' Radii variation Fourier transform
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
#' @return A list with following components:
#' \itemize{
#'  \item \code{an} vector of \eqn{a_{1->n}} harmonic coefficients 
#'  \item \code{bn} vector of \eqn{b_{1->n}} harmonic coefficients
#'  \item \code{ao} ao harmonic coefficient.
#'  \item \code{r} vector of radii lengths.
#'  }
#' @seealso \link{rFourier} for rfourier on \link{Out} objects.
#' \link{rfourier.i} for the inverse operation, \link{rfourier.shape} to play around
#' with this approach.
#' \link{efourier}, \link{tfourier} for the other members of the Fourier's
#' family.
#' @note Directly borrowed for Claude (2008), and called \code{fourier1} there.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords rFourier
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
      cat(" * 'nb.h' must be lower than half the number of points and has been set to", nb.h, "harmonics.\n")}}
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

#' Inverse radii variation Fourier transform
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
#' @note Directly borrowed for Claude (2008), and called \code{ifourier1} there.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords rFourier
#' @examples
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
#' @return A matrix of (x; y) coordinates.
#' @seealso \link{rfourier.i}, \link{efourier.shape}, \link{tfourier.shape}.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords rFourier
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

##### end rFourier