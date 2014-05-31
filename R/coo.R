# 1. coo utilities and their Coo methods ---------------------------------------
# a family of functions that do simple functions on 2d coordinates (further
# abbreviated as "shape" (either outlines, open outlines or lanfmarks)
# they can be passed either as two-column matrices colnames ("x" and "y"
# colnaming is not mandatory) or as a list with $x and $y components.
# and returns a (named) \code{matrix} of coordinates.

#' Checks "coo" shapes
#'
#' A simple utility, used internally, mostly in the coo functions and methods.
#' Returns a matrix of coordinates, when passed with either a list or a \code{matrix} of coordinates.
#'
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates or a list.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates.
#' @seealso \link{ldk.check}
#' @keywords coo_utilities
#' @examples
#' #coo.check("Not a shape")
#' #coo.check(matrix(1:10, ncol=2))
#' #coo.check(list(x=1:5, y=6:10))
#' @export
coo.check <- function(coo){
  if (is.matrix(coo)) {return(coo)}
  if (is.list(coo))   {
    if (length(coo)==1) return(l2m(coo))}
  stop(" * A list or a matrix of (x, y) coordinates must be provided.")}

#' Checks "ldk" shapes
#'
#' A simple utility, used internally, mostly in the Ldk methods,
#' in some graphical functions, and notably in \link{l2a}.
#' Returns an array of landmarks arranged as (nb.ldk) x (x; y) x (nb.shapes),
#' when passed with either a list, a matrix or an array of coordinates.
#' If a list is provided, checks that the number of landmarks is consistent.
#'
#' @param ldk a \code{matrix} of \eqn{(x; y)} coordinates, a list, or an array.
#' @return an \code{array} of \eqn{(x; y)} coordinates.
#' @seealso \link{coo.check}
#' @keywords coo_utilities
#' @examples
#' #coo.check("Not a shape")
#' #coo.check(matrix(1:10, ncol=2))
#' #coo.check(list(x=1:5, y=6:10))
#' @export
ldk.check <- function(ldk){
  if (is.array(ldk)){
    if (length(dim(ldk)==3)){ return(ldk)}
    if (length(dim(ldk)==2)){ return(array(ldk, dim=c(nrow(ldk), ncol(ldk), 1)))}
    stop(" * A matrix an array (dim=3) must be provided.")
  }
  if (is.list(ldk)) {
    l <- sapply(ldk, length)
    if (length(unique(l))==1){return(l2a(ldk))}
    stop(" * A list of matrices with the same number of coordinates must be provided.")
  }
  stop(" * A list, a matrix or a dim=3 array must be provided.")}

#' Centers coordinates
#'
#' Returns a shape centered on the origin.
#' 
#' @aliases coo.center
#' @export
#' @export
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' @export
coo.center <- function(coo){UseMethod("coo.center")}
#' @export
coo.center.default <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 2, function(x) x - mean(x)))}
#' @export
coo.center.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.center)
  return(Coo)}

#' Scales coordinates
#'
#' Scales the coordinates by a 'scale' factor. If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#' 
#' @aliases coo.scale 
#' @export
#' @export
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param scale the scaling factor, by default, the centroid size.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.scale(bot))
#' coo.plot(b)
#' coo.plot(coo.scale(b))
#' @export
coo.scale <- function(coo, scale){UseMethod("coo.scale")}
#' @export
coo.scale.default <- function (coo, scale=coo.centsize(coo)) {
  coo <- coo.check(coo)
  cp  <- coo.centpos(coo)
  coo <- coo.trans(coo.trans(coo, -cp[1], -cp[2])/scale, cp[1], cp[2])
  return(coo)}
#' @export
coo.scale.Coo <- function(coo, scale){
  Coo <- coo
  #dirty loop but had bad time trying to vectorize it
  if (missing(scale)) {
    scale <- sapply(Coo$coo, coo.centsize)}
  if (length(scale) != length(Coo)) {
    scale <- rep(scale, length(Coo))}
  for (i in seq(along=Coo$coo)){
    Coo$coo[[i]] <- coo.scale(Coo$coo[[i]], scale[i])}
  return(Coo)}

#' Rotates coordinates
#'
#' Rotates the coordinates by a 'theta' angle (in radians) If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#' 
#' @aliases coo.rotate 
#' @export
#' @export
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param theta \code{numeric}the angle (in radians) to rotate shapes.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.rotatecenter}
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.rotate(bot, pi))
#' coo.plot(b)
#' coo.plot(coo.rotate(b, pi))
#' @export
coo.rotate <- function(coo, theta=0){UseMethod("coo.rotate")}
#' @export
coo.rotate.default <- function(coo, theta=0){
  coo <- coo.check(coo)
  rmat <- matrix(c(cos(theta), sin(theta),
                   -sin(theta), cos(theta)), nrow=2)
  return(coo %*% rmat)}
#' @export
coo.rotate.Coo <- function(coo, theta=0){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.rotate, theta)
  return(Coo)}

#' Aligns coordinates
#'
#' Aligns the coordinates along their longer axis using var-cov matrix and eigen values.
#' 
#' @aliases coo.align 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.aligncalliper}, \link{coo.alignxax}
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.align(bot))
#' coo.plot(b)
#' coo.plot(coo.align(b))
#' @export
coo.align <- function(coo){UseMethod("coo.align")}
#' @export
coo.align.default <- function(coo){
  coo <- coo.check(coo)
  return(coo %*% svd(var(coo))$u)}
#' @export
coo.align.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.align)
  return(Coo)}

#' Translates coordinates
#'
#' Translates the coordinatesby a 'x' and 'y' value
#' 
#' @aliases coo.trans 
#' @export
#' @export
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param x \code{numeric}translation along the x-axis.
#' @param y \code{numeric}translation along the y-axis.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.trans(bot, 50, 100))
#' coo.plot(b)
#' coo.plot(coo.trans(b, 50, 100))
#' @export
coo.trans <- function(coo, x=0, y=0){UseMethod("coo.trans")}
#' @export
coo.trans.default <- function(coo, x=0, y=0){
  coo <- coo.check(coo)
  cbind(coo[, 1] + x, coo[, 2] + y) }
#' @export
coo.trans.Coo <- function(coo, x=0, y=0){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.trans, x, y)
  return(Coo)}

#' Slides coordinates
#'
#' Slides the coordinates so that the id1-th point become the first one.
#' @aliases coo.slide
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param id1 the id of the point that will become the new first point.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' stack(coo.slide(hearts, 1))
#' coo.plot(hearts[4])
#' coo.plot(coo.slide(hearts[4], 50))
#' @export
coo.slide <- function(coo, id1){UseMethod("coo.slide")}
#' @export
coo.slide.default <- function(coo, id1){
  coo <- coo.check(coo)
  if (id1 == 0) {return(coo)}
  n <- nrow(coo)
  slided.rows <- c(id1:n, 1:(id1-1))
  return(coo[slided.rows, ])}
#' @export
coo.slide.Coo <- function(coo, id1){
  Coo <- coo
  if (length(Coo$ldk)==0) stop(" * No landmarks defined.")
  for (i in seq(along=Coo$coo)) {
    Coo$coo[[i]] <- coo.slide(Coo$coo[[i]], Coo$ldk[[i]][id1])
    Coo$ldk[[i]] <- (Coo$ldk[[i]] - (Coo$ldk[[i]][id1] -1)) %% nrow(Coo$coo[[i]])}
  return(Coo)}

#' Sample coordinates (among points)
#'
#' Sample n coordinates among existing points
#' 
#' @aliases coo.sample 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param n an integer, the number fo points to sample.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.sample(bot, 24))
#' coo.plot(b)
#' coo.plot(coo.sample(b, 24))
#' @export
coo.sample <- function(coo, n){UseMethod("coo.sample")}
#' @export
coo.sample.default <- function (coo, n) {
  coo <- coo.check(coo)
  sampled <- round(seq(1, nrow(coo), len = n + 1)[-(n + 1)])
  return(coo[sampled, ])}
#' @export
coo.sample.Coo <- function(coo, n){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.sample, n)
  return(Coo)}

#' Samples coordinates (regular radius)
#'
#' Samples n coordinates with a regular angle.
#' 
#' @aliases coo.samplerr 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param n integer, the number of points to sample.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates or an Coo object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' stack(bot)
#' bot <- coo.center(bot)
#' stack(coo.samplerr(bot, 12))
#' coo.plot(bot[1])
#' coo.plot(rr <- coo.samplerr(bot[1], 12))
#' cpos <- coo.centpos(bot[1])
#' segments(cpos[1], cpos[2], rr[, 1], rr[, 2])
#' @export
coo.samplerr <- function(coo, n){UseMethod("coo.samplerr")}
#' @export
coo.samplerr.default <- function(coo, n){
  coo <- coo.check(coo)
  Rx <- coo[, 1]
  Ry <- coo[, 2]
  le  <-length(Rx)
  M   <-matrix(c(Rx, Ry), le, 2)
  M1  <-matrix(c(Rx-mean(Rx), Ry-mean(Ry)), le, 2)
  V1  <-complex(real=M1[, 1], imaginary=M1[, 2])
  M2  <-matrix(c(Arg(V1), Mod(V1)), le, 2)
  V2  <-NA
  for (i in 0:(n-1)){
    V2[i+1] <- which.max((cos(M2[, 1] - 2*i*pi/n)))}
  V2 <- sort(V2)
  return(M1[V2, ])}
#' @export
coo.samplerr.Coo <- function(coo, n){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.samplerr, n)
  return(Coo)}

#' Interpolates coordinates
#'
#' Interpolates n coordinates "among existing points"between" existing points,
#' along the perimeter of the coordinates provided and keeping the first point
#' 
#' @aliases coo.interpolate
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param n an integer, the number fo points to interpolate.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.scale(bot))
#' coo.plot(b)
#' coo.plot(coo.scale(b))
#' data(bot)
#' stack(bot)
#' stack(coo.interpolate(coo.sample(bot, 12), 120))
#' coo.plot(bot[1])
#' coo.plot(coo.interpolate(coo.sample(bot[1], 12), 120))
#' @export
coo.interpolate <- function(coo, n){UseMethod("coo.interpolate")}
#' @export
coo.interpolate.default <- function(coo, n){
  coo <- coo.check(coo)
  if (!is.closed(coo)) { coo <- coo.close(coo) }
  orig <- coo.perim.cum(coo)
  targ <- seq(0, coo.perim(coo), length=n+1)[-(n+1)]
  coo2 <- matrix(c(coo[1, ], rep(NA, n*2 - 2)), byrow=TRUE, nrow=n, ncol=2)
  for (i in 2:n) {
    k <- max(which(orig <= targ[i]))
    r <- (targ[i] - orig[k]) / (orig[k+1]- orig[k])
    coo2[i, ] <- edi(coo[k, ], coo[k+1, ], r)}
  return(coo2)}
#' @export
coo.interpolate.Coo <- function(coo, n){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.interpolate, n)
  return(Coo)}

#' Smoothes coordinates (closed outlines)
#'
#' Smoothes coordinates using a simple moving average.
#' May be useful to remove digitization noise.
#' @aliases coo.smooth
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param n an (integer) \code{numeric} to specify the number of smoothing iterations
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.smoothcurve}
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo.smooth(bot, 10))
#' coo.plot(bot[1])
#' coo.plot(coo.smooth(bot[1], 30))
#' @export
coo.smooth <- function(coo, n){UseMethod("coo.smooth")}
#' @export
coo.smooth.default <- function(coo, n=0){
  coo <- coo.check(coo)
  p   <- nrow(coo)
  a   <- 0
  while (a < n) {
    a <- a + 1
    coo.i <- rbind(coo[-1, ], coo[1, ])
    coo.s <- rbind(coo[p, ],  coo[-p, ])
    coo   <- coo/2 + coo.i/4 + coo.s/4}
  return(coo)}
#' @export
coo.smooth.Coo <- function(coo, n){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.smooth, n)
  return(Coo)}

#' Smoothes coordinates (open outlines)
#'
#' Smoothes coordinates using a simple moving average but let the first and last points unchanged.
#' May be useful to remove digitization noise.
#' @aliases coo.smoothcurve
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param n an (integer) \code{numeric} to specify the number of smoothing iterations
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.smooth}
#' @keywords coo_utilities
#' @examples
#' data(olea)
#' o <- olea[1]
#' coo.plot(o, border="grey50", points=FALSE)
#' coo.draw(coo.smooth(o, 24), border="blue", points=FALSE)
#' coo.draw(coo.smoothcurve(o, 24), border="red", points=FALSE)
#' @export
coo.smoothcurve <- function(coo, n){UseMethod("coo.smoothcurve")}
#' @export
coo.smoothcurve.default <- function(coo, n=0){
  coo <- coo.check(coo)
  p   <- nrow(coo)
  a   <- 0
  while (a < n) {
    a <- a + 1
    for (i in 2:(p-1)){
      coo[i, ] <- (coo[i-1, ]*0.25 + coo[i, ]*0.5 + coo[i+1, ]*0.25)}}
  return(coo)}
#' @export
coo.smoothcurve.Opn <- function(coo, n){
  Opn <- coo
  Opn$coo <- lapply(Opn$coo, coo.smoothcurve, n)
  return(Opn)}

#' Tests if shapes are closed
#'
#' Returns TRUE/FALSE whether the last coordinate of the shapes is the same
#' as the first one.
#' 
#' @aliases is.closed
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a boolean.
#' @seealso \link{coo.close}, \link{coo.unclose}
#' @keywords coo_utilities
#' @examples
#' is.closed(matrix(1:10, ncol=2))
#' is.closed(coo.close(matrix(1:10, ncol=2)))
#' data(bot)
#' is.closed(bot)
#' is.closed(coo.close(bot))
#' @export
is.closed <- function(coo){UseMethod("is.closed")}
#' @export
is.closed.default <- function(coo){
  coo <- coo.check(coo)
  identical(coo[1,], coo[nrow(coo),]) }
#' @export
is.closed.Coo <- function(coo){
  Coo <- coo
  return(sapply(Coo$coo, is.closed))}

# # is.likelyopen tries to estimate is a matrix of coordinates is likely to be a
# # closed polygon
# is.likelyclosedpolygon <- function(coo) {
#   x <- coo.perim.pts(coo)
#   d <- max(x) / median(x[-which.max(x)])
#   ifelse(d > 3, TRUE, FALSE)}

#' Closes/'Uncloses' shapes
#'
#' Returns a closed shape from (un)closed shapes. See also \link{coo.unclose}.
#' 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.unclose}, \link{is.closed}
#' @keywords coo_utilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo.close(x)
#' x3 <- coo.unclose(x2)
#' x
#' is.closed(x)
#' x2
#' is.closed(x2)
#' x3
#' is.closed(x3)
#' @export
coo.close <- function(coo){UseMethod("coo.close")}
#' @export
coo.close.default <- function(coo){
  coo <- coo.check(coo)
  ifelse(is.closed(coo), return(coo), return(rbind(coo, coo[1, ])))}
#' @export
coo.close.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.close)
  return(Coo)}

#' 'Uncloses' shapes
#' 
#' Returns a unclosed shape from (un)closed shapes. See also \link{coo.close}.
#' 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo}
#'   object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.close}, \link{is.closed}
#' @keywords coo_utilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo.close(x)
#' x3 <- coo.unclose(x2)
#' x
#' is.closed(x)
#' x2
#' is.closed(x2)
#' x3
#' is.closed(x3)
#' @export
coo.unclose <- function(coo){UseMethod("coo.unclose")}
#' @export
coo.unclose.default <- function(coo){
  coo <- coo.check(coo)
  ifelse(is.closed(coo), return(coo[-nrow(coo), ]), return(coo))}
#' @export
coo.unclose.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.unclose)
  return(Coo)}

# Some utilities documented yet less likely to be used. They may be useful for
# some testing, developing new methods, or on monday mornings.

#' Rotates shapes with a custom center
#'
#' rotates a shape of "theta" angles (in radians) and with a \eqn{(x; y)} "center".
#' @aliases coo.rotatecenter
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param theta \code{numeric} the angle (in radians) to rotate shapes.
#' @param center the \eqn{(x; y)} position of the center
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.rotate}
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' coo.draw(coo.rotatecenter(b, -pi/2, c(200, 200)), border="red")
#' @export
coo.rotatecenter <- function(coo, theta, center=c(0, 0)){UseMethod("coo.rotatecenter")}
#' @export
coo.rotatecenter.default <- function(coo, theta, center=c(0, 0)){
  coo <- coo.trans(coo, -center[1], -center[2])
  coo <- coo.rotate(coo, theta)
  return(coo.trans(coo, center[1], center[2]))}
#' @export
coo.rotatecenter.Coo <- function(coo, theta, center=c(0, 0)){
  Coo <- coo
  for (i in seq(along=Coo$coo)){
    Coo$coo[[i]] <- coo.rotatecenter(Coo$coo[[i]], theta, center)}
  return(Coo)}

#' Forces shapes to close
#'
#' An exotic function that distribute the distance between the first and the last points
#' of unclosed shapes, so that they become closed. May be useful (?) e.g. for t/rfourier methods
#' where reconstructed shapes may not be closed.
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 64)
#' b <- b[1:40,]
#' coo.plot(b)
#' coo.draw(coo.force2close(b), border="red")
#' @export
coo.force2close <- function(coo){
  coo <- coo.check(coo)
  xy <- coo.centpos(coo)
  if (is.closed(coo)) {return(coo)}
  n  <- nrow(coo)
  d  <- coo[1, ] - coo[n, ]
  dm <- cbind(seq(0, d[1], length=n), seq(0, d[2], length=n))
  coo2 <- coo + dm
  xy2 <- coo.centpos(coo2)
  coo2 <- coo.trans(coo2, xy[1] - xy2[1], xy[2] - xy2[2])
  return(coo2)}

# 2. Handling / baselines on coo and Coo -------------------------------------
# Some functions and methods to ease alignments, grabbing part of shapes, etc.
#' Retains coordinates with positive y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a 
#' bilateral symmetry) #' and when one wants to retain just the upper side. 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- coo.alignxax(bot[1])
#' coo.plot(b)
#' coo.draw(coo.up(b), border="red")
#' @export
coo.up <- function(coo){
  up <- coo[coo[,2]>=0,]
  return(up)}

#' Retains coordinates with negative y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a 
#' bilateral symmetry) and when one wants to retain just the lower side. 
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- coo.alignxax(bot[1])
#' coo.plot(b)
#' coo.draw(coo.down(b), border="red")
#' @export
coo.down <- function(coo){
  coo <- coo.check(coo)
  return(coo[coo[, 2]<=0,])}

#' Aligns shapes along the x-axis
#' 
#' Align the longest axis of a shape along the x-axis.
#' @aliases coo.alignxax
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.align}, \link{coo.aligncalliper}
#' @keywords coo_utilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' coo.plot(coo.alignxax(b))
#' }
#' @export
coo.alignxax <- function(coo){UseMethod("coo.alignxax")}
#' @export
coo.alignxax.default <- function(coo){
  coo <- coo.check(coo)
  coo <- coo.align(coo)
  return(coo.trans(coo, x=0, y= -coo.centpos(coo)[2]))}
#' @export
coo.alignxax.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.alignxax)
  return(Coo)}

#' Aligns shapes along their "calliper length"
#' 
#' And returns them registered on bookstein coordinates.
#' See \link{coo.bookstein}.
#' @aliases coo.aligncalliper
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.align}, \link{coo.alignxax}, \link{coo.calliper}
#' @keywords coo_utilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' coo.plot(coo.aligncalliper(b))
#' bot.al <- coo.aligncalliper(bot)
#' stack(bot.al)
#' }
#' @export
coo.aligncalliper <- function(coo){UseMethod("coo.aligncalliper")}
#' @export
coo.aligncalliper.default <- function(coo){
  coo <- coo.check(coo)
  cal.ind <- coo.calliper(coo, arr.ind=TRUE)$arr.ind
  coo <- coo.bookstein(coo, cal.ind[1], cal.ind[2])
  return(coo)}
#' @export
coo.aligncalliper.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo.aligncalliper)
  return(Coo)}

#' Reverses coordinates
#' 
#' Returns the reverse suite of coordinates, i.e. change shape's orientation
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 4)
#' b
#' coo.rev(b)
#' @export
coo.rev <- function(coo){
  coo <- coo.check(coo)
  return(coo[nrow(coo):1,])}

#' Defines interactively landmarks
#' Allows to interactively define a "nb.ldk" number of landarks on a shape.
#' Used in other facilities to acquire/manipulate data.
#' @aliases coo.ldk
#' @param coo a \code{matrix} or a list of \eqn{(x; y)} coordinates.
#' @param nb.ldk integer, the number of landmarks to define
#' @return \code{numeric} that corresponds to the closest ids,
#' on the shape, from cliked points.
#' @keywords coo_utilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo.ldk(b, 3) # run this, and click 3 times
#' coo.ldk(bot, 2) # this also works on Out
#' }
#' @export
coo.ldk <- function(coo, nb.ldk) {
  if (is.list(coo)) coo <- l2m(coo)
  coo.plot(coo)
  ldk <- numeric(nb.ldk)
  cat("[")
  for (i in 1:nb.ldk){
    p <- l2m(locator(1))
    l <- apply(coo, 1, function(y) sqrt(sum((p-y)^2)))
    ldk[i] <- which.min(l)
    points(coo[ldk[i], 1], coo[ldk[i], 2], pch=20, col="red", cex=0.5)
    cat("*")}
  cat("]\n")
  return(ldk)}

#' Register Bookstein's coordinates
#'
#' Registers a new baseline for the shape, with the \code{ldk1}-th
#' and \code{ldk2}-th points being set on \eqn{(x= -0.5; y=0)} and \eqn{(x= 0.5; y=0)}, respectively.
#' @aliases coo.bookstein
#' @param coo either a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param ldk1 the id of the first point of the new baseline
#' @param ldk2 the id of the second point of the new baseline
#' @return a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @seealso \link{coo.baseline}
#' @keywords coo_utilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' stack(coo.bookstein(hearts, 2, 4))
#' h <- hearts[1]
#' coo.plot(h)
#' coo.plot(coo.bookstein(h, 20, 57), border="red")
#' @export
coo.bookstein <- function(coo, ldk1, ldk2){UseMethod("coo.bookstein")}
#' @export
coo.bookstein.default <- function(coo, ldk1, ldk2){
  D <- ed(coo[ldk1, ], coo[ldk2, ])
  coo2 <- matrix(NA, nrow(coo), ncol(coo))
  ldk1 <- coo[ldk1,]
  ldk2 <- coo[ldk2,]
  coo2[, 1] <- (((ldk2[1]-ldk1[1]) * (coo[,1]-ldk1[1])
                 + (ldk2[2]-ldk1[2]) * (coo[,2]-ldk1[2])) / (D^2)) - 0.5
  coo2[, 2] <- ((ldk2[1]-ldk1[1])  * (coo[,2]-ldk1[2])
                - (ldk2[2]-ldk1[2])  * (coo[,1]-ldk1[1])) / (D^2)
  return(coo2)}
#' @export
coo.bookstein.Coo <- function(coo, ldk1, ldk2){ #id1 ?
  Coo <- coo
  for (i in seq(along=Coo$coo)){
    Coo$coo[[i]] <- coo.bookstein(Coo$coo[[i]], Coo$ldk[[i]][ldk1], 
                                  Coo$ldk[[i]][ldk2])}
  return(Coo)}

#' @export
coo.bookstein.Ldk <- function(coo, ldk1, ldk2){
  Ldk <- coo
  Ldk$coo <- lapply(Ldk$coo, coo.bookstein, ldk1=ldk1, ldk2=ldk2)
  return(Ldk)}

#' Register new baselines
#'
#' A non-exact baseline registration on \code{t1} and \code{t2} coordinates,
#' for the \code{ldk1}-th and \code{ldk2}-th points.
#' By default it returns Bookstein's coordinates.
#' @aliases coo.baseline
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates, or a \link{Coo} object.
#' @param ldk1 the id of the first point of the new baseline
#' @param ldk2 the id of the second point of the new baseline
#' @param t1 \code{numeric} the \eqn{(x; y)} coordinates of the 1st point of the new baseline
#' @param t2 \code{numeric} the \eqn{(x; y)} coordinates of the 2nd point of the new baseline
#' @return a \code{matrix} of \eqn{(x; y)} coordinates or a \link{Coo} object.
#' @seealso \link{coo.bookstein}
#' @keywords coo_utilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' stack(coo.baseline(hearts, 2, 4, c(-1, 0), c(1, 1)))
#' @export
coo.baseline <- function(coo, ldk1, ldk2, t1, t2){UseMethod("coo.baseline")}
#' @export
coo.baseline.default <- 
  function(coo, ldk1=1, ldk2=2, t1=c(-0.5, 0), t2=c(0.5, 0)){
  if (is.list(coo)) {coo <- l2m(coo)}
  t1x <- t1[1]
  t1y <- t1[2]
  t2x <- t2[1]
  t2y <- t2[2]
  r1x <- coo[ldk1, 1]
  r1y <- coo[ldk1, 2]
  r2x <- coo[ldk2, 1]
  r2y <- coo[ldk2, 2]
  # translation based on the first landmark
  ref <- coo.trans(coo, t1x - coo[ldk1, 1] , t1y - coo[ldk1, 2])
  # we calculate dx and dy for the two vectors
  rx <- ref[ldk2, 1] - t1x
  ry <- ref[ldk2, 2] - t1y 
  tx <- t2x - t1x
  ty <- t2y - t1y
  # returns difference angle and norm ratios between two vectors given as 4 numeric.
  vi <- vecs.param(rx, ry, tx, ty)
  # we rotate accordingly with a center defined
  # as the first landmark (trans, rot, untrans)
  ref <- coo.trans(ref, -t1x, -t1y)
  ref <- ref / vi$r.norms
  ref <- coo.rotate(ref, -vi$d.angle)
  ref <- coo.trans(ref, t1x, t1y)
  return(ref)}
#' @export
coo.baseline.Coo <- function(coo, ldk1=1, ldk2=2, t1=c(-0.5, 0), t2=c(0.5, 0)){
  Coo <- coo
  for (i in seq(along=Coo$coo)){
    Coo$coo[[i]] <- coo.baseline(Coo$coo[[i]], Coo$ldk[[i]][ldk1],
                                 Coo$ldk[[i]][ldk2], t1, t2)}
  return(Coo)}

# 3. coo shape descriptors -----------------------------------------------------
# Mainly intended for traditional morphometrics.
# Convert to methods ? Or an utility to get these descriptors ? #todo


# a. centroid -------------------------------------------------------------
#' Returns the position of the centroid
#'
#' Returns the \eqn{(x; y)} centroid coordinates of a shape.
#' @aliases coo.centpos
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \eqn{(x; y)} coordinates as \code{numeric}.
#' @keywords coo_utilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo.plot(b)
#' xy <- coo.centpos(b)
#' points(xy[1], xy[2], cex=2, col="blue")
#' xy
#' @export
coo.centpos <- function(coo){UseMethod("coo.centpos")}
#' @export
coo.centpos.default <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 2, mean))}
#' @export
coo.centpos.Coo <- function(coo){
  Coo <- coo
  centpos <- t(sapply(Coo$coo, coo.centpos))
  colnames(centpos) <- c("x", "y") # pure cosmetics
  return(centpos)}

#' Calculates the centroid size
#' @aliases coo.centsize
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \code{numeric}, the centroid size.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.centsize(bot[1])
#' cs <- sapply(bot$coo, coo.centsize)
#' hist(cs, breaks=10)
#' @export
coo.centsize <- function(coo){
  coo  <- coo.check(coo)
  cent <- coo.centpos(coo)
  cs   <- mean(apply(coo, 1, function(x) sqrt(sum((x-cent)^2))))
  return(cs)}

#' Returns the distance between everypoints and the centroid
#' For every point of the shape, returns the (centroid-points) distance.
#' @aliases coo.centdist
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return a \code{matrix} of \eqn{(x; y)} coordinates.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 64)
#' d <- coo.centdist(b)
#' barplot(d)
#' @export
coo.centdist <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 1, function(x) ed(coo.centpos(coo), x)))}

# b. length(s) -----------------------------------------------------------------

#' Calculates the chordal distance along a shape.
#'
#' Calculates the euclidean distance between every points of a shape for coo.perim.pts.
#' The cumulative sum for coo.perim.cum
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \code{numeric} the distance between every point.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 24)
#' coo.perim.pts(b)
#' @export
coo.perim.pts <-  function (coo){
  coo <- coo.check(coo)
  n <- nrow(coo)
  d <- sqrt(apply((coo - coo.slide(coo, n))^2, 1, sum))[-1]
  return(d)}

#' Calculates the cumulative chrodal distance a shape.
#'
#' Just a wrapper for cumsum(coo.perim.pts). See \link{coo.perim.pts}.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \code{numeric} the cumulate sum of chrodal distances
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 24)
#' coo.perim.cum(b)
#' @export
coo.perim.cum <- function(coo){
  coo <- coo.check(coo)
  d <- cumsum(sqrt(apply((coo-rbind(coo[1,],coo[-(dim(coo)[1]),]))^2,1,sum)))
  return(d)}

#' Calculates the perimeter
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \code{numeric}, the perimeter.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.perim(bot[1])
#' hist(sapply(bot$coo, coo.perim), breaks=10)
#' @export
coo.perim <- function(coo){
  return(sum(coo.perim.pts(coo)))}

#' Calculates the calliper length
#' 
#' Also called the Feret's diameter, the longest distance between two points of
#' the shape provided.
#' @aliases coo.calliper
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @param arr.ind a boolean, if provided returns
#' @return \code{numeric}, the centroid size. If arr.ind=TRUE, a list with the calliper length ($length) 
#' and the two points ($arr.ind)
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo.calliper(b)
#' p <- coo.calliper(b, arr.ind=TRUE)
#' p$length
#' ids <- p$arr.ind
#' coo.plot(b)
#' segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)
#' @export
coo.calliper <- function(coo, arr.ind=FALSE){
  coo <- coo.check(coo)
  d   <- dist(coo, method = "euclidean")
  # we check if there is no ex aequo
  ea <- length(which(d==max(d), arr.ind=TRUE))
  if (length(ea)>1) {cat(" * coo.length: at least two lengths are ex aequo.")}
  if (arr.ind) {
    arr.ind <- which(as.matrix(d)==max(d), arr.ind=TRUE)
    # to return a vector (numeric and sorted) of the rows between which the max
    # length has been found
    arr.ind <- sort(as.numeric(arr.ind[1, ]))
    return(list(length=max(d), arr.ind=arr.ind))
  } else {
    return(max(d))}}

#todo: bouding box
#todo: based on svd cov mat
#' Calculates length and width
#' 
#' Returns the length and width of a shape based on their iniertia axis
#' i.e. alignment to the x-axis. The length is defined as
#' the range along the x-axis; the width as the range on the y-axis.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return two \code{numeric}, the length and the width.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.lw(bot[1])
#' @export
coo.lw <- function(coo){
  coo <- coo.check(coo)
  d   <- apply(coo.align(coo), 2, range)
  return(abs(d[2,] - d[1,]))}

# c. area ----------------------------------------------------------------------
#todo other methods to calculate area
#todo source the algo
#' Calculates the area
#' 
#' Calculates the area for any non-crossing polygon.
#' @aliases coo.area
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return \code{numeric}, the area.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.area(bot[1])
#' hist(sapply(bot$coo, coo.area), breaks=10)
#' @export
coo.area <- function(coo){
  coo <- coo.check(coo)
  coo <- coo.close(coo)
  nr <- nrow(coo)-1
  y <- x <- numeric(nr)
  for (i in 1:nr){
    x[i] <- coo[i, 1] * coo[i+1, 2]
    y[i] <- coo[i+1 , 1] * coo[i, 2]
  }
  area <- (0.5 * (sum(x) - sum(y)))
  return(abs(area))}

# g. angle ----------------------------------------------------------------------
#' Returns the tangent angle along the perimeter
#' 
#' Calculated using complex numbers and returned in radians
#' minus the first one (modulo 2*pi).
#' @param coo a matrix of coordinates
#' @return a numeric, the tangent angle along the perimeter
#' @seealso \link{tfourier}
#' @examples
#' data(bot)
#' b <- bot[1]
#' phi  <- coo.tangle(b)
#' phi2 <- coo.tangle(coo.smooth(b, 2))
#' plot(phi, type="l")
#' plot(phi2, type="l", col="red") # ta is very sensible to noise
#' @export
coo.tangle <- function(coo){
  p <- nrow(coo)
  tangvect <- coo - rbind(coo[p, ], coo[-p, ])
  tet1   <- Arg(complex(real=tangvect[,1], imaginary = tangvect[,2]))
  tet0   <- tet1[1]
  t1     <- seq(0, 2*pi, length= (p+1))[1:p]
  phi    <- (tet1-tet0-t1)%%(2*pi)
  return(phi)}

#' The angle formed by three points.
#' 
#' Returns the angle (in radians) defined by a triplet of points
# either signed ("atan2") or not ("acos").
#' @param m a 3x2 \code{matrix} of 3 points (rows) and \eqn{(x; y)} coordinates
#' @param method one of "atan2" or "acos" for a signed or not angle.
#' @return \code{numeric} the angle in radians.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 64)
#' b <- b[c(1, 14, 24), ]
#' coo.plot(b)
#' coo.theta3(b)
#' coo.theta3(b, method="acos")
#' @export
coo.theta3 <- function(m, method=c("atan2", "acos")[1]){  
  a <- c(m[1, 1] - m[2, 1], m[1, 2] - m[2, 2])
  b <- c(m[3, 1] - m[2, 1], m[3, 2] - m[2, 2])
  if (method=="atan2") {
    return(atan2 (a[1]*b[2]-a[2]*b[1],a[1]*b[1]+a[2]*b[2]))}
  if (method=="acos") {
    return(acos(sum(a*b)/(sqrt(sum(a*a)) * sqrt(sum(b*b)))))}}

#' The angle of every edge of a shape
#' 
#' Returns the angle (in radians) of every edge of a shape,
# either signed ("atan2") or not ("acos"). A wrapper for \link{coo.theta3}
#' @param coo a \code{matrix} or a list of \eqn{(x; y)} coordinates.
#' @param method one of "atan2" or "acos" for a signed or not angle.
#' @return \code{numeric} the angles in radians for every edge.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 64)
#' coo.theta.pts(b)
#' @export
coo.theta.pts <- function(coo, method=c("atan2", "acos")[1]){
  coo <- coo.check(coo)
  coo <- coo.close(coo)
  coo   <- rbind(coo[nrow(coo)-1, ], coo)
  theta <- numeric()
  for (i in 1:(nrow(coo)-2)){
    theta[i] <- coo.theta3(coo[i:(i+2),], method=method)}
  return(theta)}

# d. Scalar shape descriptors used in traditional morphometrics -------------------

#' Calculates the rectilinearity of a shape
#' 
#' As proposed by Zunic and Rosin (see below). I do not 100% guarantee the code.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the rectilinearity
#' @note due to the laborious nature of the algorithm (in nb.pts^2), 
#' and of its implementation, it may be very long to compute.
#' @keywords coo_descriptors
#' @source Zunic J, Rosin PL. 2003. Rectilinearity measurements for polygons. 
#' IEEE Transactions on Pattern Analysis and Machine Intelligence 25: 1193-1200.
#' @examples
#' data(bot)
#' b <- coo.sample(bot[1], 32)
#' coo.rectilinearity(b)
#' @export
coo.rectilinearity <- function(coo) {
  # some check
  coo <- coo.check(coo)
  if (is.closed(coo)) {
    coo.c <- coo
    coo   <- coo.unclose(coo)
  } else {
    coo.c <- coo.close(coo)}
  # we deduce it for the algo
  n <- nrow(coo)
  k <- 4*n
  # here starts the computation as given by Zunic and Rosin
  # we calculate l1 and l2 for every edge
  l1 <- function(x1, y1, x2, y2) { abs(x1 - x2) + abs(y1 - y2) }
  l2 <- function(x1, y1, x2, y2) { sqrt((x1 - x2)^2 + (y1 - y2)^2) }
  # l2 is redefined here for coherence with the paper, but is equivalent to coo.perim.pts(coo)
  l2.e <- l1.e <- numeric(n)
  for (i in 1:n){
    x1 <- coo.c[i,   1]
    y1 <- coo.c[i,   2]
    x2 <- coo.c[i+1, 1]
    y2 <- coo.c[i+1, 2]
    l1.e[i] <- l1(x1, y1, x2, y2)
    l2.e[i] <- l2(x1, y1, x2, y2)} # sum(l2.e) == coo.perim(coo)
  # "step 1" as in Zunic and Rosin
  theta   <- coo.theta.pts(coo)
  theta.k <- abs(c(theta - pi/2, theta - pi, theta - 3*pi/2, theta - 2*pi))
  alpha.k <- sort(theta.k)
  # "step 2" as in Zunic and Rosin
  P1.Pa <- numeric(k)
  for (j in 1:k){
    P1.Pa_n <- numeric(n)
    for (i in 1:n) {
      cos.ij <- cos(theta[i] + alpha.k[j])
      sin.ij <- sin(theta[i] + alpha.k[j])  
      a.ij <- ifelse(cos.ij > 0, l2.e[i], -l2.e[i])
      b.ij <- ifelse(sin.ij > 0, l2.e[i], -l2.e[i]) 
      P1.Pa_n[i] <- a.ij*cos.ij + b.ij*sin.ij
    }
    P1.Pa[j] <- sum(P1.Pa_n)
  }
  # "step 3" as in Zunic and Rosin
  return((4/(4 - pi)) * ((sum(l2.e) / min(P1.Pa)) - (pi/4)))}

#' Calculates the Haralick's circularity of a shape
#' 
#' Returns Haralick's circularity which is less sensible
#' to digitalization noise than coo.circularity. See ...#todo
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the Haralick's circularity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.circularity.haralick(bot[1])
#' @export
coo.circularity.haralick <- function(coo) {
  cd <- coo.centdist(coo)
  return(mean(cd)/sd(cd))}

#' Calculates the circularity of a shape
#' 
#' Returns the "circularity measure". Also called compactness
# and shape factor.
#' @aliases coo.compactness coo.shapefactor
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the circularity.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.circularity(bot[1])
#' @export
coo.circularity <- function(coo) {
  return(coo.perim(coo)^2 / coo.area(coo))}

#' Calculates the "normalized" circularity of a shape
#' 
#' Returns the "circularity", also called compactness
# and shape factor, but normalized to the unit circle.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the circularity normalized to the unit circle.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.circularity.norm(bot[1])
#' @export
coo.circularity.norm <- function(coo) {
  return(coo.perim(coo)^2 / (coo.area(coo)*4*pi))}

# scale dependent... #todo
# coo.fracdim <- function(coo){
#   return((2*log(coo.perim(coo))) / log(coo.area(coo)))}
#coo. <- function(coo){UseMethod("coo.")}


#' Returns the eccentricity (eigenvalues) of a shape
#' 
#' Calculated using a ratio of the eigen values (inertia axis)
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the eccentricity (eigenvalues)
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.eccentricity.eigen(bot[1])
#' @export
coo.eccentricity.eigen <- function(coo){
  coo <- coo.check(coo)
  eig <- eigen(cov(coo))$values
  return(eig[2]/eig[1])}

#' Calculates the eccentricity (bounding box) of a shape
#' 
#' Calculated using the width / length ratio. See \link{coo.lw}
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the eccentricity (boundingbox)
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.eccentricity.boundingbox(bot[1])
#' @export
coo.eccentricity.boundingbox <- function(coo){
  coo <- coo.check(coo)
  lw <- coo.lw(coo)
  return(lw[2]/lw[1])}

#' Calculates the elongation of a shape
#' 
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the circularity normalized to the unit circle.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.elongation(bot[1])
#' @export
coo.elongation <- function(coo){
  coo <- coo.check(coo)
  lw <- coo.lw(coo)
  return(1 - lw[2]/lw[1])}

#' Calculates the rectangularity of a shape
#' 
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the rectangularity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.rectangularity(bot[1])
#' @export
coo.rectangularity <- function(coo){
  coo <- coo.check(coo)
  abr <- prod(coo.lw(coo))
  return(coo.area(coo)/abr)}

#' Calculates the convex hull of a shape
#' 
#' Returns the ids of points that define the convex hull of a shape.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return a \code{matrix} of ids defining the convex hull of the shape.
#' @keywords coo_descriptors
#' @examples
#' data(hearts)
#' h <- coo.sample(hearts[4], 32)
#' coo.plot(h)
#' ch <- coo.chull(h)
#' lines(ch, col="red", lty=2)
#' @export
coo.chull <- function(coo){
  coo <- coo.check(coo)
  return(coo[chull(coo),])}

#' Calculates the convexity of a shape
#' 
#' Calculated using a ratio of the eigen values (inertia axis)
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the convexity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.convexity(bot[1])
#' @export
coo.convexity <- function(coo){
  coo <- coo.check(coo)
  return(coo.perim(coo.chull(coo))/coo.perim(coo))}

#' Calculates the solidity of a shape
#' 
#' Returns the ids of points that define the convex hull of a shape.
#' @param coo a \code{matrix} of \eqn{(x; y)} coordinates.
#' @return numeric, the solidity of a shape.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @keywords coo_descriptors
#' @examples
#' data(bot)
#' coo.solidity(bot[1])
#' @export
coo.solidity <- function(coo){
  coo <- coo.check(coo)
  return(coo.area(coo)/coo.area(coo.chull(coo)))}

