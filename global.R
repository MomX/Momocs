#--------------------------------------------------------------------#
# Global functions                                                   #
#--------------------------------------------------------------------#

# coo utils ##########################################################



#' Centers a list or matrix of coordinates.
#' 
#' \code{coo.center} centers the \code{coo}'s centroid on the origin.
#' 
#' 
#' @usage coo.center(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords coo Utilities
#' @examples
#' 
#' def.par <- par(no.readonly = TRUE)
#' layout(matrix(1:2, 1, 2))
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo, main="A shape")
#' abline(v=0, h=0, col="grey60", lty=2)
#' coo.plot(coo.center(coo), border="red", col=NA, main="A centered shape")
#' abline(v=0, h=0, col="grey60", lty=2)
#' par(def.par)
#' 
#' @export coo.center
coo.center     <- function(coo){
  if (is.matrix(coo)) {  
    return(apply(coo, 2, function(x) x-mean(x)))
  } else if (is.list(coo)){
    return(lapply(coo, function(x) x-mean(x)))
  } else stop("A list or a coordinate matrix must be provided to coo.center")}



#' Scales a list or matrix of coordinates.
#' 
#' \code{coo.scale} resizes \code{coo} so that it can be included in a square
#' of \code{scale} side.
#' 
#' 
#' @usage coo.scale(coo, scale)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param scale The scale to use.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.template}.
#' @keywords coo Utilities
#' @export coo.scale
coo.scale <- 
  function (coo, scale) 
  {
    if (missing(scale)) {
      scale <- ifelse(is.list(coo), coo.centsize(l2m(coo)), coo.centsize(coo))}
    if (is.list(coo)) {
      return(lapply(coo, function(x) x/scale))}
    if (is.matrix(coo)) {
      return(coo/scale)}
    else stop("A list or a coordinate matrix must be provided to coo.scale")
  }



#' Rotates a list or matrix of coordinates.
#' 
#' \code{coo.rotate} rotates (counter-clockwise) \code{coo} with a \code{theta}
#' angle (in radians) .
#' 
#' For those not familiar with linear mapping the providence is there:
#' \url{http://en.wikipedia.org/wiki/Linear_mapping}
#' 
#' @usage coo.rotate(coo, theta)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param theta \code{numeric}. The angle to rotate the shape.
#' @return A \code{list} with \code{x; y} components or a a matrix of \code{(x;
#' y)}coordinates.
#' @seealso \link{coo.rotate.center} if one wants to rotate shapes with another
#' center of rotation than the origin.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo,  main="The bottle revolution", xlim=c(-1.5e3, 1.5e3))
#' r <- seq(pi/6, 2*pi, pi/6)
#' cols <- col.summer(12)
#' for (i in seq(along=r)) {
#'   coo.draw(coo.rotate(coo, r[i]), border=cols[i], col=NA)  
#' }
#' 
#' @export coo.rotate
coo.rotate     <- function(coo, theta){
  rmat <- matrix(c(cos(theta), sin(theta),
                   -sin(theta), cos(theta)), nrow=2)
  if (is.matrix(coo)) {  
    return(coo %*% rmat)
  } else if (is.list(coo)){
    coo <- cbind(coo$x, coo$y)
    coo <- coo %*% rmat
    return(list(x=coo[, 1], y=coo[, 2]))
  } else stop("A list or a coordinate matrix must be provided to coo.rotate")}



#' Aligns a list or matrix of coordinates.
#' 
#' \code{coo.align} aligns \code{coo} using its best fitting ellipse.
#' 
#' 
#' @usage coo.align(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords coo Utilities
#' @examples
#' 
#' def.par <- par(no.readonly = TRUE)
#' layout(matrix(1:2, 1, 2))
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo, main="A shape")
#' coo.plot(coo.align(coo), border="red", col=NA, main="An aligned shape")
#' par(def.par)
#' 
#' @export coo.align
coo.align      <- function(coo){
  if (is.matrix(coo)) {  
    return(coo %*% svd(var(coo))$u)
  } else if (is.list(coo)){
    coo <- cbind(coo$x, coo$y)
    coo <- coo %*% svd(var(coo))$u
    return(list(x=coo[, 1], y=coo[, 2]))
  } else stop("A list of a coordinate matrix must be provided to coo.align")}



#' Transles a list or a matrix of coordinates.
#' 
#' \code{coo.trans} translates \code{coo} by \code{x} and y on the two
#' dimensions, respectively.
#' 
#' 
#' @usage coo.trans(coo, x, y)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param x \code{numeric}. The x-axis translation.
#' @param y \code{numeric}. The y-axis translation.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords Utilities
#' @examples
#' 
#' 
#' data(bot)
#' coo <- coo.scale(coo.center(bot@coo[[1]]))
#' 
#' tx <- seq(0, 10*pi, length=50)
#' ty <- sin(tx)*5
#' cols <- col.summer(50)
#' 
#' coo.plot(xlim=c(0, 10*pi), main="The bottle's wave")
#' lines(tx, ty, col="grey60", lty=2)
#' for (i in seq(along=tx)) {
#'   coo.draw(coo.trans(coo, tx[i], ty[i]), col=NA, border=cols[i])
#' }
#' 
#' 
#' 
#' @export coo.trans
coo.trans      <- function(coo, x, y){
  if (is.matrix(coo)) {  
    return(cbind(coo[, 1] + x, coo[, 2] + y))
  } else if (is.list(coo)){
    return(list(x=coo$x + x, y=coo$y + y))
  } else stop("A list of a coordinate matrix must be provided to coo.trans")}



#' "Slides" a list or a matrix of coordinates.
#' 
#' \code{coo.slide} "slides" \code{coo}, \emph{i.e.} makes the \code{id1}
#' become the first and change the others accordingly.
#' 
#' 
#' @usage coo.slide(coo, id1)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param id1 \code{integer}. Specifies the index of the coordinates that has
#' to be defined as the first coordinate.
#' @keywords Utilities
#' @examples
#' 
#' m <- matrix(1:10, 5, 2)
#' m
#' coo.slide(m, 3)
#' 
#' @export coo.slide
coo.slide      <- function(coo, id1){
  if (id1 == 0) {return(coo)}
  if (is.matrix(coo)) {
    n <- nrow(coo)
    id.slided <- c(id1:n, 1:(id1-1))
    return(coo[id.slided, ])
  } else if (is.list(coo)){
    n <- length(coo$x)
    id.slided <- c(id1:n, 1:(id1-1))
    return(list(x=coo$x[id.slided], y=coo$y[id.slided]))
  } else stop("A list of a coordinate matrix must be provided to coo.slide")}



#' Samples points along the curvilinear abscissa.
#' 
#' \code{coo.sample} samples \code{n} points in \code{coo} along the
#' curvilinear abscissa.
#' 
#' So far points are just sampled along the \code{coo} provided) and not along
#' the "true" curvilinear abscissa, but in most cases, differences should be
#' very small.
#' 
#' @usage coo.sample(coo, n)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param n \code{integer}. The number of points to sample.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.sample.rr}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo,  main="24 points sampled along the outline")
#' points(coo.sample(coo, 24), col="red", pch=20)
#' 
#' @export coo.sample
coo.sample     <- function (coo, n) {
  if (is.matrix(coo)){
    sampled <- round(seq(1, nrow(coo), len = n + 1)[-(n + 1)])
    return(coo[sampled, ])
  } else if (is.list(coo)) {
    sampled <- round(seq(1, length(coo$x), len = n + 1)[-(n + 1)])
    return(list(x=coo$x[sampled], y=coo$y[sampled]))  
  } else stop("A list of a coordinate matrix must be provided to coo.sample")}



#' Samples points with "equally spaced" angles.
#' 
#' \code{coo.sample.rr} samples \code{n} points in \code{coo} so that the radii
#' departing from the coo's centroid are equals. This function uses polar
#' coordinates and complex algebra.
#' 
#' 
#' @usage coo.sample.rr(coo, n)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param n \code{integer}. The number of points to sample.
#' @return Returns a list with components: \itemize{ \item $\code{pixindices}
#' vector of radii indices; \item $\code{radii} vector of sampled radii
#' lengths; \item $\code{phase} vector of phases; \item $\code{coord}
#' coordinates of sampled points for a centered shape; \item $\code{orig.coord}
#' coordinates of sampled points on the original shape; }
#' @seealso \link{coo.sample}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo,  main="24 points with equally spaced radii")
#' rad <- coo.sample.rr(coo, 24)$orig.coord
#' cent <- coo.centpos(coo)
#' segments(cent[1], cent[2], rad[, 1], rad[, 2], col=col.summer(24))
#' points(coo.sample.rr(coo, 24)$orig.coord, pch=20)
#' 
#' @export coo.sample.rr
coo.sample.rr  <- function(coo, n){
 if (is.matrix(coo)) {
   Rx <- coo[, 1]
   Ry <- coo[, 2] }
  if (is.list(coo)){
   Rx <- coo$x
   Ry <- coo$y }
  le  <-length(Rx)
  M   <-matrix(c(Rx, Ry), le, 2)
  M1  <-matrix(c(Rx-mean(Rx), Ry-mean(Ry)), le, 2)
  V1  <-complex(real=M1[,1], imaginary=M1[,2])
  M2  <-matrix(c(Arg(V1), Mod(V1)), le, 2)
  V2  <-NA
  for (i in 0:(n-1)){
    V2[i+1]<-which.max((cos(M2[,1]-2*i*pi/n)))}
  V2<-sort(V2)
  list("pixindices"=V2,"radii"=M2[V2,2],"phase"=M2[V2,1],
      "coord"=M1[V2,], "orig.coord"=matrix(c(M1[,1]+mean(Rx), M1[,2]+mean(Ry)), ncol=2)[V2,])}



#' Given an outline, interpolates points along the curvilinear abscissa.
#' 
#' \code{coo.sample.int} interpolates \code{n} points in \code{coo} along the
#' curvilinear abscissa.
#' 
#' This function circumvent the problem exposed in the Details section of
#' \link{coo.sample}, \emph{i.e.} points are here samples along the "true"
#' curvilear abscissa and not based on \link{seq}.
#' 
#' @usage coo.sample.int(coo, n)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param n \code{integer}. The number of points to sample.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.sample}, \link{coo.sample.rr}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- coo.sample(bot@coo[[1]], 12)
#' coo.plot(coo, points=TRUE)
#' coo.draw(coo.sample.int(coo, 20), col="#B2222255", border=NA)
#' coo.draw(coo.sample(coo, 20),     col="#1874CD55", border=NA)
#' 
#' @export coo.sample.int
coo.sample.int <- function(coo, n){
  if (n < 3) { stop("n must be >= 3") }
  if (is.list(coo)) { coo <- l2m(coo) }
  if (!is.closed(coo)) { coo <- coo.close(coo) }
  orig <- cumsum(coo.perim.pts(coo))
  targ <- seq(0, coo.perim(coo), length=n+1)[-(n+1)]
  res <- matrix(c(coo[1, ], rep(NA, n*2 - 2)), byrow=TRUE,
                nrow=n, ncol=2, dimnames=c(list(paste0("pt", 1:n), c("x", "y"))))
  for (i in 2:n) {
    k <- max(which(orig <= targ[i]))
    r <- (targ[i] - orig[k]) / (orig[k+1]- orig[k])
    res[i, ] <- edi(coo[k, ], coo[k+1, ], r)}
  return(res)}



#' Smoothes list and matrices of coordinates.
#' 
#' \code{coo.smooth} performs \code{n} smoothing iteration on \code{coo}.
#' 
#' 
#' @usage coo.smooth(coo, n)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param n \code{integer}. The number of iterations to perform.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- coo.sample(bot@coo[[1]], 100)
#' coo.plot(coo, points=TRUE, main="Take it not too smooth")
#' s <- seq(10, 1000, length=10)
#' cols <- col.summer(10)
#' for (i in seq(along=s)) {
#'   coo.draw(coo.smooth(coo, s[i]), col=NA, border=cols[i])
#'   }
#' 
#' @export coo.smooth
coo.smooth     <- function(coo, n=0){
  if (is.matrix(coo)) {
    p   <- nrow(coo)
    a   <- 0
    while (a < n) {
      a <- a + 1
      coo.i <- rbind(coo[-1, ], coo[1, ])
      coo.s <- rbind(coo[p, ],  coo[-p, ])
      coo   <- coo/2 + coo.i/4 + coo.s/4}
    return(coo)
  } else if (is.list(coo)){
    coo <- cbind(coo$x, coo$y)
    p   <- nrow(coo)
    a   <- 0
    while (a <= n) {
      a <- a + 1
      coo.i <- rbind(coo[-1, ], coo[1, ])
      coo.s <- rbind(coo[p, ],  coo[-p, ])
      coo   <- coo/2 + coo.i/4 + coo.s/4}
    return(list(x = coo[, 1], y = coo[, 2]))
  } else stop("A list of a coordinate matrix must be provided to coo.smooth")}



#' Closes a list or matrix of coordinates.
#' 
#' \code{coo.close} closes \code{coo}, \emph{i.e.} makes the last coordinates
#' be the first than the first.
#' 
#' 
#' @usage coo.close(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.unclose} and \link{is.closed}
#' @keywords coo Utilities
#' @examples
#' 
#' \dontrun{
#' data(gorf.dat) # we import gorf.data from shapes package
#' coo <- gorf.dat[,,1]
#' is.closed(coo)
#' coo.c <- coo.close(coo)
#' is.closed(coo.c)
#' coo.cu <- coo.unclose(coo)
#' is.closed(coo.cu)
#' }
#' 
#' @export coo.close
coo.close      <- function(coo){
  if (is.matrix(coo)) {
    if (is.closed(coo)) { return(coo) } else { return(rbind(coo, coo[1, ])) }
  } else if (is.list(coo)){
    if (is.closed(coo)) { return(coo) } else {
    return(list(x=c(coo$x, coo$x[1]), y=c(coo$y, coo$y[1])))}
  } else stop("A list or a coordinate matrix must be provided to coo.center")}



#' Uncloses a list or matrix of coordinates.
#' 
#' \code{coo.unclose} tests if \code{coo} is closed, then simply removes the
#' last coordinate.
#' 
#' 
#' @usage coo.unclose(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.close} and \link{is.closed}
#' @keywords coo Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat)
#' coo <- gorf.dat[,,1]
#' is.closed(coo)
#' coo.c <- coo.close(coo)
#' is.closed(coo.c)
#' coo.cu <- coo.unclose(coo)
#' is.closed(coo.cu)
#' 	}
#' 
#' @export coo.unclose
coo.unclose      <- function(coo){
  if(!is.closed(coo)) return(coo)
  if (is.matrix(coo)) {  
    return(coo[-nrow(coo), ])
  } else if (is.list(coo)){
    return(list(x=coo$x[-length(coo$x)], y=coo$y[-length(coo$y)]))
  } else stop("A list or a coordinate matrix must be provided to coo.center")}



#' Tests if a list or matrix of coordinates is closed.
#' 
#' \code{is.closed} tests if the last coordinate of \code{coo} provided as a
#' \code{list} or a \code{matrix} is closed, \emph{i.e.} if the last coordinate
#' is identical as the first.
#' 
#' 
#' @usage is.closed(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return A \code{logical}, either \code{TRUE} or \code{FALSE}.
#' @seealso \link{coo.close} and \link{coo.unclose}.
#' @keywords coo Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat)
#' coo <- gorf.dat[,,1]
#' is.closed(coo)
#' coo.c <- coo.close(coo)
#' is.closed(coo.c)
#' coo.cu <- coo.unclose(coo)
#' is.closed(coo.cu)
#' }
#' 
#' @export is.closed
is.closed <- function(coo){
  if (is.list(coo)) {
    n <- length(coo$x)
    return((coo$x[1] == coo$x[n]) & (coo$y[1] == coo$y[n]))}
  if (is.matrix(coo)) {
    n <- nrow(coo)
    return((coo[1, 1] == coo[n, 1]) & (coo[1, 2] == coo[n, 2]))}}   



#' Utility to identify landmarks on a matrix or list of coordinates.
#' 
#' \code{coo.ldk} allows user to click to identify the closest points that
#' belong to the outline. Typically used to define landmarks.
#' 
#' 
#' @usage coo.ldk(coo, nb.ldk)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param nb.ldk \code{integer}. Indicates the number of landmarks to define.
#' @return A vector of the \code{coo} rows indices that coorespond to the
#' landmarks identified.
#' @seealso \link{defLandmarks}.
#' @keywords coo Utilities
#' @examples
#' 
#' \dontrun{
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.ldk(coo, 2) # you have to click now !
#' }
#' 
#' @export coo.ldk
coo.ldk <- function(coo, nb.ldk) {
  if (is.list(coo)) coo <- l2m(coo)
  coo.plot(coo)
  ldk <- numeric(nb.ldk)
  cat("[")
  for (i in 1:nb.ldk){
    p <- l2m(locator(1))
    l <- apply(coo, 1, function(y) sqrt(sum((p-y)^2)))
    ldk[i] <- which.min(l)
    points(coo[ldk[i], 1], coo[ldk[i], 2], pch=20, col="red")
    cat("*")
    }
  cat("]\n")
  return(ldk)}



#' Calculates the position of the centroid of a list or a matrix of
#' coordinates.
#' 
#' \code{coo.centpos} returns the centroid position of the shape that is the
#' mean of x and y coordinates.
#' 
#' 
#' @usage coo.centpos(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns the \code{(x; y)} coordinates of the centroid.
#' @seealso \link{coo.centsize}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo, cent=FALSE) # we do not plot the centroid
#' cent <- coo.centpos(coo)
#' points(cent[1], cent[2], pch=3, cex=5, col="red")
#' 
#' @export coo.centpos
coo.centpos <- function(coo){
  return(apply(coo, 2, mean))}



#' Calculates the centroid size of a list or matrix of coordinates.
#' 
#' \code{coo.centsize} returns the centroid size of the shape, \emph{i.e.} the
#' square root of the sum of squared distances from each point to the centroid
#' of the shape.
#' 
#' 
#' @usage coo.centsize(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns the centroid size.
#' @seealso \link{coo.centpos}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.centsize(coo)
#' coo.centsize(coo.scale(coo)) # by default coo.scale scales to centroid size.
#' 
#' @export coo.centsize
coo.centsize <- function(coo){
  cent <- coo.centpos(coo)
  return(mean(apply(coo, 1, function(x) sqrt(sum((x-cent)^2)))))}



#' Re-register a list or matrix of coordinates.
#' 
#' \code{coo.baseline} registers \code{coo} on a (new) baseline, \emph{e.g.}
#' Bookstein's coordinates.
#' 
#' \code{coo.baseline} returns by default Bookstein's coordinates.
#' 
#' @usage coo.baseline(coo, ldk1 = 1, ldk2 = 2, t1=c(-0.5, 0), t2=c(0.5, 0))
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param ldk1 Row index of the first reference landmark.
#' @param ldk2 Row index of the second reference landmark.
#' @param t1 (x; y) coordinates of the first target point.
#' @param t2 (x; y) coordinates of the second target point.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords coo Utilities
#' @examples
#' 
#' def.par <- par(no.readonly = TRUE)
#' layout(matrix(1:2, 1, 2))
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo <- coo.sample(coo, 12)
#' coo.plot(coo, main="Some landmarks", points=TRUE)
#' coo.plot(coo.baseline(coo), border="red", col=NA,
#'     points=TRUE, main="Reregistered using Bookstein's coordiantes")
#' abline(v=c(-0.5, 0.5), col="grey60", lty=2)
#' box()
#' par(def.par)
#' 
#' @export coo.baseline
coo.baseline <- function(coo, ldk1=1, ldk2=2, t1=c(-0.5, 0), t2=c(0.5, 0)){
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
  vi <- vecs.param(rx, ry, tx, ty)
  # we rotate accordingly with a center defined as the first landmark (trans, rot, untrans)
  ref <- coo.trans(ref, -t1x, -t1y)
  ref <- ref / vi$r.norms
  ref <- coo.rotate(ref, -vi$d.angle)
  ref <- coo.trans(ref, t1x, t1y)
  return(ref)}



#' Rotates a list or matrix of coordinates with any center of symmetry.
#' 
#' \code{coo.rotate.center} rotates (counter-clockwise) \code{coo} with a
#' \code{theta} angle (in radians) and with the center of symmetry specified by
#' \code{center} .
#' 
#' This function simply centers the shape, rotate it, and then "uncenter" it.
#' 
#' @usage coo.rotate.center(coo, theta, center=c(0, 0))
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param theta \code{numeric}. The angle to rotate the shape.
#' @param center The (x; y) coordinates of the center of rotation.
#' @return A \code{list} with \code{x; y} components or a a matrix of \code{(x;
#' y)}coordinates.
#' @seealso \link{coo.rotate}.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo <- coo.scale(coo.center(bot@coo[[1]]))
#' coo.plot(coo,  main="The bottle's revolution", ylim=c(-10, 50))
#' r <- seq(pi/6, 2*pi, pi/6)
#' cols <- col.summer(12)
#' for (i in seq(along=r)) {
#'   coo.draw(coo.rotate.center(coo, r[i], center=c(0, 20)), border=cols[i], col=NA)  
#'   }
#' abline(v=0, h=0, col="grey60", lty=2)
#' points(0, 20, pch=3, col="red")
#' 
#' 
#' @export coo.rotate.center
coo.rotate.center <- function(coo, theta, center=c(0, 0)){
  coo <- coo.trans(coo, -center[1], -center[2])
  coo <- coo.rotate(coo, theta)
  coo <- coo.trans(coo, center[1], center[2])
  return(coo)}



#' Calculates the perimeter of a list or matrix of coordinates.
#' 
#' \code{coo.perim} returns the sum of the euclidean distances between all
#' successive (x; y) coordinates.
#' 
#' 
#' @usage coo.perim(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns the perimeter length.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo.perim(bot@coo[[1]])
#' 
#' @export coo.perim
coo.perim <- function(coo){
  if (is.list(coo)) { coo <- l2m(coo) }
  n <- nrow(coo)
  perim <- sum(sqrt(apply((coo-coo.slide(coo, n))^2, 1, sum)))
  return(perim)}



#' Calculates the euclidean distance between points of a list or matrix of
#' coordinates.
#' 
#' \code{coo.perim.pts} returns the euclidean distances between all successive
#' (x; y) coordinates.
#' 
#' 
#' @usage coo.perim.pts(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a \code{vector} of euclidean distances.
#' @seealso \link{ed}, \link{coo.perim}.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo.perim.pts(bot@coo[[1]])
#' 
#' @export coo.perim.pts
coo.perim.pts <-  function (coo){
    if (is.list(coo)) {
      coo <- l2m(coo)
    }
    n <- nrow(coo)
    perim <- sqrt(apply((coo - coo.slide(coo, n))^2, 1, sum))
    return(perim)}



#' Forces a list or matrix or coordinates to close.
#' 
#' \code{coo.force2close} force one coo to close, i.e. the difference \eqn{(x_n
#' - x_1)/n} is added to each x-coordinates and similarly for y-coordinates.
#' 
#' 
#' @usage coo.force2close(coo)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo, ylim=c(-200, 1200), xlim=c(0, 500), main="A half bottle forced to close")
#' coo.draw(coo[1:1500,], border="blue", col=NA)
#' coo.draw(coo.force2close(coo[1:1500, ]), border="red", col=NA)
#' 
#' @export coo.force2close
coo.force2close <- function(coo){
  if (is.list(coo)) { coo <- l2m(coo)}
  if (is.closed(coo)) {return(coo)}
  n  <- nrow(coo)
  d  <- coo[1, ] - coo[n, ]
  dm <- cbind(seq(0, d[1], length=n), seq(0, d[2], length=n))
  return(coo+dm)}


# coo plotting functions #############################################



#' Plots a single outline.
#' 
#' \code{coo.plot} is a simple wrapper for plotting shapes. It basically tunes
#' standard \link{plot} function to display single shapes as polygons, within a
#' standardised plotting area.
#' 
#' 
#' @usage coo.plot(coo=NA, col="#F5F5F5", border="#1A1A1A", lwd=1, lty = 1,
#' xlim=c(-1, 1), ylim=c(-1, 1), points=FALSE, first.point=TRUE, centroid=TRUE,
#' points.col=border, pch=1, cex=0.8, main, ...)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param col A color to fill the shape polygon.
#' @param border A color for the shape border.
#' @param lwd The \code{lwd} for drawing shapes.
#' @param lty The \code{lty} for drawing shapes.
#' @param xlim If \code{coo.plot} is called and \code{coo} is missing, then a
#' vector of length 2 specifying the \code{ylim} of the ploting area.
#' @param ylim If \code{coo.plot} is called and \code{coo} is missing, then a
#' vector of length 2 specifying the \code{ylim} of the ploting area.
#' @param points \code{logical}. Whether to display points. If missing and
#' number of points is < 100, then points are plotted.
#' @param first.point \code{logical}. Whether to display the first point.
#' @param centroid \code{logical}. Whether to display centroid.
#' @param points.col The color for plotting points.
#' @param pch The \code{pch} for points.
#' @param cex The \code{cex} for points.
#' @param main \code{character}. A title for the plot.
#' @param ... Additional parameters for drawing the first point.
#' @return No returned value.
#' @seealso \link{coo.draw}.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo.plot(bot@coo[[1]])
#' 
#' @export coo.plot
coo.plot       <- function(coo=NA, col="#F5F5F5",
                           border="#1A1A1A", lwd=1, lty = 1, xlim=c(-1, 1), ylim=c(-1, 1),
                           points=FALSE, first.point=TRUE, centroid=TRUE,
                           points.col=border, pch=1, cex=0.8, main, ...){
  #   rewrite below without the entire par list
  #   op <- par(no.readonly = TRUE)
  #   on.exit(par(op))
  #   if (missing(main)) {
  #     par(mar=c(2.2, 2.2, 0.6, 0.6))
  #   } else {
  #     par(mar=c(2.2, 2.2, 2.2, 1.2))}
  if (is.list(coo)) coo <- l2m(coo)
  if (missing(coo)) {
    plot(NA, asp=1, xlim=xlim, ylim=ylim, ann=FALSE, frame=FALSE,
         cex.axis=0.7, cex=0.7, ...)
  } else {
	if (missing(xlim) & missing(ylim)) {
    plot(coo, type="n", asp=1, ann=FALSE, frame=FALSE,
         cex.axis=0.7, cex=0.7, las=1, ...)
	} else {
	plot(coo, type="n", asp=1, ann=FALSE, frame=FALSE,
       xlim=xlim, ylim=ylim,
	     cex.axis=0.7, cex=0.7, las=1, ...)
	}
    polygon(coo, col=col, border=border, lwd=lwd, lty=lty)
    if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20, ...)}
}
  if ((!missing(coo) & missing(points))) {
    if (nrow(coo)<=100) points(coo, pch=pch, cex=cex, col=points.col)}
  if ((!missing(coo) & points)) {
    points(coo, pch=pch, cex=cex, col=points.col)}
  if (!missing(coo) & centroid) {
    cent <- coo.centpos(coo)
    points(cent[1], cent[2], pch=3, cex=0.5)
    }
  if (!missing(main)) title(main=main)}  



#' Adds a single outline on the current plot.
#' 
#' \code{coo.draw} is a light version of \link{coo.plot} that simply adds a
#' shape on the active plot.
#' 
#' 
#' @usage coo.draw(coo = NA, col = "#70809033", border = "#708090EE", lwd=1,
#' lty = 1, points = FALSE, first.point=TRUE, centroid = FALSE, points.col =
#' border, pch = 20, cex = 0.25, ...)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param col A color to fill the shape polygon.
#' @param border A color for the shape border.
#' @param lwd The \code{lwd} for drawing shapes.
#' @param lty The \code{lty} for drawing shapes.
#' @param points \code{logical}. Whether to display points. If missing and
#' number of points is < 100, then points are plotted.
#' @param first.point \code{logical}. Whether to display the first point.
#' @param centroid \code{logical}. Whether to display centroid.
#' @param points.col The color for plotting points.
#' @param pch The \code{pch} for points.
#' @param cex The \code{cex} for points.
#' @param ... Additional parameters for drawing the first point.
#' @return No returned value.
#' @seealso \link{coo.plot}.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo.plot(bot@coo[[1]])
#' coo.plot(bot@coo[[2]], lwd=1.2)
#' 
#' @export coo.draw
coo.draw <-
  function (coo = NA, col = "#70809033", border = "#708090EE", lwd=1, lty=1,
            points = FALSE, first.point=TRUE, centroid = FALSE,
            points.col = border, pch = 20, cex = 0.25,  ...)
  {
    if (is.list(coo)) {coo <- l2m(coo)}
    polygon(coo, col = col, border = border, lwd=lwd, lty=lty)
    if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20, ...)}
    if (points) {
      points(coo, pch = pch, cex = cex, col = points.col)
    }
    if (centroid) {
      cent <- coo.centpos(coo)
      points(cent[1], cent[2], pch=3, cex=0.5)}
  }



#' "Templates" list and matrix of coordinates.
#' 
#' \code{coo.template} returns \code{coo} so that the shape it is centered on
#' the origin and inscribed in a size-side square, also centered on the origin;
#' see \link{coo.list.panel} for an illustration of this function.
#' 
#' 
#' @usage coo.template(coo, size)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param size \code{numeric}. Indicates the length of the side "inscribing"
#' the shape.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{coo.list.panel}.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo.template(coo), xlim=c(-1, 1), ylim=c(-1, 1))
#' rect(-0.5, -0.5, 0.5, 0.5)
#' 
#' s <- 0.01
#' coo.plot(coo.template(coo, s))
#' rect(-s/2, -s/2, s/2, s/2)
#' 
#' 
#' 
#' @export coo.template
coo.template   <- function(coo, size=1) {
  # only for matrices
  coo      <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift    <-  expected - observed
  return(coo.trans(coo, shift[1], shift[2]))}



#' Plots sets of shapes.
#' 
#' \code{coo.list.panel} plots a list of shapes if passed with a list of
#' coordinates. Outlines are templated and on the same graphical window with
#' the help of \link{coo.template}.
#' 
#' 
#' @usage coo.list.panel(coo.list, dim, byrow = TRUE, fromtop = TRUE, mar =
#' rep(0, 4), cols, borders, density = NULL, angle = 45)
#' @param coo.list A \code{list} of coordinates, such as those in the
#' \code{@coo} slot of \code{Coo} objects.
#' @param dim A \code{vector} of the form \code{(nb.row, nb.cols)} to specify
#' the panel display. If missing, shapes are arranged in a square.
#' @param byrow \code{logical}. Whether to succesive shape by row or by col.
#' @param fromtop \code{logical}. Whether to display shapes from the top of the
#' plotting region.
#' @param mar A \code{vector} to define margins.
#' @param cols A \code{vector} of colors to fill shapes.
#' @param borders A \code{vector} of colors to draw shape borders.
#' @param density A \code{vector} for density of shading lines. See
#' \link{polygon}
#' @param angle A \code{vector} for shading lines.
#' @return Returns (invisibly) a \code{data.frame} with position of shapes that
#' can be used for other sophisticated plotting design.
#' @seealso \link{coo.plot} and \link{coo.template}.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo.list.panel(bot@coo)
#' x <- coo.list.panel(bot@coo)
#' x # positions of shapes returned invisibly 
#' # axis(1) ; axis(2) # that's a single graphical window ;)
#' 
#' @export coo.list.panel
coo.list.panel <- function(coo.list, dim, byrow=TRUE,
                           fromtop=TRUE, mar=rep(0, 4),
                           cols, borders, density = NULL, angle = 45){
  if (class(coo.list[[1]])=="list") coo.list <- lapply(coo.list, l2m)
  # if dim is missing, we define a square
  n <- length(coo.list)
  if(missing(dim)) {
    nc    <- ceiling(sqrt(n))
    nr    <- ceiling(n/nc)
    dim   <- c(nr, nc)}
  k       <- dim[1]*dim[2]
  if (k < n) stop("dim[1]*dim[2] must be >= the length of coo.list")
  pos <- matrix(1:k, dim[1], dim[2], byrow=byrow)
  if (fromtop & dim[1]>1) { pos <- pos[dim[1]:1,] }
  # we prepare the panel
  op <- par("mar")
  par(mar=mar)
  plot(NA, asp=1,
       xlim=c(0, dim[2]),
       ylim=c(0, dim[1]),
       xaxs="i", yaxs="i", frame=FALSE, ann=FALSE, axes=FALSE)
  # we template and plot shapes
  coo.tp  <- lapply(coo.list, coo.template, size=0.9)
  if (missing(cols))    { cols      <- rep("grey80", n) }
  if (missing(borders)) { borders   <- rep("grey20", n) }
  if (missing(density)) { density   <- rep(NULL, n) }
  if (missing(angle))   { angle     <- rep(45, n) }
  res <- data.frame(pos.x=numeric(), pos.y=numeric())
  for (i in 1:n){
    trans <- which(pos==i, arr.ind=TRUE) - 0.5
    res[i, ] <- c(trans[2], trans[1])
    polygon(coo.tp[[i]][, 1] + trans[2],
            coo.tp[[i]][, 2] + trans[1],
            col=cols[i], border=borders[i], density = density[i], angle = angle[i])
  }
  par(mar=op)
  invisible(res)}



#' Momocs' "oscilloscope" for periodic functions.
#' 
#' Shape analysis deals with curve fitting, whether \eqn{x(t)} and \eqn{y(t)}
#' positions along the curvilinear abscissa or radius/tangent angle variation.
#' We may need to represent these single or double periodic functions that are
#' ajusted by Fourier-based method. \code{coo.oscillo} and \code{coo.oscillo1}
#' compute and provide standardized plot when given a matrix of coordinates or
#' a vector, respectively. These functions are mainly used for development
#' purpose but are included in the package.
#' 
#' 
#' @aliases coo.oscillo coo.oscillo1
#' @usage coo.oscillo(coo, method = c("d0", "di")[1], plot = TRUE, rug = TRUE,
#' legend = TRUE, cols = col.gallus(2), ref=FALSE, ref.nb=8, ...)
#' 
#' coo.oscillo1(coo, method = c("d0", "di")[1], plot = TRUE, rug = TRUE, legend
#' = TRUE, cols = col.gallus(1), xlab = "Points sampled along the outline",
#' ylab = "Deviation", ...)
#' @param coo A list or a matrix of coordinates.
#' @param method \code{character}. Whether to calculate differences with the
#' first point (\code{"d0"}) or the previous (\code{"di"}), ie the derivate.
#' @param plot Whether to plot the results.
#' @param rug \code{logical}. Whether to display a pseudo rug, that indicate if
#' the derivate is positive.
#' @param legend \code{logical}. Whether to add a legend.
#' @param cols A \code{vector} of two (for \code{coo.oscillo}) or a single
#' (\code{coo.oscillo1}) color for lines.
#' @param ref \code{logical}. Whether to display the original shape besides the
#' oscillo.
#' @param ref.nb \code{integer}. The number or reference points, sampled
#' equidistantly along the curvilinear abscissa and added on the oscillo
#' curves.
#' @param xlab \code{character}. Alternate label for x-axis.
#' @param ylab \code{character}. Alternate label for y-axis.
#' @param ... Additional parameters than can be passed to lines.
#' @return Returns a \code{list} with two or one component(s), giving the
#' difference calculated.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' coo.oscillo(bot@coo[[1]], lty=2)
#' coo.oscillo1(tfourier(bot@coo[[1]], 24)$phi)
#' 
#' @export coo.oscillo
coo.oscillo    <- function(coo, method=c("d0", "di")[1], plot=TRUE,
                           rug=TRUE, legend=TRUE, cols=col.gallus(2),
                           ref=FALSE, ref.nb=8, ...){
  if (is.list(coo)) coo <- l2m(coo)
  nr <- nrow(coo)
  if (method=="d0"){
    dx <- coo[, 1] - coo[1, 1]
    dy <- coo[, 2] - coo[1, 2]
  } else {
    if (method=="di"){
      dx <- coo[, 1] - coo[, 1][c(nr, (1:nr - 1))]
      dy <- coo[, 2] - coo[, 2][c(nr, (1:nr - 1))]
    } else {stop("inappropriate method in oscillo")}}
  if (ref & plot) {
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    layout(matrix(1:2, ncol=2), widths=c(1, 2))
    refs <- round(seq(1, nrow(coo), length=ref.nb+1)[-(ref.nb+1)])
    coo.plot(coo)
    text(coo[refs, 1], coo[refs, 2], labels=as.character(1:ref.nb), cex=0.7)
    }
  if (plot) {
    ry <- max(abs(range(c(dx, dy))))
    plot(NA, xlim=c(1, nr), xlab="Points sampled along the outline",
         ylim=c(-ry, ry)*1.1,   ylab="Deviation",
         xaxs="i", las=1, col=cols[1])
    lines(dx, col=cols[1], ...)
    lines(dy, col=cols[2], ...)
    abline(h=0, col="grey80", lty=2)
    if (method=="d0" & rug) {
      dx.i <- coo[, 1] - coo[, 1][c(nr, (1:nr - 1))]
      dy.i <- coo[, 2] - coo[, 2][c(nr, (1:nr - 1))]
      axis(3, at=(1:nr)[dx.i>0], line=-0.5, labels=FALSE, col=cols[1])
      axis(3, at=(1:nr)[dy.i>0], line=-1, labels=FALSE, col=cols[2])
    }
    if (method=="d0" & legend) {
      legend("bottomright", legend = c("xi - x0", "yi - y0"),
             col = cols, bg="#FFFFFFCC", cex=0.7, lty = 1, lwd=1, inset=0.005)}
    if (method=="di" & legend) {
      legend("bottomright", legend = c("dx", "dy"),
             col = cols, bg="#FFFFFFCC", cex=0.7, lty = 1, lwd=1, inset=0.005)}
    if (ref){
      #text((1:nr)[refs], dx[refs], labels=as.character(1:ref.nb), cex=0.7, col=cols[1])
      #text((1:nr)[refs], dy[refs], labels=as.character(1:ref.nb), cex=0.7, col=cols[2])
      text((1:nr)[refs], 0, labels=as.character(1:ref.nb), cex=0.7)
      }
    }
  return(list(x=dx, y=dy))}

coo.oscillo1    <- function(coo, method=c("d0", "di")[1], plot=TRUE,
                            rug=TRUE, legend=TRUE, cols=col.gallus(1), 
                            xlab="Points sampled along the outline", ylab="Deviation", ...){
  if (!is.numeric(coo)) stop("A numeric must be provided to coo.oscillo1")
  nr <- length(coo)
  if (method=="d0"){
    dx <- coo - coo[1]
  } else {
    if (method=="di"){
      dx <- coo - coo[c(nr, (1:nr - 1))]
    } else {stop("inappropriate method in oscillo")}}
  if (plot) {
    plot(dx, type="l", ylim=range(dx)*1.1,
         xlab=xlab, ylab=ylab, las=1, col=cols, ...)
    abline(h=0, col="grey80", lty=2)
    if (method=="d0" & rug) {
      dx.i <- coo - coo[c(nr, (1:nr - 1))]
      axis(3, at=(1:nr)[dx.i>0], line=-0.5, labels=FALSE, col=cols[1])
    }
    if (method=="d0" & legend) {
      legend("bottomright", legend = "xi - x0",
             col = cols, bg="#FFFFFFCC", cex=0.7, lty = 1, lwd=1, inset=0.005)}
    if (method=="di" & legend) {
      legend("bottomright", legend = "dx",
             col = cols, bg="#FFFFFFCC", cex=0.7, lty = 1, lwd=1, inset=0.005)}}
  return(list(x=dx))}

coo.ef.amplify <- function(coo, amp=rep(0.5, 4), nb.h=5, draw=FALSE, ...){
  if (is.list(coo)) {coo <- l2m(coo)}
  if (length(amp) == 1) {amp <- rep(amp, 4)}
  if (missing(nb.h)) {nb.h <- floor(dim(coo)[1]/2)-1}
  coo.ef <- efourier(coo, nb.h=nb.h, smooth.it=0)
  coo.ef.amp <- ef.amplify(coo.ef, amp=amp)
  coo.amp <- efourier.i(coo.ef.amp, nb.pts=nrow(coo))
  if (draw) {
    coo.draw(coo.amp, ...) }
  return(coo.amp)}

#  A utilities & plotting ############################################


#' Calculates the mean shape of an array of landmarks.
#' 
#' \code{A.mshape} returns the mean shape of an array of landmarks when
#' provided with landmarks as rows, \eqn{(x; y)} coordinates as columns, and
#' individuals as the third dimension of the array.
#' 
#' 
#' @usage A.mshape(A)
#' @param A An \code{array} of \eqn{(x; y)} coordinates, typically, landmarks.
#' @return Returns a matrix of \eqn{(x; y)} coordinates.
#' @seealso \link{A.points}, \link{A.segments}, \link{A.plot} \link{coo.plot},
#' and the others coo.utilities.
#' @keywords Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat) # we import gorf.data from shapes package
#' A.plot(gorf.dat, pch=20)
#' m <- A.mshape(gorf.dat)
#' points(m, pch=5)
#' 	}
#' 
#' @export A.mshape
A.mshape<-function(A){apply(A, c(1,2), mean)}



#' Plots an array of homologous coordinates, typically landmarks.
#' 
#' \code{A.plot} is used to plot an array of landmarks when provided with
#' landmarks as rows, \eqn{(x; y)} coordinates as columns, and individuals as
#' the third dimension of the array.
#' 
#' 
#' @usage A.plot(A, col, palette = col.summer, xlim, ylim, border.col = NA,
#' border.lty = 2, pch = 3, cex = 1)
#' @param A An \code{array} of \eqn{(x; y)} coordinates, typically, landmarks.
#' @param col A vector of colors to use for drawing landmarks.
#' @param palette If \code{col} is not provided, then the color palette to use
#' for landmarks.
#' @param xlim A vector of length 2 specifying a custom \code{xlim} of the
#' ploting area.
#' @param ylim A vector of length 2 specifying a custom \code{ylim} of the
#' ploting area.
#' @param border.col A color for these segments.
#' @param border.lty A \code{lty} for these segments.
#' @param pch A \code{pch} for points.
#' @param cex A \code{cex} for points.
#' @return No returned value.
#' @seealso \link{A.points}, \link{A.segments}, \link{A.mshape}
#' \link{coo.plot}, and the others coo.utilities.
#' @keywords Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat) # we import gorf.data from shapes package
#' A.plot(gorf.dat, pch=20)
#' 	}
#' 
#' @export A.plot
A.plot <- function(A, col, palette=col.summer, xlim, ylim,
                  border.col=NA, border.lty=2, pch=3, cex=1){
  if (!is.array(A)) {stop("An array must be provided.")}
  if (!is.array(A)) {stop("An 3 dimension array must be provided.")}
  if (missing(col)) {
    cols <- palette(dim(A)[1])
  } else {
    cols <- rep(col, dim(A)[1])
  }
  if (missing(xlim) & missing(ylim)) {
    wdw  <- apply(apply(A, 1:2, range), 3, range)
    xlim <- wdw[, 1]
    ylim <- wdw[, 2]}
  plot(NA, xlim=xlim, ylim=ylim, asp=1, ann=FALSE, frame=FALSE, cex.axis=0.7)
  A.segments(A, border.col=border.col, border.lty=border.lty)
  be.quiet <- apply(A, 3, points, pch=pch, cex=cex, col=cols)}



#' Plots the points that corresponds to landmarks provided as an array.
#' 
#' \code{A.points} displays landmarks when provided as an array with landmarks
#' as rows, \eqn{(x; y)} coordinates as columns, and individuals as the third
#' dimension of the array.
#' 
#' 
#' @usage A.points(A, col, palette=col.summer, pch=3, cex=1)
#' @param A An \code{array} of \eqn{(x; y)} coordinates, typically, landmarks.
#' @param col A vector of colors to use for drawing landmarks.
#' @param palette If \code{col} is not provided, then the color palette to use
#' for landmarks.
#' @param pch A \code{pch} for points.
#' @param cex A \code{cex} for points.
#' @return No returned value.
#' @seealso \link{A.segments}, \link{A.mshape}, \link{A.plot} \link{coo.plot},
#' and the others coo.utilities.
#' @keywords Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat) # we import gorf.data from shapes package
#' A.points(gorf.dat)
#' 	}
#' 
#' @export A.points
A.points <- function(A, col, palette=col.summer, pch=3, cex=1){
  if (!is.array(A)) {stop("An array must be provided.")}
  if (!is.array(A)) {stop("An 3 dimension array must be provided.")}
  if (missing(col)) {
    cols <- palette(dim(A)[1])
  } else {
    cols <- rep(col, dim(A)[1])
  }
  be.quiet <- apply(A, 3, points, pch=pch, cex=cex, col=cols)}




#' Utilities to manipulate arrays of homologous coordinates, typically
#' landmarks.
#' 
#' \code{A.segments} draws interlandmarks segments as an array with landmarks
#' as rows, \eqn{(x; y)} coordinates as columns, and individuals as the third
#' dimension of the array.
#' 
#' 
#' @usage A.segments(A, col=NA, border.col="grey60", border.lty=1)
#' @param A An \code{array} of \eqn{(x; y)} coordinates, typically, landmarks.
#' @param col A vector of colors to use for drawing landmarks.
#' @param border.col A color for these segments.
#' @param border.lty A \code{lty} for these segments.
#' @return No returned value.
#' @seealso \link{A.plot}, \link{A.points}, \link{A.mshape} \link{coo.plot},
#' and the others coo.utilities.
#' @keywords Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat) # we import gorf.data from shapes package
#' A.segments(gorf.dat, border.lty=2, border.col="grey95")
#' m <- A.mshape(gorf.dat)
#' points(m, pch=5)
#' 	}
#' 
#' @export A.segments
A.segments <- function(A, col=NA, border.col="grey60", border.lty=1){
  if (!is.array(A)) {stop("An array must be provided.")}
  if (!is.array(A)) {stop("An 3 dimension array must be provided.")}
  be.quiet <- apply(A, 3, coo.draw, col=col, border=border.col, lty=border.lty)}

# Other utilities and class converters ###############################



#' Converts a list of coordinates to a matrix.
#' 
#' \code{l2m} converts a \code{list} with x and y components to a 2-col
#' \code{matrix} of coordinates.
#' 
#' 
#' @usage l2m(l)
#' @param l A \code{list} with x and y coordinates as components.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @seealso \link{m2l}.
#' @keywords coo Utilities
#' @examples
#' 
#' l <- list(x=1:5, y=5:1)
#' l2m(l)
#' 
#' 
#' @export l2m
l2m            <- function(l) {return(cbind(l$x, l$y))}



#' Convert a matrix of coordinates to a list of coordinates.
#' 
#' \code{m2l} converts a matrix of \code{(x; y)}coordinates to a list with
#' \code{x; y} components.
#' 
#' 
#' @usage m2l(m)
#' @param m A 2-columns \code{matrix} containing x and y coordinates.
#' @return Returns a \code{list} with \code{x; y} components.
#' @seealso \link{l2m}.
#' @keywords coo Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat)
#' m2l(gorf.dat[,,1])
#' }
#' 
#' @export m2l
m2l            <- function(m) {return(list(x=m[,1], y=m[,2]))}



#' Calculates euclidean distance between two points.
#' 
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates. This function is used internally but may be of
#' interest for other analyses.
#' 
#' 
#' @usage ed(pt1, pt2)
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm.nearest}, \link{dist}.
#' @keywords Utilities
#' @examples
#' 
#' ed(c(0,1), c(1,0)) # sqrt 2
#' 
#' @export ed
ed             <- function(pt1, pt2){return(sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2))}



#' Calculates euclidean intermediate between two points.
#' 
#' \code{edi} simply calculates coordinates of a points at the relative
#' distance \code{r} on the \code{pt1-pt2} defined by their (x; y) coordinates.
#' This function is used internally but may be of interest for other analyses.
#' 
#' 
#' @usage edi(pt1, pt2, r = 0.5)
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @param r the relative distance from \code{pt1} to \code{pt2}.
#' @return Returns the (x; y) interpolated coordinates.
#' @seealso \link{ed}, \link{edm}.
#' @keywords Utilities
#' @examples
#' 
#' edi(c(0,1), c(1,0), r = 0.5)
#' 
#' @export edi
edi <- function(pt1, pt2, r=0.5){
  return(r*(pt2-pt1) + pt1) }



#' Calculates euclidean distance every pairs of points in two matrices.
#' 
#' \code{edm} returns the euclidean distances between points \deqn{1 -> n} of
#' two 2-col matrices of the same dimension. This function is used internally
#' but may be of interest for other analyses.
#' 
#' If one wishes to align two (or more shapes) Procrustes surimposition may
#' provide a better solution.
#' 
#' @usage edm(m1, m2)
#' @param m1 The first \code{matrix} of coordinates.
#' @param m2 The second \code{matrix} of coordinates.
#' @return Returns a \code{vector} of euclidean distances between pairwise
#' coordinates in the two matrices.
#' @seealso \link{ed}, \link{edm.nearest}, \link{dist}.
#' @keywords Utilities
#' @examples
#' 
#' x <- matrix(1:10, nc=2)
#' edm(x, x)
#' edm(x, x+1)
#' 
#' @export edm
edm            <- function(m1, m2){return(sqrt((m1[, 1] - m2[, 1])^2 + (m1[, 2] - m2[, 2])^2))}



#' Calculates the shortest euclidean distance found for every point of one
#' matrix among those of a second.
#' 
#' 
#' \code{edm.nearest} calculates the shortest euclidean distance found for
#' every point of one matrix among those of a second. In other words, if
#' \code{m1, m2} have \code{n} rows, the result will be the shortest distance
#' for the first point of \code{m1} to any point of \code{m2} and so on,
#' \code{n} times. This function is used internally but may be of interest for
#' other analyses.
#' 
#' So far this function is quite time consumming since it performs \deqn{ n
#' \times n } euclidean distance computation.  If one wishes to align two (or
#' more shapes) Procrustes surimposition may provide a better solution.
#' 
#' @usage edm.nearest(m1, m2, full=FALSE)
#' @param m1 The first \code{list} or \code{matrix} of coordinates.
#' @param m2 The second \code{list} or \code{matrix} of coordinates.
#' @param full \code{logical}. Whether to returns a condensed version of the
#' results.
#' @return If \code{full} is \code{TRUE}, returns a \code{list} with two
#' components: \code{d} which is for every point of \code{m1} the shortest
#' distance found between it and any point in \code{m2}, and \code{pos} the
#' (\code{m2}) row indices of these points. Otherwise returns \code{d} as a
#' numeric vector of the shortest distances.
#' @seealso \link{ed}, \link{edm}, \link{dist}.
#' @keywords Utilities
#' @examples
#' 
#' x <- matrix(1:10, nc=2)
#' edm.nearest(x, x+rnorm(10))
#' edm.nearest(x, x+rnorm(10), full=TRUE)
#' 
#' 
#' @export edm.nearest
edm.nearest <- function(m1, m2, full=FALSE){
  if (!is.matrix(m1) | !is.matrix(m2)) stop("Matrices must be provided")
  if (ncol(m1)!=2    | ncol(m2)!=2)    stop("2-cols matrices must be provided")
  nr <- nrow(m1)
  pos <- d  <- numeric(nr)
  for (i in 1:nr){
    m1.i   <- m1[i, ]
    di     <- apply(m2, 1, function(x) sqrt(sum((x - m1.i)^2)))
    d[i]   <- min(di)
    pos[i] <- which.min(di)}
  if (full) return(list(d=d, pos=pos)) else return(d) }





#' Converts a list of coordinates to an array.
#' 
#' \code{l2a} converts a list of \code{k} matrices with n-rows and n-col
#' matrices to a \code{m x n x k} array.
#' 
#' 
#' @usage l2a(l)
#' @param l A \code{list} of matrices of the same dimension.
#' @return An array of coordinates.
#' @seealso \link{a2l}.
#' @keywords coo Utilities
#' @examples
#' 
#' 	\dontrun{
#' data(gorf.dat)
#' l <- a2l(gorf.dat)
#' a <- l2a(l)
#' A.plot(a)
#' 	}
#' 
#' @export l2a
l2a            <- function(l){return(array(unlist(l), dim=c(nrow(l[[1]]), ncol(l[[1]]), length(l))))}


#' Converts an array of coordinates to a list.
#' 
#' \code{a2l} converts an array of coordinates into a list of 2-cols matrices.
#' 
#' 
#' @usage a2l(a)
#' @param a An \code{array} of coordinates.
#' @return A \code{list} with 2-cols matrices of \code{(x; y)} coordinates.
#' @seealso \link{l2a}
#' @keywords coo Utilities
#' @examples
#' 
#' #data(gorf.dat) # we import gorf.data from shapes package
#' #l <- a2l(gorf.dat)
#' #a <- l2a(l)
#' #A.plot(a)
#' 
#' @export a2l
a2l <- function(a){
  if (!is.array(a)) stop("An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {l[[i]] <- a[,,i]}
  return(l)}



#' Draws colored segments from a matrix of coordinates.
#' 
#' Given a matrix of (x; y) coordinates, draws segments between every points
#' defined by the row of the matrix and uses a color to display an information.
#' 
#' 
#' @usage dev.segments(coo, cols, lwd = 1)
#' @param coo A matrix of coordinates.
#' @param cols A vector of color of \code{length = nrow(coo)}.
#' @param lwd The \code{lwd} to use for drawing segments.
#' @keywords Utilities
#' @examples
#' 
#' # we load some data
#' data(bot)
#' guinness <- bot[9]
#' 
#' # we calculate the best possible outline and one with 12 harm.
#' out.best <- l2m(efourier.i(efourier(guinness, nb.h=-1), nb.pts=120))
#' out.12   <- l2m(efourier.i(efourier(guinness, nb.h=12), nb.pts=120))
#' 
#' # we calculate deviations, you can also try 'edm'
#' dev <- edm.nearest(out.12, out.best) / coo.centsize(out.12)
#' 
#' # we prepare the color scale
#' d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
#' cols  <- paste0(col.summer(20)[d.cut], "CC")
#' 
#' # we draw the results
#' coo.plot(out.best, border="black", col="grey80", main="Guiness fitted by 20 harm.")
#' dev.segments(out.12, cols=cols, lwd=4)
#' 
#' 
#' 
#' 
#' @export dev.segments
dev.segments <-function(coo, cols, lwd=1){
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i+1, 1], coo[i+1, 2],
             col=cols[i], lwd=lwd)}}



#' Helps to select a given number of harmonics from a numerical vector.
#' 
#' \code{coeff.sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{@coeff} slot of \code{Coe}-objects in
#' that way: \deqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{eFourier} and
#' \link{efourier}) while \deqn{C_n and D_n} harmonic are absent for radii
#' variation and tangent angle approaches (see \link{rfourier} and
#' \link{tfourier} respectively). . This function is used internally but might
#' be of interest elwewhere.
#' 
#' 
#' @usage coeff.sel(retain = 8, drop = 0, nb.h = 32, cph = 4)
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff.sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff.split} returns a named list
#' of coordinates.
#' @keywords Utilities
#' @examples
#' 
#' coeff.sel(retain=8, drop=0, nb.h=12)        #efourier 
#' coeff.sel(retain=8, drop=0, nb.h=12, cph=2) #r/tfourier
#' 
#' # if you want to export the matrix of coefficients but only the first 6 colums.
#' data(bot)
#' bot.f <- eFourier(bot, nb.h=12)
#' bot.f@coe[, coeff.sel(6, 0, 12)]
#' 
#' @export coeff.sel
coeff.sel <- function(retain=8, drop=0, nb.h=32, cph=4){
  cs <- numeric()
  for (i in 1:cph) {
    cs <- c(cs, (1+drop):retain + nb.h*(i-1))}
  return(cs)}



#' Converts a numerical description of harmonic coefficients to a named list.
#' 
#' \code{coeff.split} returns a named list of coordinates from a vector of
#' harmonic coefficients. For instance, harmonic coefficients are arranged in
#' the \code{@coeff} slot of \code{Coe}-objects in that way: \deqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{eFourier} and \link{efourier}) while \deqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#' 
#' 
#' @usage coeff.split(cs, nb.h = 8, cph = 4)
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @keywords Utilities
#' @examples
#' 
#' coeff.split(1:128, nb.h=32, cph=4) # efourier
#' coeff.split(1:64, nb.h=32, cph=2)  # t/r fourier
#' 
#' 
#' @export coeff.split
coeff.split <- function(cs, nb.h=8, cph=4){
  if (missing(nb.h)) {nb.h <- length(cs)/cph }
  cp <- list()
  for (i in 1:cph) {
    cp[[i]] <- cs[1:nb.h + (i-1)*nb.h]
  }
  names(cp) <- paste(letters[1:cph], "n", sep="")
  return(cp)}

# Import section #####################################################


#' Everything to convert \code{.txt} files to a list of coordinates.
#' 
#' Takes a path list to \code{.txt} files and returns a list of coordinates
#' arranged as matrices that can be passed to the \code{Coo} builder.
#' 
#' \code{.txt} files must have coordinates arranged in two columns (\emph{i.e.}
#' x and y) with no header. The list of paths pointing to \code{.txt} files
#' must be \emph{full} if your working directory is different from your folder
#' containing images, see \bold{Examples} below.
#' 
#' @usage import.txt(txt.list, ...)
#' @param txt.list a list of full/relative path to \code{.txt} files. (see
#' \bold{Details}).
#' @param ... Complementary arguments to be passed to \code{read.table} within
#' the function.
#' @return A \code{list} of matrices of coordinates.
#' @seealso \link{import.jpg}.
#' @keywords Import
#' @examples
#' 
#' \dontrun{
#' txt.list <- list.files(path_to_your_folder_containing_.txt_files, full=TRUE)
#' # altenartively :
#' # setwd(path_to_your_folder_containing_.txt_files)
#' # txt.list <- list.files()
#' I <- import.txt(txt.list)
#' Coo(I)
#' I # that should be a Coo object
#' }
#' 
#' @export import.txt
import.txt <- function(txt.list, ...){
  cat("Extracting", length(txt.list), ".jpg outlines...\n")
  if (length(txt.list) > 10) {
    pb <- txtProgressBar(1, length(txt.list))
    t <- TRUE } else {t <- FALSE}
  res <- list()
  for (i in seq(along = txt.list)) {
    coo <- read.table(txt.list[i], ...)
    res[[i]] <- as.matrix(coo)
    if (t) setTxtProgressBar(pb, i)
  }
  names(res) <- substr(txt.list, start=1, stop=nchar(txt.list)-4)
  return(res)}



#' Everything to convert images to a list of coordinates.
#' 
#' Functions to import \code{.jpg} images and convert them to list of
#' coordinates.
#' 
#' Typically, an object returned by \link{list.files} on a folder containing
#' your images is passed to \code{import.jpg}. \code{import.img.prepare} and
#' \code{import.img.Conte} are typically not used by front-user but internally
#' by \code{import.jpg}. They clean, binarize, threshold, etc. raw .jpg images
#' and extract a list of coordinates from a black and white \code{imagematrix},
#' respectively. The best option is to work with black and white \code{.jpg}
#' image with the black mask of the outline overlapping the center of the
#' image. \code{import.multi1.jpg} helps to extract several outlines from the
#' same \code{.jpg} image.
#' 
#' Due to troubles with ReadImages and the recent change towards the jpeg
#' package for import of images, they MUST be converted to black and white
#' (i.e. 8-bits and grey levels ) images before being imported.
#' 
#' If you get this message error : "Error in img[1, ] : incorrect number of
#' dimensions", try converting all your images to 8-bit mode, grey levels, and
#' you can also apply a threshold (128 is fine) so that you only have black and
#' white pixels, not grey. Then, save your images without compression. This can
#' be done using automated scripts in editing softwares.
#' 
#' @aliases import.jpg import.multi1.jpg import.img.prepare import.img.Conte
#' @usage import.jpg(jpg.list) import.multi1.jpg(path) import.img.prepare(path)
#' import.img.Conte(img, x, auto=TRUE, plot=TRUE)
#' @param jpg.list A \code{vector} of \code{character} containing the path to
#' your \code{.jpg} images.
#' @param path \code{character}. A single path to a \code{.jpg} image.
#' @param img an \code{imagematrix} object.
#' @param x A \code{vector} of \eqn{(x; y)} coordinates from where to start
#' Conte algorithm.
#' @param auto \code{logical}. Whether to try or not to start at the center of
#' the image(s) before asking the user to click within the shape.
#' @param plot \code{logical}. Whether to plot or not the image. Used
#' internally by \code{import.multi1.jpg} to not reload the same image.
#' @return \code{import.jpg} returns a list of (from 1 to thousands) \eqn{(x;
#' y)} coordinates arranged as matrices and that can be then converted to a
#' \code{Coo}-object. \code{import.multi1.jpg} returns a \code{list} of
#' \eqn{(x; y)} coordinates. \code{import.img.prepare} returns an
#' \code{imagematrix} object, \code{import.img.Conte} returns a matrix of
#' \eqn{(x; y)} coordinates.
#' @seealso \link{import.txt}.
#' @keywords Import
#' @examples
#' 
#' \dontrun{
#' jpg.list <- list.files(path_to_your_folder_containing_.txt_files, full=TRUE)
#' I <- import.jpg(jpg.list)
#' Coo(I)
#' }
#' 
#' @export import.jpg
import.jpg <- function(jpg.list) {
  cat("Extracting", length(jpg.list), ".jpg outlines...\n")
  if (length(jpg.list) > 10) {
    pb <- txtProgressBar(1, length(jpg.list))
    t <- TRUE } else {t <- FALSE}
  res <- list()
  for (i in seq(along=jpg.list)) {
    img <- import.img.prepare(jpg.list[i])
    res[[i]] <- import.img.Conte(img)
    if (t) setTxtProgressBar(pb, i)
  }
  names(res) <- substr(jpg.list, start=1, stop=nchar(jpg.list)-4) 
  return(res)}

import.multi1.jpg <- function(path){
  img <- import.img.prepare(path)
  res <- list()
  on.exit(return(res))
  i <- 1
  res[[i]] <- import.img.Conte(img, auto=FALSE)
  cat("1 outline extracted. Press 'ESC' to finish.\n")
  while (TRUE) {
    i <- i+1
    res[[i]] <- import.img.Conte(img, auto=FALSE, plot=FALSE)
    cat(paste(i, "outlines extracted.\n"))
  }  
return(res)}

import.img.prepare <- function(path){
  img <- readJPEG(path)
  #if (class(img)[2] == "array") {img <- rgb2grey(img)} #to2fixed...ReadImages
  img[img >  0.5] <- 1
  img[img <= 0.5] <- 0
  #img <- imagematrix(img) #to2fixed...ReadImages
  # if(any(dim(img)>500)) {cat("\t(large image)")}
  if(any(c(img[1, ], img[nrow(img), ], img[, 1], img[, ncol(img)]) != 1)){
    # cat("\t(outline spans image border)")
    img <- rbind(rep(1, ncol(img)), img, rep(1, ncol(img)))
    img <- cbind(rep(1, nrow(img)), img, rep(1, nrow(img)))
    #img <- imagematrix(img) #to2fixed...ReadImages
  }              
  return(img)}

import.img.Conte <- 
  function (img, x, auto=TRUE, plot=TRUE) 
  {
    #if (class(img)[1] != "imagematrix") {
    #  stop("An 'imagematrix' object is expected")}2befixe...ReadImages
    img <- t(img[nrow(img):1,])
    if (missing(x)) {
      if (auto) {
        x <- round(dim(img)/2)
      } else { x <- c(1, 1)}}
    while (img[x[1], x[2]] != 0) {
      if (plot) {
        plot(img, main = "Click a point within the shape")
        rect(0, 0, ncol(img), nrow(img), border = "red")}
      click <- lapply(locator(1), round)
      x <- c(nrow(img) - click$y, click$x)
      if (any(x > dim(img))) {
        x <- round(dim(img)/2)
      }
    }
    while (abs(img[x[1], x[2]] - img[x[1], (x[2] - 1)]) < 0.1) {
      x[2] <- x[2] - 1
    }
    a <- 1
    M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, 
                  -1, 0, 1), 2, 8, byrow = TRUE)
    M <- cbind(M[, 8], M, M[, 1])
    X <- 0
    Y <- 0
    x1 <- x[1]
    x2 <- x[2]
    SS <- NA
    S <- 6
    while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
      if (abs(img[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - 
        img[x[1], x[2]]) < 0.1) {
        a <- a + 1
        X[a] <- x[1]
        Y[a] <- x[2]
        x <- x + M[, S + 1]
        SS[a] <- S + 1
        S <- (S + 7)%%8
      }
      else if (abs(img[x[1] + M[1, S + 2], x[2] + M[2, S + 
        2]] - img[x[1], x[2]]) < 0.1) {
        a <- a + 1
        X[a] <- x[1]
        Y[a] <- x[2]
        x <- x + M[, S + 2]
        SS[a] <- S + 2
        S <- (S + 7)%%8
      }
      else if (abs(img[x[1] + M[1, S + 3], x[2] + M[2, S + 
        3]] - img[x[1], x[2]]) < 0.1) {
        a <- a + 1
        X[a] <- x[1]
        Y[a] <- x[2]
        x <- x + M[, S + 3]
        SS[a] <- S + 3
        S <- (S + 7)%%8
      }
      else {
        S <- (S + 1)%%8
      }
    }
    return(cbind((Y[-1]), ((dim(img)[1] - X))[-1]))
  }




#' Extracts grouping structure from images in a folder
#' 
#' Helps extracting classifiers such as grouping structure from a folder
#' containing a list of files with a regular structure.
#' 
#' For instance, if you have your images, in your working directory and in a
#' folder named "folder", e.g. "folder/ind1_group1_male.jpg,
#' folder/ind2_group2_female.jpg, etc.", the function in the examples below
#' will return a \code{data.frame} that can be defined as the \code{@fac} of a
#' \code{Coo}-object.
#' 
#' @usage \code{fac.structure("folder", names=c("ind", "group", "sex")}
#' @param path \code{character} that indicates the folder where images are.
#' @param names an optional \code{character} that specifies the column names
#' for the \code{data.frame}, i.e. the names of the groups.
#' @param split \code{character} that specifies how to break group informations
#' in the filenames.
#' @return A \code{data.frame} with the column names as specified with
#' \code{names} (if any), and with all individuals along with their group
#' coding.
#' @keywords import
#' @examples
#' 
#' \dontrun{
#' # if all your images are names on the model "indname_group_sex.jpg" (see details above), and placed in a folder called "folder", then :
#' # groups <- fac.structure("folder", names=c("ind", "group", "sex"))
#' # yourCoo@fac <- groups
#' }
#' 
fac.structure <- function(path, names=character(), split="_"){
lf0 <- list.files(path) # we get the list of files
lf0 <- strtrim(lf0, nchar(lf0)-4) # we remove the file extension
lf  <- strsplit(lf0, split=split)
nc  <- as.numeric(unique(lapply(lf, length))) # we check that all files have the same filename structure
if (length(nc) !=1 ) {
  stop("The files do not have the same filename structure. See ?get.structure")}
fac <- as.data.frame(matrix(NA, nr=length(lf), nc=nc)) # dirty
if (!missing(names)) {
  if (length(names) != nc) {
    stop("The number of 'names' is different from the number of groups. See ?get.structure")}
  names(fac) <- names}
rownames(fac) <- lf0 # nice rownames
for (i in 1:nc) {
  fac[, i] <- factor(unlist(lapply(lf, function(x) x[i])))} # ugly way to fill the df
return(fac)
}

# Babel ##############################################################



#' Converts lists and matrices of coordinates into \code{.chc} (chain-coded)
#' files.
#' 
#' \code{pix2chc} converts lists and matrices of coordinates into chain-coded
#' coordinates as used in the SHAPE suite (see Iwata in the \bold{References}
#' below).
#' 
#' Chain-code is a coding system for describing outlines in numbers from 0 to
#' 7. Given the \eqn{i^{th}} pixel taken on an outline and considering its
#' eight neighbors, the chain-code equivalent for describing the relative
#' position of the \eqn{(i+1)^{th}} pixel is a number from \eqn{0} (the next
#' cell is eastward) to, counting counter clockwise, \eqn{7} (the next cell is
#' south-eastward). See \bold{References} and \bold{Examples} below.
#' 
#' @usage pix2chc(coo)
#' @param coo A \code{list} or a 2-col \code{matrix} with entire coordinates,
#' see \bold{Details}.
#' @return Returns a vector of \code{numeric} that corresponds to chain-coded
#' outline.
#' @seealso \link{chc2pix} for the reverse operation.
#' @references Freeman H. 1974. Computer processing of line-drawing images. ACM
#' Computing Surveys (CSUR) 6: 57-97.
#' 
#' Iwata H, Ukai Y. 2002. SHAPE: a computer program package for quantitative
#' evaluation of biological shapes based on elliptic Fourier descriptors. The
#' Journal of Heredity 93: 384-385.
#' 
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#' Computer Graphics and Image Processing 18: 236-258.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.chc} format.
#' @keywords Import
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' chc <- pix2chc(coo)
#' coo.plot(chc2pix(chc))
#' 
#' # Illustration of chain coding
#' plot(NA, xlim=c(0, 3), ylim=c(0, 3), axes=FALSE, ann=FALSE, xaxs="i", yaxs="i")
#' title("Position of the next pixel and corresponding chain-code")
#' abline(h=0:3, v=0:3)
#' rect(1, 1, 2, 2, col="grey80")
#' text(1.5, 1.5, "Starting\npixel", cex=2)
#' text(x=c(2.5, 2.5, 1.5, rep(0.5, 3), 1.5, 2.5),
#' 	y=c(1.5, rep(2.5, 3), 1.5, rep(0.5, 3)), labels=0:7, cex=2)
#' 
#' @export pix2chc
pix2chc <- function(coo) {
  if (is.list(coo)) {
    coo <- l2m(coo)}
  if (is.matrix(coo) & ncol(coo)!=2) {
    stop("A 2 col matrix must be provided")}
  coo.d <- apply(coo, 2, diff)
  if (!all(coo.d %in% -1:1)) {
    stop("Matrix must contain only entire pixels indices")}
  if (any(apply(coo.d, 1, function(x) all(x==rep(0, 2))))) {
    stop("At least two succesive coordinates don't code for a displacement")}
  m   <- as.matrix(expand.grid(-1:1, -1:1))[-5,]
  g   <- c(5, 6, 7, 4, 0, 3, 2, 1)
  chc <- g[apply(coo.d, 1, function(x) which(x[1]==m[, 1] & x[2]==m[, 2]))] #dirty
  return(chc)}



#' Converts chain-coded coordinates to a matrix of coordinates.
#' 
#' \code{chc2pix} converts chain-coded coordinates such as those used in the
#' SHAPE suite (see Iwata in the \bold{References} below) to a matrix of
#' coordinates.
#' 
#' Chain-code is a coding system for describing outlines in numbers from 0 to
#' 7. Given the \eqn{i^{th}} pixel taken on an outline and considering its
#' eight neighbors, the chain-code equivalent for describing the relative
#' position of the \eqn{(i+1)^{th}} pixel is a number from \eqn{0} (the next
#' cell is eastward) to, counting counter clockwise, \eqn{7} (the next cell is
#' south-eastward). See \bold{References} and \bold{Examples} below.
#' 
#' @usage chc2pix(chc)
#' @param chc A \code{numeric} that corresponds to a chain-coded outline.
#' @return Returns a 2-col \code{matrix} that corresponds to the \eqn{(x; y)}
#' coordinates encoded in the chain provided.
#' @seealso \link{pix2chc}, for the reverse operation.
#' @references Freeman H. 1974. Computer processing of line-drawing images. ACM
#' Computing Surveys (CSUR) 6: 57-97.
#' 
#' Iwata H, Ukai Y. 2002. SHAPE: a computer program package for quantitative
#' evaluation of biological shapes based on elliptic Fourier descriptors. The
#' Journal of Heredity 93: 384-385.
#' 
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#' Computer Graphics and Image Processing 18: 236-258.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.chc} format.
#' @keywords Import
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' chc <- pix2chc(coo)
#' coo.plot(chc2pix(chc))
#' 
#' # Illustration of chain coding
#' plot(NA, xlim=c(0, 3), ylim=c(0, 3), axes=FALSE, ann=FALSE, xaxs="i", yaxs="i")
#' title("Position of the next pixel and corresponding chain-code")
#' abline(h=0:3, v=0:3)
#' rect(1, 1, 2, 2, col="grey80")
#' text(1.5, 1.5, "Starting\npixel", cex=2)
#' text(x=c(2.5, 2.5, 1.5, rep(0.5, 3), 1.5, 2.5),
#' 	y=c(1.5, rep(2.5, 3), 1.5, rep(0.5, 3)), labels=0:7, cex=2)
#' 
#' @export chc2pix
chc2pix <- function(chc){
  if (!all(chc %in% 0:7)) {
    stop("chc string must only contain integers between 0 and 7")}
  m <- matrix(c(1, 0, 1, 1, 0, 1, -1, 1,
                -1, 0, -1, -1, 0, -1, 1, -1), ncol=2, byrow=TRUE)
  pix <- apply(m[chc+1,], 2, cumsum)
  return(pix)}



#' Exports \code{Coo} objects to \code{.chc} files that can be used in SHAPE.
#' 
#' \code{Coo2chc} converts \code{Coo} objects into \code{.chc} files that can
#' then be used in the SHAPE suite (see Iwata & Ukai in the \bold{References}
#' below).
#' 
#' See \link{pix2chc} for an illustration of the functions behind and
#' chain-coding of outlines.
#' 
#' @usage Coo2chc(Coo, file="chc.chc")
#' @param Coo A \code{Coo} object.
#' @param file A \code{character} to specify where to write the \code{.chc}
#' file.
#' @return Writes a \code{.chc} file.
#' @seealso \link{Coe2nef} and \link{nef2Coe}.
#' @references Freeman H. 1974. Computer processing of line-drawing images. ACM
#' Computing Surveys (CSUR) 6: 57-97.
#' 
#' Iwata H, Ukai Y. 2002. SHAPE: a computer program package for quantitative
#' evaluation of biological shapes based on elliptic Fourier descriptors. The
#' Journal of Heredity 93: 384-385.
#' 
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#' Computer Graphics and Image Processing 18: 236-258.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.chc} format.
#' @keywords Import
#' @export Coo2chc
Coo2chc <- function(Coo, file="chc.chc"){ 
  res <- list()
  pb <- txtProgressBar(1, Coo@coo.nb)
  for (i in 1:Coo@coo.nb){
    res[[i]] <- c(Coo@names[i], rep(1, 3), Polygon(list(Coo@coo[[i]]))@area,
                  pix2chc(Coo@coo[[i]]), -1, "\n")
    setTxtProgressBar(pb, i)}
  cat(unlist(res), file=file)
  cat(".chc file succesfully written here:", file)}



#' Imports \code{.chc} files from SHAPE to \code{Coo} objects that can be used
#' in Momocs.
#' 
#' \code{chc2Coo} converts \code{.chc} files, \emph{e.g.} from the SHAPE suite
#' (see Iwata & Ukai in the \bold{References} below) that can then be used in
#' Momocs.
#' 
#' See \link{pix2chc} for an illustration of the functions behind and
#' chain-coding of outlines.
#' 
#' @usage chc2Coo(chc.path)
#' @param chc.path A \code{character} that indicates the path for the
#' \code{.chc} file to convert.
#' @return Returns a \code{Coo} object.
#' @seealso \link{Coe2nef} and \link{nef2Coe}.
#' @references Freeman H. 1974. Computer processing of line-drawing images. ACM
#' Computing Surveys (CSUR) 6: 57-97.
#' 
#' Iwata H, Ukai Y. 2002. SHAPE: a computer program package for quantitative
#' evaluation of biological shapes based on elliptic Fourier descriptors. The
#' Journal of Heredity 93: 384-385.
#' 
#' Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a closed contour.
#' Computer Graphics and Image Processing 18: 236-258.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.chc} format.
#' @keywords Import
#' @export chc2Coo
chc2Coo <- function(chc.path){
  chc <- readLines(chc.path)
  coo.list <- list()
  coo.names <- character()
  for (i in seq(along=chc)) {
    chc.i <- unlist(strsplit(chc[i], " "))
    rm    <- match(chc.i, c("", " ", "-1"), , nomatch=0)
    if (any(rm)) {chc.i <- chc.i[rm==0]}
    coo.names[i] <- chc.i[1]
    st    <- as.numeric(chc.i[2:3])
    pix.i <- chc2pix(as.numeric(chc.i[-(1:5)]))
    coo.list[[i]] <- coo.trans(pix.i, st[1], st[2])
  }
  names(coo.list) <- coo.names
  return(Coo(coo.list))}



#' Imports \code{.nef} files and creates \code{Coe} objects.
#' 
#' \code{nef2Coe} converts \code{.nef} files to \code{Coe} objects. This
#' function is intended to ease data exchange between Momocs and SHAPE suite
#' (see Iwata in the \bold{References} below).
#' 
#' 
#' @usage nef2Coe(nef.path)
#' @param nef.path A \code{character} that indicates the path for the
#' \code{.nef} file to convert.
#' @return \code{nef2Coe} returns a \code{Coe} object.
#' @seealso \link{Coe2nef}, for the reverse operation.
#' @references Iwata H, Ukai Y. 2002. SHAPE: a computer program package for
#' quantitative evaluation of biological shapes based on elliptic Fourier
#' descriptors. The Journal of Heredity 93: 384-385.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.nef} format.
#' @keywords Import
#' @examples
#' 
#' \dontrun{
#' # I (VB) just finished my post-doc in India and I wont have time for Momocs
#' # until mid march or maybe a bit after that.
#' # Ryan Felice found a bug in my own version. I provide hereafter the function
#' # he kindly sent to me and that worked fine for him.
#' # Hope this help. Thanks again to him.
#' NEF2COE<-function (nef.path){
#'   nef <- readLines(nef.path)
#'   HARMO.l <- grep(pattern = "HARMO", nef)
#'   nb.h <- as.numeric(substring(nef[HARMO.l], 8))
#'   nef <- nef[-(1:HARMO.l)]
#'   nb.coo <- length(nef)/(nb.h + 1)
#'   coo.i <- 1:nb.coo
#'   coo.beg <- (coo.i - 1) * (nb.h + 1) + 1
#'   coo.end <- coo.beg + nb.h
#'   res <- matrix(NA, nrow = nb.coo, ncol = nb.h * 4,
#'       dimnames = list(nef[coo.beg], paste(rep(LETTERS[1:4], each = nb.h),
#'       1:nb.h, sep = "")))
#'   for (i in seq(along = coo.i)) {
#'     nef.i <- nef[(coo.beg[i]+1):coo.end[i]]
#'     x <- as.numeric(unlist(strsplit(nef.i, " ")))
#'     x1<-x[!is.na(x)]
#'     a.i<-x1[seq(1,length(x1),4)]
#'     b.i<-x1[seq(2,length(x1),4)]
#'     c.i<-x1[seq(3,length(x1),4)]
#'     d.i<-x1[seq(4,length(x1),4)]
#'     res[i, ]<-c(a.i,b.i,c.i,d.i)
#'   }
#'   return(Coe(res,method="eFourier"))}
#' }
#' 
#' @export nef2Coe
nef2Coe <- function(nef.path) {
  # change nef to coe one day
  nef     <- readLines(nef.path)
  HARMO.l <- grep(pattern="HARMO", nef)
  nb.h    <- as.numeric(substring(nef[HARMO.l], 8))
  nef     <- nef[-(1:HARMO.l)]
  nb.coo  <- length(nef)/(nb.h+1)
  coo.i   <- 1:nb.coo
  coo.beg <- (coo.i-1)*(nb.h + 1)+1
  coo.end <- coo.beg + nb.h
  res     <- matrix(NA, nrow=nb.coo, ncol=nb.h*4, dimnames=
    list(nef[coo.beg],
         paste(rep(LETTERS[1:4], each=nb.h), 1:nb.h, sep="")))
  for (i in seq(along=coo.i)) {
    nef.i    <- nef[(coo.beg[i]+1) : coo.end[i]]
    x        <- as.numeric(unlist(strsplit(nef.i, " ")))
    res[i, ] <- x[!is.na(x)]}
  return(Coe(res))}



#' Exports \code{Coe} objects to \code{.nef} files.
#' 
#' \code{Coe2nef} converts \code{Coe} objects to \code{.nef} files. This
#' function is intended to ease data exchange between Momocs and SHAPE suite
#' (see Iwata in the \bold{References} below).
#' 
#' 
#' @usage Coe2nef(Coe, file="nef.nef")
#' @param Coe A \code{Coe} object.
#' @param file A \code{character} to specify where to write the \code{.nef}
#' file.
#' @return Writes a \code{.nef} file.
#' @seealso \link{nef2Coe}, for the reverse operation.
#' @references Iwata H, Ukai Y. 2002. SHAPE: a computer program package for
#' quantitative evaluation of biological shapes based on elliptic Fourier
#' descriptors. The Journal of Heredity 93: 384-385.
#' 
#' You can also have a look to the SHAPE's manual distributed with the program
#' suite, that gives a description of the \code{.nef} format.
#' @keywords Import
#' @export Coe2nef
Coe2nef <- function(Coe, file="nef.nef"){
  nb.h      <- Coe@nb.h
  coo.names <- Coe@names
  coo.i   <- 1:length(coo.names)
  coo.beg <- (coo.i-1)*(nb.h + 1)+1
  coo.end <- coo.beg + nb.h
  #nef <- c("#CONST ", constant.coeff, " \n", "#HARMO ", nb.h, " \n")
  nef <- character()
  for (i in seq(along=coo.names)) {
    nef <- append(nef, c(coo.names[i], "\n"))
    for (j in 1:nb.h){
      coeff.i <- round(as.numeric(Coe@coeff[i, (0:3)*nb.h+j]), 8)
      nef     <- append(nef, c(coeff.i, "\n"))}
  }
  cat(nef, file=file)
  cat(".nef file succesfully written here:", file)
}

# xFourier core functions ############################################

# efourier 


#' Calculates elliptical Fourier analysis.
#' 
#' \code{efourier} computes elliptical Fourier analysis from a matrix or a list
#' of coordinates.
#' 
#' These functions and their mathematical background detailed below are here
#' detailed to ease their use in new methods but are used internally by methods
#' on \code{Coo}-objects.
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
#' 
#' @usage efourier(coo, nb.h = 32, smooth.it = 0, silent = FALSE)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param nb.h \code{integer}. The number of harmonics to use
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform.
#' @param silent \code{logical}. Whether to print or not diagnosis messages.
#' @return A list with these components: \item{an }{\code{vector} of
#' \eqn{a_{1->n}} harmonic coefficients.} \item{bn }{\code{vector} of
#' \eqn{b_{1->n}} harmonic coefficients.} \item{cn }{\code{vector} of
#' \eqn{c_{1->n}} harmonic coefficients.} \item{dn }{\code{vector} of
#' \eqn{d_{1->n}} harmonic coefficients.} \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{co }{\code{co} Harmonic coefficient.}
#' @seealso \link{efourier.i} for the reverse operation and \link{eFourier} the
#' method for \code{Coo} objects. \link{Ptolemy} for an implementation of the
#' Ptolemaic ellipses. \link{rfourier}, \link{tfourier} for the other members
#' of the Fourier's family.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' 
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords elliptical Fourier analysis
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo)
#' ef  <- efourier(coo, 12)
#' ef
#' efi <- efourier.i(ef)
#' l2m(efi)
#' coo.draw(efi, border="red", col=NA)
#' 
#' @export efourier
efourier  <- function (coo, nb.h = 32, smooth.it = 0, silent = FALSE) {
  if (is.matrix(coo)) coo <- m2l(coo)
  if (is.closed(coo)) coo <- coo.unclose(coo)
  if (missing(nb.h))  {
    nb.h <- length(coo$x)/2 - 1 # should not be 1
    warning(paste(" * 'nb.h' not provided and set to", nb.h))}
  if(nb.h * 2 > length(coo$x)) {
    nb.h = floor(length(coo$x)/2)-1 # should not be -1
    if (!silent){
    warning(" * The number of harmonics to calculate should be lower than half the number of points. 
    'The number of harmonics used 'nb.h' has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(length(coo$x)/2)-1 # should not be -1
    if (!silent){
    cat(" * The number of harmonics used has been set to: ", nb.h)}}
  if (smooth.it!=0) { coo <- coo.smooth(coo, smooth.it)}
  p <- length(coo$x)
  Dx <- coo$x - coo$x[c(p, (1:p - 1))]
  Dy <- coo$y - coo$y[c(p, (1:p - 1))]
  Dt <- sqrt(Dx^2 + Dy^2)
  t1 <- cumsum(Dt)
  t1m1 <- c(0, t1[-p])
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
  ao <- 2 * sum(coo$x * Dt/T)
  co <- 2 * sum(coo$y * Dt/T)
  return(list(an = an, bn = bn, cn = cn, dn = dn, ao = ao, co = co))}



#' Calculates inverse elliptical Fourier analysis.
#' 
#' \code{efourier.i} uses the inverse elliptical Fourier transformation to
#' calculate a shape, when given a list with Fourier coefficients, typically
#' obtained computed with \link{efourier}.
#' 
#' See \link{efourier} for the mathematical background.
#' 
#' @usage efourier.i(ef, nb.h, nb.pts = 300)
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
#' @keywords elliptical Fourier analysis
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo)
#' ef  <- efourier(coo, 12)
#' ef
#' efi <- efourier.i(ef)
#' l2m(efi)
#' coo.draw(efi, border="red", col=NA)
#' 
#' 
#' @export efourier.i
efourier.i <- function(ef, nb.h, nb.pts = 300) {
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
  if (missing(nb.h)) nb.h <- length(an)
  theta <- seq(0, 2 * pi, length = nb.pts + 1)[-(nb.pts + 1)]
  hx <- matrix(NA, nb.h, nb.pts)
  hy <- matrix(NA, nb.h, nb.pts)
  for (i in 1:nb.h) {
    hx[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i * theta)
    hy[i, ] <- cn[i] * cos(i * theta) + dn[i] * sin(i * theta)}
  x <- (ao/2) + apply(hx, 2, sum)
  y <- (co/2) + apply(hy, 2, sum)
  list(x = x, y = y)}



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
#' normalizing harmonic coefficients when calculating it for \code{Coo}
#' objects.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' 
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. \emph{Systematic Biology} \bold{34}: 59-68.
#' @keywords elliptical Fourier analysis
#' @examples
#' 
#' data(bot)
#' q <- efourier(bot@coo[[1]], 24)
#' efourier.i(q) # equivalent to efourier.shape(q$an, q$bn, q$cn, q$dn)
#' efourier.norm(q)
#' efourier.shape(nb.h=5, alpha=1.2)
#' efourier.shape(nb.h=12, alpha=0.9)
#' 
#' @export efourier.norm
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
#' @usage efourier.shape(an, bn, cn, dn, nb.h, nb.pts=80, alpha=2, plot=TRUE)
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
#' @keywords elliptical Fourier analysis
#' @examples
#' 
#' data(bot)
#' ef <- efourier(bot@coo[[1]], 24)
#' efourier.shape(ef$an, ef$bn, ef$cn, ef$dn) # equivalent to efourier.i(ef)
#' efourier.shape() # is autonomous
#' 
#' efourier.shape(nb.h=12)
#' efourier.shape(nb.h=12, alpha=0.5)
#' 
#' panel(Coo(replicate(100, l2m(efourier.shape(nb.h=6, alpha=2.5, plot=FALSE))))) # Bubble family
#' panel(Coo(replicate(100, l2m(
#'     efourier.shape(nb.h=12, alpha=0.5, nb.pts=80, plot=FALSE)))))# some doodle
#' 
#' 
#' @export efourier.shape
efourier.shape <- function(an, bn, cn, dn, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
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
#' These two functions calculate dilated and eroded shapes based on elliptical
#' Fourier decomposition \emph{i.e.} taking into account the shape as a whole.
#' Lists created by \code{efourier} objects can be passed to \code{ef.amplify}
#' or a list or matrix of coordinates to \code{coo.ef.amplify}.
#' 
#' 
#' @aliases ef.amplify coo.ef.amplify
#' @usage ef.amplify(ef, amp=rep(0.5, 4)) coo.ef.amplify(coo, amp=rep(0.5, 4),
#' nb.h=5, draw=FALSE, ...)
#' @param ef \code{list}. A list containing \eqn{a_n}, \eqn{b_n}, \eqn{c_n} and
#' \eqn{d_n} Fourier coefficients, such as returned by \code{efourier}.
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param amp A vector of \code{numeric}. If \code{amp} is of length 4, the
#' value specify the multiplication factor for \eqn{a_1}, \eqn{b_1}, \eqn{c_1}
#' and \eqn{d_1} ; if only one value is provided, then the multiplication
#' factor will be the same for the four coefficients \eqn{abcd_1}.
#' @param draw \code{logical}. Whether to draw (see \link{coo.draw}) the
#' amplified \code{coo}.
#' @param nb.h \code{integer}. The number of harmonics to calculate.
#' @param ... Additional parameters to be passed to \code{coo.draw}.
#' @return \code{ef.amplify} returns the \code{ef} provided but with
#' "amplified" coefficients for the first harmonics. \code{coo.amplify} returns
#' the \code{coo} provided but "amplified" based on elliptical Fourier.
#' @seealso \link{efourier} for a description of the elliptical Fourier
#' analysis and \link{Ptolemy} for an illustration of the first
#' ellipse/harmonic defining the shape "amplitude".
#' @keywords Outline Analysis
#' @examples
#' 
#' # same amp factor for every coeff.
#' data(bot)
#' bot1 <- coo.sample(bot@coo[[1]], 50)
#' coo.plot(bot1, col=NA)
#' amp <- seq(0.9, 0.3, -0.1)
#' for (i in seq(along=amp)) {
#'   coo.ef.amplify(bot1, amp[i], draw=TRUE, first=FALSE,
#'               col=NA, border=col.summer(length(amp))[i])}
#' 
#' # random shape, separate amplification
#' poly <- efourier.shape(nb.h=5, alpha=3, plot=FALSE)
#' poly2 <- coo.sample(poly, 20)
#' layout(matrix(1:4, nc=2, byrow=TRUE))
#' par(oma=rep(0.5, 4), mar=c(2, 2, 3, 2))
#' coo.plot(poly2, col=NA, main="an")
#' for (i in seq(-1, 1, 0.5)) {
#'   coo.ef.amplify(poly2, c(i, 1, 1, 1), draw=TRUE)}
#' 
#' coo.plot(poly2, col=NA, main="bn")
#' for (i in seq(-1, 1, 0.5)) {
#'   coo.ef.amplify(poly2, c(1, i, 1, 1), draw=TRUE)}
#' 
#' coo.plot(poly2, col=NA, main="cn")
#' for (i in seq(-1, 1, 0.5)) {
#'   coo.ef.amplify(poly2, c(1, 1, i, 1), draw=TRUE)}
#' 
#' coo.plot(poly2, col=NA, main="dn")
#' for (i in seq(-1, 1, 0.5)) {
#'   coo.ef.amplify(poly2, c(1, 1, 1, i), draw=TRUE)}
#' 
#' 
#' @export ef.amplify
ef.amplify <- function(ef, amp=rep(0.5, 4)){
  ef$an <- ef$an*amp[1]
  ef$bn <- ef$bn*amp[2]
  ef$cn <- ef$cn*amp[3]
  ef$dn <- ef$dn*amp[4]
  return(ef)
}

# rfourier 


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
#' 
#' @usage rfourier(coo, nb.h, smooth.it = 0, norm = FALSE, silent=FALSE)
#' @param coo A \code{list} or \code{matrix} of coordinates.
#' @param nb.h \code{integer}. The number of harmonics to calculate/use.
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform.
#' @param norm \code{logical}. Whether to scale the outlines so that the mean
#' length of the radii used equals 1.
#' @param silent \code{logical}. Whether to display diagnosis messages.
#' @return A list with these components: \item{an }{\code{vector} of
#' \eqn{a_{1->n}} harmonic coefficients.} \item{bn }{\code{vector} of
#' \eqn{b_{1->n}} harmonic coefficients.} \item{ao }{\code{ao} Harmonic
#' coefficient.} \item{r }{\code{vector} of radii lengths.}
#' @seealso \link{rfourier.i} for the inverse operation, \link{rfourier.shape}.
#' \link{efourier}, \link{tfourier} for the other members of the Fourier's
#' family.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords radii variation Fourier analysis
#' @examples
#' 
#' data(bot)
#' coo <- coo.center(bot@coo[[1]]) # centering is almost mandatory for rfourier family
#' coo.plot(coo)
#' rf  <- rfourier(coo, 12)
#' rf
#' rfi <- rfourier.i(rf)
#' l2m(rfi)
#' coo.draw(rfi, border="red", col=NA)
#' 
#' 
#' @export rfourier
rfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, silent=FALSE){
  if (missing(nb.h))  {stop("nb.h must be provided")}
  if (is.list(coo))   {coo <- l2m(coo)}
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo)) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (!silent){
      warning("The number of harmonics to calculate should be lower than half the number of points. 
    The number of harmonics used has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (!silent){
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
#' @keywords radii variation Fourier analysis
#' @examples
#' 
#' data(bot)
#' coo <- coo.center(bot@coo[[1]]) # centering is almost mandatory for rfourier family
#' coo.plot(coo)
#' rf  <- rfourier(coo, 12)
#' rf
#' rfi <- rfourier.i(rf)
#' l2m(rfi)
#' coo.draw(rfi, border="red", col=NA)
#'     # it works since coo.draw and coo.plot retrieve "x"
#'     # and "y" components (through l2m when passed with a list.
#' 
#' @export rfourier.i
rfourier.i <- function(rf, nb.h, nb.pts=300) {
  if (!all(c("an", "bn") %in% names(rf))) {
    stop("a list containing 'an' and 'bn' harmonic coefficients must be provided")}
  ao <- ifelse(is.null(rf$ao), 1, rf$ao)
  an <- rf$an
  bn <- rf$bn
  if (missing(nb.h)) {nb.h <- length(an)}
  if (nb.h > length(an)) {
    nb.h <- length(an)
    warning("nb.h cannot be higher than length(rf$an) and has been set to: ", nb.h)}
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
#' @keywords radii variation Fourier analysis
#' @examples
#' 
#' 
#' data(bot)
#' rf <- rfourier(bot@coo[[1]], 24)
#' rfourier.shape(rf$an, rf$bn) # equivalent to rfourier.i(rf)
#' rfourier.shape() # not very interesting
#' 
#' rfourier.shape(nb.h=12) # better
#' rfourier.shape(nb.h=6, alpha=0.4, nb.pts=500)
#' 
#' panel(Coo(replicate(100, l2m(rfourier.shape(
#'     nb.h=6, alpha=0.4, nb.pts=200, plot=FALSE))))) # Butterflies
#' 
#' @export rfourier.shape
rfourier.shape <- function(an, bn, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  rf  <- list(an=an, bn=bn, ao=0)
  shp <- rfourier.i(rf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

# tfourier 


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
#' @usage tfourier(coo, nb.h, smooth.it=0, norm = FALSE, silent = TRUE)
#' @param coo A list or matrix of coordinates
#' @param nb.h \code{integer}. The number of harmonics to calculate/use
#' @param smooth.it \code{integer}. The number of smoothing iterations to
#' perform
#' @param norm \code{logical}. Whether to scale and register new coordinates so
#' that the first point used is sent on the origin.
#' @param silent \code{logical}. Whether to display diagnosis messages.
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
#' @keywords tangent angle Fourier analysis
#' @examples
#' 
#' data(bot)
#' coo <- bot@coo[[1]]
#' coo.plot(coo)
#' tf  <- tfourier(coo, 12)
#' tf
#' tfi <- tfourier.i(tf)
#' l2m(tfi)
#' coo.draw(tfi, border="red", col=NA) # the outline is not closed...
#' coo.draw(tfourier.i(tf, force2close=TRUE), border="blue", col=NA) # we force it to close.
#' 
#' 
#' @export tfourier
tfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, silent=TRUE){
  if (missing(nb.h))  {stop("nb.h must be provided")}
  if (is.list(coo))   {coo <- l2m(coo)}
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo)) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (!silent){
      warning("The number of harmonics to calculate should be lower than half the number of points. 
    The number of harmonics used has been set to: ", nb.h)}}
  if (nb.h == -1) {
    nb.h = floor(nrow(coo)/2)-1 # should not be -1
    if (!silent){
      cat("The number of harmonics used has been set to: ", nb.h)}}
  if (smooth.it!=0) { coo <- coo.smooth(coo, smooth.it)}
  if (norm) {
    coo <- coo.scale(coo.center(coo))
    coo <- coo.trans(coo, -coo[1, 1], -coo[1, 2])
  }
 
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
#' @keywords tangent angle Fourier analysis
#' @examples
#' 
#' data(bot)
#' tfourier(bot@coo[[1]], 24)
#' tfourier.shape()
#' 
#' @export tfourier.i
tfourier.i<-function(tf, nb.h, nb.pts=300, force2close=FALSE, rescale=TRUE, perim=2*pi, thetao=0){
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
  return(list(x=coo[, 1], y=coo[, 2], angle=theta, phi=phi))
}





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
#' @keywords tangent angle Fourier analysis
#' @examples
#' 
#' data(bot)
#' tf <- tfourier(bot@coo[[1]], 24)
#' tfourier.shape(tf$an, tf$bn) # equivalent to rfourier.i(rf)
#' tfourier.shape()
#' tfourier.shape(nb.h=6, alpha=0.4, nb.pts=500)
#' panel(Coo(replicate(100, coo.force2close(l2m(
#'     tfourier.shape(nb.h=6, alpha=2, nb.pts=200, plot=FALSE)))))) # biological shapes
#' 
#' @export tfourier.shape
tfourier.shape <- function(an, bn, ao=0, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  tf  <- list(an=an, bn=bn, ao=ao)
  shp <- tfourier.i(tf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

# PCA and Cie ########################################################



#' From a factorial map to a shape.
#' 
#' Converts a position in a factorial map to a shape. This function is used
#' internally but might be interesting for a direct used on a PCA calculated on
#' a matrix of harmonic coefficients.
#' 
#' 
#' @usage pca2shp(pos, rot, mean.shp, method = c("efourier", "rfourier",
#' "tfourier"), scale = 1, amp = 1, trans = TRUE, nb.pts = 64, rotate.shp)
#' @param pos A \code{vector} of positions on a given PC axis.
#' @param rot A \code{vector} of multiplying values corresponding to the PC
#' axis passed to \code{pos}.
#' @param mean.shp A \code{vector} of harmonic coefficients corresponding to
#' the mean shape.
#' @param method A character, either "efourier", "rfourier" or "tfourier"
#' (partial matches are allowed) to use to calculate morphological space.
#' @param scale \code{numeric}. To scale the shape returned.
#' @param amp \code{numeric}. To amplify the amplitude of the deformation.
#' @param trans \code{logical}. Whether to translate of not the coordinate of
#' the shape reconstructed according to \code{pos}.
#' @param nb.pts numeirc. The number of points to use for reconstructed shapes.
#' @param rotate.shp \code{numeric}. If specified, the angles (in radians) to
#' rotate shapes plotted.
#' @return Returns a \code{list} of coordinates.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' 
#' @export pca2shp
pca2shp <- function (pos, rot, mean.shp,
                     method=c("efourier", "rfourier", "tfourier"),
                     scale=1, amp=1, trans=TRUE, nb.pts=64, rotate.shp) {
  # we check a bit
  if (!is.matrix(pos))        pos <- as.matrix(pos)
  if (ncol(pos) != ncol(rot)) stop("rot an pos must have the same ncol")
  if(length(mean.shp) != nrow(rot)) stop("mean.shp length must equals the col number of rot")
  # we handle method argument
  if (missing(method)) {
    warning("Method not provided. efourier is used.")
    p <- 1 # we also need to switch below
    method.i   <- efourier.i
  } else {
    p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
    if (is.na(p)) { warning("Unvalid method. efourier is used.")
    } else {
      method.i   <- switch(p, efourier.i,   rfourier.i,   tfourier.i)}}
  
  # stupid function
  mprod <- function(m, s){
    res <- m
    for (i in 1:ncol(m)) {
      res[, i] <- m[, i]*s[i]}
    return(res)}
  nb.h <- length(mean.shp)/ifelse(p==1, 4, 2)
  n  <- nrow(pos)
  # we prepare the array
  res <- array(NA, dim=c(nb.pts, 2, n),
               dimnames=list(paste0("pt", 1:nb.pts),
                             c("x", "y"),
                             paste0("shp", 1:n)))
  for (i in 1:n) {
    ax.contrib <- mprod(rot, pos[i, ])*amp
    coe        <- mean.shp + apply(ax.contrib, 1, sum)
    if (p==1) {
      xf <- list(an=coe[1:nb.h + 0*nb.h],
                 bn=coe[1:nb.h + 1*nb.h],
                 cn=coe[1:nb.h + 2*nb.h],
                 dn=coe[1:nb.h + 3*nb.h])
    } else {
      xf <- list(an=coe[1:nb.h + 0*nb.h],
                 bn=coe[1:nb.h + 1*nb.h])
      }
    coo        <- l2m(method.i(xf, nb.h = nb.h, nb.pts=nb.pts))
    coo <- coo.template(coo, size=scale)
    # if required we rotate shapes
    if (!missing(rotate.shp)) { coo <- coo.rotate(coo, rotate.shp) }
    # by default we return translated shapes, ready to draw as a layer
    if (trans)                { coo <- coo.trans(coo, pos[i, 1], pos[i, 2]) }
    res[,,i] <- coo.force2close(coo)
  }
  invisible(res)}

# Morphological space


#' Calculate and plots morphological spaces.
#' 
#' Calculates and plots morphological spaces with many options.
#' 
#' Note that \link{tFourier} analyses since the reconstruction of shapes do not
#' always lead to closed shapes (see Rohlf F, Archie J. 1984. A comparison of
#' Fourier methods for the description of wing shape in mosquitoes (Diptera:
#' Culicidae). Systematic Biology: 302-317.)
#' 
#' @usage morpho.space(dudi, xax = 1, yax = 2, xlim, ylim, nb.pts = 300,
#' pos.shp = c("li", "circle", "range")[3], nr.shp = 6, nc.shp = 5, amp.shp =
#' 1, scale.shp = 1, rotate.shp = 0, circle.nb.shp = 12, circle.r.shp, plot =
#' TRUE, layer = TRUE, col.shp = "#70809011", border.shp = "#708090", pch.pts =
#' 20, col.pts = "grey40", first.point = FALSE)
#' @param dudi a dudi.pca object.
#' @param xax \code{integer}. The index of the first PC axis to use.
#' @param yax \code{integer}. The index of the first PC axis to use.
#' @param xlim A vector of 2 \code{numeric} indicating the x-range.
#' @param ylim A vector of 2 \code{numeric} indicating the y-range.
#' @param nb.pts The number of points to calculate.
#' @param pos.shp \code{character}, any of the \code{("li", "circle", "range")}
#' methods. It specifies the way shapes have to be drawn. \code{"li"} draws
#' shapes on the actual positions on the factorial map ; \code{"circle"} draws
#' shapes on a circle with origin as the center ; \code{"range"} draws shape on
#' a rectangle that covers PC1 and PC2 range. Alternatively, a two columns
#' matrix of coordinates where to calculate shapes.
#' @param nr.shp \code{numeric}. The number of shape rows.
#' @param nc.shp \code{numeric}. The number of shape columns.
#' @param amp.shp \code{numeric}. An amplifying factor for shape deformation.
#' @param scale.shp \code{numeric}. The size of shapes, relatively to 1/8 of
#' the highest range (x or y).
#' @param rotate.shp \code{numeric}. If specified, the angles (in radians) to
#' counter-clockwise rotate shapes plotted.
#' @param circle.nb.shp \code{integer}. When \code{pos == "circle"}, the number
#' of shapes on the circle.
#' @param circle.r.shp \code{numeric}. When \code{pos == "circle"}, the circle
#' radius.
#' @param plot \code{logical}. Whether to plot or not the morphological space.
#' @param layer \code{logical}. Whether to add calculated shapes on an existing
#' plot. Used for instance by \link{dudi.plot}.
#' @param col.shp A color string for filling the shapes.
#' @param border.shp A color string for shape borders.
#' @param pch.pts A \code{pch} for plotting the points from the
#' \code{dudi}-class object. Use \code{NA} to not display these points.
#' @param col.pts A color to plot these points.
#' @param first.point \code{logical}. Whether to draw or not the first point of
#' the shapes.
#' @seealso \link{dudi.plot}.
#' @keywords Multivariate Analysis
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' botD <- pca(botF)
#' morpho.space(botD)
#' 
#' 
#' @export morpho.space
morpho.space <- function(dudi, xax = 1, yax = 2, xlim, ylim, nb.pts=300,
                         pos.shp=c("li", "circle", "range")[3], 
                         nr.shp=6, nc.shp=5, amp.shp=1, scale.shp=1, rotate.shp=0,
                         circle.nb.shp=12, circle.r.shp,
                         plot=TRUE, layer=TRUE, col.shp="#70809011", border.shp="#708090",
                         pch.pts=20, col.pts="grey40", first.point=FALSE){
  # we first check argument passed to pos.shp, to define pos
  if (is.data.frame(pos.shp)) pos.shp <- as.matrix(pos.shp) # e.g. when passed with expand.grid
  if (is.matrix(pos.shp)) {
    if (ncol(pos.shp)!=2) {stop("When passed with a matrix, pos.shp requires a two columns matrix")}
    pos <- pos.shp
  } else if (pos.shp=="li") { # we retrieve coordinates from the dudi.object
    pos <- dudi$li[, c(xax, yax)]
  } else if (pos.shp=="circle") {
    if (missing(circle.r.shp)) { # if missing we define it as the mean distance from the origin
      li.2      <- apply(dudi$li[, c(xax, yax)], 2, function(x) x^2)
      li.len    <- apply(li.2, 1, function(x) sqrt(sum(x)))
      circle.r.shp <- mean(li.len)}
    t <- seq(0, 2*pi, len=circle.nb.shp+1)[-(circle.nb.shp+1)]
    pos <- cbind(circle.r.shp*cos(t), circle.r.shp*sin(t))
  } else if (pos.shp=="range") { # by default, we aim at covering the range covered by points on the two PC axes
    pos <- expand.grid(seq(min(dudi$li[, xax]), max(dudi$li[, xax]), len=nr.shp),
                       seq(min(dudi$li[, yax]), max(dudi$li[, yax]), len=nc.shp))
    pos <- as.matrix(pos)
  } else {
    stop("shp.pos must be passed with values li, circle, range or a matrix of coordinates")}
  # We define a scale.shp that should fit, and then modify it with scale.shp
  if (missing(scale.shp)) {
    scale.shp <- min(apply(dudi$li[,c(xax, yax)], 2, function(x) diff(range(x)))/(c(nr.shp, nc.shp)-1))}
  # Here we (finally) calculate the shapes
  shapes <- pca2shp(pos, rot=dudi$c1[, c(xax, yax)],
                    mean.shp=dudi$mean.shp, method=dudi$method,
                    scale=scale.shp, amp=amp.shp, rotate.shp=rotate.shp, nb.pts=nb.pts)
  if (plot) {
    if (missing(xlim) & missing(ylim)) {
      w <- apply(shapes, 2, range)
    } else {
      w <- cbind(xlim, ylim)}
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mar=c(3, 3, 1, 1))
    plot(dudi$li[, c(xax, yax)], xlim=w[, 1], ylim=w[,2], asp=1, las=1,
         col=col.pts, pch=pch.pts, cex=1, cex.axis=0.7, ann=FALSE)
    abline(h=0, v=0, lty=2, col="grey80")
    box() }
  if (layer) {
    apply(shapes, 3, coo.draw, points=FALSE, border=border.shp, col=col.shp, first.point=first.point)
    #A.segments(shapes, col=col.shp, border.col=border.shp)
  }
  invisible(shapes)
}

# dudi.plot


#' A wrapper for \code{dudi.pca} graphical functions.
#' 
#' A wrapper for \code{dudi.pca} objects produced by \link{dudi.pca} in
#' \code{ade4} or \link{pca} in \code{Momocs}.
#' 
#' This function takes the slightly modified \code{dudi.pca} object, obtained
#' with \link{pca} on a \code{Coe} object and widely hinges on \code{ade4}
#' plotting facilities and graphical layers. Notice that besides dedicated
#' \emph{shapes} argument and options, it can be used for plotting factorial
#' maps on "regular" \code{dudi.pca} objects. By default, \code{shapes} is set
#' to \code{FALSE} for \link{tFourier} analyses since the reconstruction of
#' shapes do not always lead to closed shapes (see Rohlf F, Archie J. 1984. A
#' comparison of Fourier methods for the description of wing shape in
#' mosquitoes (Diptera: Culicidae). Systematic Biology: 302-317.).
#' 
#' @usage dudi.plot(dudi, fac = NULL, xax = 1, yax = 2, grid = TRUE, points =
#' TRUE, pch.points = 1, col.points = "black", cex.points = 0.8, labels =
#' FALSE, label = rownames(dudi$li), boxes = TRUE, clabel = 0.6, neighbors =
#' FALSE, col.nei = "grey90", lwd.nei = 0.5, star = TRUE, col.star = "grey60",
#' cstar = 1, ellipses = TRUE, col.ellipse = "grey30", cellipse = 1, axesell =
#' TRUE, chull = FALSE, col.chull = "grey30", optchull = c(0.5, 1), arrows =
#' FALSE, edge.arrow = FALSE, box.arrow = TRUE, maxnb.arrow = 10, dratio.arrow
#' = 0.2, shapes = TRUE, pos.shp=c("li", "circle", "range", "full")[3], nr.shp
#' = 6, nc.shp = 5, amp.shp = 1, scale.shp = 0.666, nb.pts.shp=300,
#' first.point.shp = FALSE, rotate.shp=0, circle.nb.shp = 12, circle.r.shp,
#' col.shp = "#70809011", border.shp = "#708090", rug = TRUE, rug.ticksize =
#' 0.01, rug.col = "#708090", eigen = FALSE, eigen.ratio = 0.2, palette =
#' col.sari, title = substitute(dudi), legend=FALSE, center.orig = FALSE,
#' zoom.plot = 1)
#' @param dudi a dudi.pca object.
#' @param fac the \code{(col)name} of the \code{@fac} slot to use as the
#' grouping \code{factor}.
#' @param xax \code{integer}. The index of the first PC axis to use.
#' @param yax \code{integer}. The index of the first PC axis to use.
#' @param grid \code{logical}. Whether to draw the grid.
#' @param points \code{logical}. Whether to draw the points.
#' @param pch.points the \code{pch} for drawing points.
#' @param col.points the \code{col} for drawing points.
#' @param cex.points the \code{cex} for drawing points.
#' @param labels \code{logical}. Whether to draw labels.
#' @param label \code{character}. The labels to draw.
#' @param boxes \code{logical}. Whether to draw labels in boxes.
#' @param clabel The \code{cex} for labels.
#' @param neighbors \code{logical}. Whether to draw a neighboring graph.
#' @param col.nei the \code{col} for drawing neighboring graph.
#' @param lwd.nei the \code{lwd} for drawing neighboring graph.
#' @param star \code{logical}. Whether to draw the star.
#' @param col.star the \code{col} for drawing the star.
#' @param cstar \code{numeric}. The size of the star.
#' @param ellipses \code{logical}. Whether to draw bivariate confidence
#' ellipses.
#' @param col.ellipse The \code{col} for drawing these ellipses.
#' @param cellipse \code{numeric}. The size of this ellipse.
#' @param axesell \code{logical}. Whether to draw the ellipses axes.
#' @param chull \code{logical}. Whether to draw a convex hull.
#' @param col.chull The \code{col} for drawing convex hulls.
#' @param optchull \code{numeric}. A vector of quantiles of the chulls.
#' @param arrows \code{logical}. Whether to draw variables arrows.
#' @param edge.arrow \code{logical}. Whether to neutralise arrows.
#' @param box.arrow \code{logical}. Whether to draw boxes around arrows'
#' labels.
#' @param maxnb.arrow \code{numeric}. Only the \code{maxnb.arrow} most
#' important variables will be draw.
#' @param dratio.arrow \code{numeric}. Same idea as above but here the
#' threshold is a relative to the size of the grid.
#' @param shapes \code{logical}. Whether to plot shapes.
#' @param pos.shp \code{character}, any of the \code{("li", "circle", "range")}
#' methods. It specifies the way shapes have to be drawn. \code{"li"} draws
#' shapes on the actual positions on the factorial map ; \code{"circle"} draws
#' shapes on a circle with origin as the center ; \code{"full"} (not available
#' in \link{morpho.space}) allows to cover the whole plotting region ;
#' \code{"range"} draws shape on a rectangle that covers PC1 and PC2 range.
#' Alternatively, a two columns matrix of coordinates where to calculate
#' shapes.
#' @param nr.shp \code{numeric}. The number of shape rows.
#' @param nc.shp \code{numeric}. The number of shape columns.
#' @param amp.shp \code{numeric}. An amplifying factor for shape deformation.
#' @param scale.shp \code{numeric}. The size of shapes, relatively to grid size
#' (0.5 = half of the grid size).
#' @param nb.pts.shp \code{numeric}. The number of point to use to reconstruct
#' shapes.
#' @param first.point.shp \code{logical}. Whether to plot or not the first
#' point when plotting shapes.
#' @param rotate.shp \code{numeric}. If specified, the angles (in radians) to
#' counter-clockwise rotate shapes plotted.
#' @param circle.nb.shp \code{integer}. When \code{pos == "circle"}, the number
#' of shapes on the circle.
#' @param circle.r.shp \code{numeric}. When \code{pos == "circle"}, the circle
#' radius.
#' @param col.shp A color string for filling the shapes.
#' @param border.shp A color string for shape borders.
#' @param rug \code{logical}. Whether to add rug on axes (see \link{rug}).
#' @param rug.ticksize \code{numeric}. The relative size of rug.
#' @param rug.col A color string for rug ticks.
#' @param eigen \code{logical}. Whether to add the eigen values barplot.
#' @param eigen.ratio \code{numeric}. The relative size of the eigen values
#' barplot.
#' @param palette A color palette for group colors such as those produced by
#' \link{colorRampPalette}.
#' @param title A \code{character} string to be add on the graph.
#' @param legend \code{logical}. Whether to add or not a legend.
#' @param center.orig \code{logical}. Whether to center the graphical window on
#' the origin.
#' @param zoom.plot \code{numeric}. Will help you to keep your distances, e.g.
#' the value of magnification. Requires \code{center.orig} to be set true.
#' @seealso \link{pca}, \link{morpho.space}.
#' @references See the papers below that introduce \code{ade4} and also the
#' package's homepage: \url{http://pbil.univ-lyon1.fr/ADE-4/} and particularly
#' the file called "td83" : Lobry JR. 2010. Les fonctions graphiques 2D du
#' paquet ade4.
#' 
#' Dray, S. and Dufour, A.B. (2007): The ade4 package: implementing the duality
#' diagram for ecologists. \emph{Journal of Statistical Software}.
#' \bold{22}(4): 1-20.
#' 
#' Chessel, D. and Dufour, A.B. and Thioulouse, J. (2004): The ade4 package-I-
#' One-table methods. \emph{R News}. \bold{4}: 5-10.
#' 
#' Dray, S. and Dufour, A.B. and Chessel, D. (2007): The ade4 package-II:
#' Two-table and K-table methods. \emph{R News}. \bold{7}(2): 47-52.
#' @keywords Multivariate Analysis
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot, nb.h=32)
#' botD <- pca(botF)
#' dudi.plot(botD)
#' dudi.plot(botD, 1, title="botD with no class but with ellipses")
#' dudi.plot(botD, fac=1, chull=TRUE, rug=FALSE, shape=FALSE, title="botD with convex hull")
#' dudi.plot(botD, fac=1, ellipses=FALSE, neighbors=TRUE, shapes=FALSE, star=FALSE,
#' 	col.nei="black", title="botD with Gabriel's neighboring graph")
#' dudi.plot(botD, labels=TRUE, points=FALSE, boxes=FALSE, shapes=TRUE, pos.shp="li",
#' 	title="botD with labels and reconstructed shapes")
#' dudi.plot(botD, 1, points=FALSE, labels=TRUE, boxes=FALSE, shapes=FALSE,
#' 	title="botD with labels and ellipse")
#' dudi.plot(botD, 1, arrows=TRUE, dratio.arrow=0.2, shapes=FALSE,
#' 	title="botD with harmonic correlations")
#' # With some fake factors
#' botD <- pca(botF)
#' dudi.plot(botD, "type", palette=col.gallus,
#'     rotate.shp=pi/2, title="botD with classes") # rotated shapes
#' dudi.plot(botD, "type", palette=col.gallus, eigen=TRUE, title="botD with eigen values")
#' dudi.plot(botD, "type", pos.shp="full", title="botD with shapes(1)")
#' dudi.plot(botD, "type", pos.shp="range", scale.shp=0.5, shapes=TRUE,
#' 	border.shp="firebrick3", col.shp=NA, center.orig=TRUE, 
#' 	zoom.plot=0.8, title="botD with shapes(2)")
#' dudi.plot(botD, "type", pos.shp="circle", center.orig=TRUE, title="botD with shapes(3)")
#' dudi.plot(botD, "type", pos.shp="range", scale.shp=0.5, title="botD with shapes(4)")
#' dudi.plot(botD, pos.shp=as.matrix(expand.grid(seq(-0.05, 0.05, 0.025),
#'     seq(-0.05, 0.05, 0.025)))) # an example with a matrix provided to pos.shp
#' 
#' 
#' @export dudi.plot
dudi.plot <- function(dudi, fac = NULL, xax = 1, yax = 2, grid = TRUE,
                      points     = TRUE,  pch.points=1,  col.points="black", cex.points=0.8,
                      labels     = FALSE, label=rownames(dudi$li), boxes=TRUE, clabel=0.6,
                      neighbors  = FALSE, col.nei="grey90",  lwd.nei=0.5,
                      star       = TRUE,  col.star="grey60", cstar=1,
                      ellipses   = TRUE,  col.ellipse="grey30", cellipse=1, axesell=TRUE,
                      chull      = FALSE, col.chull="grey30", optchull = c(0.5, 1),
                      arrows     = FALSE, edge.arrow=FALSE, box.arrow=TRUE, maxnb.arrow=10, dratio.arrow=0.2, 
                      shapes     = TRUE,  pos.shp=c("li", "circle", "range", "full")[3],
                      nr.shp=6, nc.shp=5, amp.shp=1, scale.shp=0.666, nb.pts.shp=300, first.point.shp=FALSE, rotate.shp=0,
                      circle.nb.shp=12, circle.r.shp,
                      col.shp="#70809011", border.shp="#708090",      
                      rug        = TRUE, rug.ticksize=0.01, rug.col="#708090",
                      eigen      = FALSE, eigen.ratio=0.2,
                      palette    = col.sari,
                      title      = substitute(dudi), legend=FALSE,
                      center.orig= FALSE,
                      zoom.plot  = 1){
  
  # we prepare and check a bit
  if (!missing(fac)) {
    if (!is.factor(fac)) {
      if (ncol(dudi$fac)==0) { fac <- factor(rep("", nrow(dudi$li))) } else {fac <- dudi$fac[, fac]}}
    if ((nlevels(fac) > 1)) {
      if (missing(col.star))    col.star    <- paste(palette(nlevels(fac)), "33", sep="")
      if (missing(col.ellipse)) col.ellipse <- palette(nlevels(fac))
      if (missing(col.chull))   col.chull   <- palette(nlevels(fac))} }
  
  # we initialize the factorial map
  if (center.orig) {
    li.2      <- apply(dudi$li[,c(xax, yax)], 2, function(x) x^2)
    li.len    <- apply(li.2, 1, function(x) sqrt(sum(x)))
    xw <- max(li.len)*(1/zoom.plot)
    yw <- min(li.len)*(1/zoom.plot)
    s.label(dudi$li, xax=xax, yax=yax, xlim=c(-xw, xw), ylim=c(-yw, yw), clabel=0, cpoint=0, sub=title, grid=grid)
  } else {     
    s.label(dudi$li, xax=xax, yax=yax, clabel=0, cpoint=0, sub=title, grid=grid)}
  
  # size of the grid
  xaxp <- par("xaxp")
  ax <- (xaxp[2] - xaxp[1])/xaxp[3]
  yaxp <- par("yaxp")
  ay <- (yaxp[2] - yaxp[1])/yaxp[3]
  d <- min(ax, ay)
  
  # we redefine shorter margins
  op <- par("mar")
  par(mar=rep(0.1, 4))
  
  # rug
  if (rug) {
    rug(dudi$li[, xax], side=1, ticksize=rug.ticksize, col=rug.col, lwd=0.4)
    rug(dudi$li[, yax], side=2, ticksize=rug.ticksize, col=rug.col, lwd=0.4)
    box()}
  
  # neighbors network
  if (neighbors) {
    fun <- function(x, coo, col, lwd) {
      segments(coo$x[x[1]], coo$y[x[1]],
               coo$x[x[2]], coo$y[x[2]], 
               col = col, lwd = lwd)}
    neig <- nb2neig(tri2nb(dudi$li[, c(xax, yax)]))
    coo  <- list(x=dudi$li[, xax], y=dudi$li[, yax])
    apply(unclass(neig), 1, fun,
          coo = coo, col=col.nei, lwd=lwd.nei)}
  
  # star*
  if (star & !is.null(fac)) {
    s.class(dudi$li, xax=xax, yax=yax, fac=fac, clabel=0, cpoint=0, add.plot=TRUE,
            cstar=cstar, col=col.star, cellipse=0)}
  
  # ellipses*
  if (ellipses & !is.null(fac)) {
    s.class(dudi$li,  xax=xax, yax=yax, fac=fac,
            clabel=0, cpoint=0, add.plot=TRUE,
            cstar=0, col=col.ellipse, cellipse=cellipse, axesell=axesell)}
  
  # chull*
  if (chull & !is.null(fac)) {
    s.chull(dudi$li,xax=xax, yax=yax, fac=fac, col=col.chull, 
            optchull=optchull, add.plot=TRUE)}
  
  # arrow - bloody dirty below
  if (arrows) {
    arr.2      <- apply(dudi$co[,c(xax, yax)], 2, function(x) x^2)
    arr.len    <- apply(arr.2, 1, function(x) sqrt(sum(x)))
    if (maxnb.arrow > length(dudi$cw)) { maxnb.arrow <- length(dudi$cw) }
    arr.sorted <- order(arr.len, decreasing=TRUE)[1:maxnb.arrow]
    arr.disp <- if (missing(dratio.arrow)) {
      arr.len[arr.sorted] > 0
      } else {
      arr.len[arr.sorted] > d*dratio.arrow }
    if (sum(arr.disp)>0) {
      #arr.disp   <- arr.sorted[ arr.len[arr.sorted] > d*dratio.arrow ]
      arr.co <- dudi$co[names(which(arr.disp)), c(xax, yax)]
      #s.arrow(dudi$co[names(which(arr.disp)), c(xax, yax)],
      #        label = rownames(dudi$co[arr.disp, c(xax, yax)]),
      #        edge=edge.arrow, add.plot=TRUE, boxes=box.arrow)
      s.arrow(arr.co, 1, 2, 
              label = rownames(arr.co),
              edge=edge.arrow, add.plot=TRUE,boxes=box.arrow, clabel=clabel)
      
      
      }}
  
  # shapes
  if (!is.null(dudi$method)) { # for use on dudi.pca objects
  if ((dudi$method != "tFourier")) {
    if (shapes) {
    if (!is.matrix(pos.shp)) {
      if (pos.shp=="full") {
        w <- par("usr")
        pos.shp <- as.matrix(expand.grid(seq(w[1]+d/2, w[2]-d/2, len=nr.shp),
                                         seq(w[3]+d/2, w[4]-d/2, len=nc.shp)))}}
    shapes <- morpho.space(dudi, xax=xax, yax=yax, plot=FALSE, layer=TRUE,
                           nb.pts=nb.pts.shp, pos.shp=pos.shp,
                           nr.shp = nr.shp, nc.shp = nc.shp, amp.shp = 1, 
                           scale.shp = d*scale.shp, rotate.shp = rotate.shp,
                           circle.nb.shp = circle.nb.shp, circle.r.shp = circle.r.shp,                         
                           col.shp="#70809011", border.shp="#708090", first.point=first.point.shp, pch.pts=NA)}}
  }
  # labels and points
  if (points) {
  repeach <- function(x, each){ # forgot that but probbaly dirty
    if (length(x) != length(each)) return(rep(x[1], sum(each)))
    res <- vector(mode = class(x[1]))
    for (i in seq(along=x)) {
      res <- append(res, rep(x[i], each[i]))}
    return(res)}
  if (!is.null(fac)) {
    nb <- table(fac)
    if (missing(pch.points)) {
      pch.points <- repeach(pch.points, nb)}
    if (missing(col.points)) {
      #col.points <- repeach(palette(nlevels(fac)), nb)
      col.points <- palette(nlevels(fac))[fac]
      }
    cex.points <- repeach(cex.points, nb)} 
  points(dudi$li[, c(xax, yax)], pch=pch.points, col=col.points, cex=cex.points)
  }
  
  #  s.label(dudi$li, xax=xax, yax=yax, clabel=0, cpoint=cpoint, pch=pch, add.plot=TRUE)}
  # labels
  if (labels) {
    s.label(dudi$li, xax=xax, yax=yax, clabel=clabel, cpoint=0, boxes=boxes, add.plot=TRUE)}
  
  # ellipses* (only for the labels) #probably not the most orthodox option
  if (ellipses & !is.null(fac)) {
    s.class(dudi$li,  xax=xax, yax=yax, fac=fac,
            clabel=clabel, cpoint=0, add.plot=TRUE,
            cstar=0, col=NA, cellipse=0, axesell=FALSE)}
  
  #legend
  if ((legend) | (missing(legend) & !is.null(fac))) {
    legend("topright", col=palette(nlevels(fac)), lwd=2,
           legend=levels(fac), bty="n")}
  # eigen
  if (eigen) {
    par("mar"=op)
    add.scatter.eig(dudi$eig, nf=dudi$nf, xax=xax, yax=yax, eigen.ratio, posi="bottomright")}
  
  # we restore the margins
  par("mar"=op)
}

# PC contribution to shape


#' Shape variation along PC axis
#' 
#' \code{PC.contrib} calculates and plots shape variation along Principal
#' Component axes.
#' 
#' 
#' @usage PC.contrib(dudi, PC.r = 1:dudi$nf, sd = 2, cols = rep(NA, 3), borders
#' = c("#000080", "#000000", "#EE0000"), lwd = 1, nb.pts = 300, plot = TRUE,
#' legend = TRUE)
#' @param dudi a dudi.pca object.
#' @param PC.r A range of \code{integer}s indicating the PC axes on which to
#' display shape variation.
#' @param sd A \code{numeric} to indicate +/- the number of standard deviations
#' to consider.
#' @param cols A color string of length 3 to use to fill the shapes.
#' @param borders A color string of length 3 to use for the shape borders.
#' @param lwd A \code{numeric} to specify the \code{lwd} of the shapes.
#' @param nb.pts \code{integer} to specify the number of points to draw the
#' shapes.
#' @param plot \code{logical}. Whether to plot the results.
#' @param legend \code{logical}. Whether to add the legend.
#' @return Invisibly returns a list that contains the coordiantes of the
#' calculated shapes.
#' @keywords Multivariate Analysis
#' @examples
#' 
#' data(bot)
#' #botF <- eFourier(bot)
#' #botD <- pca(botF)
#' #PC.contrib(botD)
#' #PC.contrib(botD, sd=1) # only one sd
#' #PC.contrib(botD, PC.r=1:3, sd=1, cols=paste(col.sari(3), "55", sep=""),
#' #   borders=rep("black", 3), legend=FALSE)
#' # only 3 PC axis and some cosmectics.
#' 
#' @export PC.contrib
PC.contrib <- function(dudi, PC.r=1:dudi$nf, sd=2,
                       cols=rep(NA, 3), borders=c("#000080", "#000000", "#EE0000"),
                       lwd=1, nb.pts=300, plot=TRUE, legend=TRUE){
  # mean.coo <- efourier.i(coeff.split(dudi$mean.shp, nb.h=length(dudi$mean.shp)/4))
  if ((length(PC.r) > dudi$nf) | (max(PC.r) > dudi$nf)) {
    stop("The PC.r must correspond to PC axes present in the dudi object")}
  res <- list()
  for (i in seq(along=PC.r)) {
    pos.i <- sd*sd(dudi$li[, PC.r[i]])
    shp.i <- pca2shp(pos=matrix(c(-pos.i, 0, pos.i), nrow=3),
                     rot=as.matrix(dudi$c1[, PC.r[i]]), 
                     mean.shp=dudi$mean.shp, method=dudi$method,
                     trans=FALSE, nb.pts=nb.pts)
    shp.i <- a2l(shp.i) # we reconvert to list
    names(shp.i) <- paste0(rep(paste0("PC", PC.r[i]), 3), c("-", "m", "+"))
    res <- append(res, shp.i)}
  if (plot) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mar=c(1, 2, 1, 1), xpd=NA)
    n   <- length(PC.r)
    pos <- cbind(1:n, matrix((n+1):(4*n), nrow=n, ncol=3, byrow=TRUE))
    plot(NA, asp = 1, xlim = c(0, 4), ylim = c(0, n), 
         xaxs = "i", yaxs = "i", frame = FALSE, ann = FALSE, axes = FALSE)
    res.t <- lapply(res, coo.template, size=0.9)
    for (i in 1:n) {
      coo.draw(coo.trans(res.t[[(i-1)+1]], 0.5, n-((i-1)+0.5)),
               col=cols[1], border=borders[1], lwd=lwd, points = FALSE, first.point=FALSE)
      coo.draw(coo.trans(res.t[[(i-1)+2]], 0.5, n-((i-1)+0.5)),
               col=cols[2], border=borders[2], lwd=lwd, points = FALSE, first.point=FALSE)
      coo.draw(coo.trans(res.t[[(i-1)+3]], 0.5, n-((i-1)+0.5)),
               col=cols[3], border=borders[3], lwd=lwd, points = FALSE, first.point=FALSE)
    }
    for (i in 1:(n*3)) {
      pos.x <- rep(0:2 + 1.5, times=n)
      pos.y <- rep((n-1):0*1 + 0.5, each=3)
      coo.draw(coo.trans(res.t[[i]], pos.x[i], pos.y[i]),
               col=cols[((i-1) %% 3) +1], border=borders[((i-1) %% 3) +1],
               lwd=lwd, points=FALSE, first.point=FALSE)}
    if (legend) {
      text(1.5, n, labels=paste("-", sd, "s.d.", sep=""), adj=0.5)
      text(2.5, n, labels="Mean", adj=0.5)
      text(3.5, n, labels=paste("+", sd, "s.d.", sep=""), adj=0.5)
      text(0, (n:1) - 0.5, labels=paste("PC", PC.r), adj=1)
    }
  }
  invisible(res)}

# Thin Plate Spline ##################################################



#' Thin Plate Splines for 2D data.
#' 
#' \code{tps2d} is the core function for Thin Plate Splines. It is used
#' internally but might be useful elsewhere.
#' 
#' 
#' @usage tps2d(grid0, fr, to)
#' @param grid0 A matrix of coordinates on which to calculate deformations.
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @return Returns a matrix of \code{(x; y)} coordinates with TPS-interpolated
#' deformations.
#' @seealso The \link{tps.grid},\link{tps.iso}, \link{tps.arr} functions use
#' \code{tps2d}.
#' @keywords coo Utilities
#' @export tps2d
tps2d <- function(grid0, fr, to){
  if (is.closed(fr)) fr <- coo.unclose(fr)
  if (is.closed(to)) to <- coo.unclose(to)
  p  <- nrow(fr)
  q  <- nrow(grid0)
  P  <- matrix(NA, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      r2     <- sum((fr[i,]-fr[j,])^2)
      P[i,j] <- r2*log(r2)}}
  P[is.na(P)] <- 0
  Q  <- cbind(1, fr)
  L  <- rbind(cbind(P, Q), cbind(t(Q), matrix(0,3,3)))
  m2 <- rbind(to, matrix(0, 3, 2))
  coefx <- solve(L)%*%m2[, 1]
  coefy <- solve(L)%*%m2[, 2]
  fx <- function(fr, grid0, coef) {
    Xn <- numeric(q)
    for (i in 1:q) {
      Z     <- apply((fr-matrix(grid0[i, ], p, 2, byrow=TRUE))^2, 1, sum)
      Xn[i] <- coef[p+1]+coef[p+2]*grid0[i,1]+coef[p+3]*grid0[i,2]+
        sum(coef[1:p]*(Z*log(Z)))}
    return(Xn)}
  grid1 <- cbind(fx(fr, grid0, coefx), fx(fr, grid0, coefy))
  return(grid1)}



#' Deformation grids using Thin Plate Splines.
#' 
#' \code{tps.grid} calculates and plots deformation grids between two
#' configurations.
#' 
#' 
#' @usage tps.grid(fr, to, amp=1, plot.full=TRUE, grid.outside = 0.2, grid.size
#' = 20, grid.col = "grey40", shp = TRUE, shp.col = rep(NA, 2),
#' shp.border=col.gallus(2), shp.lwd = c(2, 2), shp.lty = c(1, 1))
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param plot.full \code{logical}. If \code{FALSE} graphical window will
#' encompasses the entire outlines but maybe not the entire grid.
#' @param grid.outside A \code{numeric} that indicates how much the grid
#' extends beyond the range of outlines. Expressed as a proportion of the
#' latter.
#' @param grid.size A \code{numeric} to specify the number of grid cells on the
#' longer axis on the outlines.
#' @param grid.col A color for drawing the grid.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.col Two colors for filling the shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- meanShapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.grid(fr, to, amp=3, grid.size=40)
#' 
#' @export tps.grid
tps.grid <- function(fr, to, amp=1, plot.full=TRUE, grid.outside = 0.2,
                     grid.size = 20, grid.col   = "grey40",
                     shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                     shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  # simple magnification
  if (!missing(amp)) to <- to + (to-fr)*amp
  # we prepare the grid
  x1     <- min(to[, 1])
  x2     <- max(to[, 1])
  y1     <- min(to[, 2])
  y2     <- max(to[, 2])
  rx     <- x2 - x1
  ry     <- y2 - y1
  dim.grid <- if (rx > ry) { c(grid.size, round(grid.size*ry / rx)) } else { c(round(grid.size*rx / ry), grid.size) }
  xgrid0 <- seq(x1-rx*grid.outside, x2+rx*grid.outside, length=dim.grid[1])
  ygrid0 <- seq(y1-ry*grid.outside, y2+ry*grid.outside, length=dim.grid[2])
  grid0 <- as.matrix(expand.grid(xgrid0, ygrid0))
  grid1 <- tps2d(grid0, fr, to)
  if (plot.full){
    wdw <- apply(rbind(grid0, grid1), 2, range)
  } else {
    wdw <- apply(rbind(fr, to), 2, range)}
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, ann=FALSE, axes=FALSE, mar=rep(0, 4))
  for (i in 1:dim.grid[2]) lines(grid1[(1:dim.grid[1]) + (i-1)*dim.grid[1],], col=grid.col)
  for (i in 1:dim.grid[1]) lines(grid1[(1:dim.grid[2]) * dim.grid[1]-i+1,],   col=grid.col)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}
}




#' Deformation "vector field" using Thin Plate Splines.
#' 
#' \code{tps.arr}(ows) calculates deformations between two configurations and
#' illustrate them using arrows.
#' 
#' 
#' @usage tps.arr(fr, to, amp=1, palette = col.summer, arr.nb = 100, arr.levels
#' = 100, arr.len = 0.1, arr.ang = 30, arr.lwd = 1, arr.col = "grey50", shp =
#' TRUE, shp.col = rep(NA, 2), shp.border=col.gallus(2), shp.lwd = c(2, 2),
#' shp.lty = c(1, 1))
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param palette A color palette such those included in Momocs or produced
#' with \link{colorRampPalette}.
#' @param arr.nb A \code{numeric}. The number of arrows to calculate.
#' @param arr.levels A \code{numeric}. The number of levels for the color of
#' arrows.
#' @param arr.len A \code{numeric}. The length of arrows.
#' @param arr.ang A \code{numeric}. The angle for arrows' heads.
#' @param arr.lwd A \code{numeric}. The \code{lwd} for drawing arrows.
#' @param arr.col If \code{palette} is not used the color for arrwos.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.col Two colors for filling the shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- meanShapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.arr(fr, to, arr.nb=400, palette=col.sari, amp=3)
#' 
#' @export tps.arr
tps.arr <- function(fr, to, amp=1, palette = col.summer,
                    arr.nb = 100, arr.levels = 100, arr.len = 0.1,
                    arr.ang = 30, arr.lwd = 1, arr.col = "grey50",
                    shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), arr.nb, type="regular")@coords
  grid1     <- tps2d(grid0, fr, to)
  # grille simple, on affiche d'abord les deux courbes
  wdw      <- apply(rbind(fr, to), 2, range)
  plot(NA, xlim=wdw[, 1]*1.05, ylim=wdw[, 2]*1.05, asp=1, axes=FALSE, ann=FALSE, mar=rep(0,4))
  if (missing(arr.levels)) {arr.levels = arr.nb}
  if (!missing(palette)) {
    q.lev   <- cut(edm(grid0, grid1), breaks=arr.levels, labels=FALSE)
    arr.cols <- palette(arr.levels)[q.lev]
  } else {
    arr.cols <- rep(arr.col, nrow(grid0))}
  arrows(grid0[, 1], grid0[, 2], grid1[, 1], grid1[, 2],
         length=arr.len, angle=arr.ang, lwd=arr.lwd, col=arr.cols)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}
}




#' Deformation isolines using Thin Plate Splines.
#' 
#' \code{tps.iso} calculates deformations between two configurations and map
#' them with or without isolines.
#' 
#' 
#' @usage tps.iso(fr, to, amp=1, palette = col.summer, iso.nb = 500, iso.levels
#' = 12, cont=TRUE, cont.col="black", shp = TRUE, shp.col = rep(NA, 2),
#' shp.border=col.gallus(2), shp.lwd = c(2, 2), shp.lty = c(1, 1))
#' @param fr The reference \eqn{(x; y)} coordinates.
#' @param to The target \eqn{(x; y)} coordinates.
#' @param amp An amplification factor of differences between \code{fr} and
#' \code{to}.
#' @param palette A color palette such those included in Momocs or produced
#' with \link{colorRampPalette}.
#' @param iso.levels \code{numeric}. The number of levels for mapping the
#' deformations.
#' @param iso.nb A \code{numeric}. The number of points to use for the
#' calculation of deformation.
#' @param cont \code{logical}. Whether to draw contour lines.
#' @param cont.col A color for drawing the contour lines.
#' @param shp \code{logical}. Whether to draw shapes.
#' @param shp.col Two colors for filling the shapes.
#' @param shp.border Two colors for drawing the borders.
#' @param shp.lwd Two \code{lwd} for drawing shapes.
#' @param shp.lty Two \code{lty} fro drawing the shapes.
#' @return No returned value.
#' @keywords coo Utilities
#' @examples
#' 
#' data(bot)
#' botF <- eFourier(bot)
#' x <- meanShapes(botF, "type", nb.pts=80)
#' fr <- x$beer
#' to <- x$whisky
#' tps.iso(fr, to, iso.nb=2000, amp=3)
#' 
#' @export tps.iso
tps.iso <- function(fr, to, amp=1, palette = col.summer,
                    iso.nb = 500, iso.levels = 12, cont=TRUE, cont.col="black",
                    shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1)){  
  if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), iso.nb, type="regular")@coords
  grid1  <- tps2d(grid0, fr, to)
  def    <- edm(grid0, grid1)
  x1     <- length(unique(grid0[,1]))
  y1     <- length(unique(grid0[,2]))
  im     <- matrix(NA,x1,y1)
  xind   <- (1:x1)[as.factor(rank(grid0[,1]))]
  yind   <- (1:y1)[as.factor(rank(grid0[,2]))]
  n      <- length(xind)
  for (i in 1:n) im[xind[i], yind[i]] <- def[i]
  iso.cols <- palette(iso.levels)
  x <- sort(unique(grid0[,1]))
  y <- sort(unique(grid0[,2]))
  image(x, y, im, col=iso.cols, asp=1, xlim=range(x)*1.05, ylim=range(y)*1.05,
        axes=FALSE, frame=FALSE, ann=FALSE)
  if (cont) contour(x, y, im, nlevels=iso.levels, add=TRUE, drawlabels=FALSE, col=cont.col)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}}

# Miscellaneous ######################################################



#' Calculate ellipse parameters.
#' 
#' Given a \code{matrix} or \code{list} of coordinates that defines an ellipse,
#' returns its geometrical parameters, namely: half-length of the major and the
#' minor axis and excentricity.
#' 
#' The aim of this function is to provide complementary descriptors for
#' ellipses that are more explicit for humans than harmonic coefficients. For
#' that reason they are also used directly instead of harmonic coefficients
#' (See Schmittbuhl et al. 2003). The same study provided an exact approach to
#' directly obtain ellipse parameters from harmonic coefficients but is not yet
#' implemented in Momocs. So far, these parameters are estimated using matrices
#' of coordinates obtained from harmonic coefficients, \emph{i.e.} by "redrawn"
#' ellipses. Keep in mind that even if error rate is low (moreover if the
#' number of points provided is high), the values returned must not be
#' considered as exact.
#' 
#' @usage ellpar(coo)
#' @param coo a \code{matrix} or a \code{list} of \code{(x; y)} coordinates.
#' @return \item{a }{\code{numeric}. Half length of the major axis.} \item{b
#' }{\code{numeric}. Half length of the minor axis.} \item{e }{\code{numeric}.
#' Ellipse excentricity.}
#' @seealso \link{ellipse.par}, the corresponding method for \code{Coe}-class
#' objects.
#' @references Schmittbuhl M, Allenbach B, Le Minor J-M, Schaaf A, Minor
#' J-marie L. 2003. Elliptical Descriptors: Some Simplified Morphometric
#' Parameters for the Quantification of Complex Outlines. \emph{Mathematical
#' Geology} \bold{35}: 853-871.
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' 
#' @export ellpar
ellpar <- function(coo){
  if (is.list(coo)) coo <- cbind(coo$x, coo$y) ### ML
  coo <- coo %*% svd(var(coo))$u
  a <- max(coo[, 1])
  b <- max(coo[, 2])
  e <- sqrt((a^2 - b^2)/a^2)
  list(a=a, b=b, e=e)}

dev.plot       <- function(mat, dev, cols, x=1:ncol(mat), 
                           lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
                           plot=FALSE, main="Deviation plot", xlab="", ylab="Deviations") {
  # we prepare and check a bit
  r <- nrow(mat)
  if (!missing(dev)){
    if (any(dim(mat)!=dim(dev))) {
      stop("mat and dev must be of the same dimension")}}
  if (missing(cols))   {cols <- rep("#000000", r)}
  if (length(cols)!=r) {cols <- rep("#000000", r)}
  # we call a new plot if required
  if (plot) {
    if (missing(dev)) {
      ylim <- range(mat)
      } else { ylim <- c(min(mat+dev), max(mat+dev)) }
    plot(NA, xlim=range(x), ylim=ylim, main=main, xlab=xlab, ylab=ylab, las=1, xaxs="i", yaxs="i")
    axis(1, at=1:ncol(mat))}
  # if a deviation matrix is provided
  if (!missing(dev)){
    for (i in 1:r){
      # if required, we draw the background polygons
      if (poly) {
        polygon(x = c(x, rev(x)),
                y = c(mat[i, ] - dev[i, ], rev(c(mat[i, ] + dev[i, ]))),
                col=paste0(cols[i], "55"), border=NA)}
      # if required we draw the dev segments
      if (segments) {
      segments(x,    mat[i, ] - dev[i, ], x, mat[i, ]    + dev[i, ], col=cols[i], lwd=0.5)
      segments(x-bw, mat[i, ] - dev[i, ], x+bw, mat[i, ] - dev[i, ], col=cols[i], lwd=0.5)
      segments(x-bw, mat[i, ] + dev[i, ], x+bw, mat[i, ] + dev[i, ], col=cols[i], lwd=0.5)}}}
  # if a dev matrix is not provided, we simply draw lines
  if (lines) {
    for (i in 1:nrow(mat)) {
      if (lines) {
      lines(x, mat[i, ], col=cols[i], type="o", cex=0.25, pch=20)}}}}

# returns difference angle and norm ratios between two vectors given as 4 numeric.


#' Some vector utilities.
#' 
#' Returns ratio of norms and signed angle between two vectors provided as four
#' numeric.
#' 
#' 
#' @usage vecs.param(r1, i1, r2, i2)
#' @param r1 the "real" part of the first vector, i.e. difference in
#' x-coordinates.
#' @param i1 the "imaginary" part of the first vector, i.e. difference in
#' y-coordinates.
#' @param r2 the "real" part of the second vector, i.e. difference in
#' x-coordinates.
#' @param i2 the "imaginary" part of the second vector, i.e. difference in
#' y-coordinates.
#' @return A list with two components: \code{r.norms} the ratio of (norm of
#' vector 1)/(norm of vector 2) and \code{d.angle} the signed angle 'from' the
#' first 'to' the second vector.
#' @keywords Utilities
#' @examples
#' 
#' vecs.param(1, 0, 0, 2)
#' 
#' @export vecs.param
vecs.param <- function(r1, i1, r2, i2){
  x <- c(r1, i1, r2, i2)
  if (!is.numeric(x)) {stop("4 numeric must be passed.")}
  if (length(x)!=4)   {stop("4 numeric must be passed.")}
  r.norms <- sqrt((r2^2 + i2^2)) / sqrt((r1^2 + i1^2))
  d1 <- sqrt(sum(r1^2 + i1^2))
  d2 <- sqrt(sum(r2^2 + i2^2))
  return(list(r.norms=d1/d2, d.angle=atan2(i2, r2) - atan2(i1, r1)))}

# Calculates harmonic power given a list from e/t/rfourier


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
#' @keywords Utilities
#' @examples
#' 
#' data(bot)
#' ef <- efourier(bot@coo[[1]], 24)
#' rf <- efourier(bot@coo[[1]], 24)
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

# Color palettes
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
col.gallus <- colorRampPalette(c("#000080", "#FFFFFF", "#EE0000"))
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
col.sari   <- colorRampPalette(c("#551A8B", "#FF7F00"))
col.india  <- colorRampPalette(c("#FF9933", "#138808"))
col.bw     <- colorRampPalette(c("#FFFFFF", "#000000"))
col.wcol   <- function(col.hex) colorRampPalette(c("#FFFFFF", col.hex))
col.bcol   <- function(col.hex) colorRampPalette(c("#000000", col.hex))
              
