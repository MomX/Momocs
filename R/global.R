################################################################################
# 1.  coo.xxx family : basic and domestic operations
# 2. shape descriptors for traditional morphometrics
# 3. plotting coo function and utilities
################################################################################


# 1. coo utilities ------------------------------------------------------------
# a family of functions that do simple functions on 2d coordinates (further
# abbreviated as "shape" (either outlines, open outlines or lanfmarks)
# they can be passed either as two-column matrices colnames ("x" and "y"
# colnaming is not mandatory) or as a list with $x and $y components.
# and returns a (named) matrix of coordinates.
# if you want a list, see ?m2l


#' Checks shape
#'
#' A simple utility, used internally, mostly in the coo functions and methods.
#' Returns a matrix, when passed with either a list or a matrix of coordinates.
#'
#' @param coo a matrix of (x,y) coordinates or a list.
#' @return a matrix of (x,y) coordinates.
#' @family coo
#' @keywords coo
#' @export
#' @examples
#' coo.check("Not a shape")
#' coo.check(matrix(1:10, ncol=2))
#' coo.check(list(x=1:5, y=6:10))
coo.check <- function(coo){
  if (is.matrix(coo)) {return(coo)}
  if (is.list(coo))   {return(l2m(coo))}
  stop("A list or a matrix of (x, y) coordinates must be provided.")}

#' Centers coordinates
#'
#' Returns a shape centered on the origin.
#' 
#' @export coo.center
#' @aliases coo.center
#' @S3method coo.center default
#' @S3method coo.center Out
#' @param coo a matrix of (x,y) coordinates or a list.
#' @return a matrix of (x,y) coordinates.
#' @family coo
#' @keywords coo
#' @examples
#' coo.center(matrix(1:10, ncol=2))
#' coo.center(list(x=1:5, y=6:10))
coo.center <- function(coo){UseMethod("coo.center")}
coo.center.default <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 2, function(x) x - mean(x)))}

coo.center.Out <- function(Out){
  Out$coo <- lapply(Out$coo, coo.center)
  return(Out)}

#' Scales coordinates
#'
#' Scales the coordinates by a 'scale' factor. If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#' 
#' @export coo.scale
#' @aliases coo.scale 
#' @S3method coo.scale default
#' @S3method coo.scale Out
#' 
#' @param coo a matrix of (x,y) coordinates or a list.
#' @return a matrix of (x,y) coordinates.
#' @family coo
#' @keywords coo
#' @examples
#' coo.scale(matrix(1:10, ncol=2))

coo.scale <- function(coo, scale){UseMethod("coo.scale")}
coo.scale.default <- function (coo, scale=coo.centsize(coo)) {
  coo <- coo.check(coo)
  cp  <- coo.centpos(coo)
  coo <- coo.trans(coo.trans(coo, -cp[1], -cp[2])/scale, cp[1], cp[2])
  return(coo)}
coo.scale.Out <- function(Out, scale){
  #dirty loop but had bad time trying to vectorize it
  if (missing(scale)) {
    scale <- sapply(Out$coo, coo.centsize)}
  if (length(scale) != length(Out)) {
    scale <- rep(scale, length(Out))}
  for (i in seq(along=Out$coo)){
    Out$coo[[i]] <- coo.scale(Out$coo[[i]], scale[i])}
    return(Out)}

#' Rotates coordinates
#'
#' Rotates the coordinates by a 'theta' angle (in radians) If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#' 
#' @export coo.rotate
#' @aliases coo.rotate 
#' @S3method coo.rotate default
#' @S3method coo.rotate Out
#' 
#' @param coo a matrix of (x,y) coordinates or a \code{Out} object.
#' @return a matrix of (x,y) coordinates or a \code{Out} object.
#' @family coo
#' @keywords coo
#' @examples
#' coo.rotate(matrix(1:10, ncol=2))
coo.rotate <- function(coo, theta=0){UseMethod("coo.rotate")}
coo.rotate.default <- function(coo, theta=0){
  coo <- coo.check(coo)
  rmat <- matrix(c(cos(theta), sin(theta),
                  -sin(theta), cos(theta)), nrow=2)
  return(coo %*% rmat)}
coo.rotate.Out <- function(Out, theta=0){
  Out$coo <- lapply(Out$coo, coo.rotate, theta)}
  
  
#' Aligns coordinates
#'
#' Aligns the coordinates along their longer axis using var-cov matrix and eigen values.
#' 
#' @export coo.align
#' @aliases coo.align 
#' @S3method coo.align default
#' @S3method coo.align Out
#' 
#' @param coo a matrix of (x,y) coordinates or a \code{Out} object.
#' @return a matrix of (x,y) coordinates or a \code{Out} object.
#' @family coo
#' @keywords coo
#' @examples
#' coo.align(matrix(1:10, ncol=2))
coo.align <- function(coo){UseMethod("coo.align")}
coo.align.default <- function(coo){
  coo <- coo.check(coo)
  return(coo %*% svd(var(coo))$u)}
coo.align.Out <- function(Out){
  Out$coo <- lapply(Out$coo, coo.align)
  return(Out)}

#' Translates coordinates
#'
#' Translates the coordinatesby a 'x' and 'y' value
#' 
#' @export coo.trans
#' @aliases coo.trans 
#' @S3method coo.trans default
#' @S3method coo.trans Out
#' 
#' @param coo a matrix of (x,y) coordinates or a \code{Out} object.
#' @return a matrix of (x,y) coordinates or a \code{Out} object.
#' @family coo
#' @keywords coo
#' @examples
#' coo.trans(matrix(1:10, ncol=2), 5, 10)
coo.trans <- function(coo, x=0, y=0){UseMethod("coo.trans")}
coo.trans.default <- function(coo, x=0, y=0){
  coo <- coo.check(coo)
  cbind(coo[, 1] + x, coo[, 2] + y) }
coo.trans.Out <- function(Out, x=0, y=0){
  Out$coo <- lapply(Out$coo, coo.trans, x, y)
  return(Out)}

# coo.slide "slides" the coordinates so that the id1-th coordinates becomes the 1st
coo.slide <- function(coo, id1){UseMethod("coo.slide")}
coo.slide.default <- function(coo, id1){
  coo <- coo.check(coo)
  if (id1 == 0) {return(coo)}
  n <- nrow(coo)
  slided.rows <- c(id1:n, 1:(id1-1))
  return(coo[slided.rows, ])}
coo.slide.Out <- function(Out, id1){
  if (length(Out$ldk)==0) stop(" * No landmarks defined.")
  for (i in seq(along=Out$coo)) {
    Out$coo[[i]] <- coo.slide(Out$coo[[i]], Out$ldk[[i]][id1])
    Out$ldk[[i]] <- (Out$ldk[[i]] - (Out$ldk[[i]][id1] -1)) %% nrow(Out$coo[[i]])}
  return(Out)}

# coo.sample samples "n" coordinates among the coordinates provided
coo.sample <- function(coo, n){UseMethod("coo.sample")}
coo.sample.default <- function (coo, n) {
  coo <- coo.check(coo)
  sampled <- round(seq(1, nrow(coo), len = n + 1)[-(n + 1)])
  return(coo[sampled, ])}
coo.sample.Out <- function(Out, n){
  Out$coo <- lapply(Out$coo, coo.sample, n)
  return(Out)}

# coo.sample.rr samples "n" coordinates with a regular radius
coo.sample.rr <- function(coo, n){UseMethod("coo.sample.rr")}
coo.sample.rr.default <- function(coo, n){
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
coo.sample.rr.Out <- function(Out, n){
  Out$coo <- lapply(Out$coo, coo.sample.rr, n)
  return(Out)}

# coo.sample.int samples/interpolates n coordinates, equally spaced along the
# perimeter of the coordinates provided and keeping the first point
coo.sample.int <- function(coo, n){
  coo <- coo.check(coo)
  if (!is.closed(coo)) { coo <- coo.close(coo) }
  orig <- cumsum(coo.perim.pts(coo))
  targ <- seq(0, coo.perim(coo), length=n+1)[-(n+1)]
  coo2 <- matrix(c(coo[1, ], rep(NA, n*2 - 2)), byrow=TRUE, nrow=n, ncol=2)
  for (i in 2:n) {
    k <- max(which(orig <= targ[i]))
    r <- (targ[i] - orig[k]) / (orig[k+1]- orig[k])
    coo2[i, ] <- edi(coo[k, ], coo[k+1, ], r)}
  return(coo2)}
coo.sample.int.Out <- function(Out){
  Out$coo <- lapply(Out$coo, coo.sample.int, n)
  return(Out)}

# coo.smooth smoothes coordinates using a simple weighting moving average
coo.smooth <- function(coo, n){UseMethod("coo.smooth")}
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
coo.smooth.Out <- function(Out, n){
  Out$coo <- lapply(Out$coo, coo.smooth, n)
  return(Out)}

#' Tests if shapes are closed
#'
#' Returns TRUE/FALSE whether the last coordianate of the shapes is the same
#' as the first one.
#' 
#' @export is.closed
#' @aliases is.closed
#' @S3method is.closed default
#' @S3method is.closed Out
#' @param coo a matrix of (x,y) coordinates or a Out object
#' @return a boolean.
#' @family coo
#' @keywords coo
#' @examples
#' is.closed(matrix(1:10, ncol=2))

is.closed <- function(coo){UseMethod("is.closed")}
is.closed.default <- function(coo){
  coo <- coo.check(coo)
  identical(coo[1,], coo[nrow(coo),]) }
is.closed.Out <- function(Out){sapply(Out$coo, is.closed)}

# # is.likelyopen tries to estimate is a matrix of coordinates is likely to be a
# # closed polygon
# is.likelyclosedpolygon <- function(coo) {
#   x <- coo.perim.pts(coo)
#   d <- max(x) / median(x[-which.max(x)])
#   ifelse(d > 3, TRUE, FALSE)}

# coo.close returns a closed shape from an un/closed one.
#coo. <- function(coo){UseMethod("coo.")}
coo.close <- function(coo){UseMethod("coo.close")}
coo.close.default <- function(coo){
  coo <- coo.check(coo)
  ifelse(is.closed(coo), return(coo), return(rbind(coo, coo[1, ])))}
coo.close.Out <- function(Out){
  Out$coo <- lapply(Out$coo, coo.close)
  return(Out)}

# coo.unclose returns an unclosed shape from an un/closed one.
#coo. <- function(coo){UseMethod("coo.")}
coo.unclose <- function(coo){UseMethod("coo.unclose")}
coo.unclose.default <- function(coo){
  coo <- coo.check(coo)
  ifelse(is.closed(coo), return(coo[-nrow(coo), ]), return(coo))}
coo.unclose.Out <- function(Out){
  Out$coo <- lapply(Out$coo, coo.unclose)
  return(Out)}

# coo.centpos returns the (x, y) centroid coordinates of a shape.
coo.centpos <- function(coo){UseMethod("coo.centpos")}
coo.centpos.default <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 2, mean))}

coo.centpos.Out <- function(Out){
  centpos <- t(sapply(Out$coo, coo.centpos))
  colnames(centpos) <- c("x", "y")
  return(centpos)}

# coo.rotate.center rotates a shape of "theta" angles (in radians)
# and with a (x, y) "center".
coo.rotate.center <- function(coo, theta, center=c(0, 0)){
  coo <- coo.trans(coo, -center[1], -center[2])
  coo <- coo.rotate(coo, theta)
  return(coo.trans(coo, center[1], center[2]))}

# coo.perim returns the distances between every edges of a shape.
coo.perim.pts <-  function (coo){
  coo <- coo.check(coo)
  n <- nrow(coo)
  d <- sqrt(apply((coo - coo.slide(coo, n))^2, 1, sum))
  return(d)}

# coo.bookstein registers a new baseline for the shape, with the ldk1-th
# and ldk2-th points being set on (-0.5, 0) and (0.5, 0), respectively.
#coo. <- function(coo){UseMethod("coo.")}

coo.bookstein <- function(coo, ldk1, ldk2){UseMethod("coo.bookstein")}
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

coo.bookstein.Out <- function(Out, ldk1, ldk2){ #id1 ?
  for (i in seq(along=Out$coo)){
    Out$coo[[i]] <- coo.bookstein(Out$coo[[i]], Out$ldk[[i]][ldk1], Out$ldk[[i]][ldk2])}
  return(Out)}
  
  
  
  
  

# coo.baseline is a non-exact baseline registration on t1 and t2 coordinates,
# for the ldk1-th and ldk2-th points. By default it returns Bookstein's coordinates.
#coo. <- function(coo){UseMethod("coo.")}

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
    # returns difference angle and norm ratios between two vectors given as 4 numeric.
    vecs.param <- function(r1, i1, r2, i2){
      x <- c(r1, i1, r2, i2)
      r.norms <- sqrt((r2^2 + i2^2)) / sqrt((r1^2 + i1^2))
      d1 <- sqrt(sum(r1^2 + i1^2))
      d2 <- sqrt(sum(r2^2 + i2^2))
      return(list(r.norms=d1/d2, d.angle=atan2(i2, r2) - atan2(i1, r1)))}
  vi <- vecs.param(rx, ry, tx, ty)
  # we rotate accordingly with a center defined as the first landmark (trans, rot, untrans)
  ref <- coo.trans(ref, -t1x, -t1y)
  ref <- ref / vi$r.norms
  ref <- coo.rotate(ref, -vi$d.angle)
  ref <- coo.trans(ref, t1x, t1y)
  return(ref)}

# coo.force2close returns closed shapes so that (if not already closed), it
# distributes the original dx and dy between the first and last points.
coo.force2close <- function(coo){
  coo <- check.coo
  if (is.closed(coo)) {return(coo)}
  n  <- nrow(coo)
  d  <- coo[1, ] - coo[n, ]
  dm <- cbind(seq(0, d[1], length=n), seq(0, d[2], length=n))
  return(coo + dm)}

# coo.up only retains the coordinates with positive/zero y-coordinates
#coo. <- function(coo){UseMethod("coo.")}

coo.up <- function(coo){
  up <- coo[coo[,2]>=0,]
  return(up)}

# coo.up only retains the coordinates with negative/zero y-coordinates
#coo. <- function(coo){UseMethod("coo.")}

coo.down <- function(coo){
  coo <- coo.check(coo)
  return(coo[coo[, 2]<=0,])}

# coo.centdist returns, for every point, the distance to the centroid
#coo. <- function(coo){UseMethod("coo.")}

coo.centdist <- function(coo){
  coo <- coo.check(coo)
  return(apply(coo, 1, function(x) ed(coo.centpos(coo), x)))}

# coo.align.xax align the longest axis of a shape along the x-axis
#coo. <- function(coo){UseMethod("coo.")}

coo.align.xax <- function(coo){
  coo <- coo.check(coo)
  coo <- coo.align(coo)
  return(coo.trans(coo, x=0, y=- coo.centpos(coo)[2]))}

# coo.rev returns the reverse suite of coordinates, change shape's orientation
#coo. <- function(coo){UseMethod("coo.")}

coo.rev <- function(coo){
  coo <- coo.check(coo)
  return(coo[nrow(coo):1,])}

# coo.ldk allows to interactively define a "nb.ldk" number of landarks on a shape.
#coo. <- function(coo){UseMethod("coo.")}

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
    cat("*")
  }
  cat("]\n")
  return(ldk)}

# 2. coo shape descriptors -----------------------------------------------------

# coo.centsize returns the centroid size a shape.
#coo. <- function(coo){UseMethod("coo.")}

coo.centsize <- function(coo){
  coo  <- coo.check(coo)
  cent <- coo.centpos(coo)
  cs   <- mean(apply(coo, 1, function(x) sqrt(sum((x-cent)^2))))
  return(cs)}

# coo.perim returns the perimeter of a shape.
#coo. <- function(coo){UseMethod("coo.")}

coo.perim <- function(coo){
  return(sum(coo.perim.pts(coo)))}

# coo.calliper returns the Feret diameter, ie the longest distance that can
# be found between a pair of coordinates.
#coo. <- function(coo){UseMethod("coo.")}

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

# coo.lw returns the length and width of a shape based on their iniertia axis
# ie alignment to the x-axis
#coo. <- function(coo){UseMethod("coo.")}
coo.lw <- function(coo){
  coo <- coo.check(coo)
  d   <- apply(coo.align(coo), 2, range)
  return(abs(d[2,] - d[1,]))}

# coo.area computes the area of any non-crossing shape (in square units)
#coo. <- function(coo){UseMethod("coo.")}
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
  return(area)}

# coo.theta.3pts returns the angle (in radians) defined by a triplet of points
# either signed ("atan2") or not ("acos"). 
theta3ldk <- function(m, method=c("atan2", "acos")[1]){  
  a <- c(m[1, 1] - m[2, 1], m[1, 2] - m[2, 2])
  b <- c(m[3, 1] - m[2, 1], m[3, 2] - m[2, 2])
  if (method=="atan2") {
    return(atan2 (a[1]*b[2]-a[2]*b[1],a[1]*b[1]+a[2]*b[2]))}
  if (method=="acos") {
    return(acos(sum(a*b)/(sqrt(sum(a*a)) * sqrt(sum(b*b)))))}}

# coo.theta returns the angle (in radians) defined by every triplet of points
# either signed ("atan2") or not ("acos"), along the shape.
#coo. <- function(coo){UseMethod("coo.")}
coo.theta.pts <- function(coo, method=c("atan2", "acos")[1]){
  coo <- coo.check(coo)
  coo <- coo.close(coo)
  coo   <- rbind(coo[nrow(coo)-1, ], coo)
  theta <- numeric()
  for (i in 1:(nrow(coo)-2)){
    theta[i] <- coo.theta.3pts(coo[i:(i+2),], method=method)
  }
  return(theta)}

# coo.rectilinearity returns the rectilinearity measurement by Zunic and Rosin
#coo. <- function(coo){UseMethod("coo.")}
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
  theta   <- coo.theta(coo)
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
  return((4/(4 - pi)) * ((sum(l2.e) / min(P1.Pa)) - (pi/4)))
}

# coo.circularity.haralick returns Haralick's circularity which is less sensible
# to digitalization noise than coo.circularity
#coo. <- function(coo){UseMethod("coo.")}
coo.circularity.haralick <- function(coo) {
  cd <- coo.centdist(coo)
  return(mean(cd)/sd(cd))}

# coo.circularity, the simplest circularity measure. Also called compactness
# and shape factor...
#coo. <- function(coo){UseMethod("coo.")}
coo.circularity <- function(coo) {
  return(coo.perim(coo)^2 / coo.area(coo))}

# coo.circularity.norm returns the circularity, normalised to the unit circle.
coo.circularity.norm <- function(coo) {
  return(coo.perim(coo)^2 / (coo.area(coo)*4*pi))}

# scale dependent...
# coo.fracdim <- function(coo){
#   return((2*log(coo.perim(coo))) / log(coo.area(coo)))}
#coo. <- function(coo){UseMethod("coo.")}
coo.eccentricity.eigen <- function(coo){
  coo <- coo.check(coo)
  eig <- eigen(cov(coo))$values
  return(eig[2]/eig[1])}

#coo. <- function(coo){UseMethod("coo.")}
coo.eccentricity.boundingbox <- function(coo){
  coo <- coo.check(coo)
  lw <- coo.lw(coo)
  return(lw[2]/lw[1])}

#coo. <- function(coo){UseMethod("coo.")}
coo.elongation <- function(coo){
  coo <- coo.check(coo)
  lw <- coo.lw(coo)
  return(1 - lw[2]/lw[1])}

#coo. <- function(coo){UseMethod("coo.")}
coo.rectangularity <- function(coo){
  coo <- coo.check(coo)
  abr <- prod(coo.lw(coo))
  return(coo.area(coo)/abr)}

#coo. <- function(coo){UseMethod("coo.")}
coo.chull <- function(coo){
  coo <- coo.check(coo)
  return(coo[chull(coo),])}

#coo. <- function(coo){UseMethod("coo.")}
coo.convexity <- function(coo){
  coo <- coo.check(coo)
  return(coo.perim(coo.chull(coo))/coo.perim(coo))}

#coo. <- function(coo){UseMethod("coo.")}
coo.solidity <- function(coo){
  coo <- coo.check(coo)
  return(coo.area(coo)/coo.area(coo.chull(coo)))}


################################################################################
# 3. plotting coo function and utilities
# --------------
################################################################################

coo.plot <- function(coo, xlim, ylim, border="#333333", col="#33333322", lwd=1, lty=1,
                     points=FALSE, first.point=TRUE, centroid=TRUE, xy.axis=TRUE,
                     pch=1, cex=0.5, main, plot.new=TRUE){
  coo <- coo.check(coo)
  if (plot.new) {
    # we setup coo.plot graphical parameters
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    if (!missing(xlim) | !missing(ylim)) {
      if (missing(xlim)){ xlim <- ylim } else {ylim <- xlim }
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE,
           xlim=xlim, ylim=ylim)
    } else {
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)}
    if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}}
  polygon(coo, col=col, border=NA)
  lines(coo, col=border, lwd=lwd, lty=lty)
  # we handle coordinate points
  if (missing(points)) { if (nrow(coo)<=100) points(coo, pch=pch, cex=cex, col=border)}
  if (points) { points(coo, pch=pch, cex=cex, col=border) }
  if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20)}
  if (centroid) {
    cent <- coo.centpos(coo)
    points(cent[1], cent[2], pch=3, col=border, cex=cex)}
  if (!missing(main)) title(main=main)} 

coo.draw <- function(coo, ...){
  coo.plot(coo, plot.new=FALSE, ...)}

coo.template   <- function(coo, size=1) {
  # only for matrices
  coo      <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift    <-  expected - observed
  return(coo.trans(coo, shift[1], shift[2]))}

coo.list.panel <- function(coo.list, dim, byrow=TRUE,
                           fromtop=TRUE, mar=rep(0, 4),
                           cols, borders){
  coo.list <- lapply(coo.list, coo.check)
  # if dim is missing, we define a square
  n <- length(coo.list)
  if(missing(dim)) {
    nc  <- ceiling(sqrt(n))
    nr  <- ceiling(n/nc)
    dim <- c(nr, nc)}
  k   <- dim[1]*dim[2]
  if (k < n) stop("dim[1]*dim[2] must be >= the length of coo.list")
  pos <- matrix(1:k, dim[1], dim[2], byrow=byrow)
  if (fromtop & dim[1]>1) { pos <- pos[dim[1]:1,] }
  # we prepare the panel
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar=mar, oma=rep(0.2, 4))
  plot(NA, asp=1,
       xlim=c(0, dim[2]),
       ylim=c(0, dim[1]),
       xaxs="i", yaxs="i", frame=FALSE, ann=FALSE, axes=FALSE)
  # we template and plot shapes
  coo.tp  <- lapply(coo.list, coo.template, size=0.95)
  if (missing(cols))    { cols      <- rep("grey80", n) }
  if (missing(borders)) { borders   <- rep("grey20", n) }
  res <- data.frame(pos.x=numeric(), pos.y=numeric())
  for (i in 1:n){
    trans <- which(pos==i, arr.ind=TRUE) - 0.5
    res[i, ] <- c(trans[2], trans[1])
    polygon(coo.tp[[i]][, 1] + trans[2],
            coo.tp[[i]][, 2] + trans[1],
            col=cols[i], border=borders[i])
  }
}

coo.oscillo <- function(coo, rug=TRUE, legend=TRUE, cols=col.gallus(2), nb.pts=12){
  coo <- coo.check(coo)
  nr <- nrow(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]

  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  layout(matrix(1:2, ncol=2), widths=c(1, 2))
  par(mar=c(3, 3, 2 , 1))
  coo.plot(coo, points=FALSE, first.point=FALSE)
  box()
  refs <- round(seq(1, nr, length=nb.pts+1)[-(nb.pts+1)])
  text(coo[refs, 1], coo[refs, 2], labels=as.character(1:nb.pts), cex=0.7)
  ry <- max(abs(range(c(dx, dy))))
  par(mar=c(3, 3, 2 , 1))
  plot(NA, xlim=c(1, nr), xlab="",
       ylim=c(-ry, ry)*1.1,   ylab="Deviation",
       las=1, frame=FALSE, axes=FALSE)
  axis(2, cex.axis=2/3, las=1)
  lines(dx, col=cols[1])
  lines(dy, col=cols[2])
  text((1:nr)[refs], dx[refs], labels=as.character(1:nb.pts), cex=0.7, col=cols[1])
  text((1:nr)[refs], dy[refs], labels=as.character(1:nb.pts), cex=0.7, col=cols[2])
  mtext("Deviation", side=2, line=1.5)
  box()
  if (legend) {
    legend("bottomright", legend = c(expression(x[i] - x[0]), expression(y[i] - y[0])),
           col = cols, bg="#FFFFFFCC", 
           cex=0.7, lty = 1, lwd=1, inset=0.05, bty="n")}}


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



dev.segments <-function(coo, cols, lwd=1){
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i+1, 1], coo[i+1, 2],
             col=cols[i], lwd=lwd)}}


# from Claude
conf.ell <- function(x, y, conf=0.95, nb.pts = 60){
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]}
  centroid <- apply(cbind(x,y), 2, mean)
  theta.i <- seq(0, 2*pi, length = nb.pts + 1)[-c(nb.pts+1)]
  z <- cbind(cos(theta.i), sin(theta.i))
  rad <- qnorm((1 - conf)/2, mean=0, sd=1, lower.tail=FALSE)
  vcvxy <- var(cbind(x,y))
  r <- cor(x, y)
  M1 <- matrix(c(1,1,-1,1), nrow=2, ncol=2)
  M2 <- matrix(c(var(x), var(y)), nrow=2, ncol=2)
  M3 <- matrix(c(1+r, 1-r), nrow=2, ncol=2, byrow=TRUE)
  ellpar <- M1 * sqrt(M2 * M3/2)
  t(centroid + rad * ellpar %*% t(z))}

####################

# new frame
.frame <- function(xy, center.origin=FALSE, zoom=1){
  if (center.origin) {
    w <- zoom*max(abs(xy))
    plot(NA, xlim=c(-w, w), ylim=c(-w, w),  asp=1, axes=FALSE, frame=TRUE) 
  } else {
    w <- zoom*apply(abs(xy), 2, max)
    plot(xy, xlim=c(-w[1], w[1]), ylim=c(-w[2], w[2]),
         type="n", asp=1,  axes=FALSE, frame=TRUE)}}

#grid layer
.grid <- function(xy, nb.grids=3){
  m <- max(abs(xy))
  g <- seq(0, m, length=nb.grids)
  g <- c(g[-1]*-1, g[-1])
  abline(h=g, v=g, col="grey90", lty=2)
  abline(h=0, v=0, col="grey80")}

#rug
.rug <- function(xy, fac, col){
  if (is.null(fac)) {
    rug(xy[, 1], ticksize=0.015, side=1, col="black")
    rug(xy[, 2], ticksize=0.015, side=2, col="black")
  } else {
    for (i in seq(along=levels(fac))) {
      rug(xy[fac==levels(fac)[i], 1], ticksize=0.015, lwd=1, side=1, col=col[i])
      rug(xy[fac==levels(fac)[i], 2], ticksize=0.015, lwd=1,  side=2, col=col[i])}}}

# convertir en vrai morphospace, à base de plotnew=TRUE/FALSE
.morphospace <- function(xy, pos.shp, rot, mshape, amp.shp=1,
                         size.shp=15, border.shp="#00000055", col.shp="#00000011", ...){
  pos <- pos.shapes(xy, pos.shp=pos.shp)
  shp <- pca2shp.efourier(pos=pos, rot=rot, mshape=mshape, amp=amp.shp, trans=TRUE)
  width <- (par("usr")[4] - par("usr")[3]) / size.shp
  shp <- lapply(shp, coo.scale, 1/width)
  burp <- lapply(shp, polygon, border=border.shp, col=col.shp)}

.ellipses <- function(xy, fac, conf=0.5, col){
  for (i in seq(along=levels(fac))) {
    pts.i <- xy[fac==levels(fac)[i], ]
    ell.i <- conf.ell(x=pts.i, conf=conf)
    lines(coo.close(ell.i), col=col[i])
    points(coo.centpos(pts.i)[1], coo.centpos(pts.i)[2], pch=3, col=col[i])
  }}

.chull <- function(coo, fac, col){
  for (i in seq(along=levels(fac))) {
    chull.i <- coo.chull(coo[fac==levels(fac)[i], ])
    lines(coo.close(chull.i), col=col[i])}}

.labels <- function(xy, fac, col){
  for (i in seq(along=levels(fac))) {
    cent.i <- coo.centpos(xy[fac==levels(fac)[i], ])
    text(cent.i[1], cent.i[2], labels=levels(fac)[i], col=col[i], pos=3)}}

.stars <- function(xy, fac, col){
  col.i <- paste0(col, "55")
  for (i in seq(along=levels(fac))) {
    pts.i  <- xy[fac==levels(fac)[i], ]
    cent.i <- coo.centpos(pts.i)
    for (j in 1:nrow(pts.i)){
      segments(cent.i[1], cent.i[2], pts.i[j, 1], pts.i[j, 2], col=col.i[i])}}}

.eigen <- function(ev, xax, yax, ratio=0.12){
  plt0 <- par("plt")
  on.exit(par(plt = plt0))
  g <- 0.015
  w <- min(c(plt0[2]-plt0[1]), plt0[4]-plt0[3])*ratio
  par(plt = c(plt0[2]-w-g, plt0[2]-g, plt0[3]+g*1.5, plt0[3]+w+g*1.5), xpd=NA, new = TRUE)
  cols <- rep("grey80", 5)
  cols[c(xax,yax)] <- "grey40"
  var <- ev^2
  cs.var <- cumsum(var)/sum(var)
  k <- ifelse(max(c(xax, yax))>5, max(c(xax, yax)), 5)
  barplot(var[1:k], axes=FALSE, col=cols, border=NA)
  text(-0.7, par("usr")[3], labels="Eigenvalues", pos=4, cex=0.6, srt=90, col="grey40")
  
}

.axisnames <- function(xax, yax){
  gx <- strwidth("PCN")/1.75
  gy <- strheight("PCN")/1.8
  text(par("usr")[2]-gx, gy, col="grey40", cex=0.8,
       labels=paste0("PC", xax))
  text(-gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0("PC", yax), srt=90)}

.axisvar <- function(ev, xax, yax){
  var <- ev^2
  var <- signif(100*var/sum(var), 3)
  gx <- strwidth("00.0%")/1.75
  gy <- strheight("00.0%")/1.8
text(par("usr")[2]-gx, -gy, col="grey40", cex=0.8,
     labels=paste0(var[xax], "%"))
text(+gy, par("usr")[4]-gx, col="grey40",cex=0.8,
     labels=paste0(var[yax], "%"), srt=90)}

.title <- function(title){
  pos <- par("usr")
  text(pos[1], pos[3]+ strheight(title), labels=title, pos=4)}

plot.OutPCA <- function(#basics
  PCA, fac, xax=1, yax=2, 
  #color choice
  col="black", pch=20, palette=col.summer2,
  #.frame
  center.origin=FALSE, zoom=1,
  #.grid
  grid=TRUE, nb.grids=3,
  #shapes
  morphospace=TRUE, pos.shp="full", amp=1,
  size.shp=20, border.shp="#00000055", col.shp="#00000011",
  #stars
  stars=TRUE,
  #ellipses
  ellipses=TRUE, conf=0.5,
  #convexhulls
  chull=TRUE,
  #labels
  labels=TRUE,
  #axisnames
  axisnames=TRUE,
  #axisvar
  axisvar=TRUE,
  #eigen
  eigen=TRUE,
  #
  rug=TRUE,
  title=substitute(PCA)
){
  xy <- PCA$x[, c(xax, yax)]
  # we check and prepare
  if (!missing(fac)) {
    if (!is.factor(fac)) { fac <- factor(PCA$fac[, fac]) }
    if (missing(col)) {
      col.groups <- palette(nlevels(fac))
      col <- col.groups[fac]}
    if (!missing(pch)) {
      if (length(pch)==nlevels(fac)) { pch <- pch[fac] }}}
  opar <- par(mar = par("mar"), xpd=FALSE)
  on.exit(par(opar))
  par(mar = rep(0.1, 4)) #0.1
  
  .frame(xy, center.origin, zoom=zoom)
  if (grid) .grid(xy)
  .morphospace(xy, pos.shp=pos.shp, rot=PCA$rotation[, c(xax, yax)], mshape=PCA$mshape,
               size.shp=size.shp, border.shp=border.shp, col.shp=col.shp)
  if (!missing(fac)) {
    if (stars)    .stars(xy, fac, col.groups)
    if (ellipses) .ellipses(xy, fac, conf=conf, col.groups) #+conf
    if (chull)    .chull(xy, fac, col.groups)
    if (labels)   .labels(xy, fac, col.groups)
    if (rug)      .rug(xy, fac, col.groups)
  } else {
    if (rug)      .rug(xy, NULL, col)
  }
  points(xy, pch=pch, col=col)
  if (axisnames)  .axisnames(xax, yax)
  if (axisvar)    .axisvar(PCA$sdev, xax, yax)
  .title(title)
  if (eigen)     .eigen(PCA$sdev, xax, yax)
  box()
}

pca2shp.efourier <- function (pos, rot, mshape, amp=1, nb.pts=60, trans=TRUE) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  # stupid function
  mprod <- function(m, s){
    res <- m
    for (i in 1:ncol(m)) { res[, i] <- m[, i]*s[i] }
    return(res)}
  nb.h <- length(mshape)/4
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- mprod(rot, pos[i, ])*amp
    coe        <- mshape + apply(ax.contrib, 1, sum)
    xf         <- coeff.split(coe)
    coo        <- efourier.i(xf, nb.h = nb.h, nb.pts=nb.pts)
    if (trans) {coo <- coo.trans(coo, x=pos[i, 1], y=pos[i, 2])}
  res[[i]] <- coo}
  return(res)}

pos.shapes <- function(xy, pos.shp=c("range", "circle")[1],
                       nb.shp=12, nr.shp=6, nc.shp=5, circle.r.shp){
  if (is.data.frame(pos.shp) | is.matrix(pos.shp)) {
    return(as.matrix(pos.shp))}
  if (pos.shp=="circle") {
    if (missing(circle.r.shp)) {
      # mean distance from origin
      circle.r.shp <- mean(apply(xy, 1, function(x) sqrt(sum(x^2))))}
    t <- seq(0, 2*pi, len=nb.shp+1)[-(nb.shp+1)]
    pos <- cbind(circle.r.shp*cos(t), circle.r.shp*sin(t))
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="range") {
    pos <- expand.grid(seq(min(xy[, 1]), max(xy[, 1]), len=nr.shp),
                       seq(min(xy[, 2]), max(xy[, 2]), len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="full") {
#     w <- par("usr")
#     pos <- expand.grid(seq(w[1], w[2], len=nr.shp),
#                        seq(w[3], w[4], len=nc.shp))
         w <- par("usr")
         pos <- expand.grid(seq(par("xaxp")[1]*0.9, par("xaxp")[2]*0.9, len=nr.shp),
                            seq(par("yaxp")[1]*0.9, par("yaxp")[2]*0.9, len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)   
  }
  # if a non-valid method is passed
  return(xy)}

# Datasets ---------------------------------------------------------------------

#' Outline coordinates of 20 beer and 20 whisky bottles.
#' 
#' @docType data
#' @name bot
#' @keywords datasets
#' @format An Out object containing the outlines coordinates and a grouping factor
#' for 20 beer and 20 whisky bottles
#' @source  Images have been grabbed on the internet and prepared by the package's
#' authors. No particular choice has been made on the dimension of the original
#' images or the brands cited here.
NULL

#' Outline coordinates of 50 cephalic outlines of trilobite
#' 
#' @docType data
#' @name trilo
#' @keywords datasets
#' @format An Out object 64 coordinates of 50 cephalic outlines from different
#' ontogenetic stages of trilobite.
#' @source  Arranged from: \url{http://folk.uio.no/ohammer/past/outlines.dat}.
#' The original data included 51 outlines and 5 ontogenetic stages, 
#' but one of them has just a single outline thas has been removed.

NULL


#' Outline coordinates of 126 mosquito wings.
#' 
#' @docType data
#' @name mosquito
#' @keywords datasets
#' @format An Out object with the 126 mosquito wing outlines outlines
#' used Rohlf and Archie (1984).
#' @source Rohlf F, Archie J. 1984. A comparison of Fourier methods for the
#' description of wing shape in mosquitoes (Diptera: Culicidae). \emph{Systematic Biology}: 302-317.
#' Arranged from: \url{http://life.bio.sunysb.edu/morph/data/RohlfArchieWingOutlines.nts}.
NULL

#' Outline coordinates of 240 hand-drawn hearts
#' 
#' @docType data
#' @name hearts
#' @keywords datasets
#' @format An Out object with the outline coordinates of 240 hand-drawn hearts
#' by 8 different persons, with 4 landmarks.
#' @source We thank the fellows of the Ecology Department of the French Institute
#' of Pondicherry that drawn the hearts, that then have been smoothed, scaled, centered, and reduced to 80 coordinates per outline.
NULL

