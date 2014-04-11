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
  d <- sqrt(apply((coo - coo.slide(coo, n))^2, 1, sum))[-1]
  return(d)}

coo.perim.cum <- function(coo){
  coo <- coo.check(coo)
  d <- cumsum(sqrt(apply((coo-rbind(coo[1,],coo[-(dim(coo)[1]),]))^2,1,sum)))
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

