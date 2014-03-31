################################################################################
# FourierCore.R
# Core functions to perform eft, rft, tft
# ------------------------------------------------------------------------------


# Fourier Core Functions -------------------------------------------------------
# Mainly due to Julien Claude with some wrapping

# Elliptical Fourier Analysis --------------------------------------------------
# smooth it should be removed from here ? #todo
# given a shape, calculate eft
efourier  <- function (coo, nb.h, smooth.it = 0, verbose = TRUE) {
  coo <- coo.check(coo)
  if (is.closed(coo)) coo <- coo.unclose(coo)
  nr <- nrow(coo)
  if (missing(nb.h))  {
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

# given an eft 'vector', calculates a shape
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

# to normalize eft cooeficients
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

# returns a random "eft" shape
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

# amplify an eft vector
ef.amplify <- function(ef, amp=rep(0.5, 4)){
  ef$an <- ef$an*amp[1]
  ef$bn <- ef$bn*amp[2]
  ef$cn <- ef$cn*amp[3]
  ef$dn <- ef$dn*amp[4]
  return(ef)}

# Radius lengths Fourier analysis ----------------------------------------------
# given a shape, returns a 'rft' vector
rfourier <- function(coo, nb.h, smooth.it=0, norm=FALSE, verbose=TRUE){
  coo <- coo.check(coo)
  if (is.closed(coo)) {coo <- coo.unclose(coo)}
  if(nb.h * 2 > nrow(coo) | missing(nb.h)) {
    nb.h = floor(nrow(coo)/2) - 1 # should not be -1 but 0 #todo
    if (verbose){
      warning("'nb.h' must be lower than half the number of points and has been set to: ", nb.h)}}
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

#given an 'rft' vector, calculates a shape
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

#calculates a random "rfourier" shape
rfourier.shape <- function(an, bn, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  rf  <- list(an=an, bn=bn, ao=0)
  shp <- rfourier.i(rf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

# Tangent angle ----------------------------------------------------------------
# given a shape returns a tft vector
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

# given a tft vector, calculates a shape
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

# calculates a random tfourier shape
tfourier.shape <- function(an, bn, ao=0, nb.h, nb.pts=80, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(an)) nb.h <- 1
  if (missing(nb.h) & !missing(an)) nb.h <- length(an)
  if (missing(an)) an <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(bn)) bn <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  tf  <- list(an=an, bn=bn, ao=ao)
  shp <- tfourier.i(tf, nb.h=nb.h, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

# calculates the harmonic power of a xft vector --------------------------------
# xft vectors should be signed by a class or something #todo
# Calculates harmonic power given a list from e/t/rfourier
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
