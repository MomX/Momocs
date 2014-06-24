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

