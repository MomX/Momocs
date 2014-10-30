##### Core functions for Procrustes alignements

# should DF be used for cal_d ? #todo
#' Full Procrustes alignment between two shapes
#'
#' Directly borrowed from Claude (2008), called there the \code{fPsup} function.
#' @param coo1 configuration matrix to be superimposed onto the centered preshape of coo2.
#' @param coo2 reference configuration matrix.
#' @return a list with components:
#' \itemize{
#' \item \code{coo1} superimposed centered preshape of coo1 onto the centered preshape of coo2
#' \item \code{coo2} centered preshape of coo2
#' \item \code{rotation} rotation matrix
#' \item \code{scale} scale parameter
#' \item \code{DF} full Procrustes distance between coo1 and coo2.
#' }
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @keywords Procrustes
#' @export
fProcrustes <- function(coo1, coo2) {
  # directly borrowed from Claude
  k <- ncol(coo1)
  Z1 <- coo_center(coo_scale(coo1))
  Z2 <- coo_center(coo_scale(coo2))
  sv <- svd(t(Z2) %*% Z1)
  U <- sv$v
  V <- sv$u
  Delt <- sv$d
  sig <- sign(det(t(Z2) %*% Z1))
  Delt[k] <- sig * abs(Delt[k])
  V[, k] <- sig * V[, k]
  Gam <- U %*% t(V)
  beta <- sum(Delt)
  DF <- ifelse((1 - beta^2) > 0, sqrt(1 - beta^2), NA)
  list(coo1 = beta * Z1 %*% Gam, coo2 = Z2, rotation = Gam,
       scale = beta, DF = DF)
}

#' Full Generalized Procrustes alignment between shapes
#'
#' Directly borrowed from Claude (2008), called there the \code{fgpa2} function.
#'
#' If performed on an \link{Out} or an \link{Opn} object, will try to use the \code{$ldk} slot,
#' if landmarks have been previousy defined, then (with a message) on the \code{$coo} slot,
#' but in that case, all shapes must have the same number of coordinates (\link{coo_sample} may help).
#' @param x an array, a list of configurations, or an \link{Out}, \link{Opn} or \link{Ldk} object
#' @param tol numeric when to stop iterations
#' @param verbose logical whether to print outputs (iteration number, and gain)
#' @param coo logical, when working on \code{Out} or \code{Opn}, whether to use \code{$coo} rather than \code{$ldk}
#' @return a list with components:
#' \itemize{
#' \item \code{rotated} array of superimposed configurations
#' \item \code{iterationnumber} number of iterations
#' \item \code{Q} convergence criterion
#' \item \code{Qi} full list of Q
#' \item \code{Qd} difference between succesive Q
#' \item \code{interproc.dist} minimal sum of squared norms of pairwise differences between
#' all shapes in the superimposed sample
#' \item \code{mshape} mean shape configuration
#' \item \code{cent.size} vector of centroid sizes.
#' } or an \link{Out}, \link{Opn} or an \link{Ldk} object.
#' @note Slightly less optimized than procGPA in the shapes package (~20% on my machine).
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @keywords Procrustes
#' @export
fgProcrustes <- function(x, tol, verbose, coo) {
  UseMethod("fgProcrustes")
}

#' @export
fgProcrustes.default <- function(x, tol = 1e-05, verbose = TRUE, coo=NULL) {
  A <- x
  A <- ldk_check(A)
  # directly borrowed from Claude
  p <- dim(A)[1]
  k <- dim(A)[2]
  n <- dim(A)[3]
  temp2 <- temp1 <- array(NA, dim = c(p, k, n))
  Siz <- numeric(n)
  for (i in 1:n) {
    Siz[i] <- coo_centsize(A[, , i])
    temp1[, , i] <- coo_center(coo_scale(A[, , i]))
  }
  iter <- 0
  sf <- NA
  M <- temp1[, , 1]
  for (i in 1:n) {
    temp1[, , i] <- fProcrustes(temp1[, , i], M)$coo1
  }
  M <- mshape(temp1)
  Qm1 <- dist(t(matrix(temp1, k * p, n)))
  Qd <- Qi <- Q <- sum(Qm1)
  iter <- 0
  sc <- rep(1, n)
  while (abs(Q) > tol) {
    for (i in 1:n) {
      Z1 <- temp1[, , i]
      sv <- svd(t(M) %*% Z1)
      U <- sv$v
      V <- sv$u
      Delt <- sv$d
      sig <- sign(det(t(Z1) %*% M))
      Delt[k] <- sig * abs(Delt[k])
      V[, k] <- sig * V[, k]
      phi <- U %*% t(V)
      beta <- sum(Delt)
      temp1[, , i] <- X <- sc[i] * Z1 %*% phi
    }
    M <- mshape(temp1)
    for (i in 1:n) {
      sf[i] <- sqrt(sum(diag(temp1[, , i] %*% t(M))) /
                      (sum(diag(M %*% t(M))) *
                         sum(diag(temp1[, , i] %*%
                                    t(temp1[, , i])))))
      temp2[, , i] <- sf[i] * temp1[, , i]
    }
    M <- mshape(temp2)
    sc <- sf * sc
    Qm2 <- dist(t(matrix(temp2, k * p, n)))
    Qd[iter] <- Q <- sum(Qm1) - sum(Qm2)
    Qm1 <- Qm2
    Qi[iter] <- sum(Qm2)
    iter <- iter + 1
    if (verbose) {
      cat("iteration: ", iter, "\tgain:", signif(abs(Q), 5), "\n")
    }
    temp1 <- temp2
  }
  list(rotated = temp2,
       iterationnumber = iter, Q = Q, Qi = Qi,
       Qd = Qd,
       intereuclidean.dist = Qm2,
       mshape = coo_centsize(mshape(temp2)),
       cent.size = Siz)
}

#' @export
fgProcrustes.Out <- function(x, tol = 1e-10, verbose = TRUE, coo=FALSE) {
  Coo <- x
  # if no $ldk defined, we convert Out into a Ldk and then
  # perform the fgProcrustes and return back an Out object.
  if (coo | length(Coo$ldk) == 0) {
    if (coo){
      cat (" * Using $coo, not $ldk.\n")
    } else {
      cat(" * No landmarks defined in $ldk, so trying to work on $coo directly.\n")}
    Coo2 <- Ldk(Coo$coo)
    Coo2 <- fgProcrustes(Coo2, tol = tol, verbose = verbose)
    Coo$coo <- Coo2$coo
    return(Coo)
  }
  Coo2 <- coo_center(coo_scale(Coo))
  ref <- get_ldk(Coo2)
  tar <- fgProcrustes(ref, tol = tol, verbose = verbose)$rotated
  # would benefit to be handled by coo_baseline ?
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
    refi <- coo_trans(refi, t[1], t[2])
    # rotation
    tx <- t2x - t1x
    ty <- t2y - t1y
    rx <- r2x - r1x
    ry <- r2y - r1y
    vi <- vecs_param(rx, ry, tx, ty)
    coo_i <- Coo2$coo[[i]]
    coo_i <- coo_trans(coo_i, t[1] - t1x, t[2] - t1y)
    coo_i <- coo_i/vi$r.norms
    coo_i <- coo_rotate(coo_i, -vi$d.angle)
    coo_i <- coo_trans(coo_i, t1x, t1y)
    Coo2$coo[[i]] <- coo_i
  }
  return(Coo2)
}

#' @export
fgProcrustes.Opn <- fgProcrustes.Out

#' @export
fgProcrustes.Ldk <- function(x, tol = 1e-10, verbose = TRUE, coo=NULL) {
  Coo <- x
  Coo2 <- Coo
  ref <- l2a(Coo2$coo)
  tar <- fgProcrustes(ref, tol = tol, verbose = verbose)$rotated
  Coo2$coo <- a2l(tar)
  Coo2$coe <- a2m(l2a(Coo2$coo))
  Coo2$method <- "fgProcrustes"
  names(Coo2$coo) <- names(Coo$coo)
  class(Coo2) <- c("LdkCoe", "Coe", class(Coo2))
  return(Coo2)
}

#' Partial Procrustes alignment between two shapes
#'
#' Directly borrowed from Claude (2008), and called \code{pPsup} there.
#' @param coo1 Configuration matrix to be superimposed onto the centered preshape of coo2.
#' @param coo2 Reference configuration matrix.
#' @return a list with components
#' \itemize{
#' \item \code{coo1} superimposed centered preshape of coo1 onto the centered preshape of coo2
#' \item \code{coo2} centered preshape of coo2
#' \item \code{rotation} rotation matrix
#' \item \code{DP} partial Procrustes distance between coo1 and coo2
#' \item \code{rho} trigonometric Procrustes distance.
#' }
#' @references Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.
#' @keywords Procrustes
#' @export
pProcrustes <- function(coo1, coo2) {
  # directly borrowed from Claude
  k <- ncol(coo1)
  Z1 <- coo_center(coo_scale(coo1))
  Z2 <- coo_center(coo_scale(coo2))
  sv <- svd(t(Z2) %*% Z1)
  U <- sv$v
  V <- sv$u
  Delt <- sv$d
  sig <- sign(det(t(Z2) %*% Z1))
  Delt[k] <- sig * abs(Delt[k])
  V[, k] <- sig * V[, k]
  Gam <- U %*% t(V)
  beta <- sum(Delt)
  list(coo1 = Z1 %*% Gam, coo2 = Z2, rotation = Gam, DP = sqrt(sum(edm(Z1 %*%
                                                                         Gam, Z2)^2)), rho = acos(beta))
}

##### end Procrustes
