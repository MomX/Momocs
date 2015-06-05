##### Core functions for Bezier analysis. will probably be
##### deprecated and never used.

#' Calculates Bezier coefficients from a shape
#' 
#' @param coo a matrix or a list of (x; y) coordinates
#' @param n the degree, by default the number of coordinates.
#' @return a list with components:
#' \itemize{
#' \item \code{$J} matrix of Bezier coefficients
#' \item \code{$B} matrix of Bezier vertices.
#' }
#' @note Directly borrowed for Claude (2008), and also called \code{bezier} there.
#' Not implemented for open outlines but may be useful for other purposes.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' Springer 316 pp.
#' @keywords Bezier
#' @examples
#' set.seed(34)
#' x <- coo_sample(efourier_shape(), 5)
#' plot(x, ylim=c(-3, 3), asp=1, type='b', pch=20)
#' b <- bezier(x)
#' bi <- bezier_i(b$B)
#' lines(bi, col='red')
#' @export
bezier <- function(coo, n) {
    coo <- coo_check(coo)
    if (missing(n)) 
        n <- nrow(coo)
    p <- nrow(coo)
    if (n != p) {
        n <- n + 1
    }
    coo1 <- coo/coo_perimcum(coo)[p]
    t1 <- 1 - coo_perimcum(coo1)
    J <- matrix(NA, p, p)
    for (i in 1:p) {
        for (j in 1:p) {
            J[i, j] <- (factorial(p - 1)/(factorial(j - 1) * 
                factorial(p - j))) * (((1 - t1[i])^(j - 1)) * 
                t1[i]^(p - j))
        }
    }
    B <- ginv(t(J[, 1:n]) %*% J[, 1:n]) %*% (t(J[, 1:n])) %*% 
        coo
    coo <- J[, 1:n] %*% B
    B <- ginv(t(J[, 1:n]) %*% J[, 1:n]) %*% (t(J[, 1:n])) %*% 
        coo
    list(J = J, B = B)
}

#' Calculates a shape from Bezier coefficients
#' 
#' @param B a matrix of Bezier vertices, such as those produced by \link{bezier}
#' @param nb.pts the number of points to sample along the curve.
#' @return a matrix of (x; y) coordinates
#' @note Directly borrowed for Claude (2008), and called \code{beziercurve} there.
#' Not implemented for open outlines but may be useful for other purposes.
#' @references Claude, J. (2008) \emph{Morphometrics with R}, Use R! series,
#' @keywords Bezier
#' @examples
#' set.seed(34)
#' x <- coo_sample(efourier_shape(), 5)
#' plot(x, ylim=c(-3, 3), asp=1, type='b', pch=20)
#' b <- bezier(x)
#' bi <- bezier_i(b$B)
#' lines(bi, col='red')
#' @export
bezier_i <- function(B, nb.pts = 120) {
  if (any(names(B)=="B")) B <- B$B
    x <- y <- numeric(nb.pts)
    n <- nrow(B) - 1
    t1 <- seq(0, 1, length = nb.pts)
    coef <- choose(n, k = 0:n)
    b1 <- 0:n
    b2 <- n:0
    for (j in 1:nb.pts) {
        vectx <- vecty <- NA
        for (i in 1:(n + 1)) {
            vectx[i] <- B[i, 1] * coef[i] * t1[j]^b1[i] * (1 - 
                t1[j])^b2[i]
            vecty[i] <- B[i, 2] * coef[i] * t1[j]^b1[i] * (1 - 
                t1[j])^b2[i]
        }
        x[j] <- sum(vectx)
        y[j] <- sum(vecty)
    }
    coo <- cbind(x, y)
    return(coo)
}

##### end Bezier 
