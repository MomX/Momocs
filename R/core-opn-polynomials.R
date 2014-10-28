##### Core functions for polynomials approaches on open outlines

#' Calculate natural or orthogonal polynomial fits on open outlines
#' 
#' Calculates natural or orthogonal polynomial coefficients,
#' through a linear model fit (see \link{lm}), from a matrix of (x; y) coordinates.
#' 
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @param degree polynomial degree for the fit (the Intercept is also returned)
#' @param ortho logical wheter to fit orthogonal (if \code{TRUE}) or natural 
#' polynomials
#' @return a list with components:
#' \itemize{
#'  \item \code{coeff} the coefficients (includint the intercept)
#'  \item \code{ortho} whether orthogonal or natural polynomials were fitted
#'  \item \code{degree} degree of the fit (could be retrieved through \code{coeff} though)
#'  \item \code{baseline1} the first baseline point (so far the first point)
#'  \item \code{baseline2} the second baseline point (so far the last point)
#'  \item \code{r2} the r2 from the fit
#'  \item \code{mod} the raw lm model
#' }
#' @note Orthogonal polynomials are sometimes called Legendre's polynomials.
#' @seealso \link{polynomials.i} and \link{rawPolynomials} and \link{orthoPolynomials} for methods
#' on \link{Opn} objects.
#' @keywords Polynomials
#' @examples
#' data(olea)
#' o <- olea[1]
#' op <- polynomials(o, degree=4)
#' op
#' # shape reconstruction
#' opi <- polynomials.i(op)
#' lines(opi, col='red')
#' # R2 for degree 1 to 10
#' r <- numeric()
#' for (i in 1:10) { r[i] <- polynomials(o, degree=i)$r2 }
#' plot(2:10, r[2:10], type='b', pch=20, col='red', main='R2 / degree')
#' @export
polynomials <- function(coo, degree, ortho = TRUE) {
    coo <- coo_check(coo)
    if (missing(degree)) {
        degree <- 5
        cat(" * 'degree' not provided and set to", degree, ".\n")
    }
    x <- poly(coo[, 1], degree = degree, raw = !ortho)
    mod <- lm(coo[, 2] ~ x)
    r2 <- summary(mod)$r.squared
    return(list(coeff = mod$coefficients, ortho = ortho, degree = degree, 
        baseline1 = coo[1, ], baseline2 = coo[nrow(coo), ], r2 = r2, 
        mod = mod))
}

#' Calculates shape from a polynomial model
#' 
#' Returns a matrix of (x; y) coordinates when passed with a list obtained with
#' \link{polynomials}.
#' @param pol a pol list such as created by \link{polynomials}
#' @param nb.pts the number of points to predict. By default (and cannot be higher)
#' the number of points in the original shape.
#' @param reregister logical whether to reregister the shape with the original baseline.
#' @return a matrix of (x; y) coordinates.
#' @keywords Polynomials
#' @examples
#' data(olea)
#' o <- olea[5]
#' coo_plot(o)
#' for (i in 2:7){
#' x <- polynomials.i(polynomials(o, i, ortho=TRUE))
#' coo_draw(x, border=col.summer(7)[i], points=FALSE)  }
#' data(olea)
#' o <- olea[1]
#' op <- polynomials(o, degree=4)
#' op
#' # shape reconstruction
#' opi <- polynomials.i(op)
#' lines(opi, col='red')
#' # R2 for degree 1 to 10
#' r <- numeric()
#' for (i in 1:10) { r[i] <- polynomials(o, degree=i)$r2 }
#' plot(2:10, r[2:10], type='b', pch=20, col='red', main='R2 / degree')
#' @export
polynomials.i <- function(pol, nb.pts = 120, reregister = TRUE) {
    x.new <- seq(pol$baseline1[1], pol$baseline2[1], length = nb.pts)
    degree <- length(pol$coeff) - 1
    if (pol$ortho) {
        x.poly <- poly(x.new, degree = degree)
        y.pred <- predict(x.poly, x.new)
        y.new <- pol$coeff[1] + apply(.mprod(m = y.pred, s = pol$coeff[-1]), 
            1, sum)
    } else {
        y.pred <- numeric(nb.pts)
        for (i in 1:degree) {
            y.pred <- y.pred + (x.new^i * pol$coeff[i + 1])
        }
        y.new <- y.pred + pol$coeff[1]
    }
    coo <- cbind(x.new, y.new)
    if (reregister) {
        coo <- coo_baseline(coo, 1, nrow(coo), t1 = pol$baseline1, 
            t2 = pol$baseline2)
    }
    colnames(coo) <- c("x", "y")
    return(coo)
}

##### end Polynomials 
