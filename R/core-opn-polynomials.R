# 2. Open outlines -------------------------------------------------------------
# 2.1 Polynomials --------------------------------------------------------------
#' Calculate polynomial fit on open outlines
#' 
#' Returns a model obtained with a lm with polynomial coefficients, orthogonal 
#' or not, from a matrix of (x; y) coordinates (assumed to be normalized).
#' @param coo a matrix or a list of (x; y) coefficients
#' @param n the degree of the polynomial to fit (the intercept is returned)
#' @param ortho logical wheter to calculate orthogonal coefficients
#' @return a lm model object
#' @keywords morphoCore
#' @export
polynomials <- function(coo, n, ortho=TRUE){
  coo <- coo.check(coo)
  if (missing(n)) {
    n <- 5
    cat(" * 'n' not provided and set to", n, ".\n")}
  x <- poly(coo[, 1], degree=n, raw=!ortho)
  mod <- lm(coo[, 2] ~ x)
  r2 <- summary(mod)$r.squared
  return(list(coeff=mod$coefficients, ortho=ortho, 
              baseline1=coo[1, ], baseline2=coo[nrow(coo), ], r2=r2, mod=mod))}

#' Calculate shape from a polynomial model
#' 
#' Returns a matrix of (x; y) coordinates when passed with a model obtained with
#' \link{polynomials}.
#' @param pol a pol list such as created by \link{polynomials}.
#' @param nb.pts the number of points to predict. By default (and can't be higher)
#' the number of points in the original shape.
#' @param reregister logical whether to reregister the shape with the original baseline.
#' @return a matrix of (x; y) coordinates.
#' @keywords morphoCore
#' @examples
#' data(olea)
#' o <- olea[5]
#' coo.plot(o)
#' for (i in 2:7){
#' x <- polynomials.i(polynomials(o, i, ortho=TRUE))
#' coo.draw(x, border=col.summer(7)[i], points=FALSE)  }
#' @export
polynomials.i <- function(pol, nb.pts=120, reregister=TRUE){
  x.new  <- seq(pol$baseline1[1], pol$baseline2[1], length=nb.pts)
  degree <- length(pol$coeff)-1
  if (pol$ortho) {
    x.poly <- poly(x.new, degree=degree)
    y.pred <- predict(x.poly, x.new)
    y.new  <- pol$coeff[1] + apply(.mprod(m=y.pred, s=pol$coeff[-1]), 1, sum) 
  } else {
    y.pred <- numeric(nb.pts)
    for (i in 1:degree){
      y.pred <- y.pred + (x.new^i * pol$coeff[i+1]) }
    y.new <- y.pred + pol$coeff[1]}
  coo <- cbind(x.new, y.new)
  if (reregister) {
    coo <- coo.baseline(coo, 1, nrow(coo), t1=pol$baseline1, t2=pol$baseline2)}
  colnames(coo) <- c("x", "y")
  return(coo)}

  