##### Core functions for polynomials approaches on open outlines

#' Calculate orthogonal polynomial fits on open outlines
#'
#' Calculates orthogonal polynomial coefficients,
#' through a linear model fit (see \link{lm}), from a matrix of (x; y) coordinates or
#' a \link{Opn} object
#'
#' @param x a matrix (or a list) of (x; y) coordinates
#' @param degree polynomial degree for the fit (the Intercept is also returned)
#' @param baseline1 numeric the \eqn{(x; y)} coordinates of the first baseline
#' by default \eqn{(x= -0.5; y=0)}
#' @param baseline2 numeric the \eqn{(x; y)} coordinates of the second baseline
#' by default \eqn{(x= 0.5; y=0)}
#' @param nb.pts number of points to sample and on which to calculate polynomials
#' @param ... useless here
#' @return a list with components when applied on a single shape:
#' \itemize{
#'  \item \code{coeff} the coefficients (including the intercept)
#'  \item \code{ortho} whether orthogonal or natural polynomials were fitted
#'  \item \code{degree} degree of the fit (could be retrieved through \code{coeff} though)
#'  \item \code{baseline1} the first baseline point (so far the first point)
#'  \item \code{baseline2} the second baseline point (so far the last point)
#'  \item \code{r2} the r2 from the fit
#'  \item \code{mod} the raw lm model
#' }
#' otherwise an \link{OpnCoe} object.
#' @note Orthogonal polynomials are sometimes called Legendre's polynomials. They are
#' preferred over natural polynomials since adding a degree do not change lower orders coefficients.
#' @family polynomials
#' @examples
#' data(olea)
#' o <- olea[1]
#' op <- opoly(o, degree=4)
#' op
#' # shape reconstruction
#' opi <- opoly_i(op)
#' coo_plot(o)
#' coo_draw(opi)
#' lines(opi, col='red')
#' # R2 for degree 1 to 10
#' r <- numeric()
#' for (i in 1:10) { r[i] <- opoly(o, degree=i)$r2 }
#' plot(2:10, r[2:10], type='b', pch=20, col='red', main='R2 / degree')
#' @rdname opoly
#' @export
opoly <- function(x,
                  ...){UseMethod("opoly")}

#' @rdname opoly
#' @export
opoly.default <- function(x,
                          degree,
                          ...) {
  coo <- x
  coo <- coo_check(coo)
  if (missing(degree)) {
    degree <- 5
    message("'degree' not provided and set to ", degree)
  }
  x <- poly(coo[, 1], degree = degree, raw = FALSE)
  mod <- lm(coo[, 2] ~ x)
  r2 <- summary(mod)$r.squared
  return(list(coeff = mod$coefficients, ortho = TRUE, degree = degree,
              baseline1 = coo[1, ], baseline2 = coo[nrow(coo), ], r2 = r2,
              mod = mod))
}

#' @rdname opoly
#' @export
opoly.Opn <- function(x,
                                 degree,
                                 baseline1 = c(-0.5, 0),
                                 baseline2 = c(0.5, 0),
                                 nb.pts = 120,
                                 ...) {
  Opn <- x
  # validates
  Opn %<>% validate()
  # we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if (missing(nb.pts)) {
      nb.pts <- min.pts
      message("'nb.pts' missing and set to ", nb.pts)
    } else {
      nb.pts <- min.pts
      message("at least one outline has less coordinates than 'nb.pts' ", nb.pts, "points")
    }
  }
  if (missing(degree)) {
    degree <- 5
    message("'degree' missing and set to ", degree)
  }
  # we normalize
  Opn <- coo_sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo_baseline, ldk1 = 1, ldk2 = nb.pts,
                t1 = baseline1, t2 = baseline2)
  # we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow = length(Opn), ncol = degree + 1,
                dimnames = list(rn, cn))
  r2 <- numeric(length(Opn))
  mod <- list()
  # the loop
  for (i in seq(along = coo)) {
    mod <- opoly(coo[[i]], degree = degree)
    # mod[[i]] <- pol
    coe[i, ] <- mod$coeff
    r2[i] <- mod$r2
  }
  # mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "opoly"
  res <- OpnCoe(coe = coe, fac = Opn$fac, method = method,
                baseline1 = baseline1, baseline2 = baseline2, r2 = r2,
                mod = mod)
  res$cuts <- ncol(res$coe)
  return(res)
}


#' Calculate natural polynomial fits on open outlines
#'
#' Calculates natural polynomial coefficients,
#' through a linear model fit (see \link{lm}), from a matrix of (x; y) coordinates
#' or an \link{Opn} object
#'
#' @param x a matrix (or a list) of (x; y) coordinates or an \link{Opn} object
#' @param degree polynomial degree for the fit (the Intercept is also returned)
#' @param baseline1 numeric the \eqn{(x; y)} coordinates of the first baseline
#' by default \eqn{(x= -0.5; y=0)}
#' @param baseline2 numeric the \eqn{(x; y)} coordinates of the second baseline
#' by default \eqn{(x= 0.5; y=0)}
#' @param nb.pts number of points to sample and on which to calculate polynomials
#' @param ... useless here
#' @return when applied on a single shape, a list with components:
#' \itemize{
#'  \item \code{coeff} the coefficients (includint the intercept)
#'  \item \code{ortho} whether orthogonal or natural polynomials were fitted
#'  \item \code{degree} degree of the fit (could be retrieved through \code{coeff} though)
#'  \item \code{baseline1} the first baseline point (so far the first point)
#'  \item \code{baseline2} the second baseline point (so far the last point)
#'  \item \code{r2} the r2 from the fit
#'  \item \code{mod} the raw lm model
#' }
#'
#' otherwise, an \link{OpnCoe} object.
#' @family polynomials
#' @examples
#' data(olea)
#' o <- olea[1]
#' op <- opoly(o, degree=4)
#' op
#' # shape reconstruction
#' opi <- opoly_i(op)
#' coo_plot(o)
#' coo_draw(opi, border="red")
#' # R2 for degree 1 to 10
#' r <- numeric()
#' for (i in 1:10) { r[i] <- npoly(o, degree=i)$r2 }
#' plot(2:10, r[2:10], type='b', pch=20, col='red', main='R2 / degree')
#' @rdname npoly
#' @export
npoly <- function(x, ...){UseMethod("npoly")}
#' @rdname npoly
#' @export
npoly.default <- function(x, degree, ...) {
  coo <- x
  coo <- coo_check(coo)
  if (missing(degree)) {
    degree <- 5
    message("'degree' not provided and set to: ", degree)
  }
  x <- poly(coo[, 1], degree = degree, raw = TRUE)
  mod <- lm(coo[, 2] ~ x)
  r2 <- summary(mod)$r.squared
  return(list(coeff = mod$coefficients, ortho = FALSE, degree = degree,
              baseline1 = coo[1, ], baseline2 = coo[nrow(coo), ], r2 = r2,
              mod = mod))
}

#' @rdname npoly
#' @export
npoly.Opn <- function(x,
                      degree,
                      baseline1 = c(-0.5, 0),
                      baseline2 = c(0.5, 0),
                      nb.pts = 120, ...) {
  Opn <- x
  # validates
  Opn %<>% validate()
  # we check a bit
  min.pts <- min(sapply(Opn$coo, nrow))
  if (nb.pts > min.pts) {
    if (missing(nb.pts)) {
      nb.pts <- min.pts
      message("'nb.pts' missing and set to: ", nb.pts)
    } else {
      nb.pts <- min.pts
      message("at least one outline has less coordinates than 'nb.pts': ", nb.pts)
    }
  }
  if (missing(degree)) {
    degree <- 5
    message("'degree' missing and set to: ", degree)
  }
  # we normalize
  Opn <- coo_sample(Opn, nb.pts)
  coo <- Opn$coo
  coo <- lapply(coo, coo_baseline, ldk1 = 1, ldk2 = nb.pts,
                t1 = baseline1, t2 = baseline2)
  # we prepare the coe matrix
  rn <- names(coo)
  cn <- paste0("x", 1:degree)
  cn <- c("Intercept", cn)
  coe <- matrix(NA, nrow = length(Opn), ncol = degree + 1,
                dimnames = list(rn, cn))
  r2 <- numeric(length(Opn))
  mod <- list()
  # the loop
  for (i in seq(along = coo)) {
    mod <- npoly(coo[[i]], degree = degree)
    # mod[[i]] <- pol
    coe[i, ] <- mod$coeff
    r2[i] <- mod$r2
  }
  # mod$coefficients <- rep(NA, length(mod$coefficients))
  method <- "npoly"
  res <- OpnCoe(coe = coe, fac = Opn$fac, method = method,
                baseline1 = baseline1, baseline2 = baseline2, r2 = r2,
                mod = mod)
  res$cuts <- ncol(res$coe)
  return(res)
}

#' Calculates shape from a polynomial model
#'
#' Returns a matrix of (x; y) coordinates when passed with a list obtained with
#' \link{opoly} or \link{npoly}.
#' @param pol a pol list such as created by \link{npoly} or \link{opoly}
#' @param nb.pts the number of points to predict. By default (and cannot be higher)
#' the number of points in the original shape.
#' @param reregister logical whether to reregister the shape with the original baseline.
#' @return a matrix of (x; y) coordinates.
#' @family polynomials
#' @examples
#' data(olea)
#' o <- olea[5]
#' coo_plot(o)
#' for (i in 2:7){
#' x <- opoly_i(opoly(o, i))
#' coo_draw(x, border=col_summer(7)[i], points=FALSE)  }
#' @rdname poly_i
#' @export
opoly_i <- function(pol, nb.pts = 120, reregister = TRUE) {
  .mprod <- function (m, s) {
    res <- m
    for (i in 1:ncol(m)) {
      res[, i] <- m[, i] * s[i]
    }
    return(res)
  }
  x.new <- seq(pol$baseline1[1], pol$baseline2[1], length = nb.pts)
  degree <- length(pol$coeff) - 1
  x.poly <- poly(x.new, degree = degree)
  y.pred <- predict(x.poly, x.new)
  y.new <- pol$coeff[1] + apply(.mprod(m = y.pred, s = pol$coeff[-1]),
                                1, sum)
  coo <- cbind(x.new, y.new)
  if (reregister) {
    coo <- coo_baseline(coo, 1, nrow(coo), t1 = pol$baseline1, t2 = pol$baseline2)
  }
  colnames(coo) <- c("x", "y")
  return(coo)
}

#' @rdname poly_i
#' @export
npoly_i <- function(pol, nb.pts = 120, reregister = TRUE) {
  x.new <- seq(pol$baseline1[1], pol$baseline2[1], length = nb.pts)
  degree <- length(pol$coeff) - 1

  y.pred <- numeric(nb.pts)
  for (i in 1:degree) {
    y.pred <- y.pred + (x.new^i * pol$coeff[i + 1])
  }
  y.new <- y.pred + pol$coeff[1]

  coo <- cbind(x.new, y.new)
  if (reregister) {
    coo <- coo_baseline(coo, 1, nrow(coo), t1 = pol$baseline1, t2 = pol$baseline2)
  }
  colnames(coo) <- c("x", "y")
  return(coo)
}

#opoly_shape #TODO
#npoly_shape #TODO

##### end Polynomials
