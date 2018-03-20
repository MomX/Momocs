##### Core function for radii variation Fourier analyses

#' Radii variation Fourier transform (equally spaced curvilinear abscissa)
#'
#' \code{sfourier} computes radii variation Fourier analysis from a matrix or a
#' list of coordinates where points are equally spaced aong the curvilinear abscissa.
#'
#' @param x A \code{list} or \code{matrix} of coordinates or an \code{Out} object
#' @param nb.h \code{integer}. The number of harmonics to use. If missing, 12 is used on shapes;
#' 99 percent of harmonic power on Out objects, both with messages.
#' @return A list with following components:
#' \itemize{
#'  \item \code{an} vector of \eqn{a_{1->n}} harmonic coefficients
#'  \item \code{bn} vector of \eqn{b_{1->n}} harmonic coefficients
#'  \item \code{ao} ao harmonic coefficient
#'  \item \code{r} vector of radii lengths
#'  }
#' @family sfourier
#' @note The implementation is still quite experimental (as of Dec. 2016)
#' @references Renaud S, Michaux JR (2003): Adaptive latitudinal trends in the mandible shape
#' of \emph{Apodemus} wood mice. \emph{J Biogeogr} 30:1617-1628.
#' @examples
#' molars[4] %>%
#' coo_center %>% coo_scale %>% coo_interpolate(1080) %>%
#' coo_slidedirection("right") %>%
#'    coo_sample(360) %T>% coo_plot(zoom=2) %>%
#'    sfourier(16) %>%
#'    sfourier_i() %>%
#'    coo_draw(bor="red", points=TRUE)
#' @rdname sfourier
#' @export
sfourier <- function(x, nb.h){
  UseMethod("sfourier")
}

#' @rdname sfourier
#' @export
sfourier.default <- function(x, nb.h){
  shp <- x
  # deduces the number of points
  N <- coo_nb(shp)
  # if missing, nb.h will be set to max (calibrate this)
  if (missing(nb.h))
    nb.h <- floor(N/2)

  # calculates radii lengths
  r <- coo_centdist(shp)
  # extract an and bn coefficients using a discrete fft
  an <- (Re(fft(r))/N)[1:(nb.h+1)]
  bn <- (Im(fft(r))/N)[1:(nb.h+1)]
  # grabs size and normalize with it
  ao <- an[1]
  # an[-1] <- an[-1]/ao
  an <- an[-1]/ao
  bn <- bn[-1]/ao
  # return results
  res <- list(an=an,
              bn=bn,
              ao=ao,
              r=r)
  # removes remaining names
  res <- lapply(res, function(x) {names(x) <- NULL; x})
  return(res)
}

#' @rdname sfourier
#' @export
sfourier.Out <- function(x, nb.h){
  Out <- x
  # validates
  Out %<>% validate()
  q <- floor(min(sapply(Out$coo, nrow)/2))
  if (missing(nb.h)) {
    # nb.h <- ifelse(q >= 32, 32, q)
    nb.h <- calibrate_harmonicpower_sfourier(Out,
                                    thresh = 99, plot=FALSE)$minh
    # if (.is_verbose()) message("'nb.h' not provided and set to ", nb.h, " (99% harmonic power)")
  }
  if (nb.h > q) {
    nb.h <- q  # should not be 1 #todo
    message("at least one outline has no more than ", q * 2,
            " coordinates. 'nb.h' has been set to ", q,
            " harmonics")
  }
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h,
                                                      times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo),
                                                                     col.n))
  ao <- vector("numeric", length=length(coo))
  for (i in seq(along = coo)) {
    rf <- sfourier(coo[[i]], nb.h = nb.h)  #todo: vectorize
    coe[i, ] <- c(rf$an, rf$bn)
    ao[i] <- rf$ao
  }
  res <- OutCoe(coe = coe, fac = Out$fac, method = "sfourier", norm = TRUE)
  res$cuts <- ncol(res$coe)
  res$ao <- ao
  return(res)
}


#' Inverse radii variation Fourier transform
#'
#' \code{sfourier_i} uses the inverse radii variation (equally spaced curvilinear abscissa) transformation to
#' calculate a shape, when given a list with Fourier coefficients, typically
#' obtained computed with \link{sfourier}.
#'
#'
#' @param rf A \code{list} with \code{ao}, \code{an} and \code{bn} components,
#' typically as returned by \code{sfourier}.
#' @param nb.h \code{integer}. The number of harmonics to calculate/use.
#' @param nb.pts \code{integer}. The number of points to calculate.
#' @param dtheta \code{logical}. Whether to use the dtheta correction method.
#' \code{FALSE} by default. When \code{TRUE}, tries to correct the angular difference between
#' reconstructed points; otherwise equal angles are used.
#' @return A list with components: \item{x }{\code{vector} of
#' \code{x}-coordinates.} \item{y }{\code{vector} of \code{y}-coordinates.}
#' \item{angle}{\code{vector} of angles used.} \item{r}{\code{vector} of radii
#' calculated.}
#' @family sfourier
#' @references Renaud S, Pale JRM, Michaux JR (2003): Adaptive latitudinal trends in the mandible shape
#' of \emph{Apodemus} wood mice. \emph{J Biogeogr} 30:1617-1628.
#' @examples
#' coo <- coo_center(bot[1]) # centering is almost mandatory for sfourier family
#' coo_plot(coo)
#' rf  <- sfourier(coo, 12)
#' rf
#' rfi <- sfourier_i(rf)
#' coo_draw(rfi, border='red', col=NA)
#'
#' @export
sfourier_i <-
  function(rf, nb.h, nb.pts = 120, dtheta=FALSE) {
    if (!all(c("an", "bn") %in% names(rf))) {
      stop("a list containing 'an' and 'bn' harmonic coefficients must be provided")
    }
    ao <- ifelse(is.null(rf$ao), 1, rf$ao)
    an <- rf$an
    bn <- rf$bn
    if (missing(nb.h)) {
      nb.h <- length(an)
    }
    if (nb.h > length(an)) {
      nb.h <- length(an)
      message("nb.h cannot be higher than length(rf$an) and has been set to ", nb.h)
    }
    theta <- seq(0, 2 * pi, length = nb.pts)
    harm <- matrix(NA, nrow = nb.h, ncol = nb.pts)
    for (i in 1:nb.h) {
      harm[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i * theta)
    }
    r <- ao/2 + apply(harm, 2, sum)
    # theta corrected using ds/dtheta method
    if (dtheta){
      ds <- (2*pi)/sum(1/r)
      theta <- numeric(length(r))
      for (i in 2:nb.pts){
        theta[i] <- theta[i-1] + ds/r[i]
      }
      coo <- cbind(r*cos(theta), r*sin(theta))
    } else {
      # theta based on the circle
      Z <- complex(modulus = r, argument = theta)
      coo <- cbind(Re(Z), Im(Z))
    }
    colnames(coo) <- c("x", "y")
    return(coo)
  }

#' Calculates and draw 'sfourier' shapes.
#'
#' \code{sfourier_shape} calculates a 'Fourier radii variation shape' given
#' Fourier coefficients (see \code{Details}) or can generate some 'sfourier'
#' shapes.
#'
#' \code{sfourier_shape} can be used by specifying \code{nb.h} and
#' \code{alpha}. The coefficients are then sampled in an uniform distribution
#' \eqn{(-\pi ; \pi)} and this amplitude is then divided by
#' \eqn{harmonicrank^alpha}. If \code{alpha} is lower than 1, consecutive
#' coefficients will thus increase. See \link{sfourier} for the mathematical
#' background.
#'
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
#' @return A matrix of (x; y) coordinates.
#' @family sfourier
#' @references Renaud S, Pale JRM, Michaux JR (2003): Adaptive latitudinal trends in the mandible shape
#' of \emph{Apodemus} wood mice. \emph{J Biogeogr} 30:1617-1628.
#' @examples
#' rf <- sfourier(bot[1], 24)
#' sfourier_shape(rf$an, rf$bn) # equivalent to sfourier_i(rf)
#' sfourier_shape() # not very interesting
#'
#' sfourier_shape(nb.h=12) # better
#' sfourier_shape(nb.h=6, alpha=0.4, nb.pts=500)
#'
#' # Butterflies of the vignette' cover
#' panel(Out(a2l(replicate(100,
#' sfourier_shape(nb.h=6, alpha=0.4, nb.pts=200, plot=FALSE)))))
#' @export
sfourier_shape <- function(an, bn, nb.h, nb.pts = 80, alpha = 2,
                           plot = TRUE) {
  if (missing(nb.h) & missing(an))
    nb.h <- 6
  if (missing(nb.h) & !missing(an))
    nb.h <- length(an)
  if (missing(an))
    an <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  if (missing(bn))
    bn <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  rf <- list(an = an, bn = bn, ao = 0)
  shp <- sfourier_i(rf, nb.h = nb.h, nb.pts = nb.pts)
  if (plot)
    coo_plot(shp)
  return(shp)
}

##### end sfourier
