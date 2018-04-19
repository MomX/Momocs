# coo_lw ------
#' Calculates length and width of a shape
#'
#' Returns the length and width of a shape based on their iniertia axis
#' i.e. alignment to the x-axis. The length is defined as
#' the range along the x-axis; the width as the range on the y-axis.
#' @param coo a \code{matrix} of (x; y) coordinates or Coo object
#' @return a vector of two \code{numeric}: the length and the width.
#' @seealso \link{coo_length}, \link{coo_width}.
#' @family coo_ descriptors
#' @examples
#' coo_lw(bot[1])
#' @export
coo_lw <- function(coo){
  UseMethod("coo_lw")
}

#' @export
coo_lw.default <- function(coo) {
  coo <- coo_check(coo)
  d <- apply(coo_align(coo), 2, range)
  return(abs(d[2, ] - d[1, ]))
}

#' @export
coo_lw.Coo <- function(coo){
  sapply(coo$coo, coo_lw)
}

#' Calculates the length of a shape
#'
#' Nothing more than \code{coo_lw(coo)[1]}.
#' @param coo a \code{matrix} of (x; y) coordinates or a Coo object
#' @return the length (in pixels) of the shape
#' @seealso \link{coo_lw}, \link{coo_width}
#' @details This function can be used to integrate size - if meaningful -
#' to Coo objects. See also \link{coo_centsize} and \link{rescale}.
#' @family coo_ descriptors
#' @examples
#' coo_length(bot[1])
#' coo_length(bot)
#' mutate(bot, size=coo_length(bot))
#' @export
coo_length <- function(coo){
  UseMethod("coo_length")
}
#' @export
coo_length.default <- function(coo) {
  return(coo_lw(coo)[1])
}
#' @export
coo_length.Coo <- function(coo){
  sapply(coo$coo, coo_length)
}

#' Calculates the width of a shape
#'
#' Nothing more than \code{coo_lw(coo)[2]}.
#' @param coo a \code{matrix} of (x; y) coordinates or Coo object
#' @return the width (in pixels) of the shape
#' @seealso \link{coo_lw}, \link{coo_length}.
#' @family coo_ descriptors
#' @examples
#' coo_width(bot[1])
#' @export
coo_width <- function(coo) {
  UseMethod("coo_width")
}

#' @export
coo_width.default <- function(coo){
  return(coo_lw(coo)[2])
}

#' @export
coo_width.Coo <- function(coo){
  sapply(coo$coo, coo_width)
}

# coo_boundingbox -----------
#' Calculates coordinates of the bounding box
#' @inheritParams coo_check
#' @return `data.frame` with coordinates of the bounding box
#' @examples
#' bot[1] %>% coo_boundingbox()
#' bot %>% coo_boundingbox()
#' @family coo_ utilities
#' @family coo_ descriptors
#' @export
coo_boundingbox <- function(coo){
  UseMethod("coo_boundingbox")
}

#' @export
coo_boundingbox.default <- function(coo){
  coo %>% apply(2, range) %>% as.numeric() %>%
    sapply(list) %>% `names<-`(c("x0", "x1", "y0", "y1")) %>%
    dplyr::as_data_frame()
}

#' @export
coo_boundingbox.Coo <- function(coo){
  lapply(coo$coo, coo_boundingbox) %>%
    do.call("rbind", .)
}




# coo_area ------
#' Calculates the area of a shape
#'
#' Calculates the area for a (non-crossing) shape.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric}, the area.
#' @note Using \code{area.poly} in gpc package is a good idea, but their licence
#' impedes Momocs to rely on it. but here is the function to do it, once gpc is loaded:
#' \code{ area.poly(as(coo, 'gpc.poly')) }
#' @family coo_ descriptors
#' @examples
#' coo_area(bot[1])
#' # for the distribution of the area of the bottles dataset
#' hist(sapply(bot$coo, coo_area), breaks=10)
#' @export
coo_area <- function(coo){
  UseMethod("coo_area")
}
#' @export
coo_area.default <- function(coo) {
  coo <- coo_check(coo)
  coo <- coo_close(coo)
  nr <- nrow(coo) - 1
  y <- x <- numeric(nr)
  for (i in 1:nr) {
    x[i] <- coo[i, 1] * coo[i + 1, 2]
    y[i] <- coo[i + 1, 1] * coo[i, 2]
  }
  area <- (0.5 * (sum(x) - sum(y)))
  return(abs(area))
}

#' @export
coo_area.Coo <- function(coo){
  sapply(coo$coo, coo_area)
}
# area.poly(as(coo, 'gpc.poly'))}

# coo_angle ------
#' Calculate the angle formed by three (x; y) coordinates
#'
#' Returns the angle (in radians) defined by a triplet of points
#' either signed ('atan2') or not ('acos').
#' @param coo a 3x2 \code{matrix} of 3 points (rows) and (x; y) coordinates
#' @param method one of 'atan2' or 'acos' for a signed or not angle.
#' @return \code{numeric} the angle in radians.
#' @note \code{coo_theta3} is deprecated and will be removed
#' in future releases.
#' @family coo_ descriptors
#' @examples
#' b <- coo_sample(bot[1], 64)
#' b <- b[c(1, 14, 24), ]
#' coo_plot(b)
#' coo_angle_edges(b)
#' coo_angle_edges(bot[1])
#' @rdname coo_angle_edge1
#' @export
coo_angle_edge1 <- function(coo, method = c("atan2", "acos")[1]) {
  .Deprecated("coo_angle_edges")
}

#' @rdname coo_angle_edge1
#' @export
coo_theta3 <- function(coo, method = c("atan2", "acos")[1]){
  .Deprecated("coo_angle_edges")
}


.coo_angle_edge1 <- function(coo, method = c("atan2", "acos")[1]) {
  .check(is.matrix(coo) && nrow(coo)==3 && ncol(coo)==2,
         "coo must be a 3x2 matrix")
  a <- apply(coo[2:1, ], 2, diff)
  b <- apply(coo[2:3, ], 2, diff)
  if (method == "atan2") {
    ang <- atan2(a[1] * b[2] - a[2] * b[1],
                 a[1] * b[1] + a[2] * b[2])
  }
  if (method == "acos") {
    ang <- acos(sum(a * b) /
                  (sqrt(sum(a * a)) * sqrt(sum(b * b))))
  }
  ang
}


#' Calculates the angle of every edge of a shape
#'
#' Returns the angle (in radians) of every edge of a shape,
# either signed ('atan2') or not ('acos'). A wrapper for
# \link{coo_angle_edge1}
#' @param coo a \code{matrix} or a list of (x; y) coordinates or any `Coo`
#' @param method 'atan2' (or 'acos') for a signed (or not) angle.
#' @return \code{numeric} the angles in radians for every edge.
#' @note \code{coo_thetapts} is deprecated and will be removed
#' in future releases.
#' @family coo_ descriptors
#' @examples
#' b <- coo_sample(bot[1], 64)
#' coo_angle_edges(b)
#' @rdname coo_angle_edges
#' @export
coo_angle_edges <- function(coo, method = c("atan2", "acos")[1]){
  UseMethod("coo_angle_edges")
}

#' @rdname coo_angle_edges
#' @export
coo_angle_edges.default <- function(coo, method = c("atan2", "acos")[1]) {
  coo <- coo_check(coo)
  coo <- coo_close(coo)
  coo <- rbind(coo[nrow(coo) - 1, ], coo)
  theta <- numeric()
  for (i in 1:(nrow(coo) - 2)) {
    theta[i] <- .coo_angle_edge1(coo[i:(i + 2), ], method = method)
  }
  return(theta)
}

#' @rdname coo_angle_edges
#' @export
coo_angle_edges.Coo <- function(coo, method = c("atan2", "acos")[1]) {
  lapply(coo$coo, coo_angle_edges, method=method)
}

#' @rdname coo_angle_edges
#' @export
coo_thetapts <- function(coo, method = c("atan2", "acos")[1]){
  .Deprecated("coo_angle_edges")
}

#' Calculates the tangent angle along the perimeter of a
#' shape
#'
#' Calculated using complex numbers and returned in radians
#' minus the first one (modulo 2*pi).
#' @param coo a matrix of coordinates or any `Coo`
#' @return `numeric`, the tangent angle along the perimeter, or a
#' `list` of those for `Coo`
#' @seealso \link{tfourier}
#' @family coo_ descriptors
#' @examples
#' b <- bot[1]
#' phi  <- coo_angle_tangent(b)
#' phi2 <- coo_angle_tangent(coo_smooth(b, 2))
#' plot(phi, type='l')
#' plot(phi2, type='l', col='red') # ta is very sensible to noise
#'
#' # on Coo
#' bot %>% coo_angle_tangent
#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent <- function(coo) {
  UseMethod("coo_angle_tangent")
}

#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent.default <- function(coo) {
  p <- nrow(coo)
  tangvect <- coo - rbind(coo[p, ], coo[-p, ])
  tet1 <- Arg(complex(real = tangvect[, 1], imaginary = tangvect[,
                                                                 2]))
  tet0 <- tet1[1]
  t1 <- seq(0, 2 * pi, length = (p + 1))[1:p]
  phi <- (tet1 - tet0 - t1)%%(2 * pi)
  return(phi)
}

#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent.Coo <- function(coo) {
  lapply(coo$coo, coo_angle_tangent)
}

#' @rdname coo_angle_tangent
#' @export
coo_tangle <- function(coo){
  .Deprecated("coo_angle_tangent")
}


# coo_rectilinearity ------
#' Calculates the rectilinearity of a shape
#'
#' As proposed by Zunic and Rosin (see below). May need some testing/review.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @note due to the laborious nature of the algorithm (in nb.pts^2),
#' and of its implementation, it may be very long to compute.
#' @source Zunic J, Rosin PL. 2003. Rectilinearity measurements for polygons.
#' IEEE Transactions on Pattern Analysis and Machine Intelligence 25: 1193-1200.
#' @family coo_ descriptors
#' @examples
#' bot[1] %>%
#'     coo_sample(32) %>% # for speed sake only
#'     coo_rectilinearity
#'
#' bot %>%
#'     slice(1:3) %>% coo_sample(32) %>% # for speed sake only
#'     coo_rectilinearity
#' @export
coo_rectilinearity <- function(coo) {
  UseMethod("coo_rectilinearity")
}

#' @export
coo_rectilinearity.default <- function(coo) {
  # some check
  coo <- coo_check(coo)
  if (coo_is_closed(coo)) {
    coo_c <- coo
    coo <- coo_unclose(coo)
  } else {
    coo_c <- coo_close(coo)
  }
  # we deduce it for the algo
  n <- nrow(coo)
  k <- 4 * n
  # here starts the computation as given by Zunic and Rosin we
  # calculate l1 and l2 for every edge
  l1 <- function(x1, y1, x2, y2) {
    abs(x1 - x2) + abs(y1 - y2)
  }
  l2 <- function(x1, y1, x2, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }
  # l2 is redefined here for coherence with the paper, but is
  # equivalent to coo_perimpts(coo)
  l2.e <- l1.e <- numeric(n)
  for (i in 1:n) {
    x1 <- coo_c[i, 1]
    y1 <- coo_c[i, 2]
    x2 <- coo_c[i + 1, 1]
    y2 <- coo_c[i + 1, 2]
    l1.e[i] <- l1(x1, y1, x2, y2)
    l2.e[i] <- l2(x1, y1, x2, y2)
  }  # sum(l2.e) == coo_perim(coo)
  # 'step 1' as in Zunic and Rosin
  theta <- coo_angle_edges(coo)
  theta.k <- abs(c(theta - pi/2, theta - pi, theta - 3 * pi/2,
                   theta - 2 * pi))
  alpha.k <- sort(theta.k)
  # 'step 2' as in Zunic and Rosin
  P1.Pa <- numeric(k)
  for (j in 1:k) {
    P1.Pa_n <- numeric(n)
    for (i in 1:n) {
      cos.ij <- cos(theta[i] + alpha.k[j])
      sin.ij <- sin(theta[i] + alpha.k[j])
      a.ij <- ifelse(cos.ij > 0, l2.e[i], -l2.e[i])
      b.ij <- ifelse(sin.ij > 0, l2.e[i], -l2.e[i])
      P1.Pa_n[i] <- a.ij * cos.ij + b.ij * sin.ij
    }
    P1.Pa[j] <- sum(P1.Pa_n)
  }
  # 'step 3' as in Zunic and Rosin
  return((4/(4 - pi)) * ((sum(l2.e)/min(P1.Pa)) - (pi/4)))
}

#' @export
coo_rectilinearity.Coo <- function(coo) {
  lapply(coo$coo, coo_rectilinearity)
}

# coo_circularity ------
#' Calculates the Haralick's circularity of a shape
#'
#' `coo_circularity` calculates the 'circularity measure'. Also called 'compactness'
#' and 'shape factor' sometimes. `coo_circularityharalick` calculates Haralick's circularity which is less sensible
#' to digitalization noise than `coo_circularity`.
#' `coo_circularitynorm` calculates 'circularity', also called compactness
#' and shape factor, but normalized to the unit circle.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for single shapes, `list` for `Coo` of
#' the corresponding circularity measurement.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#'
#' # coo_circularity
#' bot[1] %>% coo_circularity()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularity
#'
#' # coo_circularityharalick
#' bot[1] %>% coo_circularityharalick()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularityharalick
#'
#' # coo_circularitynorm
#' bot[1] %>% coo_circularitynorm()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularitynorm
#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity <- function(coo){
  UseMethod("coo_circularity")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity.default <- function(coo) {
  return(coo_perim(coo)^2/coo_area(coo))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity.Coo <- function(coo) {
  lapply(coo$coo, coo_circularity)
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick <- function(coo) {
  UseMethod("coo_circularityharalick")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick.default <- function(coo) {
  cd <- coo_centdist(coo)
  return(mean(cd)/sd(cd))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick.Coo <- function(coo) {
  lapply(coo$coo, coo_circularityharalick)
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm <- function(coo){
  UseMethod("coo_circularitynorm")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm.default <- function(coo) {
  return(coo_perim(coo)^2/(coo_area(coo) * 4 * pi))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm.Coo <- function(coo) {
  lapply(coo$coo, coo_circularitynorm)
}

# coo_eccentricity ------
#' Calculates the eccentricity of a shape
#'
#'
#' `coo_eccentricityeigen` uses the ratio of
#' the eigen values (inertia axes of coordinates).
#' `coo_eccentricityboundingbox` uses the width/length ratio (see [coo_lw]).
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for single shapes, `list` for `Coo`.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @seealso \link{coo_eccentricityboundingbox}
#' @family coo_ descriptors
#' @examples
#' # coo_eccentricityeigen
#' bot[1] %>% coo_eccentricityeigen()
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_eccentricityeigen()
#'
#' # coo_eccentricityboundingbox
#' bot[1] %>% coo_eccentricityboundingbox()
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_eccentricityboundingbox()
#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen <- function(coo) {
  UseMethod("coo_eccentricityeigen")
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen.default <- function(coo) {
  coo <- coo_check(coo)
  eig <- eigen(cov(coo))$values
  return(eig[2]/eig[1])
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen.Coo <- function(coo) {
  lapply(coo$coo, coo_eccentricityeigen)
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox <- function(coo) {
  UseMethod("coo_eccentricityboundingbox")
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox.default <- function(coo) {
  coo <- coo_check(coo)
  lw <- coo_lw(coo)
  return(lw[2]/lw[1])
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox.Coo <- function(coo) {
  lapply(coo$coo, coo_eccentricityboundingbox)
}

#' Calculates the elongation of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the eccentricity of the bounding box
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_elongation(bot[1])
#' # on Coo
#' # for speed sake
#' bot %>% slice(1:3) %>% coo_elongation
#' @export
coo_elongation <- function(coo) {
  UseMethod("coo_elongation")
}

#' @export
coo_elongation.default <- function(coo) {
  coo <- coo_check(coo)
  lw <- coo_lw(coo)
  return(1 - lw[2]/lw[1])
}

#' @export
coo_elongation.Coo <- function(coo) {
  lapply(coo$coo, coo_elongation)
}

# coo_rectangularity -----

#' Calculates the rectangularity of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_rectangularity(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_rectangularity
#' @export
coo_rectangularity <- function(coo) {
  UseMethod("coo_rectangularity")
}

#' @export
coo_rectangularity.default <- function(coo) {
  coo <- coo_check(coo)
  abr <- prod(coo_lw(coo))
  return(coo_area(coo)/abr)
}

#' @export
coo_rectangularity.Coo <- function(coo) {
  lapply(coo$coo, coo_rectangularity)
}

# coo_chull --------
#' Calculates the (recursive) convex hull of a shape
#'
#' `coo_chull` returns the ids of points that define the convex hull of a shape. A simple wrapper
#' around \link{chull}, mainly used in graphical functions.
#'
#' `coo_chull_onion` recursively find their convex hull,
#' remove them, until less than 3 points are left.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`.
#' @param close `logical` whether to close onion rings (`TRUE` by default)
#' @return `coo_chull` returns a `matrix` of points defining
#' the convex hull of the shape; a `list` for `Coo`.
#' `coo_chull_onion` returns a `list` of successive onions rings,
#'  and a `list` of `list`s for `Coo`.
#' @family coo_ descriptors
#' @examples
#' # coo_chull
#' h <- coo_sample(hearts[4], 32)
#' coo_plot(h)
#' ch <- coo_chull(h)
#' lines(ch, col='red', lty=2)
#'
#' bot %>% coo_chull
#'
#' coo_chull_onion
#' x <- bot %>% efourier(6) %>% PCA
#' all_whisky_points <- x %>% as_df() %>% filter(type=="whisky") %>% select(PC1, PC2)
#' plot(x, ~type, eig=FALSE)
#' peeling_the_whisky_onion <- all_whisky_points %>% as.matrix %>% coo_chull_onion()
#' # you may need to par(xpd=NA) to ensure all segments
#' # even those outside the graphical window are drawn
#' peeling_the_whisky_onion$coo %>% lapply(coo_draw)
#' # simulated data
#' xy <- replicate(2, rnorm(50))
#' coo_plot(xy, poly=FALSE)
#' xy %>% coo_chull_onion() %$% coo %>%
#' lapply(polygon, col="#00000022")
#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull <- function(coo){
  UseMethod("coo_chull")
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo[grDevices::chull(coo), ])
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull.Coo <- function(coo) {
  lapply(coo$coo, coo_chull)
}


# coo_chull_onion -----------
#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion <- function(coo, close=TRUE){
  UseMethod("coo_chull_onion")
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion.default <- function(coo, close=TRUE){
  coo %<>% as.matrix()
  res <- list()
  i <- 1
  while(is.matrix(coo) && nrow(coo) > 3){
    chi_ids <- grDevices::chull(coo[, 1], coo[, 2])
    # if asked to close, then close ids and coos will follow
    if(close)
      chi_ids <- c(chi_ids, chi_ids[1])

    res$ids[[i]] <- chi_ids
    res$coo[[i]] <- coo[chi_ids, ]
    coo <- coo[-chi_ids, ]
    i <- i + 1
  }
  res
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion.Coo <- function(coo, close=TRUE){
  lapply(coo$coo, coo_chull_onion)
}

# coo_convexity -------
#' Calculates the convexity of a shape
#'
#' Calculated using a ratio of the eigen values (inertia axis)
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return `numeric` for a single shape, `list` for a `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_convexity(bot[1])
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_convexity()
#' @export
coo_convexity <- function(coo) {
  UseMethod("coo_convexity")
}

#' @export
coo_convexity.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo_perim(coo_chull(coo))/coo_perim(coo))
}

#' @export
coo_convexity.Coo <- function(coo){
  lapply(coo$coo, coo_convexity)
}
# coo_solidity -------
#' Calculates the solidity of a shape
#'
#' Calculated using the ratio of the shape area and the convex hull area.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_solidity(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>%  # for speed sake only
#'     coo_solidity
#' @export
coo_solidity <- function(coo){
  UseMethod("coo_solidity")
}

#' @export
coo_solidity.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo_area(coo)/coo_area(coo_chull(coo)))
}

#' @export
coo_solidity.Coo <- function(coo) {
  lapply(coo$coo, coo_solidity)
}

# coo_tac -------
#' Calculates the total absolute curvature of a shape
#'
#' Calculated using the sum of the absolute value of the second derivative of
#' the \code{smooth.spline} prediction for each defined point.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape and for `Coo`
#' @source Siobhan Braybrook.
#'
#' @family coo_ descriptors
#' @examples
#' coo_tac(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>%  # for speed sake only
#'     coo_tac
#' @export
coo_tac <- function(coo){
  UseMethod("coo_tac")
}

#' @export
coo_tac.default <- function(coo) {
  coo <- coo_check(coo)
  tac <- sum(abs(predict(stats::smooth.spline(coo), deriv = 2)$y))
  return(tac)
}

#' @export
coo_tac.Coo <- function(coo) {
  sapply(coo$coo, stats::coo_tac)
}
