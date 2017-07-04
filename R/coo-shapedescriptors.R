# coo_lw ------
# todo: bouding box todo: based on svd cov mat
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
#' data(bot)
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
#' data(bot)
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
#' data(bot)
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
#' @return named \code{list} with coordinates of the bounding box
#' @examples
#' bot[1] %>% coo_boundingbox()
#' @family coo_ utilities
#' @family coo_ descriptors
#' @export
coo_boundingbox <- function(coo){
  coo %>% apply(2, range) %>% as.numeric() %>%
    sapply(list) %>% `names<-`(c("x0", "x1", "y0", "y1"))
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
#' data(bot)
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

# coo_tangle ------
#' Calculates the tangent angle along the perimeter of a shape
#'
#' Calculated using complex numbers and returned in radians
#' minus the first one (modulo 2*pi).
#' @param coo a matrix of coordinates
#' @return a numeric, the tangent angle along the perimeter
#' @seealso \link{tfourier}
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' b <- bot[1]
#' phi  <- coo_tangle(b)
#' phi2 <- coo_tangle(coo_smooth(b, 2))
#' plot(phi, type='l')
#' plot(phi2, type='l', col='red') # ta is very sensible to noise
#' @export
coo_tangle <- function(coo) {
    p <- nrow(coo)
    tangvect <- coo - rbind(coo[p, ], coo[-p, ])
    tet1 <- Arg(complex(real = tangvect[, 1], imaginary = tangvect[,
        2]))
    tet0 <- tet1[1]
    t1 <- seq(0, 2 * pi, length = (p + 1))[1:p]
    phi <- (tet1 - tet0 - t1)%%(2 * pi)
    return(phi)
}

# coo_thetas ------
#' Calculate the angle formed by three (x; y) coordinates
#'
#' Returns the angle (in radians) defined by a triplet of points
#' either signed ('atan2') or not ('acos').
#' @param m a 3x2 \code{matrix} of 3 points (rows) and (x; y) coordinates
#' @param method one of 'atan2' or 'acos' for a signed or not angle.
#' @return \code{numeric} the angle in radians.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 64)
#' b <- b[c(1, 14, 24), ]
#' coo_plot(b)
#' coo_theta3(b)
#' coo_theta3(b, method='acos')
#' @export
coo_theta3 <- function(m, method = c("atan2", "acos")[1]) {
    a <- c(m[1, 1] - m[2, 1], m[1, 2] - m[2, 2])
    b <- c(m[3, 1] - m[2, 1], m[3, 2] - m[2, 2])
    if (method == "atan2") {
        return(atan2(a[1] * b[2] - a[2] * b[1], a[1] * b[1] +
            a[2] * b[2]))
    }
    if (method == "acos") {
        return(acos(sum(a * b)/(sqrt(sum(a * a)) * sqrt(sum(b *
            b)))))
    }
}

#' Calculates the angle of every edge of a shape
#'
#' Returns the angle (in radians) of every edge of a shape,
# either signed ('atan2') or not ('acos'). A wrapper for
# \link{coo_theta3}
#' @param coo a \code{matrix} or a list of (x; y) coordinates.
#' @param method one of 'atan2' or 'acos' for a signed or not angle.
#' @return \code{numeric} the angles in radians for every edge.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 64)
#' coo_thetapts(b)
#' @export
coo_thetapts <- function(coo, method = c("atan2", "acos")[1]) {
    coo <- coo_check(coo)
    coo <- coo_close(coo)
    coo <- rbind(coo[nrow(coo) - 1, ], coo)
    theta <- numeric()
    for (i in 1:(nrow(coo) - 2)) {
        theta[i] <- coo_theta3(coo[i:(i + 2), ], method = method)
    }
    return(theta)
}

# coo_rectilinearity ------
#' Calculates the rectilinearity of a shape
#'
#' As proposed by Zunic and Rosin (see below). May need some testing/review.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the rectilinearity
#' @note due to the laborious nature of the algorithm (in nb.pts^2),
#' and of its implementation, it may be very long to compute.
#' @source Zunic J, Rosin PL. 2003. Rectilinearity measurements for polygons.
#' IEEE Transactions on Pattern Analysis and Machine Intelligence 25: 1193-1200.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 32)
#' coo_rectilinearity(b)
#' @export
coo_rectilinearity <- function(coo) {
    # some check
    coo <- coo_check(coo)
    if (is_closed(coo)) {
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
    theta <- coo_thetapts(coo)
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

# coo_circularity ------
#' Calculates the Haralick's circularity of a shape
#'
#' Returns Haralick's circularity which is less sensible
#' to digitalization noise than coo_circularity
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the Haralick's circularity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_circularityharalick(bot[1])
#' @export
coo_circularityharalick <- function(coo) {
    cd <- coo_centdist(coo)
    return(mean(cd)/sd(cd))
}

#' Calculates the circularity of a shape
#'
#' Returns the 'circularity measure'. Also called 'compactness'
#' and 'shape factor' sometimes.
#' @aliases coo_compactness coo_shapefactor
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the circularity.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_circularity(bot[1])
#' @export
coo_circularity <- function(coo) {
    return(coo_perim(coo)^2/coo_area(coo))
}

#' Calculates the 'normalized' circularity of a shape
#'
#' Returns the 'circularity', also called compactness
#' and shape factor, but normalized to the unit circle.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the circularity normalized to the unit circle.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_circularitynorm(bot[1])
#' @export
coo_circularitynorm <- function(coo) {
    return(coo_perim(coo)^2/(coo_area(coo) * 4 * pi))
}

# coo_eccentricity ------
#' Calculates the eccentricity (using eigenvalues) of a shape
#'
#' Calculated using a ratio of the eigen values (inertia axes oof coordinates.)
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the eccentricity (eigenvalues)
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @seealso \link{coo_eccentricityboundingbox}
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_eccentricityeigen(bot[1])
#' @export
coo_eccentricityeigen <- function(coo) {
    coo <- coo_check(coo)
    eig <- eigen(cov(coo))$values
    return(eig[2]/eig[1])
}

#' Calculates the eccentricity (bounding box) of a shape
#'
#' Calculated using the width / length ratio. See \link{coo_lw}
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the eccentricity (boundingbox)
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @seealso \link{coo_eccentricityeigen}
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_eccentricityboundingbox(bot[1])
#' @export
coo_eccentricityboundingbox <- function(coo) {
    coo <- coo_check(coo)
    lw <- coo_lw(coo)
    return(lw[2]/lw[1])
}

#' Calculates the elongation of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the eccentricity of the bounding box
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_elongation(bot[1])
#' @export
coo_elongation <- function(coo) {
    coo <- coo_check(coo)
    lw <- coo_lw(coo)
    return(1 - lw[2]/lw[1])
}

# coo_rectangularity -----

#' Calculates the rectangularity of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the rectangularity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_rectangularity(bot[1])
#' @export
coo_rectangularity <- function(coo) {
    coo <- coo_check(coo)
    abr <- prod(coo_lw(coo))
    return(coo_area(coo)/abr)
}

# coo_chull --------
#' Calculates the convex hull of a shape
#'
#' Returns the ids of points that define the convex hull of a shape. A simple wrapper
#' around \link{chull}, mainly used in graphical functions.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return a \code{matrix} of ids defining the convex hull of the shape.
#' @family coo_ descriptors
#' @examples
#' data(hearts)
#' h <- coo_sample(hearts[4], 32)
#' coo_plot(h)
#' ch <- coo_chull(h)
#' lines(ch, col='red', lty=2)
#' @export
coo_chull <- function(coo) {
    coo <- coo_check(coo)
    return(coo[chull(coo), ])
}

# coo_convexity -------
#' Calculates the convexity of a shape
#'
#' Calculated using a ratio of the eigen values (inertia axis)
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the convexity.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_convexity(bot[1])
#' @export
coo_convexity <- function(coo) {
    coo <- coo_check(coo)
    return(coo_perim(coo_chull(coo))/coo_perim(coo))
}

# coo_solidity -------
#' Calculates the solidity of a shape
#'
#' Calculated using the ratio of the shape area and the convex hull area.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the solidity of a shape.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' data(bot)
#' coo_solidity(bot[1])
#' @export
coo_solidity <- function(coo) {
    coo <- coo_check(coo)
    return(coo_area(coo)/coo_area(coo_chull(coo)))
}

# Cannot be included since it relies on gpc.lib #todo: find
# an alternative #' Calculate the area overlap between two
# shapes #' #' Simply calculates (area(coo1) + area(coo2) -
# area(union(coo1, coo2))) #' #' @param coo1 the first shape
# #' @param coo2 the second shape #' @return the area of the
# overlap #' @examples #' data(bot) #' b1 <- bot[1] #' b2 <-
# coo_trans(b1, 50) #' coo_plot(b1) #' coo_draw(b2) #'
# coo_overlap(b1, b2) #' @export coo_overlap <-
# function(coo1, coo2){ p1 <- as(coo1, 'gpc.poly') p2 <-
# as(coo2, 'gpc.poly') p0 <- union(p1,p2) ov <- area.poly(p1)
# + area.poly(p2) - area.poly(p0) return(ov)} #' Calculate
# the area union between two shapes #' #' If the two shapes
# overlaps returns the shape of their union. If not, returns
# NULL.  #' #' @param coo1 the first shape #' @param coo2 the
# second shape #' @return the area of the overlap #'
# @examples #' data(bot) #' b1 <- bot[1] #' ba <-
# coo_union(b1, coo_trans(b1, 200)) #' coo_plot(ba) #'
# coo_union(b1, coo_trans(b1, 1e3)) #' @export coo_union <-
# function(coo1, coo2){ p1 <- as(coo1, 'gpc.poly') p2 <-
# as(coo2, 'gpc.poly') pu <- union(p1, p2) if (length(pu@pts)
# > 1) return(NULL) pu <- cbind(pu@pts[[1]]$x, pu@pts[[1]]$y)
# return(pu)} #' Estimates radial symmetry #' #' This
# function implements a simple estimate of radial symmetry
# occurence, #' by calculating overlapping of rotated shapes
# against reference shapes. For n-order #' radia symmetry, it
# averages the overlap (normalized by the area of the orginal
# shapes) of #' 2*pi / (n-1) radians rotated shapes. See
# references below for a detailed explanation.  #' @param coo
# a matrix of a shape #' @param order.max the n maximal order
# (estimates will be calculated for the 2:order.max range) #'
# @return a list with $ov (the mean overlapping index), $sd
# the standard deviation, and $sym the #' n orders of radial
# symmetry #' @references Rosin, P. L. (2005). Computing
# global shape measures. In C. H. Chen and P. S. P. Wang
# (Eds.), #' Handbook of Pattern Recognition and Computer
# Vision (pp. 177-196).  #' @examples #' data(bot) #' x <-
# coo_symmetry(bot[1]) #' w <- barplot(x$ov) #' axis(1, at=w,
# labels=x$sym) #' segments(w, x$ov - x$sd, w, x$ov + x$sd)
# #' @export coo_symmetry <- function(coo, order.max=12){ coo
# <- coo_check(coo) coo <- coo_center(coo) coo_a <-
# coo_area(coo) sym <- 2:order.max sd <- ov <-
# numeric(length(sym)) #theta <- seq(0, 2*pi,
# length=n+1)[-n+1] for (i in seq(along=sym)){ n <- sym[i]
# ov.i <- numeric(n-1) for (j in 1:(n-1)) { ov.i[j] <-
# coo_overlap(coo, coo_rotate(coo, j*2*pi/n)) } ov.i <-
# ov.i/coo_a ov[i] <- mean(ov.i) sd[i] <- sd(ov.i) } sd[1] <-
# 0 return(list(ov=ov, sd=sd, sym=sym))}
