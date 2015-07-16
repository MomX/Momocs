##### Come various coo utility a family of functions that do
##### simple functions on 2d coordinates (further abbreviated as
##### 'shape' (either outlines, open outlines or lanfmarks) they
##### can be passed either as two-column matrices colnames ('x'
##### and 'y' colnaming is not mandatory) or as a list with $x
##### and $y components.  and returns a (named) \code{matrix} of
##### coordinates.  Some of them, likely the most used, have also
##### Coo methods.

# deprecate?
#' Checks 'coo' shapes
#'
#' A simple utility, used internally, mostly in the coo functions and methods.
#' Returns a matrix of coordinates, when passed with either a list or a \code{matrix} of coordinates.
#'
#' @param coo a \code{matrix} of (x; y) coordinates or a list.
#' @return a \code{matrix} of (x; y) coordinates.
#' @seealso \link{ldk_check}
#' @keywords ShapeUtilities
#' @examples
#' #coo_check('Not a shape')
#' #coo_check(matrix(1:10, ncol=2))
#' #coo_check(list(x=1:5, y=6:10))
#' @export
coo_check <- function(coo) {
  if (is.matrix(coo)) {
    return(coo)
  }
  if (is.data.frame(coo)){
    return(as.matrix(coo))
  }
  if (is.list(coo)) {
    if (length(coo) == 1)
      return(l2m(coo))
  }
  stop(" * a list or a matrix of (x; y) coordinates must be provided.")
}

#' Centers coordinates
#'
#' Returns a shape centered on the origin.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' @aliases coo_centre
#' @export
coo_center <- function(coo) {
  UseMethod("coo_center")
}
#' @export
coo_center.default <- function(coo) {
  coo <- coo_check(coo)
  # equivalent but slower: apply(coo, 2, function(x) x -
  # mean(x))
  return(scale(coo, scale = FALSE))
}
#' @export
coo_center.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_center)
  return(Coo)
}

#' brittons...
#' @rdname coo_center
#' @export
coo_centre <- coo_center

#' Scales coordinates
#'
#' Scales the coordinates by a 'scale' factor. If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#'
#' @aliases coo_scale
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param scale the scaling factor, by default, the centroid size.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_scale(bot))
#' coo_plot(b)
#' coo_plot(coo_scale(b))
#' @export
coo_scale <- function(coo, scale) {
  UseMethod("coo_scale")
}
#' @export
coo_scale.default <- function(coo, scale = coo_centsize(coo)) {
  coo <- coo_check(coo)
  cp <- coo_centpos(coo)
  coo <- coo_trans(coo_trans(coo, -cp[1], -cp[2])/scale, cp[1],
                   cp[2])
  return(coo)
}
#' @export
coo_scale.Coo <- function(coo, scale) {
  Coo <- coo
  # dirty loop but had bad time trying to vectorize it
  if (missing(scale)) {
    scale <- sapply(Coo$coo, coo_centsize)
  }
  if (length(scale) != length(Coo)) {
    scale <- rep(scale, length(Coo))
  }
  for (i in seq(along = Coo$coo)) {
    Coo$coo[[i]] <- coo_scale(Coo$coo[[i]], scale[i])
  }
  return(Coo)
}

#' Scales coordinates in one direction
#'
#' \code{coo_scalex} applies a scaling a matrix of (x; y) (or a list),
#' parallel to the x-axis,
#' \code{coo_scaley} does it parallel to the y-axis.
#' @rdname coo_scalexy
#' @param coo a matrix or a list of coordinates
#' @param k scaling factor
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(shapes)
#' coo <- coo_template(shapes[11])
#' coo_plot(coo, xlim=c(-1, 1))
#' coo_draw(coo_scalex(coo, 0.5), border="blue")
#' coo_draw(coo_scalex(coo, 1.2), border="blue", lty=2)
#' coo_draw(coo_scaley(coo, 0.5), border="red")
#' coo_draw(coo_scaley(coo, 1.2), border="red", lty=2)
#' @export
coo_scalex <- function(coo, k=1){
  smat <- matrix(c(k, 0, 0, 1), nrow=2)
  return(coo %*% smat)}
#' @rdname coo_scalexy
#' @export
coo_scaley <- function(coo, k=1){
  smat <- matrix(c(1, 0, 0, k), nrow=2)
  return(coo %*% smat)}

#' Rotates coordinates
#'
#' Rotates the coordinates by a 'theta' angle (in radians) If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pusing back to the original position.
#'
#' @aliases coo_rotate
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param theta \code{numeric}the angle (in radians) to rotate shapes.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_rotatecenter}
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_rotate(bot, pi))
#' coo_plot(b)
#' coo_plot(coo_rotate(b, pi))
#' @export
coo_rotate <- function(coo, theta = 0) {
  UseMethod("coo_rotate")
}
#' @export
coo_rotate.default <- function(coo, theta = 0) {
  coo <- coo_check(coo)
  rmat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                 nrow = 2)
  return(coo %*% rmat)
}
#' @export
coo_rotate.Coo <- function(coo, theta = 0) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_rotate, theta)
  return(Coo)
}

#' Aligns coordinates
#'
#' Aligns the coordinates along their longer axis using var-cov matrix and eigen values.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_aligncalliper}, \link{coo_alignxax}
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_align(bot))
#' coo_plot(b)
#' coo_plot(coo_align(b))
#' @export
coo_align <- function(coo) {
  UseMethod("coo_align")
}
#' @export
coo_align.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo %*% svd(var(coo))$u)
}
#' @export
coo_align.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_align)
  return(Coo)
}

#' Translates coordinates
#'
#' Translates the coordinatesby a 'x' and 'y' value
#'
#' @aliases coo_trans
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param x \code{numeric}translation along the x-axis.
#' @param y \code{numeric}translation along the y-axis.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_trans(bot, 50, 100))
#' coo_plot(b)
#' coo_plot(coo_trans(b, 50, 100))
#' @export
coo_trans <- function(coo, x = 0, y = 0) {
  UseMethod("coo_trans")
}
#' @export
coo_trans.default <- function(coo, x = 0, y = 0) {
  coo <- coo_check(coo)
  cbind(coo[, 1] + x, coo[, 2] + y)
}
#' @export
coo_trans.Coo <- function(coo, x = 0, y = 0) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_trans, x, y)
  return(Coo)
}

#' Slices shapes between successive coordinates
#' 
#' Takes a shape with n coordinates. When you pass this function with at least
#' two ids (<= n), the shape will be open on the corresponding coordinates and
#' slices returned as a list
#' @param coo a \code{matrix} of (x; y) coordinates
#' @param ids numeric of length >= 2, where to slice the shape
#' @examples 
#' data(hearts)
#' sh <- coo_slice(hearts[1], c(12, 24, 36, 48))
#' panel(Opn(sh))
#' @export
coo_slice <- function(coo, ids){
  n <- length(ids)
  if (n<=1)
    stop(" * 'ids' must contain at least 2 ids")
  res <- list()
  ids <- sort(ids)
  if (max(ids) > nrow(coo))
    stop(" * max(ids) must be lower than the number of coordinates")
  
  for (i in 1:(n-1)) {
    res[[i]] <- coo[ids[i]:ids[i+1], ]
  }
  res[[n]] <- coo[ c(ids[n]:nrow(coo), 1:ids[1]) , ]
  names(res) <- 1:length(ids)
  res
}


#' Slides coordinates
#'
#' Slides the coordinates so that the id1-th point become the first one.
#' @aliases coo_slide
#' @param x either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param id1 the id(s) of the point that will become the new first point. See details below
#' for the method on Coo objects.
#' @param ldk the id of the ldk to use as id1.
#' @param ... useless, only to maintain the generic
#' @details For Coo objects, and in particular for Out and Opn three different ways of coo_sliding
#' are available:
#' \itemize{
#' \item no ldk passed and a single id1 is passed: all id1-th points
#' within the shapes will become the first points. $ldk will be slided accordingly.
#' \item no ldk passed and a vector of ids matching the length of the Coo: for every shape,
#' the id1-th point will be used as the id1-th point. $ldk will be slided accordingly.
#' \item a single ldk is passed: the ldk-th ldk will be used to slide every shape. If an ldk is passed,
#' id1 is ignored with a message.
#' }
#' See examples to make things clear. These methods have not been exhaustively tested, so triple
#' check and do not hesitate to contact me if you detect a problem.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' # set the first landmark as the starting point
#' stack(coo_slide(hearts, ldk=1))
#' # set the 50th point as the starting point (everywhere)
#' stack(coo_slide(hearts, id1=50))
#' # set the id1-random-th point as the starting point (everywhere)
#' set.seed(123) # just for the reproducibility
#' id1_random <- sample(x=min(sapply(hearts$coo, nrow)), size=length(hearts),
#' replace=TRUE)
#' stack(coo_slide(hearts, id1=id1_random))
#' @rdname coo_slide
#' @export
coo_slide <- function(x, ...) {
  UseMethod("coo_slide")
}
#' @rdname coo_slide
#' @export
coo_slide.default <- function(x, id1, ...) {
  coo <- coo_check(x)
  if (id1 == 0) {
    return(coo)
  }
  n <- nrow(coo)
  slided.rows <- c(id1:n, 1:(id1 - 1))
  return(coo[slided.rows, ])
}
#' @rdname coo_slide
#' @export
coo_slide.Coo <- function(x, id1, ldk, ...) {
  Coo <- x
  ##### ldk case #####
  if (!missing(ldk)) {
    if (length(Coo$ldk) == 0) stop(" * No landmarks defined.")
    if (!missing(id1))        warning(" * id1 provided will be ignored.")
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_slide(Coo$coo[[i]], Coo$ldk[[i]][ldk])
      Coo$ldk[[i]] <- (Coo$ldk[[i]] - (Coo$ldk[[i]][ldk] - 1)) %% nrow(Coo$coo[[i]])
    }
    return(Coo)
  } else {
    ##### id1 case ######
    if (length(id1)==1) id1 <- rep(id1, length(Coo))

    # id1 case
    # id1=1 just rep
    #
    # allows a vector of id1s to be passed
    slide_ldk <- (length(Coo$ldk) > 0)
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_slide(Coo$coo[[i]], id1[i])
      if (slide_ldk)
        Coo$ldk[[i]] <- (Coo$ldk[[i]] - id1[i] - 1) %% nrow(Coo$coo[[i]])
    }
    return(Coo)
  }}

#' Slides coordinates in a particular direction
#'
#' Shapes are centered and then, according to direction, the point northwards, southwards,
#' eastwards or westwards the centroid, becomes the first point with \link{coo_slide}.
#' @param coo a matrix (or a list) or (x; y) coordinates or a Coo object
#' @param direction a character among \code{'N'} (by default), \code{'S'}, \code{'E'}, or \code{'W'}.
#' @param center logical whether to center or not before sliding
#' @param id whether to return the id of the point or the slided shapes
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- coo_rotate(bot[1], pi/6) # dummy example just to make it obvious
#' coo_plot(b) # not the first point
#' coo_plot(coo_slidedirection(b, 'N'))
#' coo_plot(coo_slidedirection(b, 'E'))
#' coo_plot(coo_slidedirection(b, 'W'))
#' coo_plot(coo_slidedirection(b, 'S'))
#'
#' # on Coo objects
#' stack(bot)
#' stack(coo_slidedirection(bot, 'E'))
#' @export
coo_slidedirection <- function(coo, direction, center, id) {
  UseMethod("coo_slidedirection")
}
#' @export
coo_slidedirection.default <- function(coo, direction = "N",
                                       center=TRUE, id = FALSE) {
  coo <- coo_check(coo)
  if (center) coo <- coo_center(coo)
  if (direction == "N") {
    x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
    id0 <- x0.ed[which(coo[x0.ed, 2] > 0)[1]]
  }
  if (direction == "S") {
    x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
    id0 <- x0.ed[which(coo[x0.ed, 2] < 0)[1]]
  }
  if (direction == "E") {
    y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
    id0 <- y0.ed[which(coo[y0.ed, 1] > 0)[1]]
  }
  if (direction == "W") {
    y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
    id0 <- y0.ed[which(coo[y0.ed, 1] < 0)[1]]
  }

  if (id) {
    return(id0) }
  else {
    coo <- coo_slide(coo, id0)
    return(coo) }
}

#' @export
coo_slidedirection.Coo <- function(coo, direction, center = TRUE, id = TRUE) {
  Coo <- coo
  id0 <- sapply(Coo$coo, coo_slidedirection, direction, center, id=TRUE)
  Coo <- coo_slide(Coo, id1 = id0)
  return(Coo)
}

#' Slides coordinates using the widest gap
#'
#' When slicing a shape using two landmarks, or functions such as \link{coo_up},
#' an open curve is obtained and the rank of points make wrong/artefactual results.
#' This functions uses the widest gap between points and use the latter as starting
#' and ending points. Examples are self-speaking.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates or a \link{Coo} object
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(shapes)
#' cat <- coo_center(shapes[4])
#' coo_plot(cat)
#'
#' # we only retain the bottom of the cat
#' cat_down <- coo_down(cat, slidegap=FALSE)
#'
#' # see? the segment on the x-axis coorespond to the widest gap.
#' coo_plot(cat_down)
#'
#' # that's what we meant
#' coo_plot(coo_slidegap(cat_down))
#' @export
coo_slidegap <- function(coo){
  UseMethod("coo_slidegap")
}

#' @export
coo_slidegap.default <- function(coo){
  widest_gap <- which.max(coo_perimpts(coo))
  return(coo_slide(coo, widest_gap+1))
}

#' @export
coo_slidegap.Coo <- function(coo){
  coo$coo <- lapply(coo$coo, coo_slidegap)
  coo
}


#' Sample coordinates (among points)
#'
#' Sample n coordinates among existing points.
#'
#' For the \link{Out} an \link{Opn}
#' methods (pointless for \link{Ldk}), in an \code{$ldk} component is defined,
#' it is changed accordingly by multiplying the ids by  n over the number of coordinates.
#'
#' @aliases coo_sample
#' @param coo either a \code{matrix} of (x; y) coordinates,
#' or a \link{Out} or \link{Opn}. object.
#' @param n an integer, the number fo points to sample.
#' @return a \code{matrix} of (x; y) coordinates,
#' or a \link{Out} or \link{Opn} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_sample(bot, 24))
#' coo_plot(b)
#' coo_plot(coo_sample(b, 24))
#' @export
coo_sample <- function(coo, n) {
  UseMethod("coo_sample")
}
#' @export
coo_sample.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (nrow(coo) < n) stop(" * less coordinates than n. Try coo_interpolate.")
  sampled <- round(seq(1, nrow(coo), len = n + 1)[-(n + 1)])
  return(coo[sampled, ])
}
#' @export
coo_sample.Out <- function(coo, n) {
  Out <- coo
  # if an $ldk is present, we have to change it.
  if (length(Out$ldk)!=0) {
    coo_nb <- sapply(Out$coo, nrow)
    for (i in 1:length(Out)){
      ratio.i <- n / coo_nb[i]
      Out$ldk[[i]] <- ceiling(Out$ldk[[i]] * ratio.i)
    }
    cat(" * $ldk has been changed accordingly.\n")
  }

  Out$coo <- lapply(Out$coo, coo_sample, n)

  return(Out)
}

#' @export
coo_sample.Opn <- coo_sample.Out

#' Samples coordinates (regular radius)
#'
#' Samples n coordinates with a regular angle.
#'
#' @aliases coo_samplerr
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param n integer, the number of points to sample.
#' @return a \code{matrix} of (x; y) coordinates or an Coo object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' stack(bot)
#' bot <- coo_center(bot)
#' stack(coo_samplerr(bot, 12))
#' coo_plot(bot[1])
#' coo_plot(rr <- coo_samplerr(bot[1], 12))
#' cpos <- coo_centpos(bot[1])
#' segments(cpos[1], cpos[2], rr[, 1], rr[, 2])
#' @export
coo_samplerr <- function(coo, n) {
  UseMethod("coo_samplerr")
}
#' @export
coo_samplerr.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (nrow(coo) < n) stop(" * less coordinates than n. Try coo_interpolate.")
  Rx <- coo[, 1]
  Ry <- coo[, 2]
  le <- length(Rx)
  M <- matrix(c(Rx, Ry), le, 2)
  M1 <- matrix(c(Rx - mean(Rx), Ry - mean(Ry)), le, 2)
  V1 <- complex(real = M1[, 1], imaginary = M1[, 2])
  M2 <- matrix(c(Arg(V1), Mod(V1)), le, 2)
  V2 <- NA
  for (i in 0:(n - 1)) {
    V2[i + 1] <- which.max((cos(M2[, 1] - 2 * i * pi/n)))
  }
  V2 <- sort(V2)
  return(M1[V2, ])
}
#' @export
coo_samplerr.Coo <- function(coo, n) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_samplerr, n)
  return(Coo)
}

#' Interpolates coordinates
#'
#' Interpolates n coordinates 'among existing points'between' existing points,
#' along the perimeter of the coordinates provided and keeping the first point
#'
#' @aliases coo_interpolate
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param n an integer, the number fo points to interpolate.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_scale(bot))
#' coo_plot(b)
#' coo_plot(coo_scale(b))
#' data(bot)
#' stack(bot)
#' stack(coo_interpolate(coo_sample(bot, 12), 120))
#' coo_plot(bot[1])
#' coo_plot(coo_interpolate(coo_sample(bot[1], 12), 120))
#' @export
coo_interpolate <- function(coo, n) {
  UseMethod("coo_interpolate")
}
#' @export
coo_interpolate.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (!is_closed(coo)) {
    coo <- coo_close(coo)
  }
  orig <- coo_perimcum(coo)
  targ <- seq(0, coo_perim(coo), length = n + 1)[-(n + 1)]
  coo2 <- matrix(c(coo[1, ], rep(NA, n * 2 - 2)), byrow = TRUE,
                 nrow = n, ncol = 2)
  for (i in 2:n) {
    k <- max(which(orig <= targ[i]))
    r <- (targ[i] - orig[k])/(orig[k + 1] - orig[k])
    coo2[i, ] <- edi(coo[k, ], coo[k + 1, ], r)
  }
  return(coo2)
}
#' @export
coo_interpolate.Coo <- function(coo, n) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_interpolate, n)
  return(Coo)
}

#' Smoothes coordinates (closed outlines)
#'
#' Smoothes coordinates using a simple moving average.
#' May be useful to remove digitization noise.
#' @aliases coo_smooth
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param n an (integer) \code{numeric} to specify the number of smoothing iterations
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_smoothcurve}
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' stack(bot)
#' stack(coo_smooth(bot, 10))
#' coo_plot(bot[1])
#' coo_plot(coo_smooth(bot[1], 30))
#' @export
coo_smooth <- function(coo, n) {
  UseMethod("coo_smooth")
}
#' @export
coo_smooth.default <- function(coo, n = 0) {
  coo <- coo_check(coo)
  p <- nrow(coo)
  a <- 0
  while (a < n) {
    a <- a + 1
    coo_i <- rbind(coo[-1, ], coo[1, ])
    coo_s <- rbind(coo[p, ], coo[-p, ])
    coo <- coo/2 + coo_i/4 + coo_s/4
  }
  return(coo)
}
#' @export
coo_smooth.Coo <- function(coo, n) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_smooth, n)
  return(Coo)
}

#' Smoothes coordinates (open outlines)
#'
#' Smoothes coordinates using a simple moving average but let the first and last points unchanged.
#' May be useful to remove digitization noise.
#' @aliases coo_smoothcurve
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param n an (integer) \code{numeric} to specify the number of smoothing iterations
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_smooth}
#' @keywords ShapeUtilities
#' @examples
#' data(olea)
#' o <- olea[1]
#' coo_plot(o, border='grey50', points=FALSE)
#' coo_draw(coo_smooth(o, 24), border='blue', points=FALSE)
#' coo_draw(coo_smoothcurve(o, 24), border='red', points=FALSE)
#' @export
coo_smoothcurve <- function(coo, n) {
  UseMethod("coo_smoothcurve")
}
#' @export
coo_smoothcurve.default <- function(coo, n = 0) {
  coo <- coo_check(coo)
  p <- nrow(coo)
  a <- 0
  while (a < n) {
    a <- a + 1
    for (i in 2:(p - 1)) {
      coo[i, ] <- (coo[i - 1, ] * 0.25 + coo[i, ] * 0.5 +
                     coo[i + 1, ] * 0.25)
    }
  }
  return(coo)
}
#' @export
coo_smoothcurve.Opn <- function(coo, n) {
  Opn <- coo
  Opn$coo <- lapply(Opn$coo, coo_smoothcurve, n)
  return(Opn)
}

#' Tests if shapes are closed
#'
#' Returns TRUE/FALSE whether the last coordinate of the shapes is the same
#' as the first one.
#'
#' @aliases is_closed
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a boolean.
#' @seealso \link{coo_close}, \link{coo_unclose}
#' @keywords ShapeUtilities
#' @examples
#' is_closed(matrix(1:10, ncol=2))
#' is_closed(coo_close(matrix(1:10, ncol=2)))
#' data(bot)
#' is_closed(bot)
#' is_closed(coo_close(bot))
#' @export
is_closed <- function(coo) {
  UseMethod("is_closed")
}
#' @export
is_closed.default <- function(coo) {
  coo <- coo_check(coo)
  identical(coo[1, ], coo[nrow(coo), ])
}
#' @export
is_closed.Coo <- function(coo) {
  Coo <- coo
  return(sapply(Coo$coo, is_closed))
}

# # is.likelyopen tries to estimate is a matrix of
# coordinates is likely to be a # closed polygon
# is.likelyclosedpolygon <- function(coo) { x <-
# coo_perimpts(coo) d <- max(x) / median(x[-which.max(x)])
# ifelse(d > 3, TRUE, FALSE)}

#' Closes/'Uncloses' shapes
#'
#' Returns a closed shape from (un)closed shapes. See also \link{coo_unclose}.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_unclose}, \link{is_closed}
#' @keywords ShapeUtilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo_close(x)
#' x3 <- coo_unclose(x2)
#' x
#' is_closed(x)
#' x2
#' is_closed(x2)
#' x3
#' is_closed(x3)
#' @export
coo_close <- function(coo) {
  UseMethod("coo_close")
}
#' @export
coo_close.default <- function(coo) {
  coo <- coo_check(coo)
  ifelse(is_closed(coo), return(coo), return(rbind(coo, coo[1,
                                                            ])))
}
#' @export
coo_close.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_close)
  return(Coo)
}

#' 'Uncloses' shapes
#'
#' Returns a unclosed shape from (un)closed shapes. See also \link{coo_close}.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo}
#'   object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_close}, \link{is_closed}
#' @keywords ShapeUtilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo_close(x)
#' x3 <- coo_unclose(x2)
#' x
#' is_closed(x)
#' x2
#' is_closed(x2)
#' x3
#' is_closed(x3)
#' @export
coo_unclose <- function(coo) {
  UseMethod("coo_unclose")
}
#' @export
coo_unclose.default <- function(coo) {
  coo <- coo_check(coo)
  ifelse(is_closed(coo), return(coo[-nrow(coo), ]), return(coo))
}
#' @export
coo_unclose.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_unclose)
  return(Coo)
}

# Some utilities documented yet less likely to be used. They
# may be useful for some testing, developing new methods, or
# on monday mornings.

#' Rotates shapes with a custom center
#'
#' rotates a shape of 'theta' angles (in radians) and with a (x; y) 'center'.
#' @aliases coo_rotatecenter
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param theta \code{numeric} the angle (in radians) to rotate shapes.
#' @param center the (x; y) position of the center
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_rotate}
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo_plot(b)
#' coo_draw(coo_rotatecenter(b, -pi/2, c(200, 200)), border='red')
#' @export
coo_rotatecenter <- function(coo, theta, center = c(0, 0)) {
  UseMethod("coo_rotatecenter")
}
#' @export
coo_rotatecenter.default <- function(coo, theta, center = c(0,
                                                            0)) {
  coo <- coo_trans(coo, -center[1], -center[2])
  coo <- coo_rotate(coo, theta)
  return(coo_trans(coo, center[1], center[2]))
}
#' @export
coo_rotatecenter.Coo <- function(coo, theta, center = c(0, 0)) {
  Coo <- coo
  for (i in seq(along = Coo$coo)) {
    Coo$coo[[i]] <- coo_rotatecenter(Coo$coo[[i]], theta,
                                     center)
  }
  return(Coo)
}

#' Forces shapes to close
#'
#' An exotic function that distribute the distance between the first and the last points
#' of unclosed shapes, so that they become closed. May be useful (?) e.g. for t/rfourier methods
#' where reconstructed shapes may not be closed.
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 64)
#' b <- b[1:40,]
#' coo_plot(b)
#' coo_draw(coo_force2close(b), border='red')
#' @export
coo_force2close <- function(coo) {
  coo <- coo_check(coo)
  xy <- coo_centpos(coo)
  if (is_closed(coo)) {
    return(coo)
  }
  n <- nrow(coo)
  d <- coo[1, ] - coo[n, ]
  dm <- cbind(seq(0, d[1], length = n), seq(0, d[2], length = n))
  coo2 <- coo + dm
  xy2 <- coo_centpos(coo2)
  coo2 <- coo_trans(coo2, xy[1] - xy2[1], xy[2] - xy2[2])
  return(coo2)
}


#' Shears shapes
#'
#' \code{coo_shearx} applies a shear mapping on a matrix of (x; y) coordinates (or a list), parallel
#' to the x-axis (i.e. x' = x + ky; y' = y + kx). \code{coo_sheary} does it parallel to the y-axis.
#' @rdname coo_shear
#' @param coo a matrix or a list of coordinates
#' @param k shear factor
#' @return a \code{matrix} of (x; y) coordinates.
#' @keywords ShapeUtilities
#' @examples
#' data(shapes)
#' coo <- coo_template(shapes[11])
#' coo_plot(coo)
#' coo_draw(coo_shearx(coo, 0.5), border="blue")
#' coo_draw(coo_sheary(coo, 0.5), border="red")
#' 
#' @export
coo_shearx <- function(coo, k=1){
  smat <- matrix(c(1, 0, k, 1), nrow=2)
  return(coo %*% smat)}
#' @rdname coo_shear
#' @export
coo_sheary <- function(coo, k){
  smat <- matrix(c(1, k, 0, 1), nrow=2)
  return(coo %*% smat)}

#' Flips shapes
#'
#' \code{coo_flipx} flips shapes about the x-axis; \code{coo_flipy} about the y-axis.
#' @rdname coo_flip
#' @param coo a matrix or a list of coordinates
#' @return a \code{matrix} of (x; y) coordinates
#' @keywords ShapeUtilities
#' @examples
#' data(shapes)
#' cat <- shapes[4]
#' cat <- coo_center(cat)
#' coo_plot(cat)
#' coo_draw(coo_flipx(cat), border="red")
#' coo_draw(coo_flipy(cat), border="blue")
#' 
#' #' # to flip an entire Coo:
#' shapes2 <- shapes
#' shapes$coo <- lapply(shapes2$coo, coo_flipx)
#' @export
#' @rdname coo_flip
#' @export
coo_flipx <- function(coo){
  m <- matrix(c(1, 0, 0, -1), nrow = 2)
  return(coo %*% m)
}
#' @rdname coo_flip
#' @export
coo_flipy <- function(coo){
  m <- matrix(c(-1, 0, 0, 1), nrow = 2)
  return(coo %*% m)
}

#' Calculate abscissa and ordinate on a shape
#'
#' A simple wrapper to calculate dxi - dx1 and dyi - dx1.
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a list with two components \code{dx} and \code{dy}
#' @seealso \link{coo_oscillo}
#' @examples
#' data(bot)
#' coo_dxy(bot[1])
#' @export
coo_dxy <- function(coo) {
  coo <- coo_check(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]
  return(list(dx = dx, dy = dy))
}

# 2. Handling / baselines on coo and Coo
# ------------------------------------- Some functions and
# methods to ease alignments, grabbing part of shapes, etc.
#' Retains coordinates with positive y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the upper side.
#' @param coo either a \code{matrix} of (x; y) coordinates or a \link{Coo} object
#' @param slidegap logical whether to apply \link{coo_slidegap} after coo_down
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the x-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_up or coo_down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- coo_alignxax(bot[1])
#' coo_plot(b)
#' coo_draw(coo_up(b), border='red')
#' @export
coo_up <- function(coo, slidegap=TRUE){
  UseMethod("coo_up")
}

#' @export
coo_up.default <- function(coo, slidegap=TRUE) {
  up <- coo[coo[, 2] >= 0, ]
  if (slidegap) { up <- coo_slidegap(up)}
  return(up)
}

#' @export
coo_up.Out <- function(coo, slidegap=TRUE){
  coo$coo <- lapply(coo$coo, coo_up)
  if (slidegap) coo <- coo_slidegap(coo)
  Opn(coo)
}

#' @export
coo_up.Coo <- function(coo, slidegap=TRUE){
  coo$coo <- lapply(coo$coo, coo_up)
  if (slidegap) coo <- coo_slidegap(coo)
  coo
}


#' Retains coordinates with negative y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the lower side.
#' @param coo either a \code{matrix} of (x; y) coordinates or a \link{Coo} object
#' @param slidegap logical whether to apply \link{coo_slidegap} after coo_down
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the x-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_up or coo_down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- coo_alignxax(bot[1])
#' coo_plot(b)
#' coo_draw(coo_down(b), border='red')
#' @export
coo_down <- function(coo, slidegap=TRUE){
  UseMethod("coo_down")
}

#' @export
coo_down.default <- function(coo, slidegap=TRUE) {
  down <- coo[coo[, 2] <= 0, ]
  if (slidegap) down <- coo_slidegap(down)
  return(down)
}

#' @export
coo_down.Out <- function(coo, slidegap=TRUE){
  coo$coo <- lapply(coo$coo, coo_down, slidegap=slidegap)
  Opn(coo)
}

#' @export
coo_down.Coo <- function(coo, slidegap=TRUE){
  coo$coo <- lapply(coo$coo, coo_down, slidegap=slidegap)
  coo
}


#' Aligns shapes along the x-axis
#'
#' Align the longest axis of a shape along the x-axis.
#' @aliases coo_alignxax
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @details If some shapes are upside-down
#' (or mirror of each others), try redefining a new starting point (eg with coo_slidedirection) before
#' the alignment step. This may solve your problem because coo_calliper orders the \code{$arr.ind} used by
#' coo_aligncalliper.
#' @seealso \link{coo_align}, \link{coo_aligncalliper}
#' @keywords ShapeUtilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo_plot(b)
#' coo_plot(coo_alignxax(b))
#' }
#' @export
coo_alignxax <- function(coo) {
  UseMethod("coo_alignxax")
}
#' @export
coo_alignxax.default <- function(coo) {
  coo <- coo_check(coo)
  coo <- coo_align(coo)
  return(coo_trans(coo, x = 0, y = -coo_centpos(coo)[2]))
}
#' @export
coo_alignxax.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_alignxax)
  return(Coo)
}

#' Aligns shapes along their 'calliper length'
#'
#' And returns them registered on bookstein coordinates.
#' See \link{coo_bookstein}.
#' @aliases coo_aligncalliper
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_align}, \link{coo_alignxax}, \link{coo_calliper}
#' @keywords ShapeUtilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo_plot(b)
#' coo_plot(coo_aligncalliper(b))
#' bot.al <- coo_aligncalliper(bot)
#' stack(bot.al)
#' }
#' @export
coo_aligncalliper <- function(coo) {
  UseMethod("coo_aligncalliper")
}
#' @export
coo_aligncalliper.default <- function(coo) {
  coo <- coo_check(coo)
  cal.ind <- coo_calliper(coo, arr.ind = TRUE)$arr.ind
  coo <- coo_bookstein(coo, cal.ind[1], cal.ind[2])
  return(coo)
}
#' @export
coo_aligncalliper.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_aligncalliper)
  return(Coo)
}

#' Aligns shapes using their shortest radius
#'
#' And returns them slided with the first coordinate on the east.
#' May be used as an aligning strategy on shapes with a clear 'invaginate' part.
#' @aliases coo_alignminradius
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_align}, \link{coo_alignxax}, \link{coo_calliper}
#' @keywords ShapeUtilities
#' @examples 
#' \dontrun{
#' data(hearts)
#' stack(coo_alignminradius(hearts))
#' }
coo_alignminradius <- function(coo){
  UseMethod("coo_alignminradius")
}
#' @export
coo_alignminradius.default <- function(coo){
  id_minrad <- which.min(coo_centdist(coo))
  coo <- coo_slide(coo, id_minrad)
  m <- matrix(c(coo[1, ],  0, 0, 1, 0), nrow=3, byrow=TRUE)
  th <- coo_theta3(m)
  coo_rotate(coo, -th)
}
#' @export
coo_alignminradius.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_alignminradius)
  return(Coo)
}

#' Reverses coordinates
#'
#' Returns the reverse suite of coordinates, i.e. change shape's orientation
#' @param coo either a \code{matrix} of (x; y) coordinates.
#' @return a \code{matrix} of (x; y) coordinates.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 4)
#' b
#' coo_rev(b)
#' @export
coo_rev <- function(coo) {
  coo <- coo_check(coo)
  return(coo[nrow(coo):1, ])
}

#' Defines interactively landmarks
#' Allows to interactively define a 'nb.ldk' number of landarks on a shape.
#' Used in other facilities to acquire/manipulate data.
#' @aliases coo_ldk
#' @param coo a \code{matrix} or a list of (x; y) coordinates.
#' @param nb.ldk integer, the number of landmarks to define
#' @return \code{numeric} that corresponds to the closest ids,
#' on the shape, from cliked points.
#' @keywords ShapeUtilities
#' @examples
#' \dontrun{
#' data(bot)
#' b <- bot[1]
#' coo_ldk(b, 3) # run this, and click 3 times
#' coo_ldk(bot, 2) # this also works on Out
#' }
#' @export
coo_ldk <- function(coo, nb.ldk) {
  if (is.list(coo))
    coo <- l2m(coo)
  coo_plot(coo)
  ldk <- numeric(nb.ldk)
  cat("[")
  for (i in 1:nb.ldk) {
    p <- l2m(locator(1))
    l <- apply(coo, 1, function(y) sqrt(sum((p - y)^2)))
    ldk[i] <- which.min(l)
    points(coo[ldk[i], 1], coo[ldk[i], 2], pch = 20, col = "red",
           cex = 0.5)
    cat("*")
  }
  cat("]\n")
  return(ldk)
}

#' Register Bookstein's coordinates
#'
#' Registers a new baseline for the shape, with the \code{ldk1}-th
#' and \code{ldk2}-th points being set on \eqn{(x= -0.5; y=0)} and \eqn{(x= 0.5; y=0)}, respectively.
#'
#' For \link{Out}, it tries to do it using \code{$ldk} slot. Also the case for \link{Opn}, but if
#' no landmark is defined, it will do it on the first and the last point of the shape.
#'
#' For \code{Out} and \code{Opn} defines the first landmark as the first point of the
#' new shapes with \link{coo_slide}.
#' @aliases coo_bookstein
#' @param coo either a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param ldk1 the id of the first point of the new baseline (the first, by default)
#' @param ldk2 the id of the second point of the new baseline (the last, by default)
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_baseline}
#' @keywords ShapeUtilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' stack(coo_bookstein(hearts, 2, 4))
#' h <- hearts[1]
#' coo_plot(h)
#' coo_plot(coo_bookstein(h, 20, 57), border='red')
#' @export
coo_bookstein <- function(coo, ldk1, ldk2) {
  UseMethod("coo_bookstein")
}
#' @export
coo_bookstein.default <- function(coo, ldk1 = 1, ldk2 = nrow(coo)) {
  D <- ed(coo[ldk1, ], coo[ldk2, ])
  coo2 <- matrix(NA, nrow(coo), ncol(coo))
  ldk1 <- coo[ldk1, ]
  ldk2 <- coo[ldk2, ]
  coo2[, 1] <- (((ldk2[1] - ldk1[1]) * (coo[, 1] - ldk1[1]) +
                   (ldk2[2] - ldk1[2]) * (coo[, 2] - ldk1[2]))/(D^2)) - 0.5
  coo2[, 2] <- ((ldk2[1] - ldk1[1]) * (coo[, 2] - ldk1[2]) -
                  (ldk2[2] - ldk1[2]) * (coo[, 1] - ldk1[1]))/(D^2)
  return(coo2)
}
#' @export
coo_bookstein.Out <- function(coo, ldk1=1, ldk2=2) {
  # id1 ?
  Out <- coo
  for (i in seq(along = Out$coo)) {
    Out$coo[[i]] <- coo_bookstein(Out$coo[[i]], Out$ldk[[i]][ldk1], Out$ldk[[i]][ldk2])
  }
  #Out$coo <- coo_i
  return(Out)
}

#' @export
coo_bookstein.Opn <- function(coo, ldk1=1, ldk2=2) {
  # id1 ?
  Opn <- coo
  # by default, using the first and last coordinate
  if (length(Opn$ldk) == 0) {
    Opn$coo <- lapply(Opn$coo, coo_bookstein)
  } else {
    for (i in seq(along = Opn$coo)) {
      Opn$coo[[i]] <- coo_bookstein(Opn$coo[[i]], Opn$ldk[[i]][ldk1], Opn$ldk[[i]][ldk2])
    }
  }
  return(Opn)
}

#' @export
coo_bookstein.Ldk <- function(coo, ldk1, ldk2) {
  Ldk <- coo
  Ldk$coo <- lapply(Ldk$coo, coo_bookstein, ldk1 = ldk1, ldk2 = ldk2)
  return(Ldk)
}

#' Register new baselines
#'
#' A non-exact baseline registration on \code{t1} and \code{t2} coordinates,
#' for the \code{ldk1}-th and \code{ldk2}-th points.
#' By default it returns Bookstein's coordinates.
#' @aliases coo_baseline
#' @param coo a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @param ldk1 the id of the first point of the new baseline
#' @param ldk2 the id of the second point of the new baseline
#' @param t1 \code{numeric} the (x; y) coordinates of the 1st point of the new baseline
#' @param t2 \code{numeric} the (x; y) coordinates of the 2nd point of the new baseline
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object.
#' @seealso \link{coo_bookstein}
#' @keywords ShapeUtilities
#' @examples
#' data(hearts)
#' stack(hearts)
#' stack(coo_baseline(hearts, 2, 4, c(-1, 0), c(1, 1)))
#' @export
coo_baseline <- function(coo, ldk1, ldk2, t1, t2) {
  UseMethod("coo_baseline")
}
#' @export
coo_baseline.default <- function(coo, ldk1 = 1, ldk2 = 2, t1 = c(-0.5,
                                                                 0), t2 = c(0.5, 0)) {
  if (is.list(coo)) {
    coo <- l2m(coo)
  }
  t1x <- t1[1]
  t1y <- t1[2]
  t2x <- t2[1]
  t2y <- t2[2]
  r1x <- coo[ldk1, 1]
  r1y <- coo[ldk1, 2]
  r2x <- coo[ldk2, 1]
  r2y <- coo[ldk2, 2]
  # translation based on the first landmark
  ref <- coo_trans(coo, t1x - coo[ldk1, 1], t1y - coo[ldk1,
                                                      2])
  # we calculate dx and dy for the two vectors
  rx <- ref[ldk2, 1] - t1x
  ry <- ref[ldk2, 2] - t1y
  tx <- t2x - t1x
  ty <- t2y - t1y
  # returns difference angle and norm ratios between two
  # vectors given as 4 numeric.
  vi <- vecs_param(rx, ry, tx, ty)
  # we rotate accordingly with a center defined as the first
  # landmark (trans, rot, untrans)
  ref <- coo_trans(ref, -t1x, -t1y)
  ref <- ref/vi$r.norms
  ref <- coo_rotate(ref, -vi$d.angle)
  ref <- coo_trans(ref, t1x, t1y)
  return(ref)
}
#' @export
coo_baseline.Coo <- function(coo, ldk1 = 1, ldk2 = 2, t1 = c(-0.5,
                                                             0), t2 = c(0.5, 0)) {
  Coo <- coo
  for (i in seq(along = Coo$coo)) {
    Coo$coo[[i]] <- coo_baseline(Coo$coo[[i]], Coo$ldk[[i]][ldk1],
                                 Coo$ldk[[i]][ldk2], t1, t2)
  }
  return(Coo)
}

# 3. coo shape descriptors
# Mainly intended for traditional morphometrics.  Convert to
# methods ? Or an utility to get these descriptors ? #todo


# a. centroid
#' Returns the position of the centroid
#'
#' Returns the (x; y) centroid coordinates of a shape.
#' @aliases coo_centpos
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return (x; y) coordinates as \code{numeric}.
#' @keywords ShapeUtilities
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo_plot(b)
#' xy <- coo_centpos(b)
#' points(xy[1], xy[2], cex=2, col='blue')
#' xy
#' @export
coo_centpos <- function(coo) {
  UseMethod("coo_centpos")
}
#' @export
coo_centpos.default <- function(coo) {
  coo <- coo_check(coo)
  return(apply(coo, 2, mean))
}
#' @export
coo_centpos.Coo <- function(coo) {
  Coo <- coo
  centpos <- t(sapply(Coo$coo, coo_centpos))
  colnames(centpos) <- c("x", "y")  # pure cosmetics
  return(centpos)
}

#' Calculates the centroid size
#' @aliases coo_centsize
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric}, the centroid size.
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' coo_centsize(bot[1])
#' cs <- sapply(bot$coo, coo_centsize)
#' hist(cs, breaks=10)
#' @export
coo_centsize <- function(coo) {
  coo <- coo_check(coo)
  sq <- (coo - apply(coo, 2, mean))^2
  mean(sqrt(rowSums(sq)))
}

#' Returns the distance between everypoints and the centroid
#' 
#' For every point of the shape, returns the (centroid-points) distance.
#' @aliases coo_centdist
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return a \code{matrix} of (x; y) coordinates.
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 64)
#' d <- coo_centdist(b)
#' barplot(d)
#' @export
coo_centdist <- function(coo) {
  coo <- coo_check(coo)
  return(apply(coo, 1, function(x) ed(coo_centpos(coo), x)))
}

# b. length(s)

#' Calculates the chordal distance along a shape.
#'
#' Calculates the euclidean distance between every points of a shape for coo_perimpts.
#' The cumulative sum for coo_perimcum
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric} the distance between every point.
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 24)
#' coo_perimpts(b)
#' @export
coo_perimpts <- function(coo) {
  coo <- coo_check(coo)
  n <- nrow(coo)
  d <- sqrt(apply((coo - coo_slide(coo, n))^2, 1, sum))[-1]
  return(d)
}

#' Calculates the cumulative chrodal distance a shape.
#'
#' Just a wrapper for cumsum(coo_perimpts). See \link{coo_perimpts}.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric} the cumulate sum of chrodal distances
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' b <- coo_sample(bot[1], 24)
#' coo_perimcum(b)
#' @export
coo_perimcum <- function(coo) {
  coo <- coo_check(coo)
  d <- cumsum(sqrt(apply((coo - rbind(coo[1, ], coo[-(dim(coo)[1]),
                                                    ]))^2, 1, sum)))
  return(d)
}

#' Calculates the perimeter
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric}, the perimeter.
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' coo_perim(bot[1])
#' hist(sapply(bot$coo, coo_perim), breaks=10)
#' @export
coo_perim <- function(coo) {
  return(sum(coo_perimpts(coo)))
}

#' Calculates the calliper length
#'
#' Also called the Feret's diameter, the longest distance between two points of
#' the shape provided.
#' @aliases coo_calliper
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @param arr.ind a boolean, if provided returns
#' @return \code{numeric}, the centroid size. If arr.ind=TRUE, a list with the calliper length ($length)
#' and the two points ($arr.ind)
#' @keywords ShapeDescriptors
#' @examples
#' data(bot)
#' b <- bot[1]
#' coo_calliper(b)
#' p <- coo_calliper(b, arr.ind=TRUE)
#' p$length
#' ids <- p$arr.ind
#' coo_plot(b)
#' segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)
#' @export
coo_calliper <- function(coo, arr.ind = FALSE) {
  coo <- coo_check(coo)
  d <- dist(coo, method = "euclidean")
  # we check if there is no ex aequo
  ea <- length(which(d == max(d), arr.ind = TRUE))
  if (length(ea) > 1) {
    cat(" * coo_length: at least two lengths are ex aequo.")
  }
  if (arr.ind) {
    arr.ind <- which(as.matrix(d) == max(d), arr.ind = TRUE)
    # to return a vector (numeric and sorted) of the rows between
    # which the max length has been found
    arr.ind <- sort(as.numeric(arr.ind[1, ]))
    return(list(length = max(d), arr.ind = arr.ind))
  } else {
    return(max(d))
  }
}
