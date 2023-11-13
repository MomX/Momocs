##### Come various coo utility a family of functions that do

# coo_check ----------------
#' Checks shapes
#'
#' A simple utility, used internally, mostly in the coo functions and methods.
#' Returns a matrix of coordinates, when passed with either a list or a \code{matrix} of coordinates.
#'
#' @param coo `matrix` of `(x; y)` coordinates or any [Coo] object.
#' @return `matrix` of `(x; y)` coordinates or a [Coo] object.
#' @examples
#' #coo_check('Not a shape')
#' #coo_check(iris)
#' #coo_check(matrix(1:10, ncol=2))
#' #coo_check(list(x=1:5, y=6:10))
#' @export
coo_check <- function(coo){
  UseMethod("coo_check")
}

#' @export
coo_check.default <- function(coo) {
  if (is.data.frame(coo) && all(sapply(coo, class)=="numeric")){
    coo <- as.matrix(coo)
  }
  if (is.list(coo)) {
    if (length(coo) == 1)
      coo <- l2m(coo)
  }
  # checks for ncol==2 and no NAs
  if (is_shp(coo))
    coo
  else
    stop("do not know how to turn into a coo")
}

#' @export
coo_check.Coo <- function(coo){
  .which.is.error <- function(x){
  if (is.list(x))
    return(which(sapply(x, .is.error)))
  which(class(x)=="try-error")
}

  res <- sapply(coo$coo, function(x) try(coo_check(x), silent=TRUE))
  if (.is.error(res)){
    stop(paste(.which.is.error(res), collapse=", "), " do not pass coo_check")
  }
  return(coo)
}

# coo_range ----------------
#' Calculate coordinates range
#'
#' `coo_range` simply returns the range,
#' `coo_range_enlarge` enlarges it by a `k` proportion.
#' `coo_diffrange` return the amplitude (ie diff after `coo_range`)
#'
#' @inheritParams  coo_check
#' @param k `numeric` proportion by which to enlarge it
#'
#' @return a matrix of range such as `(min, max) x (x, y)`
#' @family coo_ utilities
#' @name coo_range
#' @rdname coo_range
#' @examples
#' bot[1] %>% coo_range # single shape
#' bot    %>% coo_range # Coo object
#'
#' bot[1] %>% coo_range_enlarge(1/50) # single shape
#' bot    %>% coo_range_enlarge(1/50) # Coo object
#' @export
coo_range <- function(coo){
  UseMethod("coo_range")
}

#' @rdname coo_range
#' @export
coo_range.default <- function(coo){
  res <- apply(coo, 2, range)
  dimnames(res) <- list(c("min", "max"), c("x", "y"))
  res
}

#' @rdname coo_range
#' @export
coo_range.Coo <- function(coo){
  lapply(coo$coo, coo_range) %>%
    do.call("rbind", .) %>%
    coo_range
}


#' @rdname coo_range
#' @export
coo_range_enlarge <- function(coo, k){
  UseMethod("coo_range_enlarge")
}

#' @rdname coo_range
#' @export
coo_range_enlarge.default <- function(coo, k=0){
  m <- coo_range(coo)
  g <- apply(m, 2, diff)*k
  m[1, ] <- m[1, ] - g
  m[2, ] <- m[2, ] + g
  m
}

#' @rdname coo_range
#' @export
coo_range_enlarge.Coo <- coo_range_enlarge.default

#' @rdname coo_range
#' @export
coo_range_enlarge.list <- function(coo, k=0){
  m <- lapply(coo, coo_range) %>%
    do.call("rbind", .) %>%
    coo_range
  g <- apply(m, 2, diff)*k
  m[1, ] <- m[1, ] - g
  m[2, ] <- m[2, ] + g
  m
}

#' @rdname coo_range
#' @export
coo_diffrange <- function(coo){
  UseMethod("coo_diffrange")
}

#' @rdname coo_range
#' @export
coo_diffrange.default <- function(coo){
  res <- apply(coo, 2, function(x) diff(range(x)))
  names(res) <- c("x", "y")
  res
}

#' @rdname coo_range
#' @export
coo_diffrange.Coo <- function(coo){
  lapply(x$coo, coo_diffrange) %>%
    do.call("rbind", .)
}


#' @rdname coo_range
#' @export
coo_diffrange.list <- function(coo){
  lapply(coo, coo_diffrange) %>%
    do.call("rbind", .)
}

# coo_nb ----------------
#' Counts coordinates
#'
#' Returns the number of coordinates, for a single shape or a Coo object
#' @inheritParams coo_check
#' @return either a single numeric or a vector of numeric
#' @family coo_ utilities
#' @examples
#' # single shape
#' coo_nb(bot[1])
#' # Coo object
#' coo_nb(bot)
#' @export
coo_nb <- function(coo){
  UseMethod("coo_nb")
}

#' @export
coo_nb.default <- function(coo){
  nrow(coo)
}

#' @export
coo_nb.Coo <- function(coo){
  sapply(coo$coo, nrow) %>% unlist()
}

# coo_center ----------------
#' Centers coordinates
#'
#' Returns a shape centered on the origin. The two functions are strictly equivalent.
#'
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family coo_ utilities
#' @examples
#' coo_plot(bot[1])
#' # same as
#' coo_plot(coo_centre(bot[1]))
#' # this
#' coo_plot(coo_center(bot[1]))
#'
#' # on Coo objects
#' b <- slice(bot, 1:5) # speed sake
#' stack(slice(b, 1:5))
#' stack(coo_center(b))
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

#' @rdname coo_center
#' @export
coo_centre <- coo_center

# coo_scale -----------------
#' Scales coordinates
#'
#' `coo_scale` scales the coordinates by a 'scale' factor. If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', pushing back to the original position.
#' `coo_scalex` applies a scaling (or shrinking) parallel to the x-axis,
#'  `coo_scaley` does the same for the y axis.
#'
#' @inheritParams coo_check
#' @param scale the scaling factor,
#' by default, the centroid size for `coo_scale`; 1 for `scalex` and `scaley`.
#' @return a single shape or a `Coo` object
#' @family coo_ utilities
#' @examples
#' # on a single shape
#' b <- bot[1] %>% coo_center %>% coo_scale
#' coo_plot(b, lwd=2)
#' coo_draw(coo_scalex(b, 1.5), bor="blue")
#' coo_draw(coo_scaley(b, 0.5), bor="red")
#'
#' # this also works on Coo objects:
#' b <- slice(bot, 5) # for speed sake
#' stack(b)
#' b %>% coo_center %>% coo_scale %>% stack
#' b %>% coo_center %>% coo_scaley(0.5) %>% stack
#' #equivalent to:
#' #b %>% coo_center %>% coo_scalex(2) %>% stack
#' @family scaling functions
#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scale <- function(coo, scale) {
  UseMethod("coo_scale")
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scale.default <- function(coo, scale = coo_centsize(coo)) {
  coo <- coo_check(coo)
  cp  <- coo_centpos(coo)
  coo %>% coo_center %>% `/`(scale) %>% coo_trans(cp[1], cp[2])
}

#' @rdname coo_scale
#' @name coo_scale
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

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scalex <- function(coo, scale=1){
  UseMethod("coo_scalex")
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scalex.default <- function(coo, scale=1){
  smat <- matrix(c(scale, 0, 0, 1), nrow=2)
  return(coo %*% smat)
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scalex.Coo <- function(coo, scale=1){
  coo$coo <- lapply(coo$coo, coo_scalex, scale=scale)
  coo
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scaley <- function(coo, scale=1){
  UseMethod("coo_scaley")
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scaley.default <- function(coo, scale=1){
  smat <- matrix(c(1, 0, 0, scale), nrow=2)
  return(coo %*% smat)
}

#' @rdname coo_scale
#' @name coo_scale
#' @export
coo_scaley.Coo <- function(coo, scale=1){
  coo$coo <- lapply(coo$coo, coo_scaley, scale=scale)
  coo
}

# coo_template --------------
#' 'Templates' shapes
#'
#' \code{coo_template} returns shape centered on the origin
#' and inscribed in a \code{size}-side square. `coo_template_relatively`
#' does the same but the biggest shape (as `prod(coo_diffrange)`) will
#' be of `size=size` and consequently not defined on single shapes.
#'
#' See \link{coo_listpanel} for an illustration of this function. The morphospaces
#' functions also take profit of this function. May be useful to develop other graphical functions.
#'
#' @usage coo_template(coo, size)
#' @param coo A \code{list} or a \code{matrix} of coordinates.
#' @param size \code{numeric}. Indicates the length of the side 'inscribing'
#' the shape.
#' @return Returns a matrix of \code{(x; y)}coordinates.
#' @family coo_ utilities
#' @examples
#'
#' coo <- bot[1]
#' coo_plot(coo_template(coo), xlim=c(-1, 1), ylim=c(-1, 1))
#' rect(-0.5, -0.5, 0.5, 0.5)
#'
#' s <- 0.01
#' coo_plot(coo_template(coo, s))
#' rect(-s/2, -s/2, s/2, s/2)
#' @family scaling functions
#' @rdname coo_template
#' @name coo_template
#' @export
coo_template <- function(coo, size){
  UseMethod("coo_template")
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template.default <- function(coo, size = 1) {
  # only for matrices
  coo <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift <- expected - observed
  coo <- coo_trans(coo, shift[1], shift[2])
  # if (keep.pos) {coo2 <- coo_trans(coo2, coo_centpos(coo)[1],
  # coo_centpos(coo)[2])}
  return(coo)
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template.list <- function(coo, size=1){
  lapply(coo, coo_template, size=size)
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template.Coo <- function(coo, size=1){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_template, size=size)
  return(Coo)
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template_relatively <- function(coo, size=1){
  UseMethod("coo_template_relatively")
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template_relatively.list <- function(coo, size = 1) {
  how_big <- coo_diffrange(coo) %>% apply(1, prod)
  rel_big <- (how_big/(max(how_big))) * size
  res <- lapply(seq_along(coo),
                function(i) coo_template(coo[[i]], size = rel_big[i]))
  names(res) <- names(coo)
  res
}

#' @rdname coo_template
#' @name coo_template
#' @export
coo_template_relatively.Coo <- function(coo, size = 1) {
  coo$coo %<>% coo_template_relatively(size=size)
  coo
}

# coo_rotate -----------------
#' Rotates coordinates
#'
#' Rotates the coordinates by a 'theta' angle (in radians) in
#' the trigonometric direction (anti-clockwise). If not provided,
#' assumed to be the centroid size. It involves three steps: centering from current position,
#' dividing coordinates by 'scale', translating to the original position.
#'
#' @aliases coo_rotate
#' @param coo either a \code{matrix} of (x; y) coordinates, or any \link{Coo} object.
#' @param theta \code{numeric}the angle (in radians) to rotate shapes.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family coo_ utilities
#' @examples
#' coo_plot(bot[1])
#' coo_plot(coo_rotate(bot[1], pi/2))
#'
#' # on Coo
#' b <- bot %>% slice(1:5) # for speed sake
#' stack(b)
#' stack(coo_rotate(b, pi/2))
#' @family rotation functions
#' @export
coo_rotate <- function(coo, theta = 0) {
  UseMethod("coo_rotate")
}

#' @export
coo_rotate.default <- function(coo, theta = 0) {
  coo <- coo_check(coo)
  rmat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  return(coo %*% rmat)
}

#' @export
coo_rotate.Coo <- function(coo, theta = 0) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_rotate, theta)
  return(Coo)
}

# coo_rotatecenter ----------
#' Rotates shapes with a custom center
#'
#' rotates a shape of 'theta' angles (in radians) and with a (x; y) 'center'.
#' @aliases coo_rotatecenter
#' @inheritParams coo_check
#' @param theta \code{numeric} the angle (in radians) to rotate shapes.
#' @param center \code{numeric} the (x; y) position of the center
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family rotation functions
#' @family coo_ utilities
#' @examples
#' b <- bot[1]
#' coo_plot(b)
#' coo_draw(coo_rotatecenter(b, -pi/2, c(200, 200)), border='red')
#' @family rotation functions
#' @export
coo_rotatecenter <- function(coo, theta, center = c(0, 0)) {
  UseMethod("coo_rotatecenter")
}

#' @export
coo_rotatecenter.default <- function(coo, theta, center = c(0,0)) {
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

# coo_untiltx --------
#' Removes rotation so that the centroid and a given point are parallel to the x-axis
#'
#' Rotationnal biases appear after [coo_slidedirection] (and friends).
#' Typically useful for outline analysis where phasing matters. See examples.
#'
#' @aliases coo_untilt
#' @inheritParams coo_check
#' @param id \code{numeric} the id of the point that will become the new first point. See details below
#' for the method on Coo objects.
#' @param ldk \code{numeric} the id of the ldk to use as id, only on \code{Out}
#' @details For Coo objects, and in particular for Out and Opn two different ways of coo_sliding
#' are available:
#' \itemize{
#' \item \strong{no ldk passed and an id is passed}: all id-th points
#' within the shapes will become the first points.
#' \item \strong{a single ldk is passed}: the ldk-th ldk will be used to slide every shape.
#' If an id is (also) passed, id is ignored with a message.
#' }
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_slide} and friends.
#' @family coo_ utilities
#' @examples
#' # on a single shape
#' bot[1] %>% coo_center %>% coo_align %>%
#'    coo_sample(12) %>% coo_slidedirection("right") %T>%
#'    coo_plot() %>% # the first point is not on the x-axis
#'    coo_untiltx() %>%
#'    coo_draw(border="red") # this (red) one is
#'
#' # on an Out
#' # prepare bot
#' prebot <- bot %>% coo_center %>% coo_scale %>%
#'    coo_align %>% coo_slidedirection("right")
#' prebot %>% stack # some dephasing remains
#' prebot %>% coo_slidedirection("right") %>% coo_untiltx() %>% stack # much better
#' # _here_ there is no change but the second, untilted, is correct
#' prebot %>% efourier(8, norm=FALSE) %>% PCA %>% plot_PCA(~type)
#' prebot %>% coo_untiltx %>% efourier(8, norm=FALSE) %>% PCA %>% plot_PCA(~type)
#'
#' # an example using ldks:
#' # the landmark #2 is on the x-axis
#' hearts %>%
#'   slice(1:5) %>% fgProcrustes(tol=1e-3) %>% # for speed sake
#'   coo_center %>% coo_untiltx(ldk=2) %>% stack
#' @export
coo_untiltx <- function(coo, id, ldk){
  UseMethod("coo_untiltx")
}

#' @export
coo_untiltx.default <- function(coo, id=1, ldk){
  if (!missing(ldk))
    message("only 'id' is supported on single shapes")
  # find the right triplet of points to calculate the angle to untilt
  # this one is the concerned point
  edge_xy <- coo[id,, drop=FALSE]
  # then, the centroid
  cent_xy <- coo_centpos(coo) %>% matrix(nrow=1)
  # finally the target (the x of the edge and the y of the centroid since untiltX)
  targ_xy <- c(edge_xy[, 1], cent_xy[, 2])
  # build the triplet and take the right (signed) angle
  theta <- rbind(edge_xy, cent_xy, targ_xy) %>%
    coo_angle_edges() %>% `[`(2)
  # remove this difference and return the untilted shape
  coo %>%
    coo_rotatecenter(theta=theta, center=cent_xy) %>%
    return()
}

#' @export
coo_untiltx.Coo <- function(coo, id=1, ldk){
  Coo <- coo
  # when ldk is provided ---
  # first we check a bit
  if (!missing(ldk)) {
    .check(is_ldk(Coo), "this object has no $ldk")
    if (!missing(id))
      warning("'id' provided will be ignored")
    # here we loop and take the corresponding id
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_untiltx(Coo$coo[[i]], id=Coo$ldk[[i]][ldk])
    }
    return(Coo)
  } else {
    # when id is provided ---
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_untiltx(Coo$coo[[i]], id=id)
    }
  }
  return(Coo)
}

# coo_align ----------
#' Aligns coordinates
#'
#' Aligns the coordinates along their longer axis using var-cov matrix and eigen values.
#'
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' coo_plot(bot[1])
#' coo_plot(coo_align(bot[1]))
#'
#' # on a Coo
#' b <- bot %>% slice(1:5) # for speed sake
#' stack(coo_align(b))
#' @family aligning functions
#' @family coo_ utilities
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

# coo_alignxax --------------
#' Aligns shapes along the x-axis
#'
#' Align the longest axis of a shape along the x-axis.
#' @aliases coo_alignxax
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or any \link{Coo} object.
#' @details If some shapes are upside-down
#' (or mirror of each others), try redefining a new starting point (eg with coo_slidedirection) before
#' the alignment step. This may solve your problem because coo_calliper orders the \code{$arr.ind} used by
#' coo_aligncalliper.
#' @examples
#' b <- bot[1]
#' coo_plot(b)
#' coo_plot(coo_alignxax(b))
#' @family aligning functions
#' @family coo_ utilities
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

# coo_aligncalliper --------------

#' Aligns shapes along their 'calliper length'
#'
#' And returns them registered on bookstein coordinates.
#' See \link{coo_bookstein}.
#' @aliases coo_aligncalliper
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or any \link{Coo} object.
#' @examples
#' b <- bot[1]
#' coo_plot(b)
#' coo_plot(coo_aligncalliper(b))
#'
#' b <- bot %>% slice(1:5) # for speed sake
#' bot.al <- coo_aligncalliper(b)
#' stack(bot.al)
#' @family aligning functions
#' @family coo_ utilities
#' @export
coo_aligncalliper <- function(coo) {
  UseMethod("coo_aligncalliper")
}

#' @export
coo_aligncalliper.default <- function(coo) {
  coo <- coo_check(coo)
  cal.ind <- coo_calliper(coo, arr.ind = TRUE)$arr_ind[[1]]
  coo <- coo_bookstein(coo, cal.ind[1], cal.ind[2])
  return(coo)
}

#' @export
coo_aligncalliper.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_aligncalliper)
  return(Coo)
}

# coo_alignminradius --------------
#' Aligns shapes using their shortest radius
#'
#' And returns them slided with the first coordinate on the east.
#' May be used as an aligning strategy on shapes with a clear 'invaginate' part.
#' @aliases coo_alignminradius
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' b <- bot %>% slice(1:5) # for speed sake
#' stack(coo_alignminradius(b))
#' @family aligning functions
#' @family coo_ utilities
#' @export
coo_alignminradius <- function(coo){
  UseMethod("coo_alignminradius")
}

#' @export
coo_alignminradius.default <- function(coo){
  id_minrad <- which.min(coo_centdist(coo))
  coo <- coo_slide(coo, id_minrad)
  m <- matrix(c(coo[1, ],  0, 0, 1, 0), nrow=3, byrow=TRUE)
  th <- coo_angle_edges(m)[2]
  coo_rotate(coo, -th)
}

#' @export
coo_alignminradius.Coo <- function(coo){
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_alignminradius)
  return(Coo)
}

# coo_translate ----------
#' Translates coordinates
#'
#' Translates the coordinates by a 'x' and 'y' value
#'
#' @inheritParams coo_check
#' @param x \code{numeric} translation along the x-axis.
#' @param y \code{numeric} translation along the y-axis.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family coo_ utilities
#' @examples
#' coo_plot(bot[1])
#' coo_plot(coo_trans(bot[1], 50, 100))
#'
#' # on Coo
#' b <- bot %>% slice(1:5) # for speed sake
#' stack(b)
#' stack(coo_trans(b, 50, 100))
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

# coo_slice------------------
#' Slices shapes between successive coordinates
#'
#' Takes a shape with n coordinates. When you pass this function with at least
#' two ids (<= n), the shape will be open on the corresponding coordinates and
#' slices returned as a list
#' @inheritParams coo_check
#' @param ids \code{numeric} of length >= 2, where to slice the shape(s)
#' @param ldk \code{numeric} the id of the ldk to use as ids, only on \code{Out} and \code{Opn}.
#' If provided, \code{ids} will be ignored.
#' @seealso Have a look to [coo_slidegap] if you have problems with gaps
#' after slicing around landmarks and/or starting points.
#' @return a list of shapes or a list of \link{Opn}
#' @examples
#' h <- slice(hearts, 1:5)  # speed purpose only
#' # single shape, a list of matrices is returned
#' sh <- coo_slice(h[1], c(12, 24, 36, 48))
#' coo_plot(sh[[1]])
#' panel(Opn(sh))
#' # on a Coo, a list of Opn is returned
#' # makes no sense if shapes are not normalized first
#' sh2 <- coo_slice(h, c(12, 24, 36, 48))
#' panel(sh2[[1]])
#'
#' # Use coo_slice with `ldk` instead:
#' # hearts as an example
#' x <- h %>% fgProcrustes(tol=1)
#' # 4 landmarks
#' stack(x)
#' x$ldk[1:5]
#'
#' # here we slice
#' y <- coo_slice(x, ldk=1:4)
#'
#' # plotting
#' stack(y[[1]])
#' stack(y[[2]])
#'
#' # new ldks from tipping points, new ldks from angle
#' olea %>% slice(1:5) %>% # for the sake of speed
#' def_ldk_tips %>%
#' def_ldk_angle(0.75*pi) %>% def_ldk_angle(0.25*pi) %>%
#' coo_slice(ldk =1:4) -> oleas
#' oleas[[1]] %>% stack
#' oleas[[2]] %>% stack # etc.
#'
#' # domestic operations
#' y[[3]] %>% coo_area()
#' # shape analysis of a slice
#' y[[1]] %>% coo_bookstein() %>% npoly %>% PCA %>% plot(~aut)
#'
#' @family slicing functions
#' @family coo_ utilities
#' @export
coo_slice <- function(coo, ids, ldk){
  UseMethod("coo_slice")
}

#' @export
coo_slice.default <- function(coo, ids, ldk){
  n <- length(ids)
  if (n<=1)
    stop("'ids' must contain at least 2 ids")
  res <- list()
  ids <- sort(ids)
  if (max(ids) > nrow(coo))
    stop("max(ids) must be lower than the number of coordinates")

  for (i in 1:(n-1)) {
    res[[i]] <- coo[ids[i]:ids[i+1], ]
  }
  res[[n]] <- coo[ c(ids[n]:nrow(coo)) , ]
  names(res) <- 1:length(ids)
  res
}

#' @export
coo_slice.Out <- function(coo, ids, ldk){
  # ldk case
  if (!missing(ldk)){
    n_ldk <- unique(sapply(coo$ldk, length))
    .check(length(n_ldk == 1),
           " * $ldk number must be homegeneous")
    .check(length(ldk)<=n_ldk,
           " * ldk must be <= the number of $ldk")

    RES <- vector("list", length(ldk))
    for (i in seq_along(coo)){
      res <- coo_slice(coo$coo[[i]], coo$ldk[[i]][ldk])
      for (j in seq_along(ldk)){
        RES[[j]][[i]] <- res[[j]]
      }
    }
  } else { # coo case
    # some checks
    .check(all(coo_nb(coo) > max(ids)),
           " * max(ids) must be lower than any number of coordinates")

    RES <- vector("list", length(ids))

    for (i in seq_along(coo)){
      res <- coo_slice(coo$coo[[i]], ids)
      for (j in seq_along(ids)){
        RES[[j]][[i]] <- res[[j]]
      }
    }
  }
  return(lapply(RES, Opn, fac=coo$fac))
}

#' @export
coo_slice.Opn <- coo_slice.Out

#' @export
coo_slice.Ldk <- function(coo, ids, ldk){
  .check(all(coo_nb(coo) > max(ids)),
         " * max(ids) must be lower than any number of coordinates")
  RES <- vector("list", length(ids))
  for (i in seq_along(coo)){
    res <- coo_slice(coo$coo[[i]], ids)
    for (j in seq_along(ids)){
      RES[[j]][[i]] <- res[[j]]
    }
  }
  return(lapply(RES, Ldk, fac=coo$fac))
}


# coo_slide --------
#' Slides coordinates
#'
#' Slides the coordinates so that the id-th point become the first one.
#' @aliases coo_slide
#' @inheritParams coo_check
#' @param id \code{numeric} the id of the point that will become the new first point. See details below
#' for the method on Coo objects.
#' @param ldk \code{numeric} the id of the ldk to use as id, only on \code{Out}
#' @details For Coo objects, and in particular for Out and Opn three different ways of coo_sliding
#' are available:
#' \itemize{
#' \item \strong{no ldk passed and a single id is passed}: all id-th points
#' within the shapes will become the first points. $ldk will be slided accordingly.
#' \item \strong{no ldk passed and a vector of ids matching the length of the Coo}: for every shape,
#' the id-th point will be used as the id-th point. $ldk will be slided accordingly.
#' \item \strong{a single ldk is passed}: the ldk-th ldk will be used to slide every shape.
#' If an id is (also) passed, it is ignored with a message.
#' }
#' See examples.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @seealso \link{coo_slice} and friends.
#' @examples
#' h <- hearts %>% slice(1:5) # for speed sake
#' stack(h)
#' # set the first landmark as the starting point
#' stack(coo_slide(h, ldk=1))
#' # set the 50th point as the starting point (everywhere)
#' stack(coo_slide(h, id=50))
#' # set the id-random-th point as the starting point (everywhere)
#' set.seed(123) # just for the reproducibility
#' id_random <- sample(x=min(sapply(h$coo, nrow)), size=length(h),
#' replace=TRUE)
#' stack(coo_slide(h, id=id_random))
#' @family sliding functions
#' @family coo_ utilities
#' @export
coo_slide <- function(coo, id, ldk) {
  UseMethod("coo_slide")
}

#' @export
coo_slide.default <- function(coo, id, ldk) {
  coo <- coo_check(coo)
  if (id == 0) {
    return(coo)
  }
  n <- nrow(coo)
  slided.rows <- unique(c(id:n, 1:(id - 1)))
  return(coo[slided.rows, ])
}

#' @export
coo_slide.Coo <- function(coo, id, ldk) {
  Coo <- coo
  ##### ldk case #####
  if (!missing(ldk)) {
    .check(is_ldk(Coo),
           "this object has no $ldk")
    if (!missing(id))        warning("'id' provided will be ignored")
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_slide(Coo$coo[[i]], Coo$ldk[[i]][ldk])
      Coo$ldk[[i]] <- (Coo$ldk[[i]] - (Coo$ldk[[i]][ldk] - 1)) %% nrow(Coo$coo[[i]])
    }
    return(Coo)
  } else {
    ##### id case ######
    if (length(id)==1) id <- rep(id, length(Coo))

    # id case
    # id=1 just rep
    #
    # allows a vector of ids to be passed
    slide_ldk <- (length(Coo$ldk) > 0)
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_slide(Coo$coo[[i]], id[i])
      if (slide_ldk){
        new_ldk <- (Coo$ldk[[i]] - id[i]) %% nrow(Coo$coo[[i]])
        Coo$ldk[[i]] <- ifelse(new_ldk==0, 1, new_ldk)
      }

    }
    return(Coo)
  }
}

# coo_intersect_segment -----------

#' Nearest intersection between a shape and a segment
#'
#' Take a shape, and an intersecting segment, which point is the nearest
#' of where the segment intersects with the shape? Most of the time,
#' centering before makes more sense.
#' @inheritParams coo_check
#' @param seg a 2x2 \code{matrix} defining the starting and ending points;
#' or a list or a numeric of length 4.
#' @param center \code{logical} whether to center the shape (TRUE by default)
#' @return \code{numeric} the id of the nearest point, a `list` for `Coo`. See examples.
#' @family coo_ intersect
#' @examples
#' coo <- bot[1] %>% coo_center %>% coo_scale
#' seg <- c(0, 0, 2, 2) # passed as a numeric of length(4)
#' coo_plot(coo)
#' segments(seg[1], seg[2], seg[3], seg[4])
#' coo %>% coo_intersect_segment(seg) %T>% print %>%
#' # prints on the console and draw it
#'    coo[., , drop=FALSE] %>% points(col="red")
#'
#' # on Coo
#' bot %>%
#'     slice(1:3) %>% # for the sake of speed
#'     coo_center %>%
#'     coo_intersect_segment(matrix(c(0, 0, 1000, 1000), ncol=2, byrow=TRUE))
#' @export
coo_intersect_segment <- function(coo, seg, center=TRUE){
  UseMethod("coo_intersect_segment")
}

#' @export
coo_intersect_segment.default <- function(coo, seg, center=TRUE){
  # in most cases, centering first is useful
  if (center)
    coo <- coo_center(coo)
  # if seg is provided as a list, first unlist
  if (is.list(seg) && length(seg)==4){
    seg <- as.numeric(seg)
  }
  # if seg is now a numeric, then turn it into a shape
  if (!is.matrix(seg) && length(seg)==4){
    seg <- matrix(seg, ncol=2, byrow = TRUE)
  }
  # turns outlines into a SpatialPolygons
  # and seg into a SpatialLines
  # sp_out <- coo %>%
  #   sp::Polygon() %>% list %>%
  #   sp::Polygons(ID="useless_yet_required") %>% list %>%
  #   sp::SpatialPolygons()
  # sp_seg  <- seg %>%
  #   sp::Line() %>% list %>%
  #   sp::Lines(ID="useless_yet_required") %>% list %>%
  #   sp::SpatialLines()
  # rgeos function that returns another sp object
  # inter <- rgeos::gIntersection(sp_out, sp_seg)
  # extract coordinates of intersection points
  # inter_xy <- inter@lines[[1]]@Lines[[1]]@coords

  sf_out <- coo %>% coo_close %>% list %>% sf::st_polygon()
  sf_seg <- seg %>% sf::st_linestring()

  inter <- sf::st_intersection(sf_out, sf_seg)
  inter_xy <- sf::st_coordinates(inter)[, 1:2]

  # find the if of the closest point on the coo
  # and return its id
  edm_nearest(inter_xy[2,, drop=FALSE], coo, full=TRUE)$pos
}

#' @export
coo_intersect_segment.Coo <- function(coo, seg, center=TRUE){
  lapply(coo$coo, coo_intersect_segment, seg=seg, center=center)
}

#' Nearest intersection between a shape and a segment specified with an angle
#'
#' Take a shape, and segment starting on the centroid and having a particular angle, which point is the nearest
#' where the segment intersects with the shape?
#' @inheritParams coo_check
#' @param angle \code{numeric} an angle in radians (0 by default).
#' @param direction \code{character} one of \code{"down", "left", "up", "right"} ("right" by default)
#' @note shapes are always centered before this operation. If you need a simple
#' direction such as \code{(down, left, up, right)ward}, then use \link{coo_intersect_direction} which
#' does not need to find an intersection but relies on coordinates and is about 1000.
#' @return \code{numeric} the id of the nearest point or a `list` for `Coo` See examples.
#' @family coo_ intersect
#' @examples
#' coo <- bot[1] %>% coo_center %>% coo_scale
#' coo_plot(coo)
#' coo %>% coo_intersect_angle(pi/7) %>%
#'    coo[., , drop=FALSE] %>% points(col="red")
#'
#'  # many angles
#'  coo_plot(coo)
#'  sapply(seq(0, pi, pi/12),
#'        function(x) coo %>% coo_intersect_angle(x)) -> ids
#'  coo[ids, ] %>% points(col="blue")
#'
#'  coo %>%
#'  coo_intersect_direction("down") %>%
#'  coo[.,, drop=FALSE] %>% points(col="orange")
#'

#' @export
coo_intersect_angle <- function(coo, angle=0){
  UseMethod("coo_intersect_angle")
}

#' @export
coo_intersect_angle.default <- function(coo, angle=0){
  # only defined on centered coo
  coo <- coo_center(coo)
  origin <- matrix(0, nrow=1, ncol=2)
  # calculate a landing point "outside" the shape (modulus)
  # in the right direction (argument)
  # using complex numbers / polar coordinates
  # below the "outside" (modulus) part
  coo_centdist(coo) %>% max %>% `*`(2) %>%
    complex(modulus = ., argument = angle) %>% cpx2coo() %>%
    # add the origin
    rbind(origin, .) %>%
    coo_intersect_segment(coo, ., center=TRUE)
}

#' @export
coo_intersect_angle.Coo <-
  function(coo, angle=0){
    lapply(coo$coo, coo_intersect_angle, angle=angle)
  }

#' @rdname coo_intersect_angle
#' @export
coo_intersect_direction <-
  function(coo,
           direction=c("down", "left", "up", "right")[4]){
    UseMethod("coo_intersect_direction")
  }


#' @rdname coo_intersect_angle
#' @export
coo_intersect_direction.default <-
  function(coo,
           direction=c("down", "left", "up", "right")[4]){

    coo <- coo_check(coo)

    if (direction == "down") {
      x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
      id0 <- x0.ed[which(coo[x0.ed, 2] < 0)[1]]
    }

    if (direction == "left") {
      y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
      id0 <- y0.ed[which(coo[y0.ed, 1] < 0)[1]]
    }

    if (direction == "up") {
      x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
      id0 <- x0.ed[which(coo[x0.ed, 2] > 0)[1]]
    }

    if (direction == "right") {
      y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
      id0 <- y0.ed[which(coo[y0.ed, 1] > 0)[1]]
    }
    # return the id
    id0
  }

#' @rdname coo_intersect_angle
#' @export
coo_intersect_direction.Coo <-
  function(coo,
           direction=c("down", "left", "up", "right")[4]){
    lapply(coo$coo, coo_intersect_direction, direction=direction)
  }

# coo_slidedirection --------
#' Slides coordinates in a particular direction
#'
#' Shapes are centered and then, according to direction, the point northwards, southwards,
#' eastwards or westwards the centroid, becomes the first point with \link{coo_slide}.
#' 'right' is possibly the most sensible option (and is by default),
#' since 0 radians points eastwards, relatively to the origin.
#' This should be followed by a [coo_untiltx] is most cases to remove any rotationnal dephasing/bias.
#'
#' @inheritParams coo_check
#' @param direction \code{character} one of \code{"down", "left", "up", "right"} ("right" by default)
#' @param center \code{logical} whether to center or not before sliding
#' @param id \code{numeric} whether to return the id of the point or the slided shapes
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' b <- coo_rotate(bot[1], pi/6) # dummy example just to make it obvious
#' coo_plot(b) # not the first point
#' coo_plot(coo_slidedirection(b, "up"))
#' coo_plot(coo_slidedirection(b, "right"))
#' coo_plot(coo_slidedirection(b, "left"))
#' coo_plot(coo_slidedirection(b, "down"))
#'
#' # on Coo objects
#' b <- bot %>% slice(1:5) # for speed sake
#' stack(b)
#' stack(coo_slidedirection(b, "right"))
#'
#' # This should be followed by a [coo_untiltx] in most (if not all) cases
#' stack(coo_slidedirection(b, "right") %>% coo_untiltx)
#'
#' @family sliding functions
#' @family coo_ utilities
#' @export
coo_slidedirection <-
  function(coo, direction=c("down", "left", "up", "right")[4], center, id) {
    UseMethod("coo_slidedirection")
  }

#' @export
coo_slidedirection.default <-
  function(coo, direction=c("down", "left", "up", "right")[4],
           center=TRUE, id = FALSE) {
    # for the sake of compatibility
    if (any(direction %in% c("S", "W", "N", "E"))){
      message("direction specification has changed and retrocompatibility
               wil be removed in future version. See ?coo_slidedirection")
    }
    if (direction=="S") direction <- "down"
    if (direction=="W") direction <- "left"
    if (direction=="N") direction <- "up"
    if (direction=="E") direction <- "right"

    coo <- coo_check(coo)
    if (center) coo <- coo_center(coo)

    if (direction == "down") {
      x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
      id0 <- x0.ed[which(coo[x0.ed, 2] < 0)[1]]
    }

    if (direction == "left") {
      y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
      id0 <- y0.ed[which(coo[y0.ed, 1] < 0)[1]]
    }

    if (direction == "up") {
      x0.ed <- order(abs(coo[, 1]), decreasing = FALSE)
      id0 <- x0.ed[which(coo[x0.ed, 2] > 0)[1]]
    }

    if (direction == "right") {
      y0.ed <- order(abs(coo[, 2]), decreasing = FALSE)
      id0 <- y0.ed[which(coo[y0.ed, 1] > 0)[1]]
    }

    if (id) {
      return(id0)
    } else {
      coo <- coo_slide(coo, id=id0)
      return(coo)
    }
  }

#' @export
coo_slidedirection.Coo <-
  function(coo, direction=c("down", "left", "up", "right")[4],
           center = TRUE, id = TRUE) {
    Coo <- coo
    id0 <- sapply(Coo$coo, coo_slidedirection, direction, center, id=TRUE)
    Coo <- coo_slide(Coo, id = id0)
    return(Coo)
  }

# coo_slidegap ---------
#' Slides coordinates using the widest gap
#'
#' When slicing a shape using two landmarks, or functions such as \link{coo_up},
#' an open curve is obtained and the rank of points make wrong/artefactual results.
#' If the widest gap is > 5 * median of other gaps, then the couple of coordinates
#' forming this widest gap is used as starting and ending points. This switch helps
#' to deal with open curves. Examples are self-speaking.
#' Use \code{force=TRUE} to bypass this check
#'
#' @inheritParams coo_check
#' @param force \code{logical} whether to use the widest gap, with no check, as the real gap
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object.
#' @examples
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
#' @family sliding functions
#' @family coo_ utilities
#' @export
coo_slidegap <- function(coo, force){
  UseMethod("coo_slidegap")
}

#' @export
coo_slidegap.default <- function(coo, force=FALSE){
  gaps <- coo_perimpts(coo)
  widest_gap <- which.max(gaps)
  if (force) {
    return(coo_slide(coo, widest_gap+1))
  }
  if (gaps[widest_gap] > 5*median(gaps)) {
    return(coo_slide(coo, widest_gap+1))
  }
  return(coo)
}

#' @export
coo_slidegap.Coo <- function(coo, force=FALSE){
  coo$coo <- lapply(coo$coo, coo_slidegap, force=force)
  coo
}

# coo_extract -------------
#' Extract coordinates from a shape
#'
#' Extract ids coordinates from a single shape or a Coo object.
#'
#' It probably only make sense for Coo objects with the same number of coordinates
#' and them being homologous, typically on Ldk.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates or a \link{Coo} object.
#' @param ids \code{integer}, the ids of points to sample.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family sampling functions
#' @family coo_ utilities
#' @examples
#' coo_extract(bot[1], c(3, 9, 12)) # or :
#' bot[1] %>% coo_extract(c(3, 9, 12))
#' @export
coo_extract <- function(coo, ids){
  UseMethod("coo_extract")
}

#' @export
coo_extract.default <- function(coo, ids){
  .check(!missing(ids),
         "ids must be provided")
  .check(is.numeric(ids),
         "ids must be numeric")
  coo[ids, ]
}

#' @export
coo_extract.Coo <- function(coo, ids){
  .check(all(coo_nb(coo) > length(ids)),
         "at least one shape has less coordinates than ids length")
  .check(all(coo_nb(coo) > max(ids)),
         "at least one shape has less coordinates than max(ids)")
  coo$coo <- lapply(coo$coo, coo_extract, ids)
  coo
}

# coo_sample -------------
#' Sample coordinates (among points)
#'
#' Sample n coordinates among existing points.
#'
#' For the \link{Out} an \link{Opn}
#' methods (pointless for \link{Ldk}), in an \code{$ldk} component is defined,
#' it is changed accordingly by multiplying the ids by n over the number of coordinates.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates or an \link{Out} or an \link{Opn} object.
#' @param n \code{integer}, the number fo points to sample.
#' @return a \code{matrix} of (x; y) coordinates, or an \link{Out} or an \link{Opn} object.
#' @examples
#' b <- bot[1]
#' stack(bot)
#' stack(coo_sample(bot, 24))
#' coo_plot(b)
#' coo_plot(coo_sample(b, 24))
#' @family sampling functions
#' @family coo_ utilities
#' @export
coo_sample <- function(coo, n) {
  UseMethod("coo_sample")
}

#' @export
coo_sample.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (nrow(coo) < n) stop("less coordinates than n, try coo_interpolate")
  sampled <- round(seq(1, nrow(coo), len = n + 1)[-(n + 1)])
  return(coo[sampled, ])
}

#' @export
coo_sample.Out <- function(coo, n) {
  Out <- coo
  # if an $ldk is present, we have to change it.
  if (is_ldk(Out)) {
    coo_nb <- sapply(Out$coo, nrow)
    for (i in 1:length(Out)){
      ratio.i <- n / coo_nb[i]
      Out$ldk[[i]] <- ceiling(Out$ldk[[i]] * ratio.i)
    }
    message("$ldk has been changed accordingly")
  }

  Out$coo <- lapply(Out$coo, coo_sample, n)

  return(Out)
}

#' @export
coo_sample.Opn <- coo_sample.Out

# coo_sample -------------
#' Sample a proportion of coordinates (among points)
#'
#' A simple wrapper around \link{coo_sample}
#'
#' As for \link{coo_sample} if an \code{$ldk} component is defined,
#' it is changed accordingly by multiplying the ids by n over the number of coordinates.
#'
#' @param coo either a \code{matrix} of (x; y) coordinates or an \link{Out} or an \link{Opn} object.
#' @param prop \code{numeric}, the proportion of points to sample
#' @return a \code{matrix} of (x; y) coordinates, or an \link{Out} or an \link{Opn} object.
#' @examples
#' # single shape
#' bot[1] %>% coo_nb()
#' bot[1] %>% coo_sample_prop(0.5) %>% coo_nb()
#' @family sampling functions
#' @family coo_ utilities
#' @export
coo_sample_prop <- function(coo, prop=1) {
  UseMethod("coo_sample_prop")
}

#' @export
coo_sample_prop.default <- function(coo, prop=1) {
  coo <- coo_check(coo)
  coo_sample(coo, round(nrow(coo)*prop))
}

#' @export
coo_sample_prop.Out <- function(coo, prop=1) {
  Out <- coo
  # if an $ldk is present, we have to change it.
  if (is_ldk(Out)) {
    N <- coo_nb(Out)
    n <- round(N*prop)
    for (i in 1:length(Out)){
      ratio.i <- n[i] / N[i]
      Out$ldk[[i]] <- ceiling(Out$ldk[[i]] * (n[i] / N[i]))
    }
    message("$ldk has been changed accordingly")
  }
  # now sample $coo
  Out$coo <- lapply(Out$coo, coo_sample_prop, prop)
  return(Out)
}

#' @export
coo_sample_prop.Opn <- coo_sample_prop.Out


# coo_samplerr --------------
#' Samples coordinates (regular radius)
#'
#' Samples n coordinates with a regular angle.
#'
#' By design, this function samples among existing points, so using
#' \link{coo_interpolate} prior to it may be useful to have
#' more homogeneous angles. See examples.
#'
#' @inheritParams coo_check
#' @param n \code{integer}, the number of points to sample.
#' @return a \code{matrix} of (x; y) coordinates or a Coo object.
#' @examples
#' stack(bot)
#' bot <- coo_center(bot)
#' stack(coo_samplerr(bot, 12))
#' coo_plot(bot[1])
#' coo_plot(rr <- coo_samplerr(bot[1], 12))
#' cpos <- coo_centpos(bot[1])
#' segments(cpos[1], cpos[2], rr[, 1], rr[, 2])
#'
#' # Sometimes, interpolating may be useful:
#' shp <- hearts[1] %>% coo_center
#'
#' # given a shp, draw segments from each points on it, to its centroid
#' draw_rads <- function(shp, ...){
#'  segments(shp[, 1], shp[, 2], coo_centpos(shp)[1], coo_centpos(shp)[2], ...)
#'}
#'
#' # calculate the sd of argument difference in successive points,
#' # in other words a proxy for the homogeneity of angles
#' sd_theta_diff <- function(shp)
#'    shp %>% complex(real=.[, 1], imaginary=.[, 2]) %>%
#'    Arg %>% `[`(-1) %>% diff %>% sd
#'
#' # no interpolation: all points are sampled from existing points but the
#' # angles are not equal
#' shp %>% coo_plot(points=TRUE, main="no interpolation")
#' shp %>% coo_samplerr(64) %T>% draw_rads(col="red") %>% sd_theta_diff
#' # with interpolation: much more homogeneous angles
#' shp %>% coo_plot(points=TRUE)
#' shp %>% coo_interpolate(360) %>% coo_samplerr(64) %T>% draw_rads(col="blue") %>% sd_theta_diff
#' @family sampling functions
#' @family coo_ utilities
#' @export
coo_samplerr <- function(coo, n) {
  UseMethod("coo_samplerr")
}

#' @export
coo_samplerr.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (nrow(coo) < n) stop("less coordinates than n, try coo_interpolate")
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

# coo_interpolate -----------
#' Interpolates coordinates
#'
#' Interpolates n coordinates 'among existing points'between' existing points,
#' along the perimeter of the coordinates provided and keeping the first point
#'
#' @inheritParams coo_check
#' @param n  \code{integer}, the number fo points to interpolate.
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' b5 <- bot %>% slice(1:5) # for speed sake
#' stack(b5)
#' stack(coo_scale(b5))
#' stack(b5)
#' stack(coo_interpolate(coo_sample(b5, 12), 120))
#' coo_plot(bot[1])
#' coo_plot(coo_interpolate(coo_sample(bot[1], 12), 120))
#' @family sampling functions
#' @family coo_ utilities
#' @export
coo_interpolate <- function(coo, n) {
  UseMethod("coo_interpolate")
}

#' @export
coo_interpolate.default <- function(coo, n) {
  coo <- coo_check(coo)
  if (!coo_is_closed(coo)) {
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

# coo_smooth ---------------
#' Smoothes coordinates
#'
#' Smoothes coordinates using a simple moving average.
#' May be useful to remove digitization noise, mainly on outlines and open outlines.
#'
#' @inheritParams coo_check
#' @param n \code{integer} the number of smoothing iterations
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' b5 <- slice(bot, 1:5) # for speed sake
#' stack(b5)
#' stack(coo_smooth(b5, 10))
#' coo_plot(b5[1])
#' coo_plot(coo_smooth(b5[1], 30))
#' @family smoothing functions
#' @family coo_ utilities
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

# coo_smoothcurve -----------
#' Smoothes coordinates on curves
#'
#' Smoothes coordinates using a simple moving average but let the first and last points unchanged.
#' May be useful to remove digitization noise on curves.
#'
#' @inheritParams coo_check
#' @param n \code{integer} to specify the number of smoothing iterations
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' o <- olea[1]
#' coo_plot(o, border='grey50', points=FALSE)
#' coo_draw(coo_smooth(o, 24), border='blue', points=FALSE)
#' coo_draw(coo_smoothcurve(o, 24), border='red', points=FALSE)
#' @family smoothing functions
#' @family coo_ utilities
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

# coo_is_closed ----------
#' Test if shapes are closed
#'
#' Returns TRUE/FALSE whether the last coordinate of the shapes is the same
#' as the first one.
#'
#' @aliases coo_is_closed
#' @inheritParams coo_check
#' @return a single or a vector of \code{logical}.
#' @family coo_ utilities
#' @examples
#' coo_is_closed(matrix(1:10, ncol=2))
#' coo_is_closed(coo_close(matrix(1:10, ncol=2)))
#' coo_is_closed(bot)
#' coo_is_closed(coo_close(bot))
#' @export
coo_is_closed <- function(coo) {
  UseMethod("coo_is_closed")
}

#' @export
coo_is_closed.default <- function(coo) {
  coo <- coo_check(coo)
  identical(coo[1, ], coo[nrow(coo), ])
}

#' @export
coo_is_closed.Coo <- function(coo) {
  Coo <- coo
  return(sapply(Coo$coo, coo_is_closed))
}

#' @rdname coo_is_closed
#' @export
is_open <- function(coo) !coo_is_closed(coo)

# is_equallyspacedradii ----------
#' Tests if coordinates likely have equally spaced radii
#'
#' Returns TRUE/FALSE whether the sd of angles between all successive
#' radii is below/above \code{thesh}
#'
#' @inheritParams coo_check
#' @param thres numeric a threshold (arbitrarily \code{pi/90}, eg 2 degrees, by default)
#' @return a single or a vector of \code{logical}. If \code{NA} are returned,
#' some coordinates are likely identical, at least for x or y.
#' @family coo_ utilities
#' @examples
#' bot[1] %>% is_equallyspacedradii
#' bot[1] %>% coo_samplerr(36) %>% is_equallyspacedradii
#' # higher tolerance but wrong
#' bot[1] %>% coo_samplerr(36) %>% is_equallyspacedradii(thres=5*2*pi/360)
#' # coo_interpolate is a better option
#' bot[1] %>% coo_interpolate(1200) %>% coo_samplerr(36) %>% is_equallyspacedradii
#' # Coo method
#' bot %>% coo_interpolate(360) %>% coo_samplerr(36) %>% is_equallyspacedradii
#' @export
is_equallyspacedradii <- function(coo, thres) {
  UseMethod("is_equallyspacedradii")
}

#' @export
is_equallyspacedradii.default <- function(coo, thres=pi/90){
  coo1 <- coo_slide(coo, id = 2)
  cent <- coo_centpos(coo)
  res <- vector("numeric", nrow(coo))
  for (i in 1:nrow(coo)){
    res[i] <- rbind(coo[i, ], cent, coo1[i, ]) %>%
      .coo_angle_edge1("acos") %>% `[`(2)
  }
  sd(res) < thres
}

#' @export
is_equallyspacedradii.Coo <- function(coo, thres=pi/90){
  Coo <- coo
  suppressWarnings(sapply(Coo$coo, is_equallyspacedradii, thres=thres))
}

# # is.likelyopen tries to estimate is a matrix of
# coordinates is likely to be a # closed polygon
# is.likelyclosedpolygon <- function(coo) { x <-
# coo_perimpts(coo) d <- max(x) / median(x[-which.max(x)])
# ifelse(d > 3, TRUE, FALSE)}

# coo_clockwise
# see http://en.wikipedia.org/wiki/Shoelace_formula

#' Tests if shapes are (likely) developping clockwise or anticlockwise
#'
#' @inheritParams coo_check
#' @return a single or a vector of \code{logical}.
#' @family coo_ utilities
#' @examples
#' shapes[4] %>% coo_sample(64) %>% coo_plot()  #clockwise cat
#' shapes[4] %>% coo_likely_clockwise()
#' shapes[4] %>% coo_rev() %>% coo_likely_clockwise()
#'
#' # on Coo
#' shapes %>% coo_likely_clockwise %>% `[`(4)
#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise <- function(coo)
  UseMethod("coo_likely_clockwise")

#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise.default <- function(coo){
  # it uses, complex numbers, polar coordinates and
  # the rolling difference in argument.
 coo %>%
    # cartesian to polar
    coo2cpx() %>%
    # retrieve argument
    Arg() %>%
    # and the diff along successive points
    diff() %>%
    # test if on average it is negative
    `>`(0) %>% mean() %>% `<`(0.5)
}

#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise.Coo <- function(coo){
  sapply(coo$coo, coo_likely_clockwise)
}

#' @rdname coo_likely_clockwise
#' @export
coo_likely_anticlockwise <- function(coo){
  !coo_likely_clockwise(coo)
}

# coo_close -----------------
#' Closes/uncloses shapes
#'
#' Returns a closed shape from (un)closed shapes. See also \link{coo_unclose}.
#'
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family coo_ utilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo_close(x)
#' x3 <- coo_unclose(x2)
#' x
#' coo_is_closed(x)
#' x2
#' coo_is_closed(x2)
#' x3
#' coo_is_closed(x3)
#' @rdname coo_close
#' @export
coo_close <- function(coo) {
  UseMethod("coo_close")
}

#' @export
coo_close.default <- function(coo) {
  coo <- coo_check(coo)
  ifelse(coo_is_closed(coo), return(coo), return(rbind(coo, coo[1, ])))
}

#' @export
coo_close.Coo <- function(coo) {
  Coo <- coo
  Coo$coo <- lapply(Coo$coo, coo_close)
  return(Coo)
}

# coo_unclose -----------------
#' 'Uncloses' shapes
#'
#' Returns a unclosed shape from (un)closed shapes. See also \link{coo_close}.
#'
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @family coo_ utilities
#' @examples
#' x <- (matrix(1:10, ncol=2))
#' x2 <- coo_close(x)
#' x3 <- coo_unclose(x2)
#' x
#' coo_is_closed(x)
#' x2
#' coo_is_closed(x2)
#' x3
#' coo_is_closed(x3)
#' @rdname coo_close
#' @export
coo_unclose <- function(coo) {
  UseMethod("coo_unclose")
}

#' @export
coo_unclose.default <- function(coo) {
  coo <- coo_check(coo)
  ifelse(coo_is_closed(coo), return(coo[-nrow(coo), ]), return(coo))
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



# coo_force2close -----------
#' Forces shapes to close
#'
#' An exotic function that distribute the distance between the first and the last points
#' of unclosed shapes, so that they become closed. May be useful (?) e.g. for t/rfourier methods
#' where reconstructed shapes may not be closed.
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' b <- coo_sample(bot[1], 64)
#' b <- b[1:40,]
#' coo_plot(b)
#' coo_draw(coo_force2close(b), border='red')
#' @family coo_ utilities
#' @export
coo_force2close <- function(coo){
  UseMethod("coo_force2close")
}

#' @export
coo_force2close.default <- function(coo) {
  coo <- coo_check(coo)
  xy <- coo_centpos(coo)
  if (coo_is_closed(coo)) {
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

#' @export
coo_force2close.Coo <- function(coo){
  coo$coo <- lapply(coo$coo, coo_force2close)
}

# coo_shearx -----------------
#' Shears shapes
#'
#' \code{coo_shearx} applies a shear mapping on a matrix of (x; y) coordinates (or a list), parallel
#' to the x-axis (i.e. x' = x + ky; y' = y + kx). \code{coo_sheary} does it parallel to the y-axis.
#' @rdname coo_shear
#' @inheritParams coo_check
#' @param k \code{numeric} shear factor
#' @return a \code{matrix} of (x; y) coordinates.
#' @examples
#' coo <- coo_template(shapes[11])
#' coo_plot(coo)
#' coo_draw(coo_shearx(coo, 0.5), border="blue")
#' coo_draw(coo_sheary(coo, 0.5), border="red")
#'
#' @family transforming functions
#' @family coo_ utilities
#' @export
coo_shearx <- function(coo, k){
  UseMethod("coo_shearx")
}

#' @export
coo_shearx.default <- function(coo, k=1){
  smat <- matrix(c(1, 0, k, 1), nrow=2)
  return(coo %*% smat)}

#' @export
coo_shearx.Coo <-function(coo, k=1){
  coo$coo <- lapply(coo$coo, coo_shearx, k=k)
  coo
}

# coo_sheary ----------------
#' @rdname coo_shear
#' @export
coo_sheary <- function(coo, k){
  UseMethod("coo_sheary")
}

#' @export
coo_sheary.default <- function(coo, k=1){
  smat <- matrix(c(1, k, 0, 1), nrow=2)
  return(coo %*% smat)}

#' @export
coo_sheary.Coo <- function(coo, k=1){
  coo$coo <- lapply(coo$coo, coo_sheary, k=k)
  coo
}

# coo_flipx -----------------
#' Flips shapes
#'
#' \code{coo_flipx} flips shapes about the x-axis; \code{coo_flipy} about the y-axis.
#' @rdname coo_flip
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates
#' @examples
#' cat <- shapes[4]
#' cat <- coo_center(cat)
#' coo_plot(cat)
#' coo_draw(coo_flipx(cat), border="red")
#' coo_draw(coo_flipy(cat), border="blue")
#'
#' #' # to flip an entire Coo:
#' shapes2 <- shapes
#' shapes$coo <- lapply(shapes2$coo, coo_flipx)
#' @family transforming functions
#' @family coo_ utilities
#' @export
coo_flipx <- function(coo){
  UseMethod("coo_flipx")
}

#' @export
coo_flipx.default <- function(coo){
  m <- matrix(c(1, 0, 0, -1), nrow = 2)
  return(coo %*% m)
}

#' @export
coo_flipx.Coo <- function(coo){
  if (length(coo$ldk) != 0)
    message("note that $ldk has not been changed")
  coo$coo <- lapply(coo$coo, coo_flipx)
  coo
}

# coo_flipy -----------------
#' @rdname coo_flip
#' @export
coo_flipy <- function(coo){
  UseMethod("coo_flipy")
}

#' @export
coo_flipy.default <- function(coo){
  m <- matrix(c(-1, 0, 0, 1), nrow = 2)
  return(coo %*% m)
}

#' @export
coo_flipy.Coo <- function(coo){
  if (length(coo$ldk) != 0)
    message("note that $ldk has not been changed")
  coo$coo <- lapply(coo$coo, coo_flipy)
  coo
}

# coo_dxy -------------------
#' Calculate abscissa and ordinate on a shape
#'
#' A simple wrapper to calculate dxi - dx1 and dyi - dx1.
#' @param coo a matrix (or a list) of (x; y) coordinates or any `Coo`
#' @return a `data.frame` with two components \code{dx} and \code{dy} for single shapes
#' or a `list` of such `data.frame`s for `Coo`
#' @family exemplifying functions
#' @family coo_ utilities
#' @examples
#' coo_dxy(coo_sample(bot[1], 12))
#'
#' bot %>%
#'     slice(1:5) %>% coo_sample(12) %>%  # for readability and speed only
#'     coo_dxy()
#' @export
coo_dxy <- function(coo) {
  UseMethod("coo_dxy")
}

#' @export
coo_dxy.default <- function(coo) {
  coo <- coo_check(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]
  return(dplyr::tibble(dx = dx, dy = dy))
}

#' @export
coo_dxy.Coo <- function(coo) {
  lapply(coo$coo, coo_dxy)
}

# 2. Handling / baselines on coo and Coo
# Some functions and methods to ease alignments, grabbing part of shapes, etc.

# coo_up --------------------
#' Retains coordinates with positive y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the upper side.
#' @inheritParams coo_check
#' @param slidegap \code{logical} whether to apply \link{coo_slidegap} after coo_down
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the x-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_left/right/up/down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @examples
#' b <- coo_alignxax(bot[1])
#' coo_plot(b)
#' coo_draw(coo_up(b), border='red')
#' @family opening functions
#' @family coo_ utilities
#' @export
coo_up <- function(coo, slidegap=FALSE){
  UseMethod("coo_up")
}

#' @export
coo_up.default <- function(coo, slidegap=FALSE) {
  up <- coo[coo[, 2] >= 0, ]
  if (slidegap) { up <- coo_slidegap(up)}
  return(up)
}

#' @export
coo_up.Out <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_up)
  if (slidegap) coo <- coo_slidegap(coo)
  Opn(coo)
}

#' @export
coo_up.Coo <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_up)
  if (slidegap) coo <- coo_slidegap(coo)
  coo
}

# coo_down --------
#' coo_down
#' Retains coordinates with negative y-coordinates
#'
#' Useful when shapes are aligned along the x-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the lower side.
#' @inheritParams coo_check
#' @param slidegap \code{logical} whether to apply \link{coo_slidegap} after coo_down
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the x-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_left/right/up/down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @examples
#' b <- coo_alignxax(bot[1])
#' coo_plot(b)
#' coo_draw(coo_down(b), border='red')
#' @family opening functions
#' @family coo_ utilities
#' @export
coo_down <- function(coo, slidegap=FALSE){
  UseMethod("coo_down")
}

#' @export
coo_down.default <- function(coo, slidegap=FALSE) {
  down <- coo[coo[, 2] <= 0, ]
  if (slidegap) down <- coo_slidegap(down)
  return(down)
}

#' @export
coo_down.Out <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_down, slidegap=slidegap)
  Opn(coo)
}

#' @export
coo_down.Coo <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_down, slidegap=slidegap)
  coo
}

# coo_right -----------------
#' Retains coordinates with positive x-coordinates
#'
#' Useful when shapes are aligned along the y-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the upper side.
#' @inheritParams coo_check
#' @param slidegap \code{logical} whether to apply \link{coo_slidegap} after coo_right
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the y-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_left/right/up/down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @examples
#' b <- coo_center(bot[1])
#' coo_plot(b)
#' coo_draw(coo_right(b), border='red')
#' @family opening functions
#' @family coo_ utilities
#' @export
coo_right <- function(coo, slidegap=FALSE){
  UseMethod("coo_right")
}

#' @export
coo_right.default <- function(coo, slidegap=FALSE) {
  right <- coo[coo[, 1] >= 0, ]
  if (slidegap) { right <- coo_slidegap(right)}
  return(right)
}

#' @export
coo_right.Out <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_right)
  if (slidegap) coo <- coo_slidegap(coo)
  Opn(coo)
}

#' @export
coo_right.Coo <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_right)
  if (slidegap) coo <- coo_slidegap(coo)
  coo
}

# coo_left ------------------
#' Retains coordinates with negative x-coordinates
#'
#' Useful when shapes are aligned along the y-axis (e.g. because of a
#' bilateral symmetry) and when one wants to retain just the lower side.
#' @inheritParams coo_check
#' @param slidegap \code{logical} whether to apply \link{coo_slidegap} after coo_left
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object (\link{Out} are returned as \link{Opn})
#' @note When shapes are "sliced" along the y-axis, it usually results on open curves and thus to huge/artefactual
#' gaps between points neighboring this axis. This is usually solved with \link{coo_slidegap}. See examples there.
#'
#' Also, when apply a coo_left/right/up/down on an \link{Out} object, you then obtain an \link{Opn} object, which is done
#' automatically.
#' @examples
#' b <- coo_center(bot[1])
#' coo_plot(b)
#' coo_draw(coo_left(b), border='red')
#' @family opening functions
#' @family coo_ utilities
#' @export
coo_left <- function(coo, slidegap=FALSE){
  UseMethod("coo_left")
}

#' @export
coo_left.default <- function(coo, slidegap=FALSE) {
  left <- coo[coo[, 1] <= 0, ]
  if (slidegap) left <- coo_slidegap(left)
  return(left)
}

#' @export
coo_left.Out <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_left, slidegap=slidegap)
  Opn(coo)
}

#' @export
coo_left.Coo <- function(coo, slidegap=FALSE){
  coo$coo <- lapply(coo$coo, coo_left, slidegap=slidegap)
  coo
}

# coo_rev
#' Reverses coordinates
#'
#' Returns the reverse suite of coordinates, i.e. change shape's orientation
#' @inheritParams coo_check
#' @return a \code{matrix} of (x; y) coordinates or a Coo object
#' @family coo_ utilities
#' @examples
#' b <- coo_sample(bot[1], 4)
#' b
#' coo_rev(b)
#' @export
coo_rev <- function(coo) {
  UseMethod("coo_rev")
}

#' @export
coo_rev.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo[nrow(coo):1, ])
}

#' @export
coo_rev.Coo <- function(coo) {
  coo$coo <- lapply(coo$coo, coo_rev)
  # ldk ids (if any) also have to be changed
  if (length(coo$ldk)!=0){
    for (i in 1:length(coo)){
      coo$ldk[[i]] <- (nrow(coo$coo[[i]])+1) - coo$ldk[[i]]
    }
    message("$ldk has been changed accordingly")
  }
  return(coo)
}

#' Jitters shapes
#'
#' A simple wrapper around \link{jitter}.
#'
#' @inheritParams coo_check
#' @param ... additional parameter for \link{jitter}
#' @return a \code{matrix} of (x; y) coordinates or a Coo object
#' @seealso \link{get_pairs}
#' @family coo_ utilities
#' @examples
#' b <-bot[1]
#' coo_plot(b, zoom=0.2)
#' coo_draw(coo_jitter(b, amount=3), border="red")
#'
#' # for a Coo example, see \link{get_pairs}
#' @export
coo_jitter <- function(coo, ...){
  UseMethod("coo_jitter")
}

#' @export
coo_jitter.default <- function(coo, ...){
  coo_check(coo)
  jitter(coo, ...)
}

#' @export
coo_jitter.Coo <- function(coo, ...){
  coo$coo <- lapply(coo$coo, coo_jitter, ...)
  coo
}

# coo_bookstein -------------
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
#' @inheritParams coo_check
#' @param ldk1 \code{numeric} the id of the first point of the new baseline (the first, by default)
#' @param ldk2 \code{numeric} the id of the second point of the new baseline (the last, by default)
#' @return a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
#' @examples
#' h <- hearts %>% slice(1:5) # for the sake of speed
#' stack(h)
#' stack(coo_bookstein(h, 2, 4))
#' h <- hearts[1]
#' coo_plot(h)
#' coo_plot(coo_bookstein(h, 20, 57), border='red')
#' @family baselining functions
#' @family coo_ utilities
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
  Out <- coo
  for (i in seq(along = Out$coo)) {
    Out$coo[[i]] <- coo_bookstein(Out$coo[[i]], Out$ldk[[i]][ldk1], Out$ldk[[i]][ldk2])
  }
  #Out$coo <- coo_i
  return(Out)
}

#' @export
coo_bookstein.Opn <- function(coo, ldk1=1, ldk2=2) {
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

# coo_baseline --------------
#' Register new baselines
#'
#' A non-exact baseline registration on \code{t1} and \code{t2} coordinates,
#' for the \code{ldk1}-th and \code{ldk2}-th points.
#' By default it returns Bookstein's coordinates.
#' @inheritParams coo_check
#' @param ldk1 \code{numeric} the id of the first point of the new baseline
#' @param ldk2 \code{numeric} the id of the second point of the new baseline
#' @param t1 \code{numeric} the (x; y) coordinates of the 1st point of the new baseline
#' @param t2 \code{numeric} the (x; y) coordinates of the 2nd point of the new baseline
#' @return a \code{matrix} of (x; y) coordinates or a \link{Coo} object.
#' @examples
#' h <- hearts %>% slice(1:5) # for speed sake
#' stack(h)
#' stack(coo_baseline(h, 2, 4, c(-1, 0), c(1, 1)))
#' @family baselining functions
#' @family coo_ utilities
#' @export
coo_baseline <- function(coo, ldk1, ldk2, t1, t2) {
  UseMethod("coo_baseline")
}

#' @export
coo_baseline.default <- function(coo,
                                 ldk1 = 1, ldk2 = nrow(coo),
                                 t1 = c(-0.5, 0), t2 = c(0.5, 0)) {
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
  vi <- .vecs_param(rx, ry, tx, ty)
  # we rotate accordingly with a center defined as the first
  # landmark (trans, rot, untrans)
  ref <- coo_trans(ref, -t1x, -t1y)
  ref <- ref/vi$r.norms
  ref <- coo_rotate(ref, -vi$d.angle)
  ref <- coo_trans(ref, t1x, t1y)
  return(ref)
}

#' @export
coo_baseline.Coo <- function(coo, ldk1 = 1, ldk2 = 2,
                             t1 = c(-0.5, 0), t2 = c(0.5, 0)) {
  Coo <- coo
  if (length(Coo$ldk)>0) {
    for (i in seq(along = Coo$coo)) {
      Coo$coo[[i]] <- coo_baseline(Coo$coo[[i]], Coo$ldk[[i]][ldk1],
                                   Coo$ldk[[i]][ldk2], t1, t2)
    }
  } else {
    Coo$coo <- lapply(Coo$coo, coo_baseline, t1=t1, t2=t2)
  }

  return(Coo)
}

# 3. coo shape descriptors
# coo_centpos --------------
#' Calculate centroid coordinates
#'
#' Returns the (x; y) centroid coordinates of a shape.
#' @inheritParams coo_check
#' @return (x; y) coordinates of the centroid as a vector or a matrix.
#' @examples
#' b <- bot[1]
#' coo_plot(b)
#' xy <- coo_centpos(b)
#' points(xy[1], xy[2], cex=2, col='blue')
#' # on a Coo
#' coo_centpos(bot)
#' @family centroid functions
#' @family coo_ utilities
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

# coo_centsize --------------
#' Calculates centroid size
#' @inheritParams coo_check
#' @return \code{numeric}, the centroid size.
#' @details This function can be used to integrate size - if meaningful -
#' to Coo objects. See also \link{coo_length} and \link{rescale}.
#' @examples
#' coo_centsize(bot[1])
#' # on a Coo
#' coo_centsize(bot)
#' # add it to $fac
#' mutate(bot, size=coo_centsize(bot))
#' @family centroid functions
#' @family coo_utilities
#' @export
coo_centsize <- function(coo){
  UseMethod("coo_centsize")
}

#' @export
coo_centsize.default <- function(coo) {
  coo <- coo_check(coo)
  cp <- coo_centpos(coo)
  sqrt(mean((coo[, 1] - cp[1])^2 + (coo[, 2] - cp[2])^2))
}

#' @export
coo_centsize.Coo <- function(coo){
  sapply(coo$coo, coo_centsize)
}

# coo_centdist --------------
#' Returns the distance between everypoints and the centroid
#'
#' For every point of the shape, returns the (centroid-points) distance.
#' @aliases coo_centdist
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return a \code{matrix} of (x; y) coordinates.
#' @examples
#' b <- coo_sample(bot[1], 64)
#' d <- coo_centdist(b)
#' barplot(d, xlab="Points along the outline", ylab="Distance to the centroid (pixels)")
#' @family centroid functions
#' @family coo_ utilities
#' @export
coo_centdist <- function(coo){
  UseMethod("coo_centdist")
}
#' @export
coo_centdist.default <- function(coo) {
  coo <- coo_check(coo)
  return(apply(coo, 1, function(x) ed(coo_centpos(coo), x)))
}

#' @export
coo_centdist.Coo <- function(coo){
  lapply(coo$coo, coo_centdist)
}

# coo_perimpts --------------
#' Calculates perimeter and variations
#'
#' `coo_perim` calculates the perimeter;
#' `coo_perimpts` calculates the euclidean distance between every points of a shape;
#'`coo_perimcum` does the same and calculates and cumulative sum.
#' @param coo \code{matrix} of (x; y) coordinates or any `Coo`
#' @return \code{numeric} the distance between every point or
#' a `list` of those.
#' @examples
#' # for speed sake
#' b1 <- coo_sample(bot[1], 12)
#' b5 <- bot %>% slice(1:5) %>% coo_sample(12)
#'
#' # coo_perim
#' coo_perim(b1)
#' coo_perim(b5)
#'
#' # coo_perimpts
#' coo_perimpts(b1)
#' b5 %>% coo_perimpts()
#'
#' # coo_perimcum
#' b1 %>% coo_perimcum()
#' b5 %>% coo_perimcum()
#' @family perimeter functions
#' @family coo_ utilities
#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts <- function(coo) {
  UseMethod("coo_perimpts")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts.default <- function(coo) {
  coo <- coo_check(coo)
  n <- nrow(coo)
  d <- sqrt(apply((coo - coo_slide(coo, n))^2, 1, sum))[-1]
  return(d)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts.Coo <- function(coo) {
  lapply(coo$coo, coo_perimpts)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum <- function(coo) {
  UseMethod("coo_perimcum")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum.default <- function(coo) {
  coo <- coo_check(coo)
  d <- cumsum(sqrt(apply((coo - rbind(coo[1, ], coo[-(dim(coo)[1]),
                                                    ]))^2, 1, sum)))
  return(d)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum.Coo <- function(coo) {
  lapply(coo$coo, coo_perimcum)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim <- function(coo) {
  UseMethod("coo_perim")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim.default <- function(coo) {
  return(sum(coo_perimpts(coo)))
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim.Coo <- function(coo) {
  sapply(coo$coo, coo_perim)
}


# coo_calliper --------------
#' Calculates the calliper length
#'
#' Also called the Feret's diameter, the longest distance between two points of
#' the shape provided.
#' @aliases coo_calliper
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @param arr.ind \code{logical}, see below.
#' @return \code{numeric}, the centroid size. If \code{arr.ind=TRUE}, a `data_frame`.
#' @examples
#' b <- bot[1]
#' coo_calliper(b)
#' p <- coo_calliper(b, arr.ind=TRUE)
#' p
#' p$length
#' ids <- p$arr_ind[[1]]
#' coo_plot(b)
#' segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)
#'
#' # on a Coo
#' bot %>%
#' coo_sample(32) %>% # for speed sake
#' coo_calliper()
#'
#' bot %>%
#' coo_sample(32) %>% # for speed sake
#' coo_calliper(arr.ind=TRUE)
#'
#' @family calliper functions
#' @family coo_ utilities
#' @export
coo_calliper <- function(coo, arr.ind=FALSE){
  UseMethod("coo_calliper")
}

#' @export
coo_calliper.default <- function(coo, arr.ind = FALSE) {
  coo <- coo_check(coo)
  d <- dist(coo, method = "euclidean")
  # we check if there is no ex aequo
  ea <- length(which(d == max(d), arr.ind = TRUE))
  if (length(ea) > 1) {
    message("coo_length: at least two lengths are ex aequo")
  }
  if (arr.ind) {
    arr.ind <- which(as.matrix(d) == max(d), arr.ind = TRUE)
    # to return a vector (numeric and sorted) of the rows between
    # which the max length has been found
    arr.ind <- sort(as.numeric(arr.ind[1, ]))
    return(dplyr::tibble(length = max(d), arr_ind = list(arr.ind)))
  } else {
    return(max(d))
  }
}

#' @export
coo_calliper.Coo <- function(coo, arr.ind = FALSE) {
  if (arr.ind)
    lapply(coo$coo, coo_calliper, arr.ind=arr.ind) %>%
    do.call("rbind", .)
  else
    lapply(coo$coo, coo_calliper, arr.ind=arr.ind)
}

# coo_trim ------------------
#' Trims both ends coordinates from shape
#'
#' Removes \code{trim} coordinates at both ends of a shape, ie
#' from top and bottom of the shape matrix.
#' @inheritParams coo_check
#' @param trim \code{numeric}, the number of coordinates to trim
#' @family coo_ utilities
#' @family coo_trimming functions
#' @return a trimmed shape
#' @examples
#' olea[1] %>% coo_sample(12) %T>%
#'    print() %T>% ldk_plot() %>%
#'    coo_trim(1) %T>% print() %>% points(col="red")
#' @export
coo_trim <- function(coo, trim=1){
  UseMethod("coo_trim")
}

#' @export
coo_trim.default <- function(coo, trim=1){
  coo %<>% coo_check()
  return(coo[(trim+1):(nrow(coo)-trim),])
}

#' @export
coo_trim.Coo <- function(coo, trim=1){
  coo$coo %<>% lapply(coo_trim, trim)
}

#' Trims top coordinates from shape
#'
#' Removes \code{trim} coordinates from the top of a shape.
#' @inheritParams coo_check
#' @param trim \code{numeric}, the number of coordinates to trim
#' @return a trimmed shape
#' @family coo_ utilities
#' @family coo_trimming functions
#' @examples
#' olea[1] %>% coo_sample(12) %T>%
#'    print() %T>% ldk_plot() %>%
#'    coo_trimtop(4) %T>% print() %>% points(col="red")
#' @export
coo_trimtop <- function(coo, trim=1){
  UseMethod("coo_trimtop")
}

#' @export
coo_trimtop.default <- function(coo, trim=1){
  coo %<>% coo_check()
  return(coo[(trim+1):nrow(coo),])
}

#' @export
coo_trimtop.Coo <- function(coo, trim=1){
  coo$coo %<>% lapply(coo_trimtop, trim)
  # we also trim landmarks
  coo$ldk %<>% lapply(function(x) x[x > trim] - trim)
  coo
}


#' Trims bottom coordinates from shape
#'
#' Removes \code{trim} coordinates from the bottom of a shape.
#' @inheritParams coo_check
#' @param trim \code{numeric}, the number of coordinates to trim
#' @return a trimmed shape
#' @family coo_ utilities
#' @family coo_trimming functions
#' @examples
#' olea[1] %>% coo_sample(12) %T>%
#'    print() %T>% ldk_plot() %>%
#'    coo_trimbottom(4) %T>% print() %>% points(col="red")
#' @export
coo_trimbottom <- function(coo, trim=1){
  UseMethod("coo_trimbottom")
}

#' @export
coo_trimbottom.default <- function(coo, trim=1){
  coo %<>% coo_check()
  return(coo[1:(nrow(coo)-trim),])
}

#' @export
coo_trimbottom.Coo <- function(coo, trim=1){
  coo$coo %<>% lapply(coo_trimbottom, trim)
}

# distance utils ------------

#' Calculates euclidean distance between two points.
#'
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates.
#'
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm_nearest}, \link{dist}.
#' @examples
#' ed(c(0,1), c(1,0))
#' @export
ed <- function(pt1, pt2) {
  return(sqrt((pt1[1] - pt2[1])^2 + (pt1[2] - pt2[2])^2))
}

#' Calculates euclidean intermediate between two points.
#'
#' \code{edi} simply calculates coordinates of a points at the relative
#' distance \code{r} on the \code{pt1-pt2} defined by their (x; y) coordinates.
#' This function is used internally but may be of interest for other analyses.
#'
#' @param pt1 \eqn{(x; y)} coordinates of the first point.
#' @param pt2 \eqn{(x; y)} coordinates of the second point.
#' @param r the relative distance from \code{pt1} to \code{pt2}.
#' @return returns the \eqn{(x; y)} interpolated coordinates.
#' @seealso \link{ed}, \link{edm}.
#' @examples
#' edi(c(0,1), c(1,0), r = 0.5)
#' @export
edi <- function(pt1, pt2, r = 0.5) {
  return(r * (pt2 - pt1) + pt1)
}

#' Calculates euclidean distance every pairs of points in two matrices.
#'
#' \code{edm} returns the euclidean distances between points \eqn{1 -> n} of
#' two 2-col matrices of the same dimension. This function is used internally
#' but may be of interest for other analyses.
#'
#' If one wishes to align two (or more shapes) Procrustes surimposition may
#' provide a better solution.
#' @param m1 The first \code{matrix} of coordinates.
#' @param m2 The second \code{matrix} of coordinates.
#' @return Returns a \code{vector} of euclidean distances between pairwise
#' coordinates in the two matrices.
#' @seealso \link{ed}, \link{edm_nearest}, \link{dist}.
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm(x, x)
#' edm(x, x+1)
#' @export
edm <- function(m1, m2) {
  return(sqrt(apply((m1 - m2)^2, 1, sum)))
}

#' Calculates the shortest euclidean distance found for every point of one
#' matrix among those of a second.
#'
#' \code{edm_nearest} calculates the shortest euclidean distance found for
#' every point of one matrix among those of a second. In other words, if
#' \code{m1, m2} have \code{n} rows, the result will be the shortest distance
#' for the first point of \code{m1} to any point of \code{m2} and so on,
#' \code{n} times. This function is used internally but may be of interest for
#' other analyses.
#'
#' So far this function is quite time consumming since it performs \eqn{ n
#' \times n } euclidean distance computation.  If one wishes to align two (or
#' more shapes) Procrustes surimposition may provide a better solution.
#' @param m1 The first \code{list} or \code{matrix} of coordinates.
#' @param m2 The second \code{list} or \code{matrix} of coordinates.
#' @param full \code{logical}. Whether to returns a condensed version of the
#' results.
#' @return If \code{full} is \code{TRUE}, returns a \code{list} with two
#' components: \code{d} which is for every point of \code{m1} the shortest
#' distance found between it and any point in \code{m2}, and \code{pos} the
#' (\code{m2}) row indices of these points. Otherwise returns \code{d} as a
#' numeric vector of the shortest distances.
#' @seealso \link{ed}, \link{edm}, \link{dist}.
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm_nearest(x, x+rnorm(10))
#' edm_nearest(x, x+rnorm(10), full=TRUE)
#' @export
edm_nearest <- function(m1, m2, full = FALSE) {
  m1 <- coo_check(m1)
  m2 <- coo_check(m2)
  if (!is.matrix(m1) | !is.matrix(m2))
    stop("Matrices must be provided")
  if (ncol(m1) != 2 | ncol(m2) != 2)
    stop("2-cols matrices must be provided")
  nr <- nrow(m1)
  pos <- d <- numeric(nr)
  for (i in 1:nr) {
    m1.i <- m1[i, ]
    di <- apply(m2, 1, function(x) sqrt(sum((x - m1.i)^2)))
    d[i] <- min(di)
    pos[i] <- which.min(di)
  }
  if (full)
    return(list(d = d, pos = pos)) else return(d)
}

# end of coo_utilities
