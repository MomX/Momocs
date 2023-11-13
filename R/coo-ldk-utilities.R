# ldk utilities --------------------------------------------

#' Checks 'ldk' shapes
#'
#' A simple utility, used internally, mostly by \link{Ldk} methods,
#' in some graphical functions, and notably in \link{l2a}.
#' Returns an array of landmarks arranged as \code{(nb.ldk) x (x; y) x (nb.shapes)},
#' when passed with either a list, a matrix or an array of coordinates.
#' If a list is provided, checks that the number of landmarks is consistent.
#'
#' @param ldk a \code{matrix} of (x; y) coordinates, a list, or an array.
#' @return an \code{array} of (x; y) coordinates.
#' @family ldk helpers
#' @examples
#' #coo_check('Not a shape')
#' #coo_check(matrix(1:10, ncol=2))
#' #coo_check(list(x=1:5, y=6:10))
#' @export
ldk_check <- function(ldk) {
    if (is.array(ldk)) {
        if (length(dim(ldk) == 3)) {
            return(ldk)
        }
        if (length(dim(ldk) == 2)) {
            return(array(ldk, dim = c(nrow(ldk), ncol(ldk), 1)))
        }
        stop("a matrix an array (dim=3) must be provided")
    }
    if (is.list(ldk)) {
        l <- sapply(ldk, length)
        if (length(unique(l)) == 1) {
            return(l2a(ldk))
        }
        stop("a list of matrices with the same number of coordinates must be provided")
    }
    stop("a list, a matrix or a dim=3 array must be provided")
}


#' Creates links (all pairwise combinations) between landmarks
#'
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a matrix that can be passed to \link{ldk_links}, etc. The columns
#' are the row ids of the original shape.
#' @family ldk helpers
#' @examples
#' w <- wings[1]
#' coo_plot(w)
#' links <- links_all(w)
#' ldk_links(w, links)
#' @export
links_all <- function(coo) {
    coo <- coo_check(coo)
    links <- t(utils::combn(1:nrow(coo), 2))
    return(links)
}

#' Creates links (Delaunay triangulation) between landmarks
#'
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a matrix that can be passed to \link{ldk_links}, etc. The columns
#' are the row ids of the original shape.
#' @details uses \link{delaunayn} in the \code{geometry} package.
#' @family ldk helpers
#' @examples
#' w <- wings[1]
#' coo_plot(w, poly=FALSE)
#' links <- links_delaunay(w)
#' ldk_links(w, links)
#' @export
links_delaunay <- function(coo) {
    coo <- coo_check(coo)
    links <- geometry::delaunayn(coo)
    links <- rbind(links[, -1], links[, -2], links[, -3])
    links <- links[-which(duplicated(links)), ]
    return(links)
}


#' Defines landmarks interactively
#'
#' Allows to interactively define a \code{nb.ldk} number of landarks on a shape.
#' Used in other facilities to acquire/manipulate data.
#' @param coo a \code{matrix} or a list of (x; y) coordinates.
#' @param nb.ldk \code{integer}, the number of landmarks to define
#' @param close \code{logical} whether to close (typically for outlines)
#' @param points \code{logical} whether to display points
#' @return \code{numeric} that corresponds to the closest ids,
#' on the shape, from cliked points.
#' @examples
#' \dontrun{
#' b <- bot[1]
#' coo_ldk(b, 3) # run this, and click 3 times
#' coo_ldk(bot, 2) # this also works on Out
#' }
#' @export
coo_ldk <- function(coo, nb.ldk, close=FALSE, points=TRUE) {
  if (is.list(coo))
    coo <- l2m(coo)
  if (close) coo <- coo_close(coo)
  coo_plot(coo, points=points)
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

#' Defines links between landmarks
#'
#' Works on Ldk objects, on 2-cols matrices, 3-dim arrays ([MSHAPES] turns it into a matrix).
#' @param x Ldk, matric or array
#' @param nb.ldk numeric the iterative procedure is stopped when the
#' user click on the top of the graphical window.
#' @family ldk helpers
#' @return a Momocs object of same class
#' @examples
#' \dontrun{
#' wm <- MSHAPES(wings)
#' links <- def_links(wm, 3) # click to define pairs of landmarks
#' ldk_links(wm, links)
#' }
#' @export
def_links <- function(x, nb.ldk){
  UseMethod("def_links")
}


#' @export
def_links.matrix <- function(x, nb.ldk){
  def_2ldk <- function(x, hmax){
    res <- numeric(2)
    # 1st point
    message("click on the starting landmark...")
    xy <- as.numeric(locator(1))
    if (!missing(hmax)) { if (xy[2] >= hmax) return() }
    d <- (x[, 1] - xy[1])^2 + (x[, 2] - xy[2])^2
    res[1] <- which.min(d)
    # 2nd point
    message("...on the ending landmark")
    xy <- as.numeric(locator(1))
    if (!missing(hmax)) { if (xy[2] >= hmax) return() }
    d <- (x[, 1] - xy[1])^2 + (x[, 2] - xy[2])^2
    res[2] <- which.min(d)
    return(res)
  }


  ldk_plot(x)
  ldk_labels(x)

  links <- matrix(NA, nrow=ifelse(missing(nb.ldk), 0, nb.ldk), ncol=2)

  # case where nb.ldk is specified
  if (!missing(nb.ldk)){
    for (i in 1:nb.ldk){
      cat(" * [", i, "/", nb.ldk, "] - ")
      links[i, ] <- def_2ldk(x)
    }
    return(unique(links))
  } else {
    # case where nb.ldk is specified
    hmax <- par("usr")[4]*0.99
    abline(h=hmax)
    text(x=mean(par("usr")[1:2]), y=hmax, pos = 3, labels = "end")
    over = FALSE
    while (!over){
      cat(" *", nrow(links)+1, "th link - ")
      links.i <- def_2ldk(x, hmax)
      if (!is.null(links.i)) {
        links <- rbind(links, links.i)
      } else {
        over = TRUE
      }
    }
    rownames(links) <- NULL
    return(unique(links))
  }
}

#' @export
def_links.Ldk <- function(x, nb.ldk){
  m <- MSHAPES(x)
  links <- def_links.matrix(m, nb.ldk)
  x$links <- links
  return(x)
}

#' @export
def_links.array <- function(x, nb.ldk){
  if (length(dim(x)) == 3){
    x <- MSHAPES(x)
    links <- def_links.matrix(x)
  } else {
    stop("only defined on Ldk objects, matrices and 3-dim arrays")
  }
  return(links)
}

#' @export
def_links.default <- function(x, nb.ldk){
    stop("only defined on Ldk objects, matrices and 3-dim arrays")
}


# handling $ldk --------------------------------------------

#' Defines new landmarks on Out and Opn objects
#'
#' Helps to define landmarks on a \code{Coo} object.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to define on every shape
#' @param close \code{logical} whether to close (typically for outlines)
#' @param points \code{logical} whether to display points
#' @return an Out or an Opn object with some landmarks defined
#' @family ldk/slidings methods
#' @examples
#' \dontrun{
#' bot <- bot[1:5] # to make it shorter to try
#' # click on 3 points, 5 times.
#' # Don't forget to save the object returned by def_ldk...
#' bot2 <- def_ldk(bot, 3)
#' stack(bot2)
#' bot2$ldk
#' }
#' @export
#' @export
def_ldk <- function(Coo, nb.ldk, close, points) {
  UseMethod("def_ldk")
}
#' @export
def_ldk.Out <- function(Coo, nb.ldk, close=TRUE, points=FALSE) {
  if (missing(nb.ldk))
    stop("'nb.ldk' must be specified")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    cat(i, "/", length(Coo$coo), " ")
    Coo$ldk[[i]] <- coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk, close=close, points=points)
  }
  return(Coo)
}
#' @export
def_ldk.Opn <- def_ldk.Out


#' Add new landmarks based on angular positions
#'
#' A wrapper on \link{coo_intersect_angle} and \link{coo_intersect_direction} for
#' \link{Out} and \link{Opn} objects.
#'
#' @param coo a \code{Out} or \code{Opn} object
#' @param angle \code{numeric} an angle in radians (0 by default).
#' @param direction \code{character} one of \code{"down", "left", "up", "right"} ("right" by default)
#' @note any existing ldk will be preserved.
#' @return a Momocs object of same class
#' @seealso Typically used before \link{coo_slice} and \link{coo_slide}.
#'  See \link{def_ldk_tips} as well.
#'
#' @examples
#' # adds a new landmark towards south east
#' hearts %>%
#'    slice(1:5) %>% # for speed purpose only
#'    def_ldk_angle(-pi/6) %>%
#' stack()
#'
#' # on Out and towards NW and NE here
#' olea %>%
#'    slice(1:5) %>% #for speed purpose only
#'    def_ldk_angle(3*pi/4) %>%
#'    def_ldk_angle(pi/4) %>%
#'    stack
#'
#' @export
def_ldk_angle <- function(coo, angle){
  UseMethod("def_ldk_angle")
}

#' @export
def_ldk_angle.default <- function(coo, angle){
  stop("only defined on Out and Opn")
}

#' @export
def_ldk_angle.Out <- function(coo, angle){
  # for Coo with no ldks
  if (!is_ldk(coo))
    coo$ldk <- vector("list", length(coo))
  # grab the new id
  new_ldk <- lapply(coo$coo, coo_intersect_angle, angle)
  # add it to $ldk and return the Coo
  coo$ldk <- mapply(c, coo$ldk, new_ldk, SIMPLIFY = FALSE)
  coo
}

#' @export
def_ldk_angle.Opn <- def_ldk_angle.Out

#' @rdname def_ldk_angle
#' @export
def_ldk_direction <-
  function(coo, direction=c("down", "left", "up", "right")[4]){
  UseMethod("def_ldk_direction")
}

#' @rdname def_ldk_angle
#' @export
def_ldk_direction.default <-
  function(coo, direction=c("down", "left", "up", "right")[4]){
  stop("only defined on Out and Opn")
}

#' @rdname def_ldk_angle
#' @export
def_ldk_direction.Out <-
  function(coo, direction=c("down", "left", "up", "right")[4]){
  # for Coo with no ldks
  if (!is_ldk(coo))
    coo$ldk <- vector("list", length(coo))
  # grab the new id
  new_ldk <- lapply(coo$coo, coo_intersect_direction, direction)
  # add it to $ldk and return the Coo
  coo$ldk <- mapply(c, coo$ldk, new_ldk, SIMPLIFY = FALSE)
  coo
}

#' @rdname def_ldk_angle
#' @export
def_ldk_direction.Opn <- def_ldk_direction.Out

#' Define tips as new landmarks
#'
#' On \link{Opn} objects, this can be used before \link{coo_slice}. See examples.
#'
#' @param coo \code{Opn} object
#' @note any existing ldk will be preserved.
#' @return a Momocs object of same class
#' @examples
#' is_ldk(olea) # no ldk for olea
#' olea %>%
#' slice(1:3) %>% #for the sake of speed
#' def_ldk_tips %>%
#' def_ldk_angle(3*pi/4) %>% def_ldk_angle(pi/4) %T>% stack %>%
#' coo_slice(ldk=1:4) -> oleas
#' stack(oleas[[1]])
#' stack(oleas[[2]]) # etc.
#' @export
def_ldk_tips <- function(coo){
  UseMethod("def_ldk_tips")
}

#' @export
def_ldk_tips.default <- function(coo){
  stop("only defined on Opn")
}

#' @export
def_ldk_tips.Opn <- function(coo){
  # for Coo with no ldks
  if (!is_ldk(coo))
    coo$ldk <- vector("list", length(coo))
  # first ldk is trivial
  first_ldk <- rep(1, length(coo))
  last_ldk  <- coo_nb(coo)
  # add it to $ldk and return the Coo
  coo$ldk <- mapply(c, first_ldk, coo$ldk, last_ldk, SIMPLIFY = FALSE)
  coo
}

#' Adds new landmarks on Out and Opn objects
#'
#' Helps to add new landmarks on a \code{Coo} object on top of existing ones.
#' The number of landmarks must be specified and rows indices that
#' correspond to the nearest points clicked on every outlines are
#' stored in the \code{$ldk} slot of the \code{Coo} object.
#' @param Coo an Out or Opn object
#' @param nb.ldk the number of landmarks to add on every shape
#' @return an Out or an Opn object with some landmarks defined
#' @details Note that if no landmarks are already defined,
#' then this function is equivalent to \link{def_ldk}.
#' @family ldk/slidings methods
#' @examples
#' \dontrun{
#' hearts <- slice(hearts, 1:5) # to make it shorter to try
#' # click on 3 points, 5 times.
#' hearts <- def_ldk(hearts, 3)
#' # Don't forget to save the object returned by def_ldk...
#' hearts2 <- add_ldk(hearts, 3)
#' stack(hearts2)
#' hearts2$ldk
#' }
#' @export
add_ldk <- function(Coo, nb.ldk) {
  UseMethod("add_ldk")
}
#' @export
add_ldk.Out <- function(Coo, nb.ldk) {
  if (missing(nb.ldk))
    stop("'nb.ldk' must be specified")
  ldk <- list()
  for (i in seq(along = Coo$coo)) {
    cat(i, "/", length(Coo$coo), " ")
    Coo$ldk[[i]] <- c(Coo$ldk[[i]], coo_ldk(Coo$coo[[i]], nb.ldk = nb.ldk))
  }
  return(Coo)
}
#' @export
add_ldk.Opn <- add_ldk.Out


#' Retrieves landmarks coordinates
#'
#' See Details for the different behaviors implemented.
#'
#' @param Coo an Out, Opn or Ldk object
#' @return a list of shapes
#' @details Different behaviors depending on the class of the object:
#' \itemize{
#' \item \link{Ldk}: retrieves landmarks.
#' \item Ldk with slidings defined: retrieves only the fixed landmarks, not the sliding ones.
#' See also \link{get_slidings}.
#' \item \link{Out} landmarks from \code{$ldk} and \code{$coo}, if any.
#' \item \link{Opn}: same as above.
#' }
#' @family ldk/slidings methods
#' @examples
#' # Out example
#' ldk.h <- get_ldk(hearts)
#' stack(Ldk(ldk.h))
#'
#' # on Ldk (no slidings)
#' get_ldk(wings) # equivalent to wings$coo
#'
#' # on Ldk (slidings)
#' get_ldk(chaff)
#' get_ldk(chaff) %>% Ldk %>% fgProcrustes(tol=0.1) %>% stack
#' @export
get_ldk <- function(Coo) {
  UseMethod("get_ldk")
}

#' @export
get_ldk.Ldk <- function(Coo){
  # sliding case
  # we need to retrieve all sliding landmarks
  # (including first and last from all partitions)
  if (is_slidings(Coo)){
    all_ids <- 1:unique(coo_nb(Coo))
    sliding_ids <- Coo %>% slidings_scheme() %$% id %>%
      apply(1, function(x) x[1]:x[2])  %>% as.numeric()
    fixed_ids   <- all_ids[-sliding_ids]
    ldk <- lapply(Coo$coo, function(x) x[fixed_ids, ])
    return(ldk)
  } else {
    Coo$coo
  }
}

#' @export
get_ldk.Out <- function(Coo) {
  if (!is_ldk(Coo))
    return(NULL)
  coo <- Coo$coo
  ldk <- Coo$ldk
  ref <- array(NA, dim = c(length(ldk[[1]]), ncol(coo[[1]]),
                           length(coo)))
  for (i in seq(along = coo)) {
    ref[, , i] <- coo[[i]][ldk[[i]], ]
  }
  # cases where single ldk (otherwise converted to a numeric)
  res <- lapply(a2l(ref), function(x) matrix(x, ncol=2))
  return(res)
}
#' @export
get_ldk.Opn <- get_ldk.Out

#' Rearrange, (select and reorder) landmarks to retain
#'
#' Helps reorder and retain landmarks by simply changing the order in which they
#' are recorded in the \code{Coo} objects. Note that for \code{Out} and \code{Opn}
#'  objects, this rearranges the \code{$ldk} component. For \code{Ldk}, it rearranges
#'   the \code{$coo} directly.
#'
#' @param Coo any appropriate \code{Coo} object (typically an \code{Ldk})
#' with landmarks inside
#' @param new_ldk_ids a vector of numeric with the ldk to retain \emph{and}
#' in the right order (see below)
#' @return a Momocs object of same class
#' @family ldk/slidings methods
#' @examples
#' # Out example
#' hearts %>% slice(1) %T>% stack %$% ldk
#' hearts %>% rearrange_ldk(c(4, 1)) %>%
#'        slice(1) %T>%stack %$% ldk
#'
#' # Ldk example
#' wings %>% slice(1) %T>% stack %$% coo
#' wings %>% rearrange_ldk(c(1, 3, 12:15)) %>%
#'       slice(1) %T>% stack %$% coo
#' @export
rearrange_ldk <- function(Coo, new_ldk_ids){
  UseMethod("rearrange_ldk")
}

#' @export
rearrange_ldk.default <- function(Coo, new_ldk_ids){
  message("only defined on Coo objects")
}

#' @export
rearrange_ldk.Out <- function(Coo, new_ldk_ids){
  if (is.null(get_ldk(Coo))){
    message("no ldk to arrange")
    return(Coo)
  }
  Coo$ldk %<>% lapply(function(.) .[new_ldk_ids])
  return(Coo)
}

#' @export
rearrange_ldk.Opn <- rearrange_ldk.Out

#' @export
rearrange_ldk.Ldk <- function(Coo, new_ldk_ids){
  if (is.null(get_ldk(Coo))){
    message("no ldk to arrange")
    stop()
  }
  Coo$coo %<>% lapply(function(.) .[new_ldk_ids, ])
  return(Coo)
}

# sliding getters/setters ------------------------------------------------------

# prepares a matrix for sliding landmarks ($slidings in Ldk)
.slidings_matrix <- function(nrow=0){
  matrix(NA, nrow=nrow, ncol=3, dimnames = list(NULL, c("before", "slide", "after")))
}

# given a partition, create and fill a $slidings matrix
# partition can be passed as numeric, or list of numeric
# for both cases, the first and last points are considered fixed
# and not allowed to slide
# .slidings_def_from_partition(4:9)
# .slidings_def_from_partition(list(4:9, 34:45))
# .slidings_def_from_partition("error")
.slidings_def_from_partition <- function(x){
  .check(is.numeric(x) | is.list(x), "partition(s) must be a numeric or a list of numeric")
  # single partition passed as a numeric
  if (is.numeric(x)){
    .check(length(x)>=3, "partition(s) must contain at least 3 points")
    # first and last points are fixed
    x_sliding <- x[2]:x[length(x)-1]
    # prepare the slidings matrix
    slidings <- .slidings_matrix(length(x_sliding))
    # fill it
    for (i in seq_along(x_sliding))
      slidings[i, ] <- x_sliding[i] + c(-1, 0, 1)
  }
  # multi partition cased, passed as a list
  if (is.list(x)){
    .check(all(sapply(x, is.numeric)),
           "all partitions must be numeric")
    slidings_list <- vector("list", length(x))
    for (i in seq_along(x))
      slidings_list[[i]] <- .slidings_def_from_partition(x[[i]])
    slidings <- do.call(rbind, slidings_list)
  }
  # return this beauty
  return(slidings)
}

# deduces partition scheme from sliding matrix
.slidings_scheme <- function(x){
  .check(is.matrix(x), "slidings must be a matrix")
  .check(ncol(x)==3,   "slidings must be a 3-columns matrix")
  d <- which(diff(x[, 1])>1)
  # nb of partitions
  n <- length(d)+1
  # deduce their position
  id <- cbind(c(x[1, 1], x[d+1, 1]), c(x[d, 3], x[nrow(x), 3]))
  # cosmetics
  dimnames(id) <- list(paste0("partition", 1:nrow(id)), c("start", "end"))
  return(list(n=n, id=id))
}


#' Extracts partitions of sliding coordinates
#'
#' Helper function that deduces (likely to be a reminder)
#' partition scheme from \code{$slidings} of \code{Ldk} objects.
#'
#' @param Coo an Ldk object
#' @return a list with two components: \code{n} the number of partition; \code{id}
#' their position. Or a NULL if no slidings are defined
#' @family ldk/slidings methods
#' @examples
#' # no slidings defined a NULL is returned with a message
#' slidings_scheme(wings)
#'
#' # slidings defined
#' slidings_scheme(chaff)
#'
#' @export
slidings_scheme <- function(Coo){
  UseMethod("slidings_scheme")
}

#' @export
slidings_scheme.default <- function(Coo){
  stop("only defined on Ldk")
}

#' @export
slidings_scheme.Ldk <- function(Coo){
  if(!is_slidings(Coo)){
    message("no sliding defined")
    return(NULL)}
  .slidings_scheme(Coo$slidings)
}

#' Defines sliding landmarks matrix
#' @param Coo an \link{Ldk} object
#' @param slidings a matrix, a numeric or a list of numeric. See Details
#' @details \code{$slidings} in \link{Ldk} must be a 'valid' matrix: containing
#' ids of coordinates, none of them being lower than 1 and higher the number of coordinates
#' in \code{$coo}.
#'
#' \code{slidings} matrix contains 3 columns (\code{before}, \code{slide}, \code{after}).
#' It is inspired by \code{geomorph} and should be compatible with it.
#'
#' This matrix can be passed directly if the \code{slidings} argument is a matrix. Of course,
#' it is strictly equivalent to \code{Ldk$slidings <- slidings}.
#'
#' \code{slidings} can also be passed as "partition(s)", when sliding landmarks
#' identified by their ids (which are a row number) are consecutive in the \code{$coo}.
#'
#' A single partition can be passed either as a numeric (eg \code{4:12}), if points
#' 5 to 11 must be considered as sliding landmarks (4 and 12 being fixed); or as a list of numeric.
#'
#' See examples below.
#' @return a Momocs object of same class
#' @family ldk/slidings methods
#' @examples
#' #waiting for a sliding dataset...
#'
#' @export
def_slidings <- function(Coo, slidings){
  UseMethod("def_slidings")
}

#' @export
def_slidings.default <- function(Coo, slidings){
  stop("only defined on Ldk")
}

#' @export
def_slidings.Ldk <- function(Coo, slidings){
  .check((is.numeric(slidings) | is.matrix(slidings) | is.list(slidings)),
         "sliding must be a matrix, a numeric or a list of numeric")
  # matrix case
  if (is.matrix(slidings))
    Coo$slidings <- slidings
  else
    Coo$slidings <- .slidings_def_from_partition(slidings)
  return(Coo)
}

#' Extracts sliding landmarks coordinates
#'
#' From an \link{Ldk} object.
#'
#' @param Coo an Ldk object
#' @param partition numeric which one(s) to get.
#' @return a list of list(s) of coordinates.
#' @family ldk/slidings methods
#' @examples
#' # for each example below a list with partition containign shapes is returned
#' # extracts the first partition
#' get_slidings(chaff, 1) %>% names()
#' # the first and the fourth
#' get_slidings(chaff, c(1, 4)) %>%  names()
#' # all of them
#' get_slidings(chaff) %>%  names
#' # here we want to see it
#' get_slidings(chaff, 1)[[1]] %>%  Ldk %>% stack
#' @export
get_slidings <- function(Coo, partition){
  UseMethod("get_slidings")
}

#' @export
get_slidings.default <- function(Coo, partition){
  stop("only defined on Ldk")
}

#' @export
get_slidings.Ldk <- function(Coo, partition){
  .check(is_slidings(Coo), "no slidings defined")
  # we retrieve the scheme
  scheme <- .slidings_scheme(Coo$slidings)
  n  <- scheme$n
  # all by default
  if (missing(partition))
    partition <- 1:n
  id <- scheme$id[partition, ]
  if (is.numeric(id)) id <- matrix(id, ncol=2)
  # nice try
  .check(all(partition<=n), "some partition do not exist")
  # prepare the nest
  slidings <- vector("list", length(partition))
  names(slidings) <- rownames(scheme$id)[partition]
  # loop and grab
  for (i in 1:nrow(id)){
    slidings[[i]] <- lapply(Coo$coo, coo_extract, id[i, 1]:id[i, 2])
  }
  return(slidings)
}
