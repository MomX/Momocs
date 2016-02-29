##### Various ldk utilities some could have been coo, but they
##### tend to be dedicated to landmarks

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


#' Creates links (all pariwise combinations) between landmarks
#'
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @return a matrix that can be passed to \link{ldk_links}, etc. The columns
#' are the row ids of the original shape.
#' @family ldk helpers
#' @examples
#' data(wings)
#' w <- wings[1]
#' coo_plot(w)
#' links <- links_all(w)
#' ldk_links(w, links)
#' @export
links_all <- function(coo) {
    coo <- coo_check(coo)
    links <- t(combn(1:nrow(coo), 2))
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
#' data(wings)
#' w <- wings[1]
#' coo_plot(w, poly=FALSE)
#' links <- links_delaunay(w)
#' ldk_links(w, links)
#' @export
links_delaunay <- function(coo) {
    coo <- coo_check(coo)
    links <- delaunayn(coo)
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
#' @return \code{numeric} that corresponds to the closest ids,
#' on the shape, from cliked points.
#' @examples
#' \dontrun{
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

#' Defines links between landmarks
#'
#' Works on Ldk objects, on 2cols matrices, 3dim arrays (msshapes turns it into a matrix).
#' @param x Ldk, matric or array
#' @param nb.ldk numeric the iterative procedure is stopped when the
#' user click on the top of the graphical window.
#' @family ldk helpers
#' @examples
#' \dontrun{
#' data(wings)
#' wm <- mshapes(wings)
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
    message("Click on the starting landmark...")
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
  m <- mshapes(x)
  links <- def_links.matrix(m, nb.ldk)
  x$links <- links
  return(x)
}

#' @export
def_links.array <- function(x, nb.ldk){
  if (length(dim(x)) == 3){
    x <- mshapes(x)
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
