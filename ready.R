# ready -------
rm(list=ls())
devtools::load_all("~/Research/Momocs")

#' List available methods for any object
#'
#' A simple wrapper around \link{methods} that given any object
#' returns all available methods for its class.
#'
#' @param x any object
#' @examples
#' methods for data.frame
#' list_methods(iris)
#'
#' methods for Coo objects
#' list_methods(bot)
#'
#' methods for Coe objects
#' list_methods(bot %>% efourier)
#'
#' methods for PCA objects
#' list_methods(bot %>% efourier %>% PCA)
#'
#' @export
list_methods <- function(x){
  for (i in class(x)){
    cat("* '", i, "' class:\n", sep="")
    cat(paste0("  - ", methods(class=i), "\n"), sep="")
  }
}

tell_methods <- function(x) {
  m <- try(suppressWarnings(methods(x)), silent=TRUE)
  if (!("try-error" %in% class(m)) && length(m)!=0)
    cat(paste0("  - ", m, "\n"), sep="")
}
tell_methods(PCAAA) # nothing
tell_methods(PCA)
tell_methods(sum) # nothing

coo_chull_onion <- function(coo, ...){
  res <- list()
  i <- 1
  while(is.matrix(coo) && nrow(coo) > 3){
    chi_ids <- chull(coo[, 1], coo[, 2])
    res$ids[[i]] <- chi_ids
    res$coo[[i]] <- coo[chi_ids, ]
    coo <- coo[-chi_ids, ]
    i <- i + 1
  }
  res
}
# shapes[4] %>% coo_chull_onion


draw_rads <- function(shp, ...){
  segments(shp[, 1], shp[, 2], coo_centpos(shp)[1], coo_centpos(shp)[2], ...)
}
# see coo_samplerr for an example


# turns many things into a coo (formely a shp)
prepend_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(class_to_add, .)
  x
}

append_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(., class_to_add)
  x
}

as_out <- function(x){
  x$coo %<>% lapply(prepend_class, "out")
  x
}


# bot %>% class
# add_class(bot, "Out") %>% class()
# add_class(bot, "Coo") %>% class()
# add_class(bot, "plop") %>% class()

as_coo <- function(x, ...){
  x %>% coo_check() %>% prepend_class("coo")
}

coo_boundingbox <- function(coo){
  coo %>% apply(2, range) %>% as.numeric() %>%
    sapply(list) %>% `names<-`(c("x0", "x1", "y0", "y1"))
}

# see http://en.wikipedia.org/wiki/Shoelace_formula
is_clockwise <- function(coo){
  res <- numeric(nrow(coo)-1)
  for (i in seq_along(res)){
    res[i] <- (coo[i+1, 1] - coo[i, 1]) * (coo[i+1, 2] - coo[i, 2])
  }
  sum(res)>0
}

is_anticlockwise <- function(coo){
  !is_clockwise(coo)
}

complex2coo <- function(Z){
  cbind(Re(Z), Im(Z)) %>% `colnames<-`(c("x", "y"))
}

.axes_corner <- function(bb, w, col="grey20", signif=2, lwd=0.5, cex=0.5){
  # if required we prettify bb coordinates
  if (is.numeric(signif))
    pretty_bb <- sapply(bb, signif, signif)
  else
    pretty_bb <- bb
  # ticks
  segments(bb$x0, bb$y0, bb$x0, bb$y0-w, lwd=lwd, col=col)
  segments(bb$x1, bb$y1, bb$x1, bb$y1+w, lwd=lwd, col=col)
  segments(bb$x0, bb$y0, bb$x0-w, bb$y0, lwd=lwd, col=col)
  segments(bb$x1, bb$y1, bb$x1+w, bb$y1, lwd=lwd, col=col)
  # and their legend
  text(bb$x0,   bb$y0-w, pretty_bb[1], cex=cex, col=col, adj=c(0.5,  5/3))
  text(bb$x1,   bb$y1+w, pretty_bb[2], cex=cex, col=col, adj=c(0.5, -2/3))
  text(bb$x0-w, bb$y0,   pretty_bb[3], cex=cex, col=col, adj=c(5/3,  0.5))
  text(bb$x1+1, bb$y1,   pretty_bb[4], cex=cex, col=col, adj=c(-2/3, 0.5))
}

# working section --------

as_out <- function(x){
  x$coo %<>% lapply(prepend_class, "out")
  x
}

g <- function(x, axes_corner=TRUE, ...){
  # dots <- as.list(sys.call())[-(1:2)]
  dots <- cook_dots()
  # return(dots)
  # preliminaries ------------------
  # defines a new par, saves old one and restore it on exit
  op <- par(mai=rep(0, 4), mar=rep(0, 4), xpd=NA)
  on.exit(par(op))
  # bounding box coordinates
  bb <- coo_boundingbox(x)
  # initializes plot window
  plot(NA, xlim=c(bb$x0, bb$x1), ylim=c(bb$y0, bb$y1),
       asp=1, axes=FALSE, ann=FALSE)
  # grabs window actual coordinates
  usr <- par("usr")
  w <- min(.wdw()*(1/100))

  # layers ------------
  do_with_args("chessboard", dots)


  # if coo spans origins, then draw x- and y- axes
  span_origin <- bb$x0<0 & bb$x1>0 & bb$y0 < 0 & bb$y1 > 0
  if (span_origin){
    # abline(h=0, v=0, col="grey80", lty="dotdash", lwd=0.5)
    segments(usr[1], 0, usr[2], 0, col="grey80", lty="dotdash", lwd=0.5)
    segments(0, usr[3], 0, usr[4], col="grey80", lty="dotdash", lwd=0.5)
  }

  # deduced layers ------------------

  do_with_args("axes_corner", dots, with=list(bb=bb, w=w))
  # draws a cross for centroid position
  cp <- coo_centpos(x)
  segments(cp[1]-w/2, cp[2], cp[1]+w/2, cp[2], lwd=0.5, col="grey20")
  segments(cp[1], cp[2]-w/2, cp[1], cp[2]+w/2, lwd=0.5, col="grey20")

  # outline case -------
  # arrow on first point
  x1 <- x[1, ]
  x2 <- x[2, ]
  # theta <- atan2(x2[2] - x1[2], x2[1] - x1[1]) * (180 / pi) - 90
  # text(x1[1], x1[2], labels = "^", cex=1, srt=theta)

  # new arrow
  theta <- atan2(x2[2] - x1[2], x2[1] - x1[1]) * (180 / pi)
  # starts from x1 and do 1/100 of the road to centroid
  a0 <- edi(x1, cp, -1/20)
  a1 <- edi(x2, cp, -1/20)
  a1 <- edi(a0, a1, w*2 / ed(a0, a1))
  arrows(a0[1], a0[2], a1[1], a1[2], lwd=0.5, col="grey20", length=0.05)
  # given the length of the arrow, some trig to deduce a1
  # a1 <- c(a0[1] - cos(theta)*w, a0[2] - sin(theta)*w)
  # arrows(a0[1], a0[2], a1[1], a1[2], lwd=0.5, col="grey20", length=0.05)

  # outline drawing
  lines(x, col="grey75")
  points(x, pch=20, cex=1/5)

  # return(dots)
}

prepare_dots <- function(x, gare=TRUE, ...){
  cook_dots()
}

cook_dots <- function(){
  # get full call as a list
  s <- as.list(sys.call(1))
  # return(s)
  # case where passed within a pipe
  if (s[[1]]=="%>%")
    s <- as.list(s[[3]])
  # get formals list
  f <- formals(as.character(s[[1]]))
  # return(list(s=s, f=f))
  # remove any "..." from formals list
  dots_id <- match("...", names(f))
  if (!is.na(dots_id))
    f <- f[-dots_id]
  # remove function name from the call
  s <- s[-1]
  # don't include unnammed arguments
  f <- f[names(f)!=""]
  s <- s[names(s)!=""]

  vetoed <- intersect(names(f), names(s))
  c(f[!(names(f) %in% vetoed)], s)
}

dots <- list(x="", chessboard=T, axes_corner=TRUE)
what="chessboard"
with=NULL

do_with_args <- function(what, dots, with=NULL){
  dot_what <- paste0(".", what)
  what_dot <- paste0(what, ".")
  # turns dots into a named list
  l <- dots
  # tests for empty list
  if (length(l)==0)
    return()
  args <- l %>% names %>%
    # only take what's on the left of the dot
    strsplit("\\.") %>% sapply(`[`, 1) %>%
    # test if a least one correspond to the function of interest
    `%in%`(., what)
  # test for corresponding function
  if (!any(args))
    return()
  # only pick corresponding arguments
  l <- l[args]


  # case where only the function with default argument is asked
  if (what %in% names(l)){
    l_flag <- l[[which(names(l)==what)]]
    if (is.logical(l_flag) && !l_flag)
      return()
    do.call(what=dot_what, args=c(list(), with))
    return()
  }
  # removes the "functioname" pattern (if a default value for plotting
  # the layer is declared in the generic)
  l <- l[!(names(l) %in% what)]
  # removes the "functionname." pattern
  names(l) %<>% gsub(what_dot, "", .)
  # finally call the function which name needs a '.' before
  # here a special Momocs case where
  do.call(dot_what, args=c(l, with))
  return()
}


coo_plot(bot[1])
bb <- coo_boundingbox(bot[1])
w <- min(.wdw()*(1/100))
do.call(".axes_corner", list(bb=bb, w=w))

# x <- shapes[18] %>% coo_sample(360) %>% coo_slidedirection("E") %>%  coo_rev
bot[1] %>% g()
g(bot[1], chessboard=T)

links <- replicate(2, sample(1:12, 6))
wings[1] %>% g()
ldk_links(wings[1], links)



