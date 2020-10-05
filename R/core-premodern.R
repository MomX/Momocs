#' A wrapper to calculates euclidean distances between two points
#'
#' The main advantage over \link{ed} is that it is a method that can
#' be passed to different objects and used in combination with \link{measure}.
#' See examples.
#'
#' @param x a Ldk (typically), an Out or a matrix
#' @param id1 id of the 1st row
#' @param id2 id of the 2nd row
#' @note On Out objects, we first \link{get_ldk}.
#' @seealso if you want all pairwise combinations, see \link{coo_truss}
#' @examples
#' # single shape
#' d(wings[1], 1, 4)
#' # Ldk object
#' d(wings, 1, 4)
#' # Out object
#' d(hearts, 2, 4)
#' @export
d <- function(x, id1, id2){
  UseMethod("d")
}

#' @export
d.default <- function(x, id1, id2){
  if (length(id1)==2 & missing(id2)){
    id2 <- id1[2]
    id1 <- id1[1]
  }
  id1 %<>% as.numeric()
  id2 %<>% as.numeric()
  ed(x[id1,, drop=FALSE], x[id2,, drop=FALSE])
}

#' @export
d.Ldk <- function(x, id1, id2){
  sapply(x$coo, d, id1, id2)
}

#' @export
d.Out <- function(x, id1, id2){
  get_ldk(x) %>% Ldk() %>% d(id1, id2)
}

#' Measures shape descriptors
#'
#' Calculates shape descriptors on Coo and other objects.
#' Any function that returns a scalar when fed coordinates can be passed
#' and naturally those of Momocs (pick some there \code{apropos("coo_")}). Functions
#' without arguments (eg \link{coo_area}) have to be passed without brackets but
#' functions with arguments (eg \link{d}) have to be passed "entirely". See examples.
#' @param x any \code{Coo} object, or a list of shapes, or a shape as a matrix.
#' @param ... a list of functions. See examples.
#' @return a \link{TraCoe} object, or a raw data.frame
#' @family premodern
#' @examples
#' bm <- measure(bot, coo_area, coo_perim)
#' bm
#' bm$coe
#'
#' # how to use arguments, eg with the d() function
#' measure(wings, coo_area, d(1, 3), d(4, 5))
#'
#' # alternatively, to get a data_frame
#' measure(bot$coo, coo_area, coo_perim)
#'
#' # and also, to get a data_frame (one row)
#' measure(bot[1], coo_area, coo_perim)
#' @export
measure <- function(x, ...){
  UseMethod("measure")
}

#' @export
measure.default <- function(x, ...){
  stop("only defined on Coo and lists objects")
}

#' @export
measure.matrix <- function(x, ...){
  x %>% coo_check %>% list() %>% measure(...)
}

#' @export
measure.list <- function(x, ...){
  funs <- as.character(substitute(list(...)))[-1L]
  l <- vector("list", length(funs))
  names(l) <- gsub("coo_", "", funs)
  x %<>% lapply(coo_check)
  for (i in seq_along(funs)){
    l[[i]] <- sapply(x, funs[i])
  }
  data.frame(l) %>% tibble::as_tibble() %>% return()
}

#' @export
measure.Coo <- function(x, ...){
  funs <- as.character(substitute(list(...)))[-1L]
  l <- vector("list", length(funs))
  # we remove coo_, (, ), commas and any space
  names(l) <- gsub("coo_|[[:punct:]]", "", funs)
  for (i in seq_along(funs)){
    # case where a full function name is present
    if (length(grep("\\(", funs[i])) > 0) {
      fun_i <- measure_nse(funs[i])
      l[[i]] <- sapply(x$coo, fun_i$fun, fun_i$args)
      # otherwise, classical case
    } else {
      l[[i]] <- sapply(x$coo, funs[i])
    }
  }
  coe <- data.frame(l) %>% tibble::as_tibble()
  fac <- x$fac
  TraCoe(coe = coe,
         fac = fac)
}

# dirty attemps at NSE-ishing
# when passed a character (eg a substituted function name), returns the function name
# otherwise returns a list with $fun function name and $args arguments as characters
# see implementation in measure method.
measure_nse <- function(ch){
  ch <- gsub("[[:space:]]", "", ch)
  open_par <- regexpr("\\(", ch)
  if (open_par == -1) return(ch)
  fun  <- substr(ch, 1, (open_par-1))
  args <- substr(ch, open_par+1, nchar(ch)-1) %>% strsplit(",")
  list(fun=fun, args=args[[1]])
}

#' Truss measurement
#'
#' A method to calculate on shapes or on [Coo] truss measurements,
#' that is all pairwise combinations of euclidean distances
#' @param x a shape or an Ldk object
#' @return a named numeric or matrix
#' @note Mainly implemented for historical/didactical reasons.
#' @family premodern
#' @examples
#' # example on a single shape
#' cat <- coo_sample(shapes[4], 6)
#' coo_truss(cat)
#'
#' # example on wings dataset

#' tx <- coo_truss(wings)
#' dim(tx)
#' # we normalize and plot an heatmap
#' txn <- apply(tx$coe, 2, .normalize)
#' # heatmap(txn)
#'
#' txp <- PCA(tx, scale. = TRUE, center=TRUE, fac=wings$fac)
#' plot(txp, 1)
#' @export
coo_truss <- function(x){
  UseMethod("coo_truss")
}

#' @export
coo_truss.default <- function(x){
  res <- as.numeric(dist(x))
  names(res) <- apply(utils::combn(1:nrow(x), 2), 2, paste, collapse="-")
  return(res)
}

#' @export
coo_truss.Coo <- function(x){
  .check(length(unique(coo_nb(x)))==1,
         "all shapes must have the same number of coordinates. See ?coo_sample")
  TraCoe(coe=x$coo %>% sapply(coo_truss) %>% t(),
         fac=x$fac)
}




