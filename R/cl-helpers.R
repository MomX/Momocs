
# class appenders ------------------------------------------
.prepend_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(class_to_add, .)
  x
}

.append_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(., class_to_add)
  x
}

# class testers -------------
#' Class and component testers
#'
#' Class testers test if any of the classes of an object _is_ of a given class. For instance
#' `is_PCA` on a [PCA] object (of classes `PCA` and `prcomp`) will return `TRUE`.
#' Component testers check if _there_is_ a particular component (eg `$fac`, etc.) in an object.
#' @param x the object to test
#' @return `logical`
#' @examples
#' is_Coo(bot)
#' is_Out(bot)
#' is_Ldk(bot)
#' is_ldk(hearts) # mind the capitals!
#' @name is
#' @export
is_Coo <- function(x){
  ifelse(any(class(x) == "Coo"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_PCA <- function(x){
  ifelse(any(class(x) == "PCA"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_LDA <- function(x){
  ifelse(any(class(x) == "LDA"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_Out <- function(x){
  ifelse(any(class(x) == "Out"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_Opn <- function(x){
  ifelse(any(class(x) == "Opn"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_Ldk <- function(x){
  ifelse(any(class(x) == "Ldk"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_Coe <- function(x){
  ifelse(any(class(x) == "Coe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_OutCoe <- function(x){
  ifelse(any(class(x) == "OutCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_OpnCoe <- function(x){
  ifelse(any(class(x) == "OpnCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_LdkCoe <- function(x){
  ifelse(any(class(x) == "LdkCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_TraCoe <- function(x){
  ifelse(any(class(x) == "TraCoe"), TRUE, FALSE)
}

#' @rdname is
#' @export
is_shp <- function(x){
  if (is.matrix(x))
    if (ncol(x)==2 & all(!is.na(x)))
      return(TRUE)
  FALSE
}

#' @rdname is
#' @export
is_fac <- function(x) length(x$fac) > 0

#' @rdname is
#' @export
is_ldk   <- function(x) length(x$ldk) > 0

#' @rdname is
#' @export
is_slidings   <- function(x) length(x$slidings) > 0

#' @rdname is
#' @export
is_links <- function(x) is.matrix(x$links)

# outliers ---------

#' Identify outliers
#'
#' A simple wrapper around \link{dnorm} that helps identify outliers. In particular,
#' it may be useful on \link{Coe} object (in this case a PCA is first calculated) and also
#' on \link{Ldk} for detecting possible outliers on freshly digitized/imported datasets.
#'
#' @param x object, either Coe or a numeric on which to search for outliers
#' @param conf confidence for dnorm (1e-3 by default)
#' @param nax number of axes to retain (only for Coe),
#' if <1 retain enough axes to retain this proportion of the variance
#' @param ... additional parameters to be passed to PCA (only for Coe)
#' @return a vector of indices
#' @note experimental. dnorm parameters used are \code{median(x), sd(x)}
#' @examples
#' # on a numeric
#' x <- rnorm(10)
#' x[4] <- 99
#' which_out(x)
#'
#' # on a Coe
#' bf <- bot %>% efourier(6)
#' bf$coe[c(1, 6), 1] <- 5
#' which_out(bf)
#'
#' # on Ldk
#' w_no <- w_ok <- wings
#' w_no$coo[[2]][1, 1] <- 2
#' w_no$coo[[6]][2, 2] <- 2
#' which_out(w_ok, conf=1e-12) # with low conf, no outliers
#' which_out(w_no, conf=1e-12) # as expected
#'
#' # a way to illustrate, filter outliers
#' # conf has been chosen deliberately low to show some outliers
#'x_f <- bot %>% efourier
#'x_p <- PCA(x_f)
#'# which are outliers (conf is ridiculously low here)
#'which_out(x_p$x[, 1], 0.5)
#'cols <- rep("black", nrow(x_p$x))
#'outliers <- which_out(x_p$x[, 1], 0.5)
#'cols[outliers] <- "red"
#'plot(x_p, col=cols)
#'# remove them for Coe, rePCA, replot
#'x_f %>% slice(-outliers) %>% PCA %>% plot
#'
#'# or directly with which_out.Coe
#'# which relies on a PCA
#'outliers <- x_f %>% which_out(0.5, nax=0.95) %>% na.omit()
#'x_f %>% slice(-outliers) %>% PCA %>% plot
#' @export
which_out <- function(x, conf, nax, ...){
  UseMethod("which_out")
}

#' @export
which_out.default <- function(x, conf=1e-3, ...){
  out <- which(dnorm(x, median(x), sd(x)) < conf)
  if(length(out)==0) {
    return(NA)
  } else {
    return(out)}
}

#' @export
which_out.Coe <- function(x, conf=1e-3, nax=0.99, ...){
  p <- PCA(x, ...)
  if (length(nax)==1){
    if (nax < 1)
      nax <- scree_min(p, nax)
  }
  m <- p$x[, 1:nax]
  m <- matrix(m, ncol=nax)
  outliers <- apply(m, 2, which_out, conf=conf)
  outliers <- unlist(outliers)
  outliers %>% as.numeric %>%
    na.omit %>% unique %>% return()
}

#' @export
which_out.Ldk <- function(x, conf=1e-3, ...){
  arr <- x$coo %>% l2a %>% apply(1:2, function(.) dnorm(., mean(.), sd(.)))
  out <- which(arr < conf, arr.ind=TRUE)
  if (nrow(out)==0){
    return(NA)
  } else {
    message("found ", nrow(arr), " possible outliers")
    dplyr::tibble(shape=names(x)[out[, 1]],
               id=out[, 1],
               row=out[, 2],
               coordinate=c("x", "y")[out[, 3]])
  }
}

##### End Miscellaneous
