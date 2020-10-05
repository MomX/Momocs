##### Simple bridges between R classes

# coordinates -------------
#' Convert complex to/from cartesian coordinates
#'
#' @param coo coordinates expressed in the cartesian form
#' @param Z coordinates expressed in the complex form
#' @return coordinates expressed in the cartesian/complex form
#' @family bridges functions
#' @examples
#' shapes[4] %>%            # from cartesian
#'     coo_sample(24) %>%
#'     coo2cpx() %T>%       # to complex
#'     cpx2coo()            # and back
#' @name complex
#' @aliases complex
#' @rdname complex
#' @export
cpx2coo <- function(Z){
  cbind(Re(Z), Im(Z)) %>% `colnames<-`(c("x", "y")) %>% return()
}

#' @rdname complex
#' @export
coo2cpx <- function(coo){
  if (!is.matrix(coo) & length(coo)==2)
    coo %<>% matrix(nrow=1)
  complex(real = coo[, 1], imaginary = coo[, 2], length.out=nrow(coo))
}

# shp methods -------------
#' Convert between different classes
#'
#' @param a \code{array} of (x; y) coordinates
#' @param d \code{data.frame} with two columns
#' @param l \code{list} with x and y coordinates as components
#' @param m \code{matrix} of (x; y) coordinates
#' @param index \code{numeric}, the number of coordinates for every slice
#'
#'
#' @note \code{a2m}/\code{m2a} change, by essence, the dimension of the data.
#' \code{m2ll} is used internally to hanle coo and cur in \code{Ldk} objects but may be
#' useful elsewhere
#' @return the data in the required class
#' @examples
#' # matrix/list
#' wings[1] %>% coo_sample(4) %>%
#'    m2l() %T>% print %>%        # matrix to list
#'    l2m()                       # and back
#'
#' # data.frame/matrix
#' wings[1] %>% coo_sample(4) %>%
#'    m2d() %T>% print %>%        # matrix to data.frame
#'    d2m                         # and back
#'
#'  # list/array
#'  wings %>% slice(1:2) %$%
#'  coo %>% l2a %T>% print %>%    # list to array
#'  a2l                           # and back
#'
#'  # array/matrix
#'  wings %>% slice(1:2) %$%
#'  l2a(coo) %>%                  # and array (from a list)
#'  a2m %T>% print %>%            # to matrix
#'  m2a                           # and back
#'
#'  # m2ll
#' m2ll(wings[1], c(6, 4, 3, 5)) # grab slices and coordinates
#' @family bridges functions
#' @name bridges
#' @aliases bridges
#' @rdname bridges
#' @export
l2m <- function(l) {
  if (length(l) == 1 && is_shp(l[[1]]))
    return(l[[1]])
  m <- cbind(l$x, l$y)
  colnames(m) <- c("x", "y")
  return(m)
}

#' @rdname bridges
#' @export
m2l <- function(m) {
  return(list(x = m[, 1], y = m[, 2]))
}

#' @rdname bridges
#' @export
d2m <- function(d) {
  .check(ncol(d) == 2,
         "data.frame must have two columns")
  d %>% as.matrix() %>% `colnames<-`(c("x", "y"))
}

#' @rdname bridges
#' @export
m2d <- function(m) {
  .check(is_shp(m),
         "matrix must be a shp")
  dplyr::data_frame(x=m[, 1], y=m[, 2])
}

#' @rdname bridges
#' @export
l2a <- function(l) {
  .check(length(unique(sapply(l, length))) == 1,
         "matrices in list must have the same dimensions")
  nr <- nrow(l[[1]])
  nc <- 2
  ni <- length(l)
  a <- array(unlist(l), dim = c(nr, nc, ni), dimnames = list(1:nr,
                                                             c("x", "y"), names(l)))
  return(a)
}

#' @rdname bridges
#' @export
a2l <- function(a) {
  .check(is.array(a) & length(dim(a)==3),
         "An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {
    l[[i]] <- a[, , i]
  }
  return(l)
}


#' @rdname bridges
#' @export
a2m <- function(a) {
  # ugly
  m <- sapply(a, as.numeric)
  nc <- dim(a)[1]
  m <- matrix(m, nrow = dim(a)[3], ncol = nc * 2, byrow = TRUE)
  colnames(m) <- paste0(rep(c("x", "y"), each = nc), 1:nc)
  if (!is.null(dimnames(a))) {
    rownames(m) <- dimnames(a)[[3]]
  }
  return(m)
}

#' @rdname bridges
#' @export
m2a <- function(m) {
  # ugly
  a <- array(NA,
             dim = c(ncol(m)/2, 2, nrow(m)),
             dimnames = list(1:(ncol(m)/2), c("x", "y"), rownames(m)))
  for (i in 1:nrow(m)) {
    a[, , i] <- matrix(m[i, ], ncol = 2)
  }
  return(a)
}

#' @rdname bridges
#' @export
m2ll <- function(m, index=NULL){
  # no slicing case, we return a matrix
  if (is.null(index))
    return(m)
  # slicing case, we slices
  .check(sum(index)==nrow(m),
         "nrow(m) and sum(index) must match")
  start <- cumsum(c(1, index[-length(index)]))
  end   <- cumsum(index)
  ll <- vector("list", length(start))
  for (i in seq_along(start)){
    ll[[i]] <- m[start[i]:end[i], ]
  }
  return(ll)
}

# home made melt for Coe
.melt_mat <- function(x){
  if (is.null(colnames(x)))
    colnames(x) <- 1:ncol(x)
  dplyr::data_frame(key=rep(colnames(x), each=nrow(x)),
                    value=as.numeric(x))

}

# as_df methods ------------------

#' Turn Momocs objects into tydy data_frames
#'
#' Used in particular for compatibility with the \code{tidyverse}
#'
#' @param x an object, typically a Momocs object
#' @param retain numeric for use with [scree] methods. Defaut to all. If `<1`,
#' enough axes to retain this proportion of variance; if `>1`, this number of axes.
#' @param ... useless here
#' @return a [dplyr::data_frame]
#' @examples
#' # first, some (baby) objects
#' b <- bot %>% coo_sample(12)
#' bf <- b %>% efourier(5, norm=TRUE)

#' # Coo object
#' b %>% as_df
#' # Coe object
#' bf %>% as_df
#'
#' # PCA object
#' bf %>% PCA %>% as_df     # all PCs by default
#' bf %>% PCA %>% as_df(2) # or 2
#' bf %>% PCA %>% as_df(0.99) # or enough for 99%
#'
#' # LDA object
#' bf %>% LDA(~fake) %>% as_df
#' # same options apply
#' @family bridges functions
#' @rdname as_df
#' @export
as_df <- function(x, ...){
  UseMethod("as_df")
}

#' @rdname as_df
#' @export
as_df.Coo <- function(x, ...){
  # res <- lapply(seq_along(x$coo),
  #               function(i) data.frame(id=names(x$coo)[i],
  #                                      x=x$coo[[i]][, 1],
  #                                      y=x$coo[[i]][, 2]))
  #
  # # if there is a fac
  # if (is_fac(x)){
  #   # create a list of data.frames, each row repeated (number of coefficients) times
  #   fac <- lapply(seq_along(res), function(i) x$fac[rep(i, nrow(res[[i]])),, drop=FALSE])
  #   # and cbind them
  #   res <- lapply(seq_along(res), function(i) dplyr::bind_cols(res[[i]], fac[[i]]))
  # }
  # #rbind them all and return
  # do.call("rbind", res) %>%
  #   dplyr::as_data_frame() %>%
  #   return()
  dplyr::bind_cols(
    tibble::as_tibble(coo=x$coo),
    x$fac
  )
}

#' @rdname as_df
#' @export
as_df.Coe <- function(x, ...){
  # need this since if no fac, bind_cols wont be happy
  if (!is_fac(x))
    return(tibble::as_tibble(x$coe))
  # shortcut
  dplyr::bind_cols(
    x$fac,
    tibble::as_tibble(x$coe)
  )
}


#' @rdname as_df
#' @export
as_df.PCA <- function(x, retain, ...){

  scores_df <- tibble::as_tibble(x$x)

  # if retain is not provided, all returned
  if (missing(retain)){
    retain <- nrow(scores_df)
  }

  # check for too ambitious
  if (retain > ncol(scores_df)){
    cat("`retain` is too ambitious. All axes returned\n")
    retain <- ncol(scores_df)
  }

  # proportion case
  if (retain<1){
    retain <- scree_min(x, retain)
  }

  # select the concerned columns
  scores_df <- select(scores_df, 1:retain)

  # return fac and scores
  dplyr::bind_cols(x$fac, scores_df)
}

#' @rdname as_df
#' @export
as_df.LDA <- function(x, retain, ...){
  scores_df <- tibble::as_tibble(x$mod.pred$x)

  # if retain is not provided, all returned
  if (missing(retain)){
    retain <- ncol(scores_df)
  }

  # check for too ambitious
  if (retain > nrow(scores_df)){
    cat("`retain` is too ambitious. All axes returned\n")
    retain <- ncol(scores_df)
  }

  # proportion case
  if (retain<1){
    retain <- scree_min(x, retain)
  }

  # proportion case
  if (retain<1)
    retain <- scree_min(x, retain)
  # select the concerned columns
  scores_df <- select(scores_df, 1:retain)

  # now the big one
  dplyr::bind_cols(
    # actual, predicted, posterior
    dplyr::data_frame(actual     = x$f,
                      predicted  = x$mod.pred$class,
                      posterior  = apply(x$mod.pred$posterior, 1, max)),
    # the original fac
    x$fac,
    # and LD scores
    scores_df)
}

##### end bridges
