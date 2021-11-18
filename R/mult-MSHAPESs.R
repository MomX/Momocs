##### mean shapes on coefficients todo: better handling of $slots
##### (eg r2 for Opn, etc.)
#' Mean shape calculation for Coo, Coe, etc.
#'
#' Quite a versatile function that calculates mean (or median, or whatever function)
#' on list or an array of shapes, an Ldk object. It can also be used on Coe objects.
#' In that case, the reverse transformation (from coefficients to shapes) is calculated, (within
#' groups defined with the fac argument if provided) and the Coe object is _also_ returned
#' (in `$Coe`) along with a list of shapes (in `$shp`) and can then be passed to `plot_MSHAPES`.
#'
#' @param x a list, array, Ldk, LdkCoe, OutCoe or OpnCoe or PCA object
#' @param fac factor specification for [fac_dispatcher]
#' @param FUN a function to compute the mean shape (\link{mean} by default, by \link{median} can be considered)
#' @param nb.pts numeric the number of points for calculated shapes (only Coe objects)
#' @param ... useless here.
#' @return the averaged shape; on Coe objects, a list with two components: \code{$Coe} object of the same class, and
#' \code{$shp} a list of matrices of (x, y) coordinates. On [PCA] and [LDA] objects, the FUN (typically mean or median)
#' of scores on `PCs` or `LDs`. This method used on the latter objects may be moved to another function at some point.
#' @rdname MSHAPES
#' @family multivariate
#' @examples
#' #### on shapes
#' MSHAPES(wings)
#' MSHAPES(wings$coo)
#' MSHAPES(coo_sample(bot, 24)$coo)
#' stack(wings)
#' coo_draw(MSHAPES(wings))
#'
#' bot.f <- efourier(bot, 12)
#' MSHAPES(bot.f) # the mean (global) shape
#' ms <- MSHAPES(bot.f, 'type')
#' ms$Coe
#' class(ms$Coe)
#' ms <- ms$shp
#' coo_plot(ms$beer)
#' coo_draw(ms$whisky, border='forestgreen')
#' @export
MSHAPES <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  UseMethod("MSHAPES")
}

#' @export
MSHAPES.list <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  A <- ldk_check(x)
  return(apply(A, 1:2, FUN, na.rm = TRUE))
}

#' @export
MSHAPES.array <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  if (length(dim(x)) == 3) {
    A <- ldk_check(x)
    return(apply(A, 1:2, FUN, na.rm = TRUE))
  }
}

#' @export
MSHAPES.Ldk <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  Ldk <- x
  A <- ldk_check(Ldk$coo)
  return(apply(A, 1:2, mean, na.rm = TRUE))
}

#' @export
MSHAPES.OutCoe <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  OutCoe <- x
  nb.h <- ncol(OutCoe$coe)/4  #todo
  if (is.null(fac)) {
    message("no 'fac' provided, returns meanshape")
    coe.mshape <- apply(OutCoe$coe, 2, FUN)
    xf <- coeff_split(coe.mshape, nb.h, 4)
    return(efourier_i(xf, nb.pts = nb.pts))
  }

  f <- fac_dispatcher(x, fac) %>% factor

  fl <- levels(f)
  shp <- list()
  rows <- numeric()
  coe <- matrix(NA, nrow = nlevels(f), ncol = ncol(OutCoe$coe),
                dimnames = list(fl, colnames(OutCoe$coe)))
  for (i in seq(along = fl)) {
    coe.i <- OutCoe$coe[f == fl[i], ]
    rows[i] <- which(f == fl[i])[1]
    if (is.matrix(coe.i) | is.data.frame(coe.i)) {
      coe.i <- apply(coe.i, 2, FUN)
    }
    coe[i, ] <- coe.i
    xf <- coeff_split(cs = coe.i, nb.h = nb.h, cph = 4)
    shp[[i]] <- efourier_i(xf, nb.h = nb.h, nb.pts = nb.pts)
  }
  names(shp) <- fl
  Coe2 <- OutCoe
  Coe2$coe <- coe
  Coe2$fac <- slice(Coe2$fac, rows)

  res <- list(Coe = Coe2, shp = shp) %>%
    .prepend_class("MSHAPES")
  return(res)
}

#' @export
MSHAPES.OpnCoe <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  OpnCoe <- x
  #todo: check if method is all identical
  p <- pmatch(tolower(OpnCoe$method[1]), c("opoly", "npoly", "dfourier"))
  if (is.na(p)) {
    stop("unvalid method\n")
  } else {
    method_i <- switch(p, opoly_i, npoly_i, dfourier_i) # dfourier_i
  }
  n <- length(OpnCoe$mshape)  #todo
  if (is.null(fac)) {
    message("no 'fac' provided, returns meanshape")
    coe.mshape <- apply(OpnCoe$coe, 2, FUN)
    mod.mshape <- OpnCoe$mod
    mod.mshape$coefficients <- coe.mshape
    return(method_i(mod.mshape))
  }

  f <- fac_dispatcher(x, fac) %>% factor

  fl <- levels(f)
  shp <- list()
  rows <- numeric()
  coe <- matrix(NA, nrow = nlevels(f), ncol = ncol(OpnCoe$coe),
                dimnames = list(fl, colnames(OpnCoe$coe)))
  mod.mshape <- OpnCoe$mod
  for (i in seq(along = fl)) {
    coe.i <- OpnCoe$coe[f == fl[i], ]
    rows[i] <- which(f == fl[i])[1]
    if (is.matrix(coe.i)) {
      coe.i <- apply(coe.i, 2, FUN)
    }
    mod.mshape$coeff <- coe.i
    coe[i, ] <- coe.i
    shp[[i]] <- method_i(mod.mshape)
  }
  names(shp) <- fl
  Coe2 <- OpnCoe
  Coe2$coe <- coe
  Coe2$fac <- slice(Coe2$fac, rows)

  res <- list(Coe = Coe2, shp = shp) %>%
    .prepend_class("MSHAPES")
  return(res)

}

#' @export
MSHAPES.LdkCoe <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...) {
  LdkCoe <- x
  if (is.null(fac)) {
    message("no 'fac' provided. Returns meanshape")
    return(MSHAPES(LdkCoe$coo))
  }

  f <- fac_dispatcher(x, fac) %>% factor

  fl <- levels(f)
  shp <- list()
  rows <- numeric()
  for (i in seq(along = fl)) {
    shp[[i]] <- MSHAPES(LdkCoe$coo[f == fl[i]], FUN=FUN)
    rows[i] <- which(f == fl[i])[1]
  }
  names(shp) <- fl
  Coe2 <- Ldk(shp, fac=slice(LdkCoe$fac, rows))

  res <- list(Coe = Coe2, shp = shp) %>%
    .prepend_class("MSHAPES")
  return(res)

}

#' @export
MSHAPES.PCA <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...){
  # check for single individuals within a group..
  x0 <- x

  # f data_frame
  # first dispatch
  f <- fac_dispatcher(x, fac)
  fdf <- dplyr::tibble(fac=f)

  # res data_frame
  res <- x$x %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(fdf, .) %>%
    dplyr::group_by(fac) %>%
    dplyr::summarise_all(FUN)

  # just change the fac for the real name and return
  colnames(res)[1] <- fac %>% as.character() %>% `[`(-1)
  res
}

#' @export
MSHAPES.LDA <- function(x, fac=NULL, FUN=mean, nb.pts = 120, ...){
  # check for single individuals within a group..
  x0 <- x
  # # if fac provided, dispatch it - not sure this has an utility
  # if (!missing(fac))
    f <- fac_dispatcher(x, fac)
  #otherwise use the one from the LDA
  # else
  #   f <- x$f

  # f data_frame
  # first dispatch
  fdf <- dplyr::tibble(fac=f)

  # res data_frame
  res <- x$mod.pred$x %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(fdf, .) %>%
    dplyr::group_by(fac) %>%
    dplyr::summarise_all(FUN)

  # just change the fac for the real name and return
  colnames(res)[1] <- fac %>% as.character() %>% `[`(-1)
  res
}

##### end MSHAPES
