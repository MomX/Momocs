##### mean shapes on coefficients todo: better handling of $slots
##### (eg r2 for Opn, etc.)
#' Mean shape calculation for Coo, Coe, etc.
#'
#' Quite a versatile function that calculates mean (or median, or whatever function)
#' on list or an array of shapes, an Ldk object. It can also be used on OutCoe and OpnCoe objects.
#' In that case, the reverse transformation (from coefficients to shapes) is calculated, (within
#' groups defined with the fac argument if provided) and the Coe object is returned.
#'
#' @param x a list, array, Ldk, LdkCoe, OutCoe or OpnCoe or PCA object
#' @param fac factor from the $fac slot (only for Coe objects). See examples below.
#' @param FUN a function to compute the mean shape (\link{mean} by default, by \link{median} can be considered)
#' @param nb.pts numeric the number of points for calculated shapes (only Coe objects)
#' @param ... useless here.
#' @return the averaged shape; on Coe objects, a list with two components: \code{$Coe} object of the same class, and
#' \code{$shp} a list of matrices of (x, y) coordinates.
#' @details Note that on Coe objects, the average can be made within levels of the passed $fac (if any);
#' in that case, the other columns of the fac are also returned, usingthe first row within every level, but they may
#' not be representive of the group. Also notice that for PCA objects, mean scores are returned
#' within a PCA object (accesible with PCA$x) that can be plotted directly but other slots are left
#' unchanged.
#' @rdname mshapes
#' @family multivariate
#' @examples
#' #### on shapes
#' mshapes(wings)
#' mshapes(wings$coo)
#' mshapes(coo_sample(bot, 24)$coo)
#' stack(wings)
#' coo_draw(mshapes(wings))
#'
#' bot.f <- efourier(bot, 12)
#' mshapes(bot.f) # the mean (global) shape
#' ms <- mshapes(bot.f, 'type')
#' ms$Coe
#' class(ms$Coe)
#' ms <- ms$shp
#' coo_plot(ms$beer)
#' coo_draw(ms$whisky, border='forestgreen')
#' tps_arr(ms$whisky, ms$beer) #etc.
#'
#' op <- npoly(filter(olea, view=='VL'), 5)
#' ms <- mshapes(op, 'var') #etc
#' ms$Coe
#' panel(Opn(ms$shp), names=TRUE)
#'
#' wp <- fgProcrustes(wings, tol=1e-4)
#' ms <- mshapes(wp, 1)
#' ms$Coe
#' panel(Ldk(ms$shp), names=TRUE) #etc.
#' panel(ms$Coe) # equivalent (except the $fac slot)
#' @rdname mshapes
#' @export
mshapes <- function(x, ...) {
  UseMethod("mshapes")
}

#' @rdname mshapes
#' @export
mshapes.list <- function(x, FUN=mean, ...) {
  A <- ldk_check(x)
  return(apply(A, 1:2, FUN, na.rm = TRUE))
}

#' @rdname mshapes
#' @export
mshapes.array <- function(x, FUN=mean, ...) {
  if (length(dim(x)) == 3) {
    A <- ldk_check(x)
    return(apply(A, 1:2, FUN, na.rm = TRUE))
  }
}

#' @rdname mshapes
#' @export
mshapes.Ldk <- function(x, FUN=mean, ...) {
  Ldk <- x
  A <- ldk_check(Ldk$coo)
  return(apply(A, 1:2, mean, na.rm = TRUE))
}

#' @rdname mshapes
#' @export
mshapes.OutCoe <- function(x, fac, FUN=mean, nb.pts = 120, ...) {
    OutCoe <- x
    nb.h <- ncol(OutCoe$coe)/4  #todo
    if (missing(fac)) {
        message("no 'fac' provided, returns meanshape")
        coe.mshape <- apply(OutCoe$coe, 2, FUN)
        xf <- coeff_split(coe.mshape, nb.h, 4)
        return(efourier_i(xf, nb.pts = nb.pts))
    }

    f <- OutCoe$fac[, fac]
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
    return(list(Coe = Coe2, shp = shp))
}

#' @rdname mshapes
#' @export
mshapes.OpnCoe <- function(x, fac, FUN=mean, nb.pts = 120, ...) {
    OpnCoe <- x
    #todo: check if method is all identical
        	p <- pmatch(tolower(OpnCoe$method[1]), c("opoly", "npoly", "dfourier"))
    	if (is.na(p)) {
      		warning("unvalid method. efourier is used.\n")
    	} else {
      method_i <- switch(p, opoly_i, npoly_i, dfourier_i) # dfourier_i
    }
    n <- length(OpnCoe$mshape)  #todo
    if (missing(fac)) {
        message("no 'fac' provided, returns meanshape")
        coe.mshape <- apply(OpnCoe$coe, 2, FUN)
        mod.mshape <- OpnCoe$mod
        mod.mshape$coefficients <- coe.mshape
        return(method_i(mod.mshape))
    }

    f <- OpnCoe$fac[, fac]
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
    return(list(Coe = Coe2, shp = shp))
}

#' @rdname mshapes
#' @export
mshapes.LdkCoe <- function(x, fac, FUN=mean, ...) {
    LdkCoe <- x
    if (missing(fac)) {
        message("no 'fac' provided. Returns meanshape")
        return(mshapes(LdkCoe$coo))
    }
    f <- LdkCoe$fac[, fac]
    fl <- levels(f)
    shp <- list()
    rows <- numeric()
    for (i in seq(along = fl)) {
        shp[[i]] <- mshapes(LdkCoe$coo[f == fl[i]], FUN=FUN)
        rows[i] <- which(f == fl[i])[1]
    }
    names(shp) <- fl
    Coe2 <- Ldk(shp, fac=slice(LdkCoe$fac, rows))
    return(list(Coe = Coe2, shp = shp))
}

#' @rdname mshapes
#' @export
mshapes.PCA <- function(x, fac, ...){
  # cehck for single individuals within a group..
  x0 <- x
  f <- x$fac[, fac]
  x <- x$x
  res <- matrix(NA, nrow=nlevels(f), ncol=ncol(x),
                dimnames=list(levels(f), colnames(x)))
  for (i in seq(along=levels(f))){
    x.i <- x[f == levels(f)[i], ]
    if (!is.matrix(x.i)) {
      res[i, ] <- x.i
      next()
    }
    res[i, ] <- apply(x.i, 2, mean)
  }
  x0$x <- res
  # should retain the true name and not "fac"
  x0$fac <- dplyr::data_frame(fac=levels(f))
  x0
}

#' @export
#' @rdname mshapes
MSHAPES <- mshapes

##### end mshapes
