##### mean shapes on coefficients todo: better handling of $slots
##### (eg r2 for Opn, etc.)
#' Mean shape calculation from Coe objects
#' 
#' Calculates mean shapes on matrices of coefficients by groups (if passed with
#' a 'fac') or globally (if not), and on \link{Coe} objects.
#' 
#' @param Coe a \link{Coe} object
#' @param fac factor from the $fac slot. See examples below.
#' @param FUN a function to compute the mean shape (\link{mean} by default, by \link{median} can be considered)
#' @param nb.pts numeric the number of points for calculated shapes
#' @return a list with two components: \code{$Coe} object of the same class, and
#' \code{$shp} a list of matrices of (x, y) coordinates.
#' @rdname mshapes
#' @keywords Multivariate
#' @seealso \link{mshape} for operations on raw coordinates.
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 12)
#' mshapes(bot.f) # the mean (global) shape
#' ms <- mshapes(bot.f, 'type')
#' ms$Coe
#' class(ms$Coe)
#' ms <- ms$shp
#' coo_plot(ms$beer)
#' coo_draw(ms$whisky, border='forestgreen')
#' tps.arr(ms$whisky, ms$beer) #etc.
#' 
#' data(olea)
#' op <- rawPolynomials(subset(olea, view=='VL'), 5)
#' ms <- mshapes(op, 'cep') #etc
#' ms$Coe
#' panel(Opn(ms$shp), names=TRUE) 
#' 
#' data(wings)
#' wp <- fgProcrustes(wings, tol=1e-4)
#' ms <- mshapes(wp, 1)
#' ms$Coe
#' panel(Ldk(ms$shp), names=TRUE) #etc.
#' panel(ms$Coe) # equivalent (except the $fac slot)
#' 
#' @export
mshapes <- function(Coe, fac, FUN, nb.pts) {
    UseMethod("mshapes")
}

#' @rdname mshapes
#' @export
mshapes.default <- function(Coe, fac, FUN, nb.pts) {
  cat("* this method must be used on a Coe object.")
}

#' @rdname mshapes
#' @export
mshapes.OutCoe <- function(Coe, fac, FUN=mean, nb.pts = 120) {
    OutCoe <- Coe
    nb.h <- ncol(OutCoe$coe)/4  #todo
    if (missing(fac)) {
        cat("* no 'fac' provided. Returns meanshape.\n")
        coe.mshape <- apply(OutCoe$coe, 2, FUN)
        xf <- coeff.split(coe.mshape, nb.h, 4)
        return(efourier.i(xf, nb.pts = nb.pts))
    }
    
    f <- OutCoe$fac[, fac]
    fl <- levels(f)
    shp <- list()
    coe <- matrix(NA, nrow = nlevels(f), ncol = ncol(OutCoe$coe), 
        dimnames = list(fl, colnames(OutCoe$coe)))
    for (i in seq(along = fl)) {
        coe.i <- OutCoe$coe[f == fl[i], ]
        if (is.matrix(coe.i)) {
            coe.i <- apply(coe.i, 2, FUN)
        }
        coe[i, ] <- coe.i
        xf <- coeff.split(cs = coe.i, nb.h = nb.h, cph = 4)
        shp[[i]] <- efourier.i(xf, nb.h = nb.h, nb.pts = nb.pts)
    }
    names(shp) <- fl
    Coe2 <- OutCoe
    Coe2$coe <- coe
    Coe2$fac <- data.frame(fac = fl)
    names(Coe2$fac) <- as.character(fac)
    return(list(Coe = Coe2, shp = shp))
}

#' @rdname mshapes
#' @export
mshapes.OpnCoe <- function(Coe, fac, FUN=mean, nb.pts = 120) {
    OpnCoe <- Coe
    n <- length(OpnCoe$mshape)  #todo
    if (missing(fac)) {
        cat("* no 'fac' provided. Returns meanshape.\n")
        coe.mshape <- apply(OpnCoe$coe, 2, FUN)
        mod.mshape <- OpnCoe$mod
        mod.mshape$coefficients <- coe.mshape
        return(polynomials.i(mod.mshape))
    }
    
    f <- OpnCoe$fac[, fac]
    fl <- levels(f)
    shp <- list()
    coe <- matrix(NA, nrow = nlevels(f), ncol = ncol(OpnCoe$coe), 
        dimnames = list(fl, colnames(OpnCoe$coe)))
    mod.mshape <- OpnCoe$mod
    for (i in seq(along = fl)) {
        coe.i <- OpnCoe$coe[f == fl[i], ]
        if (is.matrix(coe.i)) {
            coe.i <- apply(coe.i, 2, FUN)
        }
        mod.mshape$coeff <- coe.i
        coe[i, ] <- coe.i
        shp[[i]] <- polynomials.i(mod.mshape)
    }
    names(shp) <- fl
    Coe2 <- OpnCoe
    Coe2$coe <- coe
    Coe2$fac <- data.frame(fac = fl)
    names(Coe2$fac) <- as.character(fac)
    return(list(Coe = Coe2, shp = shp))
}

#' @rdname mshapes
#' @export
mshapes.LdkCoe <- function(Coe, fac, FUN=NULL, nb.pts = 120) {
    LdkCoe <- Coe
    if (missing(fac)) {
        cat("* no 'fac' provided. Returns meanshape.\n")
        return(mshape(LdkCoe))
    }
    
    f <- LdkCoe$fac[, fac]
    fl <- levels(f)
    shp <- list()
    for (i in seq(along = fl)) {
        shp[[i]] <- mshape(LdkCoe$coo[f == fl[i]])
    }
    names(shp) <- fl
    Coe2 <- Ldk(shp)  # todo, probably wrong
    Coe2$fac <- data.frame(fac = fl)
    names(Coe2$fac) <- as.character(fac)
    return(list(Coe = Coe2, shp = shp))
}

##### end mshapes 
