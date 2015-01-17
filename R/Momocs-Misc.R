##### Miscellaneous functions

#' Calculates euclidean distance between two points.
#'
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates.
#'
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm_nearest}, \link{dist}.
#' @keywords Miscellaneous
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
#' @keywords Miscellaneous
#' @examples
#' edi(c(0,1), c(1,0), r = 0.5)
#' @export
edi <- function(pt1, pt2, r = 0.5) {
    return(r * (pt2 - pt1) + pt1)
}

#' Calculates euclidean distance every pairs of points in two matrices.
#'
#' \code{edm} returns the euclidean distances between points \deqn{1 -> n} of
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
#' @keywords Miscellaneous
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
#' So far this function is quite time consumming since it performs \deqn{ n
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
#' @keywords Miscellaneous
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm_nearest(x, x+rnorm(10))
#' edm_nearest(x, x+rnorm(10), full=TRUE)
#' @export
edm_nearest <- function(m1, m2, full = FALSE) {
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

##### Miscellaneous functions for Fourier-based approaches

#' Helps to select a given number of harmonics from a numerical vector.
#'
#' \code{coeff_sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{$coe} slot of \code{\link{Coe}}-objects in
#' that way: \deqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{efourier} and
#' \link{efourier}) while \deqn{C_n and D_n} harmonic are absent for radii
#' variation and tangent angle approaches (see \link{rfourier} and
#' \link{tfourier} respectively). . This function is used internally but might
#' be of interest elwewhere.
#'
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff_sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff_split} returns a named list
#' of coordinates.
#' @keywords Miscellaneous
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 32)
#' coe <- bot.f$coe # the raw matrix
#' coe
#' # if you want, say the first 8 harmonics but not the first one
#' retain <- coeff_sel(retain=8, drop=1, nb.h=32, cph=4)
#' head(coe[, retain])
#' @export
coeff_sel <- function(retain = 8, drop = 0, nb.h = 32, cph = 4) {
    cs <- numeric()
    for (i in 1:cph) {
        cs <- c(cs, (1 + drop):retain + nb.h * (i - 1))
    }
    return(cs)
}

#' Converts a numerical description of harmonic coefficients to a named list.
#'
#' \code{coeff_split} returns a named list of coordinates from a vector of
#' harmonic coefficients. For instance, harmonic coefficients are arranged in
#' the \code{$coe} slot of \code{Coe}-objects in that way: \deqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{efourier} and \link{efourier}) while \deqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#'
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @keywords Miscellaneous
#' @examples
#' coeff_split(1:128, nb.h=32, cph=4) # efourier
#' coeff_split(1:64, nb.h=32, cph=2)  # t/r fourier
#' @export
coeff_split <- function(cs, nb.h = 8, cph = 4) {
    if (missing(nb.h)) {
        nb.h <- length(cs)/cph
    }
    cp <- list()
    for (i in 1:cph) {
        cp[[i]] <- cs[1:nb.h + (i - 1) * nb.h]
    }
    names(cp) <- paste(letters[1:cph], "n", sep = "")
    return(cp)
}

#' Calculates harmonic power given a list from e/t/rfourier
#'
#' Given a list with \code{an, bn (and eventually cn and dn)}, returns the
#' harmonic power.
#'
#' @param xf A list with an, bn (and cn, dn) components, typically from a
#' e/r/tfourier passed on coo_
#' @return Returns a \code{vector} of harmonic power
#' @keywords Miscellaneous
#' @examples
#'
#' data(bot)
#' ef <- efourier(bot[1], 24)
#' rf <- efourier(bot[1], 24)
#' harm_pow(ef)
#' harm_pow(rf)
#'
#' plot(cumsum(harm_pow(ef)[-1]), type='o',
#'   main='Cumulated harmonic power without the first harmonic',
#'   ylab='Cumulated harmonic power', xlab='Harmonic rank')
#'
#' @export
harm_pow <- function(xf) {
    if (is.list(xf)) {
        if (all(c("an", "bn", "cn", "dn") %in% names(xf))) {
            hp <- (xf$an^2 + xf$bn^2 + xf$cn^2 + xf$dn^2)/2
        } else {
            if (all(c("an", "bn") %in% names(xf))) {
                hp <- (xf$an^2 + xf$bn^2)/2
            }
        }
        names(hp) <- paste0("H", 1:length(hp))
        return(hp)
    } else {
        stop(" * a list containing 'an', 'bn' ('cn', 'dn') harmonic coefficients must be provided")
    }
}

##### end misc Fourier

#' Some vector utilities.
#'
#' Returns ratio of norms and signed angle between two vectors provided as four
#' numeric.
#'
#' @param r1 the 'real' part of the first vector, i.e. difference in
#' x-coordinates.
#' @param i1 the 'imaginary' part of the first vector, i.e. difference in
#' y-coordinates.
#' @param r2 the 'real' part of the second vector, i.e. difference in
#' x-coordinates.
#' @param i2 the 'imaginary' part of the second vector, i.e. difference in
#' y-coordinates.
#' @return A list with two components: \code{r.norms} the ratio of (norm of
#' vector 1)/(norm of vector 2) and \code{d.angle} the signed angle 'from' the
#' first 'to' the second vector.
#' @keywords Miscellaneous
#' @examples
#' vecs_param(1, 0, 0, 2)
#'
#' @export vecs_param
vecs_param <- function(r1, i1, r2, i2) {
    x <- c(r1, i1, r2, i2)
    if (!is.numeric(x)) {
        stop("4 numeric must be passed.")
    }
    if (length(x) != 4) {
        stop("4 numeric must be passed.")
    }
    r.norms <- sqrt((r2^2 + i2^2))/sqrt((r1^2 + i1^2))
    d1 <- sqrt(sum(r1^2 + i1^2))
    d2 <- sqrt(sum(r2^2 + i2^2))
    return(list(r.norms = d1/d2, d.angle = atan2(i2, r2) - atan2(i1,
        r1)))
}

##### Various utilities

# x = any vector
# conf = gaussian quantile
#' @export
.which.out <- function(x, conf=1e-4){
  out <- which(dnorm(x, mean(x), sd(x))< conf)
  if(length(out)==0) {
    return(NA)
  } else {
    return(out)}}

# Was used in $fac handling when creating Coo object, before numeric could be
# accepted in it (as covariables)
#'@export
.refactor <- function(df) {
    data.frame(lapply(df, factor))
}

# Used in Coo/Coe printers
#'@export
.print.fac <- function(fac){
  nf <- ncol(fac)
  # here we print the number of classifiers
  if (nf == 0) {
    cat(" - $fac: No classifier defined in $fac\n")
  } else {
    if (nf<2) {
      cat(" - $fac:", nf, "classifier:\n")
    } else {
      cat(" - $fac:", nf, "classifiers:\n")}
    # here we print every classifier
    for (i in 1:nf) {
      if (is.numeric(fac[, i])){
        xi <- fac[, i]
        nas <- sum(is.na(xi))
        xi  <- xi[!is.na(xi)]
        xi.sum <- list(min=min(xi), med=median(xi), max=max(xi), mean=mean(xi), sd=sd(xi))
        xi.sum <- lapply(xi.sum, signif, 3)
        xi.sum$nas <- nas
        cat("     '", colnames(fac)[i], "' (numeric): ",
            #"min:", xi.sum$min,
            #", med:", xi.sum$med,
            #", max: ", xi.sum$max,
            #"mean:", xi.sum$mean,
            #", sd: ", xi.sum$sd,
            "mean: ", xi.sum$mean, ", sd: ", xi.sum$sd,
            ifelse(xi.sum$nas==0, ".\n", paste0(" (", xi.sum$nas, " NA).\n")), sep="")
      } else {
        # case where the column is a factor
        lev.i <- levels(fac[, i])
        # cosmectics below
        if (sum(nchar(lev.i))>60){
          maxprint <- which(cumsum(nchar(lev.i))>30)[1]
          cat("     '", colnames(fac)[i], "' (factor ", nlevels(fac[, i]), "): ", paste(lev.i[1:maxprint], collapse=", "),
              " ... + ", length(lev.i) - maxprint, " more.\n", sep="")
        } else {
          cat("     '", colnames(fac)[i], "' (factor ", nlevels(fac[, i]), "): ", paste(lev.i, collapse=", "), ".\n", sep="")
        }
      }
    }
  }
}

### prepare a factor according to waht is passed to various methods,
# notably multivariate plotters..prep.fac(bp, 1)
# eg
#  bp <- PCA(efourier(bot))
# .prep.fac(bp, 2)
# .prep.fac(bp, "type")
# .prep.fac(bp, factor(rep(letters[1:4], each=10)))
# .prep.fac(bp, ~type)
# .prep.fac(bp)
#' @export
.prep.fac <- function(x, fac){
  ### missing case
  if (missing(fac)) {
    fac <- NULL
  }
  ### formula case (is.formula doesnt exist)
  if (class(fac)=="formula"){
    f0 <- x$fac[, attr(terms(fac), "term.labels")]
    fac <- interaction(f0)
  }
  ### column id case
  if (is.numeric(fac)) {
    if (fac > ncol(x$fac)) 
      stop(fac, " is not a valid column id")
    fac <- factor(x$fac[, fac]) }
  ### column name case
  if (is.character(fac)) {
    if (!any(colnames(x$fac) == fac)) 
      stop(fac, " is not an existing column name")
    fac <- factor(x$fac[, fac]) }
  ### factor case
  if (is.factor(fac)) {
    if (length(fac) != nrow(x$fac)) 
      stop("'fac' length and number of individuals differ")
    # we need it to refactor in subset cases
    fac <- factor(fac) 
  }
  return(fac)
}


#' @export
.trim.ext <- function(lf, width = nchar(lf) - 4) {
    return(strtrim(lf, width = width))
}

#' @export
.trim.path <- function(lf) {
    lf0 <- strsplit(lf, "/")
    lf0 <- sapply(lf0, function(x) x[length(x)])
#     lf0 <- substr(lf0, 1, nchar(lf0) - 4)
    return(lf0)
}

#' @export
.lf.auto <- function() {
    p <- file.choose()
    # damn ugly
    p <- strsplit(p, split = "/")
    p <- p[[1]][-length(p[[1]])]
    p <- paste0(p, collapse = "/")
    lf <- list.files(p, full.names = TRUE)
    return(lf)
}

#' @export
.normalize <- function(x, min.x, max.x) {
    # damn long but default arguments are not accepted
    if (missing(min.x))
        min.x <- min(x)
    x <- x - min(x)
    if (missing(max.x))
        max.x <- max(x)
    x <- x/max.x
    return(x)
}

#' @export
.mat.buffer <- function(m, buff.size, buff.fill = 1) {
    nr <- nrow(m)
    c.buff <- matrix(buff.fill, nrow = nr, ncol = buff.size)
    m <- cbind(c.buff, m, c.buff)
    nc <- ncol(m)
    r.buff <- matrix(buff.fill, nrow = buff.size, ncol = nc)
    m <- rbind(r.buff, m, r.buff)
    return(m)
}

#' @export
.mat.unbuffer <- function(m, unbuff.size) {
    nr <- nrow(m)
    m <- m[-c(1:unbuff.size, (nr - unbuff.size + 1):nr), ]
    nc <- ncol(m)
    m <- m[, -c(1:unbuff.size, (nc - unbuff.size + 1):nc)]
    return(m)
}

#' @export
.mat.resize <- function(m, ratio) {
    dm <- floor(dim(m)/ratio)
    return(m[round(seq(1, nrow(m), len = dm[1])), round(seq(1,
        ncol(m), len = dm[2]))])
}

##### End Miscellaneous
