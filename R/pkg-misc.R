##### Miscellaneous functions

# run this one to update DESCRIPTION with no pain
# source("~/Research/Momocs/update.R")

#' Install and load the last version of Momocs
#'
#' Download the last version of Momocs from its GitHub account
#' from \code{http://www.github.com/vbonhomme/Momocs}), install it and load it (\code{library(Momocs)}).
#' You need devtools, but it is checked anyway.
#' @rdname Momocs_version
#' @examples
#' \dontrun{
#' Momocs_currentGitHubversion()
#' Momocs_currentCRANversion()
#' }
#' @export
Momocs_lastversion <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools needed for this function to work. Please install it with install.packages('devtools')",
         call. = FALSE)
  }
  devtools::install_github("vbonhomme/Momocs", build_vignettes= TRUE)
  library(Momocs)
  message("Last version of Momocs has been installed from GitHub and loaded into R")
}

#' @rdname Momocs_version
#' @export
Momocs_currentGitHubversion <- function(){
  "https://raw.githubusercontent.com/vbonhomme/Momocs/master/DESCRIPTION" %>%
    readLines(n=3) %>%
    `[`(3) %>%
    gsub("Version: ", "", .)
}

#' @rdname Momocs_version
#' @export
Momocs_currentCRANversion <- function(){
  x <- readLines("https://cran.r-project.org/web/packages/Momocs/index.html")
  x[grep("<td>Version:</td>", x)+1] %>%
    gsub("<td>", "", .) %>% gsub("</td>", "", .)
}


#' Browse Momocs online doc
#'
#' Launch a browser to an online version of the manual
#' @param topic the function name to access. If not specified the homepage of
#' the online manual is accessed.
#' @export
Momocs_help <- function(topic=NULL){
  url <- "http://vbonhomme.github.io/Momocsdoc/"
  if (!is.null(topic)) url <- paste0(url, topic, ".html")
  browseURL(url)
}

#' Get Momocs installed, CRAN and GitHubversions
#'
#' Simple wrappers to check installed and available Momocs versions.
#' @export
Momocs_version <- function() packageVersion("Momocs")

## ' @export
# Momocs_currentCRANversion <- function(){
#   "https://raw.githubusercontent.com/vbonhomme/Momocs/master/DESCRIPTION" %>%
#     readLines(n=3) %>%
#     `[`(3) %>%
#     gsub("Version: ", "", .)
# }

#' @export
Momocs_currentGitHubversion <- function(){
  "https://raw.githubusercontent.com/vbonhomme/Momocs/master/DESCRIPTION" %>%
    readLines(n=3) %>%
    `[`(3) %>%
    gsub("Version: ", "", .)
}

#' List available methods for any object and other classes for a method
#'
#' Given any object
#' returns all available methods for its class, or given a method
#' list all other supported classes
#'
#' @param x any object
#' @examples
#' #methods for data.frame
#' list_methods_for_this_class(iris)
#'
#' #methods for Coo objects
#' list_methods_for_this_class(bot)
#'
#' #methods for Coe objects
#' list_methods_for_this_class(bot %>% efourier)
#'
#' #methods for PCA objects
#' list_methods_for_this_class(bot %>% efourier %>% PCA)
#'
#' # classes supported for
#' list_classes_for_this_method(PCAAA) # nothing
#' list_classes_for_this_method(PCA)
#' list_classes_for_this_method(sum) # nothing
#' @rdname list_classes
#' @export
list_methods_for_this_class <- function(x){
  for (i in class(x)){
    cat("* '", i, "' class:\n", sep="")
    cat(paste0("  - ", methods(class=i), "\n"), sep="")
  }
}

#' @rdname list_classes
#' @export
list_classes_for_this_method <- function(x) {
  m <- try(suppressWarnings(methods(x)), silent=TRUE)
  if (!("try-error" %in% class(m)) && length(m)!=0)
    cat(paste0("  - ", m, "\n"), sep="")
  else
    NULL
}

#' Calculates euclidean distance between two points.
#'
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates.
#'
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm_nearest}, \link{dist}.
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
#' @examples
#' edi(c(0,1), c(1,0), r = 0.5)
#' @export
edi <- function(pt1, pt2, r = 0.5) {
  return(r * (pt2 - pt1) + pt1)
}

#' Calculates euclidean distance every pairs of points in two matrices.
#'
#' \code{edm} returns the euclidean distances between points \eqn{1 -> n} of
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
#' So far this function is quite time consumming since it performs \eqn{ n
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
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm_nearest(x, x+rnorm(10))
#' edm_nearest(x, x+rnorm(10), full=TRUE)
#' @export
edm_nearest <- function(m1, m2, full = FALSE) {
  m1 <- coo_check(m1)
  m2 <- coo_check(m2)
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
    data.frame(shape=names(x)[out[, 1]],
               id=out[, 1],
               row=out[, 2],
               coordinate=c("x", "y")[out[, 3]], row.names = NULL)
  }
}

##### Miscellaneous functions for Fourier-based approaches

#' Helps to select a given number of harmonics from a numerical vector.
#'
#' \code{coeff_sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{$coe} slot of \code{\link{Coe}}-objects in
#' that way: \eqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{efourier} and
#' \link{efourier}) while \eqn{C_n and D_n} harmonic are absent for radii
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
#' the \code{$coe} slot of \code{Coe}-objects in that way: \eqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{efourier} and \link{efourier}) while \eqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#'
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
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

#' Rearrange a matrix of (typically Fourier) coefficients
#'
#' Momocs uses colnamed matrices to store (typically) Fourier coefficients
#' in \link{Coe} objects (typically \link{OutCoe}). They are arranged as rank-wise:
#' \code{A1, A2, ..., An, B1, ..., Bn, C1, ..., Cn, D1, ..., Dn}. From other softwares they may arrive
#' as \code{A1, B1, C1, D1, ..., An, Bn, Cn, Dn}, this functions helps to go
#' from one to the other format. In short, this function rearranges column order. See examples.
#'
#' @param x matrix (with colnames)
#' @param by character either "name" (\code{A1, A2, ..}) or "rank" (\code{A1, B1, ...})
#' @examples
#' m_name <- m_rank <- matrix(1:32, 2, 16)
#' # this one is order by name
#' colnames(m_name) <- paste0(rep(letters[1:4], each=4), 1:4)
#' # this one is order by rank
#' colnames(m_rank) <- paste0(letters[1:4], rep(1:4, each=4))
#'
#' m_rank
#' m_rank %>% coeff_rearrange(by="name")
#' m_rank %>% coeff_rearrange(by="rank") #no change
#'
#' m_name
#' m_name %>% coeff_rearrange(by="name") # no change
#' m_name %>% coeff_rearrange(by="rank")
#' @export
coeff_rearrange <- function(x, by=c("name", "rank")[1]){
  map <- data.frame(old_id=1:ncol(x),
                    old_cn=colnames(x),
                    name=x %>% colnames %>% substr(1, 1),
                    rank=x %>% colnames %>% substr(2, nchar(.)) %>% as.numeric) %>%
    dplyr::arrange_(by) %>%
    mutate(new_cn=paste0(name, rank))
  return(x[, map$old_id])
}


#' Calculates harmonic power given a list from e/t/rfourier
#'
#' Given a list with \code{an, bn (and eventually cn and dn)}, returns the
#' harmonic power.
#'
#' @param xf A list with an, bn (and cn, dn) components, typically from a
#' e/r/tfourier passed on coo_
#' @return Returns a \code{vector} of harmonic power
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
    stop("a list containing 'an', 'bn' ('cn', 'dn') harmonic coefficients must be provided")
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
#' @examples
#' vecs_param(1, 0, 0, 2)
#'
#' @export vecs_param
vecs_param <- function(r1, i1, r2, i2) {
  x <- c(r1, i1, r2, i2)
  if (!is.numeric(x)) {
    stop("4 numeric must be passed")
  }
  if (length(x) != 4) {
    stop("4 numeric must be passed")
  }
  r.norms <- sqrt((r2^2 + i2^2))/sqrt((r1^2 + i1^2))
  d1 <- sqrt(sum(r1^2 + i1^2))
  d2 <- sqrt(sum(r2^2 + i2^2))
  return(list(r.norms = d1/d2, d.angle = atan2(i2, r2) - atan2(i1,
                                                               r1)))
}

##### Various utilities

# check a condition. if not satisfied, stops with a message
.check <- function(cond_to_pass, msg_if_not) {
  if (!cond_to_pass)
    stop(msg_if_not, call. = FALSE)
}

# can be used is combination with try()
# TRUE/FALSE
is.error <- function(x) {
  if (is.list(x))
    return(any(sapply(x, is.error)))
  any(class(x)=="try-error")
}

#id
which.is.error <- function(x){
  if (is.list(x))
    return(which(sapply(x, is.error)))
  which(class(x)=="try-error")
}

# refactor factors in a data.frame (mainly to drop levels) but respect anything else
#' @export
.refactor <- function(df) {
  data.frame(lapply(df, function(x) if (is.factor(x)) factor(x) else x))
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


# tests for the presence of a $fac slot
is.fac <- function(x) length(x$fac) > 0



.mprod <- function(m, s) {
  res <- m
  for (i in 1:ncol(m)) {
    res[, i] <- m[, i] * s[i]
  }
  return(res)
}


# x = any vector
# conf = gaussian quantile

.which.out <- function(x, conf=1e-4){
  out <- which(dnorm(x, mean(x), sd(x))< conf)
  if(length(out)==0) {
    return(NA)
  } else {
    return(out)}}

# Was used in $fac handling when creating Coo object, before numeric could be
# accepted in it (as covariables)

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

.trim.ext <- function(lf, width = nchar(lf) - 4) {
  return(strtrim(lf, width = width))
}


.trim.path <- function(lf) {
  lf0 <- strsplit(lf, "/")
  lf0 <- sapply(lf0, function(x) x[length(x)])
  return(lf0)
}


.lf.auto <- function() {
  p <- file.choose()
  # damn ugly
  p <- strsplit(p, split = "/")
  p <- p[[1]][-length(p[[1]])]
  p <- paste0(p, collapse = "/")
  lf <- list.files(p, full.names = TRUE)
  return(lf)
}


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


.mat.buffer <- function(m, buff.size, buff.fill = 1) {
  nr <- nrow(m)
  c.buff <- matrix(buff.fill, nrow = nr, ncol = buff.size)
  m <- cbind(c.buff, m, c.buff)
  nc <- ncol(m)
  r.buff <- matrix(buff.fill, nrow = buff.size, ncol = nc)
  m <- rbind(r.buff, m, r.buff)
  return(m)
}


.mat.unbuffer <- function(m, unbuff.size) {
  nr <- nrow(m)
  m <- m[-c(1:unbuff.size, (nr - unbuff.size + 1):nr), ]
  nc <- ncol(m)
  m <- m[, -c(1:unbuff.size, (nc - unbuff.size + 1):nc)]
  return(m)
}


.mat.resize <- function(m, ratio) {
  dm <- floor(dim(m)/ratio)
  return(m[round(seq(1, nrow(m), len = dm[1])), round(seq(1,
                                                          ncol(m), len = dm[2]))])
}

.rcolors2hex <- function(x){
  x <- x %>% grDevices::col2rgb()
  if ("alpha" %in% rownames(x)){
    apply(x, 2, function(.) rgb(.[1], .[2], .[3], alpha=.[4], maxColorValue = 255))
  } else {
    apply(x, 2, function(.) rgb(.[1], .[2], .[3], maxColorValue = 255))
  }
}

# handles fac grabbing via formula or $fac column name
.fac_dispatcher <- function(x, fac){
  # factor case
  if (is.factor(fac))
    return(fac)
  # formula case
  if (class(fac) == "formula") {
    column_name <- attr(terms(fac), "term.labels")
    if (any(is.na(match(column_name, colnames(x$fac)))))
      stop("formula provided must match with $fac column names")
    fac <- x$fac[, column_name]
    if (is.data.frame(fac))
      fac <- factor(apply(fac, 1, paste, collapse = "_"))
  }
  # column case
  if (length(fac) == 1) {
    if (!(fac %in% colnames(fac)))
        stop("invalid column name")
    fac <- x$fac[, fac]
  }
  return(fac)
}
# .fac_dispatcher(bot, "bot") # expect invalid
# .fac_dispatcher(bot, 1)
# .fac_dispatcher(bot, "type")
# .fac_dispatcher(bot, ~type)
##### End Miscellaneous
