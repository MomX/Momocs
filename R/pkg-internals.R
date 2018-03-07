
# Some vector utilities.
#
# Returns ratio of norms and signed angle between two vectors provided as four
# numeric.
#
# @param r1 the 'real' part of the first vector, i.e. difference in
# x-coordinates.
# @param i1 the 'imaginary' part of the first vector, i.e. difference in
# y-coordinates.
# @param r2 the 'real' part of the second vector, i.e. difference in
# x-coordinates.
# @param i2 the 'imaginary' part of the second vector, i.e. difference in
# y-coordinates.
# @return A list with two components: \code{r.norms} the ratio of (norm of
# vector 1)/(norm of vector 2) and \code{d.angle} the signed angle 'from' the
# first 'to' the second vector.
# @examples
# vecs_param(1, 0, 0, 2)
#
# @export vecs_param
.vecs_param <- function(r1, i1, r2, i2) {
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
.is.error <- function(x) {
  if (is.list(x))
    return(any(sapply(x, .is.error)))
  any(class(x)=="try-error")
}

#id
.which.is.error <- function(x){
  if (is.list(x))
    return(which(sapply(x, .is.error)))
  which(class(x)=="try-error")
}

# refactor factors in a data.frame (mainly to drop levels) but respect anything else
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
    return(fac)
  }
  # column case as character
  if (is.character(fac) && length(fac)==1) {
    if (!(fac %in% colnames(fac)))
      stop("invalid column name")
    return(x$fac[, fac])
  }
  # column case as numeric for column id
  if (is.numeric(fac) && length(fac)==1){
    if (fac > ncol(x$fac))
      stop("invalid column id")
    return(x$fac[, fac])
  }
}
# .fac_dispatcher(bot, "bot") # expect invalid
# .fac_dispatcher(bot, 1)
# .fac_dispatcher(bot, "type")
# .fac_dispatcher(bot, ~type)

