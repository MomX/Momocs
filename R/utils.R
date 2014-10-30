
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

### prepare a factor according to waht is passed to various methods,
# notably multivariate plotters..prep.fac(bp, 1)
# eg
#  bp <- PCA(eFourier(bot))
# .prep.fac(bp, 2)
# .prep.fac(bp, "type")
# .prep.fac(bp, factor(rep(letters[1:4], each=10)))
# .prep.fac(bp, ~type)
# .prep.fac(bp)

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



.trim.ext <- function(lf, width = nchar(lf) - 4) {
  return(strtrim(lf, width = width))
}


.trim.path <- function(lf) {
  lf0 <- strsplit(lf, "/")
  lf0 <- sapply(lf0, function(x) x[length(x)])
  lf0 <- substr(lf0, 1, nchar(lf0) - 4)
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

