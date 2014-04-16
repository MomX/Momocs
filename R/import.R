# Functions and utilities to import data in Momocs, notably from raw images,
# and to ease the ahndling of these data (define landmarks, outlines, etc.)

# Domestic utilities

#' Extract structure from filenames
#' 
#' If filenames are consistently named: eg 'speciesI_siteA_ind1_dorsalview', 
#' returns a data.frame from it that can be passed to Out, Opn, Ldk, objects.
#' @export lf.structure
#' @param lf a list filenames, as characters, typically such as
#' those obtained with \link{list.files}.
#' @param names the names of the groups.
#' @param split character, the spliiting factor used for the file names.
#' @param trim.extension logical. Whether to remove the last for characters in
#' filenames, typically their extension, eg. ".jpg".
#' @return data.frame with, for every individual, the corresponding label
#' for every group.
#' @details #todo
#' @keywords import
#' @examples
#' data(bot)
#' coo.area(bot[4])
#todo : work either from a path or from a char vect
lf.structure <- function(lf, names=character(), split="_", trim.extension=TRUE){
  if (trim.extension) {
    lf0 <- strtrim(lf0, nchar(lf0)-4)}
  lf  <- strsplit(lf0, split=split)
  # we check that all files have the same filename structure
  nc  <- as.numeric(unique(lapply(lf, length)))
  if (length(nc) !=1 ) {
    stop("The files do not have the same filename structure. See ?get.structure")}
  fac <- as.data.frame(matrix(NA, nrow=length(lf), ncol=nc)) # dirty
  if (!missing(names)) {
    if (length(names) != nc) {
      stop("The number of 'names' is different from the number of groups. See ?get.structure")}
    names(fac) <- names}
  # nice rownames
  rownames(fac) <- lf0
  for (i in 1:nc) {
    fac[, i] <- factor(unlist(lapply(lf, function(x) x[i])))} # ugly way to fill the df
  return(fac)}

# useless ?
.trim <- function(lf, width=nchar(lf)-4) {return(strtrim(lf, width=width))}

.img.plot <- function(img){
  # dirty here but made for convenience
  # to have a fast img plotter..
  if (!is.matrix(img)) { 
    img <- (img[,,1] + img[,,2] + img[,,3])/3 }
  op <- par(mar=rep(5, 4))
  on.exit(par(op))
  h <- nrow(img)
  w <- ncol(img)
  plot(NA, xlim=c(1, w), ylim=c(1, h), asp=1,
       frame=FALSE, axes=TRUE, ann=FALSE)
  rasterImage(img, 1, 1, w, h, interpolate=FALSE)
  .title(paste(w, h, sep=" x "))
  box()}

import.Conte <- function (img, x){ 
  while (abs(img[x[1], x[2]] - img[x[1], (x[2] - 1)]) < 0.1) {
    x[2] <- x[2] - 1
  }
  a <- 1
  M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1), 
              nrow=2, ncol=8, byrow = TRUE)
  M <- cbind(M[, 8], M, M[, 1])
  X <- 0
  Y <- 0
  x1 <- x[1]
  x2 <- x[2]
  SS <- NA
  S <- 6
  while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
    if (abs(img[x[1] + M[1, S + 1], x[2] +
                  M[2, S + 1]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 1]
      SS[a] <- S + 1
      S <- (S + 7)%%8
    } else if (abs(img[x[1] + M[1, S + 2], x[2] +
                         M[2, S + 2]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 2]
      SS[a] <- S + 2
      S <- (S + 7)%%8
    } else if (abs(img[x[1] + M[1, S + 3], x[2] +
                         M[2, S +  3]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 3]
      SS[a] <- S + 3
      S <- (S + 7)%%8
    }
    else {
      S <- (S + 1)%%8
    }
  }
  return(cbind((Y[-1]), ((dim(img)[1] - X))[-1]))
}

# Import raw data ---------------------------------------------------------
import.txt <- function(txt.list, ...){
  cat("Extracting", length(txt.list), ".jpg outlines...\n")
  if (length(txt.list) > 10) {
    pb <- txtProgressBar(1, length(txt.list))
    t <- TRUE } else {t <- FALSE}
  res <- list()
  for (i in seq(along = txt.list)) {
    coo <- read.table(txt.list[i], ...)
    res[[i]] <- as.matrix(coo)
    if (t) setTxtProgressBar(pb, i)
  }
  names(res) <- substr(txt.list, start=1, stop=nchar(txt.list)-4)
  return(res)}

import.jpg <- function(jpg.list, auto.notcentered=FALSE, threshold=0.5) {
  cat("Extracting", length(jpg.list), ".jpg outlines...\n")
  if (length(jpg.list) > 10) {
    pb <- txtProgressBar(1, length(jpg.list))
    t <- TRUE } else {t <- FALSE}
  res <- list()
  for (i in seq(along=jpg.list)) {
    img <- readJPEG(jpg.list[i])
    if (!is.matrix(img)) {
      img <- (img[,,1] + img[,,2] + img[,,3])/3}
    img[img >  threshold] <- 1
    img[img <= threshold] <- 0
    x <- round(dim(img)/2)
    if (img[x[1], x[2]] != 0){
      if (auto.notcentered){
        while (img[x[1], x[2]] != 0) {
          x[1] <- sample(dim(img)[1], 1)
          x[2] <- sample(dim(img)[2], 1)}
      } else {
        .img.plot(img)
        while (img[x[1], x[2]] != 0) {
          cat(" * Click a point within the shape\n")
          x <- rev(round(unlist(locator(1))))
          if (x[1]>dim(img)[1]) x[1] <- dim(img)[1]
          if (x[2]>dim(img)[2]) x[2] <- dim(img)[2]
        }
      }
    }
    res[[i]] <- import.Conte(img, x)
    if (t) setTxtProgressBar(pb, i)}
  names(res) <- substr(jpg.list, start=1, stop=nchar(jpg.list)-4) 
  return(res)}

# Manipulate raw data inside R--------------------------------------------------


splines <- function(coo, method="natural", deriv=2){
  coo <- coo.check(coo)
  z <- coo.perim.cum(coo)
  fx <- splinefun(z, coo[, 1], method=method)
  fy <- splinefun(z, coo[, 2], method=method)
  xcoe <- fy(z, deriv=2)
  ycoe <- fy(z, deriv=2)
  return(list(xcoe=xcoe, ycoe=ycoe))}

splines2 <- function(coo, nb.pts=100){
  z <- coo.perim.cum(coo)
  x.i <- spline(z, coo[, 1], method="natural", n=100)$y
  y.i <- spline(z, coo[, 2], method="natural", n=100)$y
  return(cbind(x.i, y.i))}

click.bez <- function(x, n=10){
  x <- as.raster(x)
  plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
  grid.raster(x)
  ldk <- matrix(NA, n, 2)
  bez <- NA
  ldk[1, ] <- l2m(locator(1))
  for (i in 2:n){
    grid.raster(x)
    lines(bez, col="red")
    ldk[i, ] <- l2m(locator(1))
    cat(ldk)
    bez <- bezier.i(bezier(ldk[1:i,])$B)
  }}
click.splines <- function(x, n=20){
  x <- as.raster(x)
  plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
  grid.raster(x)
  ldk <- matrix(NA, n, 2)
  spl <- NA
  ldk[1, ] <- l2m(locator(1))
  for (i in 2:n){
    grid.raster(x)
    points(ldk[1:i,], pch=20, col="black")
    lines(spl, col="red")
    ldk[i, ] <- l2m(locator(1))
    cat(ldk)
    spl <- splines2(ldk[1:i,])
  }}

