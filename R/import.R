# Functions and utilities to import data in Momocs, notably from raw images,
# and to ease the ahndling of these data (define landmarks, outlines, etc.)


#' Extract outlines from an image mask
#' 
#' Provided with an image mask (black/1 pixels over a white/0 background), 
#' and with the coordinates of a point within the shape, returns the (x; y)
#' coordinates of the outline.
#' @export import.Conte
#' @param img a matrix that corresponds to an binary image mask
#' @param x numeric the (x; y) coordinates of a starting point within the shape.
#' @return a matrix, the (x; y) coordinates of the outline points.
#' @keywords import
import.Conte <- function (img, x){ 
  while (abs(img[x[1], x[2]] - img[x[1] - 1, x[2]]) < 0.1) {
    x[1] <- x[1] + 1
  }

#   while (abs(img[x[1], x[2]] - img[x[1], x[2]-1]) < 0.1) {
#     x[2] <- x[2] -1
#   }
  
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

#' Import coordinates from a .txt file
#' 
#' A wrapper around read.table. May be used to import outline/landmark coordinates
#' By default it assumes that the columns are not named in the .txt files. You can
#' tune this using the '...' argument.
#' @export import.txt
#' @param txt.list a vector of paths corresponding to the .txt files to import
#' @param ... arguments to be passed to \link{read.table}, eg. 'skip', 'dec', etc.
#' @return a list of matrix(ces) of (x; y) coordinates that can be passed to
#' Out, Opn, Ldk, etc.
#' @keywords import
import.txt <- function(txt.list, ...){
  cat("Extracting", length(txt.list), "..txt coordinates...\n")
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

#' Extract outline coordinates from a single .jpg file
#' 
#' Used to import outline coordinates from .jpg files. This function is used for
#' single images and is the core function of \link{import.jpg}
#' @export import.jpg1
#' @param jpg.path a vector of paths corresponding to the .jpg files to import
#' @param auto.notcentered logical if TRUE random locations will be used until
#' one of them is (assumed) to be within the shape (because of a black pixel);
#' if FALSE a \link{locator} will be called, and you will have to click on a 
#' point within the shape.
#' @param threshold the threshold value use to binarize the images. Above, pixels
#' are turned to 1, below to 0.
#' @param ... arguments to be passed to \link{read.table}, eg. 'skip', 'dec', etc.
#' @details jpegs can be provided
#' either as RVB or as 8-bit greylevels or monochrome. The function binarizes
#' pixels values using the 'threshold' argument. It will try to start apply
#' the \link{import.Conte} algortih of outline extraction from the center of
#' the image and 'loinking' downwards for the first black -> white 'frontier' in
#' the pixels. This point will be the first of the outlines and it may be useful
#' if you align manually the images and if you want to retain this information
#' in the consequent morphometric analyses. If the point at the center of the 
#' image is not within the shape, ie is "white" you have two choices defined by
#' the 'auto.notcentered' argument. If it's TRUE, some random starting points
#' will be tried until on of them is "black" and within the shape; if FALSE
#' you will be asked to click on a point within the shape.
#' @return a matrix of (x; y) coordinates that can be passed to Out
import.jpg1 <- function(jpg.path, auto.notcentered=FALSE, threshold=0.5){
  img <- readJPEG(jpg.path)
  if (!is.matrix(img)) {
    img <- (img[,,1] + img[,,2] + img[,,3])/3}
  img[img >  threshold] <- 1
  img[img <= threshold] <- 0
#   img <- x[dim(x)[1]:1,] #Conte/readJPEG, etc.
  x <- round(dim(img)/2)
  if (img[x[1], x[2]] != 0){
    if (auto.notcentered){
      while (img[x[1], x[2]] != 0) {
        x[1] <- sample(dim(img)[1], 1)
        x[2] <- sample(dim(img)[2], 1)}
    } else {
      img.plot(img)
      while (img[x[1], x[2]] != 0) {
        cat(" * Click a point within the shape\n")
        x <- rev(round(unlist(locator(1))))
        if (x[1]>dim(img)[1]) x[1] <- dim(img)[1]
        if (x[2]>dim(img)[2]) x[2] <- dim(img)[2]}}}
  out <- import.Conte(img, x)
  return(out)}

#import.jpg.multi  #todo

#' Extract outline coordinates from multiple .jpg files
#' 
#' This function is used to import outline coordinates and is built around 
#' \link{import.jpg1}
#' @export import.jpg
#' @param jpg.paths a vector of paths corresponding to the .jpg files to import
#' @param auto.notcentered logical if TRUE random locations will be used until
#' one of them is (assumed) to be within the shape (because of a black pixel);
#' if FALSE a \link{locator} will be called, and you will have to click on a 
#' point within the shape.
#' @param threshold the threshold value use to binarize the images. Above, pixels
#' are turned to 1, below to 0.
#' @param verbose whether to print which file is being treated. Useful to detect problems.
#' @details see \link{import.jpg1} and \link{import.Conte}.
#' @return a list of matrices of (x; y) coordinates that can be passed to Out
import.jpg <- function(jpg.paths, auto.notcentered=FALSE, threshold=0.5, verbose=TRUE) {
  cat("Extracting", length(jpg.paths), ".jpg outlines...\n")
  if (length(jpg.paths) > 10) {
    pb <- txtProgressBar(1, length(jpg.paths))
    t <- TRUE } else {t <- FALSE}
  res <- list()
  for (i in seq(along=jpg.paths)) {
    res[[i]] <- import.jpg1(jpg.paths[i],
                            auto.notcentered=auto.notcentered, threshold=threshold)
    if (verbose) {
      cat(jpg.paths[i], "\n")
    } else {
      if (t) setTxtProgressBar(pb, i)
    }
  }
    names(res) <- .trim.path(jpg.paths)
  return(res)}


# # Manipulate raw data inside R--------------------------------------------------
# splines <- function(coo, method="natural", deriv=2){
#   coo <- coo.check(coo)
#   z <- coo.perim.cum(coo)
#   fx <- splinefun(z, coo[, 1], method=method)
#   fy <- splinefun(z, coo[, 2], method=method)
#   xcoe <- fy(z, deriv=2)
#   ycoe <- fy(z, deriv=2)
#   return(list(xcoe=xcoe, ycoe=ycoe))}
# 
# splines2 <- function(coo, nb.pts=100){
#   z <- coo.perim.cum(coo)
#   x.i <- spline(z, coo[, 1], method="natural", n=100)$y
#   y.i <- spline(z, coo[, 2], method="natural", n=100)$y
#   return(cbind(x.i, y.i))}
# 
# click.bez <- function(x, n=10){
#   x <- as.raster(x)
#   plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
#   grid.raster(x)
#   ldk <- matrix(NA, n, 2)
#   bez <- NA
#   ldk[1, ] <- l2m(locator(1))
#   for (i in 2:n){
#     grid.raster(x)
#     lines(bez, col="red")
#     ldk[i, ] <- l2m(locator(1))
#     cat(ldk)
#     bez <- bezier.i(bezier(ldk[1:i,])$B)
#   }}
# click.splines <- function(x, n=20){
#   x <- as.raster(x)
#   plot(NA, xlim=c(1, dim(x)[1]), ylim=c(1, dim(x)[2]), asp=1)
#   grid.raster(x)
#   ldk <- matrix(NA, n, 2)
#   spl <- NA
#   ldk[1, ] <- l2m(locator(1))
#   for (i in 2:n){
#     grid.raster(x)
#     points(ldk[1:i,], pch=20, col="black")
#     lines(spl, col="red")
#     ldk[i, ] <- l2m(locator(1))
#     cat(ldk)
#     spl <- splines2(ldk[1:i,])
#   }}

# Import utilities --------------------------------------------------------

#' Extract structure from filenames
#' 
#' If filenames are consistently named: eg 'speciesI_siteA_ind1_dorsalview', 
#' returns a data.frame from it that can be passed to Out, Opn, Ldk, objects.
#' @export lf.structure
#' @param lf a list filenames, as characters, typically such as
#' those obtained with \link{list.files} OR a path to a folder 
#' containing the files. Actually, if lf is of length 1 (a single character),
#' the function assumes it is a path and do a \link{list.files}.
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
lf.structure <- function(lf, names=character(), split="_", trim.extension=FALSE){
  if(length(lf)==1) {
    lf <- list.files(lf, full.names=FALSE)}
  if (trim.extension) {
    lf <- strtrim(lf, nchar(lf)-4)}
  lf0 <- strsplit(lf, split=split)
  # we check that all files have the same name structure
  nc  <- unique(sapply(lf0, length))
  if (length(nc) !=1 ) {
    stop("The files do not have the same filename structure.")}
  fac <- as.data.frame(matrix(NA, nrow=length(lf), ncol=nc)) # dirty
  if (!missing(names)) {
    if (length(names) != nc) {
      stop("The number of 'names' is different from the number of groups.")}
    names(fac) <- names}
  # nice rownames
  rownames(fac) <- lf
  for (i in 1:nc) {
    # ugly way to fill the df
    fac[, i] <- factor(sapply(lf0, function(x) x[i]))} 
  return(fac)}
