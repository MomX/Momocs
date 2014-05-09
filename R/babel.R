# 1. Simple bridges between R classes ---------------------------------------------
#' Converts a list of coordinates to a matrix.
#' 
#' \code{l2m} converts a \code{list} with x and y components to a 2-col
#' \code{matrix} of coordinates.
#' 
#' @export
#' @usage l2m(l)
#' @param l A \code{list} with x and y coordinates as components.
#' @return Returns a matrix of \eqn{(x; y)}coordinates.
#' @seealso \link{m2l}.
#' @keywords Babel
#' @examples
#' 
#' l <- list(x=1:5, y=5:1)
#' l2m(l)

l2m  <- function(l) {
  m <- cbind(l$x, l$y)
  colnames(m) <- c("x", "y")
  return(m)}

#' Convert a matrix of coordinates to a list of coordinates.
#' 
#' \code{m2l} converts a matrix of \eqn{(x; y)}coordinates to a list with
#' \code{x; y} components.
#' 
#' @export
#' @usage m2l(m)
#' @param m A 2-columns \code{matrix} containing x and y coordinates.
#' @return Returns a \code{list} with \eqn{x; y} components.
#' @seealso \link{l2m}.
#' @keywords Babel
#' @examples
#' 
#' \dontrun{
#' data(gorf.dat)
#' m2l(gorf.dat[,,1])
#' }
m2l  <- function(m) {return(list(x=m[,1], y=m[,2]))}

#' Converts a list of coordinates to an array.
#' 
#' \code{l2a} converts a list of \code{k} matrices with n-rows and n-col
#' matrices to a \code{m x n x k} array.
#' 
#' @export
#' @usage l2a(l)
#' @param l A \code{list} of matrices of the same dimension.
#' @return An array of coordinates.
#' @seealso \link{a2l}.
#' @keywords Babel
#' @examples
#' 
#' \dontrun{
#' data(gorf.dat)
#' l <- a2l(gorf.dat)
#' a <- l2a(l)
#' A.plot(a)
#' 	}
l2a  <- function(l){
  return(array(unlist(l), dim=c(nrow(l[[1]]), ncol(l[[1]]), length(l))))}

#' Converts an array of coordinates to a list.
#' 
#' \code{a2l} converts an array of coordinates into a list of 2-cols matrices.
#' 
#' 
#' @usage a2l(a)
#' @param a An \code{array} of coordinates.
#' @return A \code{list} with 2-cols matrices of \eqn{(x; y)} coordinates.
#' @seealso \link{l2a}
#' @keywords Babel
#' @examples
#' 
#' #data(gorf.dat) # we import gorf.data from shapes package
#' #l <- a2l(gorf.dat)
#' #a <- l2a(l)
#' #A.plot(a)
#' 
#' @export a2l
a2l <- function(a){
  if (!is.array(a)) stop("An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {l[[i]] <- a[,,i]}
  return(l)}

# Import/Export morphometrics formats ------------------------------------------

# pix2chc <- function(coo) {
#   if (is.list(coo)) {
#     coo <- l2m(coo)}
#   if (is.matrix(coo) & ncol(coo)!=2) {
#     stop("A 2 col matrix must be provided")}
#   coo.d <- apply(coo, 2, diff)
#   if (!all(coo.d %in% -1:1)) {
#     stop("Matrix must contain only entire pixels indices")}
#   if (any(apply(coo.d, 1, function(x) all(x==rep(0, 2))))) {
#     stop("At least two succesive coordinates don't code for a displacement")}
#   m   <- as.matrix(expand.grid(-1:1, -1:1))[-5,]
#   g   <- c(5, 6, 7, 4, 0, 3, 2, 1)
#   chc <- g[apply(coo.d, 1, function(x) which(x[1]==m[, 1] & x[2]==m[, 2]))] #dirty
#   return(chc)}
# 
# chc2pix <- function(chc){
#   if (!all(chc %in% 0:7)) {
#     stop("chc string must only contain integers between 0 and 7")}
#   m <- matrix(c(1, 0, 1, 1, 0, 1, -1, 1,
#                 -1, 0, -1, -1, 0, -1, 1, -1), ncol=2, byrow=TRUE)
#   pix <- apply(m[chc+1,], 2, cumsum)
#   return(pix)}
# 
# Coo2chc <- function(Coo, file="chc.chc"){ 
#   res <- list()
#   pb <- txtProgressBar(1, Coo@coo.nb)
#   for (i in 1:Coo@coo.nb){
#     res[[i]] <- c(Coo@names[i], rep(1, 3), Polygon(list(Coo@coo[[i]]))@area,
#                   pix2chc(Coo@coo[[i]]), -1, "\n")
#     setTxtProgressBar(pb, i)}
#   cat(unlist(res), file=file)
#   cat(".chc file succesfully written here:", file)}
# 
# chc2Coo <- function(chc.path){
#   chc <- readLines(chc.path)
#   coo.list <- list()
#   coo.names <- character()
#   for (i in seq(along=chc)) {
#     chc.i <- unlist(strsplit(chc[i], " "))
#     rm    <- match(chc.i, c("", " ", "-1"), , nomatch=0)
#     if (any(rm)) {chc.i <- chc.i[rm==0]}
#     coo.names[i] <- chc.i[1]
#     st    <- as.numeric(chc.i[2:3])
#     pix.i <- chc2pix(as.numeric(chc.i[-(1:5)]))
#     coo.list[[i]] <- coo.trans(pix.i, st[1], st[2])
#   }
#   names(coo.list) <- coo.names
#   return(Coo(coo.list))}
# 
nef2Coe <- function(nef.path) {
  # change nef to coe one day
  nef     <- readLines(nef.path)
  HARMO.l <- grep(pattern="HARMO", nef)
  nb.h    <- as.numeric(substring(nef[HARMO.l], 8))
  nef     <- nef[-(1:HARMO.l)]
  nb.coo  <- length(nef)/(nb.h+1)
  coo.i   <- 1:nb.coo
  coo.beg <- (coo.i-1)*(nb.h + 1)+1
  coo.end <- coo.beg + nb.h
  res     <- matrix(NA, nrow=nb.coo, ncol=nb.h*4, dimnames=
                      list(nef[coo.beg],
                           paste0(rep(LETTERS[1:4], each=nb.h), 1:nb.h)))
  reorder <- c(1:nb.h *4 - 3, 1:nb.h *4 - 2, 1:nb.h *4 - 1, 1:nb.h *4)
  for (i in seq(along=coo.i)) {
    nef.i    <- nef[(coo.beg[i]+1) : coo.end[i]]
    x        <- as.numeric(unlist(strsplit(nef.i, " ")))
    x        <- x[!is.na(x)]
    res[i, ] <- x[reorder]}
  return(res)}

# todo
# Coe2nef <- function(Coe, file="nef.nef"){
#   nb.h      <- Coe@nb.h
#   coo.names <- Coe@names
#   coo.i   <- 1:length(coo.names)
#   coo.beg <- (coo.i-1)*(nb.h + 1)+1
#   coo.end <- coo.beg + nb.h
#   #nef <- c("#CONST ", constant.coeff, " \n", "#HARMO ", nb.h, " \n")
#   nef <- character()
#   for (i in seq(along=coo.names)) {
#     nef <- append(nef, c(coo.names[i], "\n"))
#     for (j in 1:nb.h){
#       coeff.i <- round(as.numeric(Coe@coeff[i, (0:3)*nb.h+j]), 8)
#       nef     <- append(nef, c(coeff.i, "\n"))}
#   }
#   cat(nef, file=file)
#   cat(".nef file succesfully written here:", file)
# }
# 
# 
# Coo2morphoJ <- function(arr, file="morphoJ.txt"){
#   if (file.exists(file=file)) {file.remove(file=file)}
#   for (i in 1:length(arr)) {
#     cat(names(arr)[i], ", ",
#         paste(as.character(t(arr[[i]])), collapse=", "),
#         "\n",
#         sep="", file=file, labels=NULL, append=TRUE)}
# }

