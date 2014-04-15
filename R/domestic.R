
# 1. Package documentation and NAMESPACE import ---------------------------

#' Momocs
#' 
#' Lubridate provides tools that make it easier to parse and 
#' manipulate dates. These tools are grouped below by common 
#' purpose. More information about each function can be found in 
#' its help documentation.
#'
#' Parsing dates
#'
#' Lubridate's parsing functions read strings into R as POSIXct 
#' date-time objects. Users should choose the function whose name 
#' models the order in which the year ('y'), month ('m') and day 
#' ('d') elements appear the string to be parsed: 
#' 
#' ...
#' 
#' @references Garrett Grolemund, Hadley Wickham (2011). Dates and Times
#'   Made Easy with lubridate. Journal of Statistical Software, 40(3),
#'   1-25. \url{http://www.jstatsoft.org/v40/i03/}.
#' @import ape
#' @importFrom jpeg readJPEG
#' @importFrom sp spsample Polygon
#' @importFrom spdep tri2nb
#' @importFrom shapes procGPA
#' @importFrom methods showDefault
#' @importFrom MASS ginv
#' @docType package
#' @name Momocs
NULL

# 5. Morphospace functions -----------------------------------------------------
pca2shp.efourier <- function (pos, rot, mshape, amp=1, nb.pts=60, trans=TRUE) {
  if (ncol(pos) != ncol(rot)) stop("'rot' and 'pos' must have the same ncol")
  if(length(mshape) != nrow(rot)) stop("'mshape' and ncol(rot) lengths differ")
  # stupid function
  mprod <- function(m, s){
    res <- m
    for (i in 1:ncol(m)) { res[, i] <- m[, i]*s[i] }
    return(res)}
  nb.h <- length(mshape)/4
  n  <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- mprod(rot, pos[i, ])*amp
    coe        <- mshape + apply(ax.contrib, 1, sum)
    xf         <- coeff.split(coe)
    coo        <- efourier.i(xf, nb.h = nb.h, nb.pts=nb.pts)
    if (trans) {coo <- coo.trans(coo, x=pos[i, 1], y=pos[i, 2])}
  res[[i]] <- coo}
  return(res)}

pos.shapes <- function(xy, pos.shp=c("range", "circle")[1],
                       nb.shp=12, nr.shp=6, nc.shp=5, circle.r.shp){
  if (is.data.frame(pos.shp) | is.matrix(pos.shp)) {
    return(as.matrix(pos.shp))}
  if (pos.shp=="circle") {
    if (missing(circle.r.shp)) {
      # mean distance from origin
      circle.r.shp <- mean(apply(xy, 1, function(x) sqrt(sum(x^2))))}
    t <- seq(0, 2*pi, len=nb.shp+1)[-(nb.shp+1)]
    pos <- cbind(circle.r.shp*cos(t), circle.r.shp*sin(t))
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="range") {
    pos <- expand.grid(seq(min(xy[, 1]), max(xy[, 1]), len=nr.shp),
                       seq(min(xy[, 2]), max(xy[, 2]), len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)}
  if (pos.shp=="full") {
#     w <- par("usr")
#     pos <- expand.grid(seq(w[1], w[2], len=nr.shp),
#                        seq(w[3], w[4], len=nc.shp))
         w <- par("usr")
         pos <- expand.grid(seq(par("xaxp")[1]*0.9, par("xaxp")[2]*0.9, len=nr.shp),
                            seq(par("yaxp")[1]*0.9, par("yaxp")[2]*0.9, len=nc.shp))
    pos <- as.matrix(pos)
    colnames(pos) <- c("x", "y") # pure cosmetics
    return(pos)   
  }
  # if a non-valid method is passed
  return(xy)}

# 6. Domestic functions -------------------------------------------------------------
ed             <- function(pt1, pt2){return(sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2))}

edi <- function(pt1, pt2, r=0.5){
  return(r*(pt2-pt1) + pt1) }

edm            <- function(m1, m2){return(sqrt((m1[, 1] - m2[, 1])^2 + (m1[, 2] - m2[, 2])^2))}

edm.nearest <- function(m1, m2, full=FALSE){
  if (!is.matrix(m1) | !is.matrix(m2)) stop("Matrices must be provided")
  if (ncol(m1)!=2    | ncol(m2)!=2)    stop("2-cols matrices must be provided")
  nr <- nrow(m1)
  pos <- d  <- numeric(nr)
  for (i in 1:nr){
    m1.i   <- m1[i, ]
    di     <- apply(m2, 1, function(x) sqrt(sum((x - m1.i)^2)))
    d[i]   <- min(di)
    pos[i] <- which.min(di)}
  if (full) return(list(d=d, pos=pos)) else return(d) }

.refactor <- function(df){
  w <- sapply(df, is.factor)
  df[w] <- lapply(df[w], factor)
  df}



# 7. Babel functions and utilities ---------------------------------------------
l2m            <- function(l) {return(cbind(l$x, l$y))}

m2l            <- function(m) {return(list(x=m[,1], y=m[,2]))}

l2a            <- function(l){return(array(unlist(l), dim=c(nrow(l[[1]]), ncol(l[[1]]), length(l))))}
a2l <- function(a){
  if (!is.array(a)) stop("An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {l[[i]] <- a[,,i]}
  return(l)}

fac.structure <- function(path, names=character(), split="_"){
  lf0 <- list.files(path) # we get the list of files
  lf0 <- strtrim(lf0, nchar(lf0)-4) # we remove the file extension
  lf  <- strsplit(lf0, split=split)
  nc  <- as.numeric(unique(lapply(lf, length))) # we check that all files have the same filename structure
  if (length(nc) !=1 ) {
    stop("The files do not have the same filename structure. See ?get.structure")}
  fac <- as.data.frame(matrix(NA, nrow=length(lf), ncol=nc)) # dirty
  if (!missing(names)) {
    if (length(names) != nc) {
      stop("The number of 'names' is different from the number of groups. See ?get.structure")}
    names(fac) <- names}
  rownames(fac) <- lf0 # nice rownames
  for (i in 1:nc) {
    fac[, i] <- factor(unlist(lapply(lf, function(x) x[i])))} # ugly way to fill the df
  return(fac)
}

lf.trim <- function(lf, width=nchar(lf)-4) {return(strtrim(lf, width=width))}

coeff.sel <- function(retain=8, drop=0, nb.h=32, cph=4){
  cs <- numeric()
  for (i in 1:cph) {
    cs <- c(cs, (1+drop):retain + nb.h*(i-1))}
  return(cs)}

coeff.split <- function(cs, nb.h=8, cph=4){
  if (missing(nb.h)) {nb.h <- length(cs)/cph }
  cp <- list()
  for (i in 1:cph) {
    cp[[i]] <- cs[1:nb.h + (i-1)*nb.h]
  }
  names(cp) <- paste(letters[1:cph], "n", sep="")
  return(cp)}


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
# nef2Coe <- function(nef.path) {
#   # change nef to coe one day
#   nef     <- readLines(nef.path)
#   HARMO.l <- grep(pattern="HARMO", nef)
#   nb.h    <- as.numeric(substring(nef[HARMO.l], 8))
#   nef     <- nef[-(1:HARMO.l)]
#   nb.coo  <- length(nef)/(nb.h+1)
#   coo.i   <- 1:nb.coo
#   coo.beg <- (coo.i-1)*(nb.h + 1)+1
#   coo.end <- coo.beg + nb.h
#   res     <- matrix(NA, nrow=nb.coo, ncol=nb.h*4, dimnames=
#                       list(nef[coo.beg],
#                            paste(rep(LETTERS[1:4], each=nb.h), 1:nb.h, sep="")))
#   for (i in seq(along=coo.i)) {
#     nef.i    <- nef[(coo.beg[i]+1) : coo.end[i]]
#     x        <- as.numeric(unlist(strsplit(nef.i, " ")))
#     res[i, ] <- x[!is.na(x)]}
#   return(Coe(res))}
# 
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

# 8. Import functions ----------------------------------------------

# import.txt <- function(txt.list, ...){
#   cat("Extracting", length(txt.list), ".jpg outlines...\n")
#   if (length(txt.list) > 10) {
#     pb <- txtProgressBar(1, length(txt.list))
#     t <- TRUE } else {t <- FALSE}
#   res <- list()
#   for (i in seq(along = txt.list)) {
#     coo <- read.table(txt.list[i], ...)
#     res[[i]] <- as.matrix(coo)
#     if (t) setTxtProgressBar(pb, i)
#   }
#   names(res) <- substr(txt.list, start=1, stop=nchar(txt.list)-4)
#   return(res)}
# 
# import.jpg <- function(jpg.list) {
#   cat("Extracting", length(jpg.list), ".jpg outlines...\n")
#   if (length(jpg.list) > 10) {
#     pb <- txtProgressBar(1, length(jpg.list))
#     t <- TRUE } else {t <- FALSE}
#   res <- list()
#   for (i in seq(along=jpg.list)) {
#     img <- import.img.prepare(jpg.list[i])
#     res[[i]] <- import.img.Conte(img)
#     if (t) setTxtProgressBar(pb, i)
#   }
#   names(res) <- substr(jpg.list, start=1, stop=nchar(jpg.list)-4) 
#   return(res)}
# 
# import.multi1.jpg <- function(path){
#   img <- import.img.prepare(path)
#   res <- list()
#   on.exit(return(res))
#   i <- 1
#   res[[i]] <- import.img.Conte(img, auto=FALSE)
#   cat("1 outline extracted. Press 'ESC' to finish.\n")
#   while (TRUE) {
#     i <- i+1
#     res[[i]] <- import.img.Conte(img, auto=FALSE, plot=FALSE)
#     cat(paste(i, "outlines extracted.\n"))
#   }  
#   return(res)}
# 
# import.img.prepare <- function(path){
#   img <- readJPEG(path)
#   #if (class(img)[2] == "array") {img <- rgb2grey(img)} #to2fixed...ReadImages
#   img[img >  0.5] <- 1
#   img[img <= 0.5] <- 0
#   #img <- imagematrix(img) #to2fixed...ReadImages
#   # if(any(dim(img)>500)) {cat("\t(large image)")}
#   if(any(c(img[1, ], img[nrow(img), ], img[, 1], img[, ncol(img)]) != 1)){
#     # cat("\t(outline spans image border)")
#     img <- rbind(rep(1, ncol(img)), img, rep(1, ncol(img)))
#     img <- cbind(rep(1, nrow(img)), img, rep(1, nrow(img)))
#     #img <- imagematrix(img) #to2fixed...ReadImages
#   }              
#   return(img)}
# 
# import.img.Conte <- 
#   function (img, x, auto=TRUE, plot=TRUE) 
#   {
#     #if (class(img)[1] != "imagematrix") {
#     #  stop("An 'imagematrix' object is expected")}2befixe...ReadImages
#     img <- t(img[nrow(img):1,])
#     if (missing(x)) {
#       if (auto) {
#         x <- round(dim(img)/2)
#       } else { x <- c(1, 1)}}
#     while (img[x[1], x[2]] != 0) {
#       if (plot) {
#         plot(img, main = "Click a point within the shape")
#         rect(0, 0, ncol(img), nrow(img), border = "red")}
#       click <- lapply(locator(1), round)
#       x <- c(nrow(img) - click$y, click$x)
#       if (any(x > dim(img))) {
#         x <- round(dim(img)/2)
#       }
#     }
#     while (abs(img[x[1], x[2]] - img[x[1], (x[2] - 1)]) < 0.1) {
#       x[2] <- x[2] - 1
#     }
#     a <- 1
#     M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, 
#                   -1, 0, 1), 2, 8, byrow = TRUE)
#     M <- cbind(M[, 8], M, M[, 1])
#     X <- 0
#     Y <- 0
#     x1 <- x[1]
#     x2 <- x[2]
#     SS <- NA
#     S <- 6
#     while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
#       if (abs(img[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - 
#                 img[x[1], x[2]]) < 0.1) {
#         a <- a + 1
#         X[a] <- x[1]
#         Y[a] <- x[2]
#         x <- x + M[, S + 1]
#         SS[a] <- S + 1
#         S <- (S + 7)%%8
#       }
#       else if (abs(img[x[1] + M[1, S + 2], x[2] + M[2, S + 
#                                                       2]] - img[x[1], x[2]]) < 0.1) {
#         a <- a + 1
#         X[a] <- x[1]
#         Y[a] <- x[2]
#         x <- x + M[, S + 2]
#         SS[a] <- S + 2
#         S <- (S + 7)%%8
#       }
#       else if (abs(img[x[1] + M[1, S + 3], x[2] + M[2, S + 
#                                                       3]] - img[x[1], x[2]]) < 0.1) {
#         a <- a + 1
#         X[a] <- x[1]
#         Y[a] <- x[2]
#         x <- x + M[, S + 3]
#         SS[a] <- S + 3
#         S <- (S + 7)%%8
#       }
#       else {
#         S <- (S + 1)%%8
#       }
#     }
#     return(cbind((Y[-1]), ((dim(img)[1] - X))[-1]))
#   }
# 



# 9. TPS ------------------------------------------------------------------

# Thin Plate Spline ##################################################

tps2d <- function(grid0, fr, to){
  if (is.closed(fr)) fr <- coo.unclose(fr)
  if (is.closed(to)) to <- coo.unclose(to)
  p  <- nrow(fr)
  q  <- nrow(grid0)
  P  <- matrix(NA, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      r2     <- sum((fr[i,]-fr[j,])^2)
      P[i,j] <- r2*log(r2)}}
  P[is.na(P)] <- 0
  Q  <- cbind(1, fr)
  L  <- rbind(cbind(P, Q), cbind(t(Q), matrix(0,3,3)))
  m2 <- rbind(to, matrix(0, 3, 2))
  coefx <- solve(L)%*%m2[, 1]
  coefy <- solve(L)%*%m2[, 2]
  fx <- function(fr, grid0, coef) {
    Xn <- numeric(q)
    for (i in 1:q) {
      Z     <- apply((fr-matrix(grid0[i, ], p, 2, byrow=TRUE))^2, 1, sum)
      Xn[i] <- coef[p+1]+coef[p+2]*grid0[i,1]+coef[p+3]*grid0[i,2]+
        sum(coef[1:p]*(Z*log(Z)))}
    return(Xn)}
  grid1 <- cbind(fx(fr, grid0, coefx), fx(fr, grid0, coefy))
  return(grid1)}

tps.grid <- function(fr, to, amp=1, plot.full=TRUE, grid.outside = 0.2,
                     grid.size = 20, grid.col   = "grey40",
                     shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                     shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  # simple magnification
  if (!missing(amp)) to <- to + (to-fr)*amp
  # we prepare the grid
  x1     <- min(to[, 1])
  x2     <- max(to[, 1])
  y1     <- min(to[, 2])
  y2     <- max(to[, 2])
  rx     <- x2 - x1
  ry     <- y2 - y1
  dim.grid <- if (rx > ry) { c(grid.size, round(grid.size*ry / rx)) } else { c(round(grid.size*rx / ry), grid.size) }
  xgrid0 <- seq(x1-rx*grid.outside, x2+rx*grid.outside, length=dim.grid[1])
  ygrid0 <- seq(y1-ry*grid.outside, y2+ry*grid.outside, length=dim.grid[2])
  grid0 <- as.matrix(expand.grid(xgrid0, ygrid0))
  grid1 <- tps2d(grid0, fr, to)
  if (plot.full){
    wdw <- apply(rbind(grid0, grid1), 2, range)
  } else {
    wdw <- apply(rbind(fr, to), 2, range)}
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, ann=FALSE, axes=FALSE, mar=rep(0, 4))
  for (i in 1:dim.grid[2]) lines(grid1[(1:dim.grid[1]) + (i-1)*dim.grid[1],], col=grid.col)
  for (i in 1:dim.grid[1]) lines(grid1[(1:dim.grid[2]) * dim.grid[1]-i+1,],   col=grid.col)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}
}


tps.arr <- function(fr, to, amp=1, palette = col.summer,
                    arr.nb = 100, arr.levels = 100, arr.len = 0.1,
                    arr.ang = 30, arr.lwd = 1, arr.col = "grey50",
                    shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1)){
  if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), arr.nb, type="regular")@coords
  grid1     <- tps2d(grid0, fr, to)
  # grille simple, on affiche d'abord les deux courbes
  wdw      <- apply(rbind(fr, to), 2, range)
  plot(NA, xlim=wdw[, 1]*1.05, ylim=wdw[, 2]*1.05, asp=1, axes=FALSE, ann=FALSE, mar=rep(0,4))
  if (missing(arr.levels)) {arr.levels = arr.nb}
  if (!missing(palette)) {
    q.lev   <- cut(edm(grid0, grid1), breaks=arr.levels, labels=FALSE)
    arr.cols <- palette(arr.levels)[q.lev]
  } else {
    arr.cols <- rep(arr.col, nrow(grid0))}
  arrows(grid0[, 1], grid0[, 2], grid1[, 1], grid1[, 2],
         length=arr.len, angle=arr.ang, lwd=arr.lwd, col=arr.cols)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}
}


tps.iso <- function(fr, to, amp=1, palette = col.summer,
                    iso.nb = 500, iso.levels = 12, cont=TRUE, cont.col="black",
                    shp = TRUE, shp.col =  rep(NA, 2), shp.border=col.gallus(2),
                    shp.lwd = c(2, 2), shp.lty = c(1, 1)){  
  if (!missing(amp)) to <- to + (to-fr)*amp
  grid0  <- spsample(Polygon(coo.close(fr)), iso.nb, type="regular")@coords
  grid1  <- tps2d(grid0, fr, to)
  def    <- edm(grid0, grid1)
  x1     <- length(unique(grid0[,1]))
  y1     <- length(unique(grid0[,2]))
  im     <- matrix(NA,x1,y1)
  xind   <- (1:x1)[as.factor(rank(grid0[,1]))]
  yind   <- (1:y1)[as.factor(rank(grid0[,2]))]
  n      <- length(xind)
  for (i in 1:n) im[xind[i], yind[i]] <- def[i]
  iso.cols <- palette(iso.levels)
  x <- sort(unique(grid0[,1]))
  y <- sort(unique(grid0[,2]))
  image(x, y, im, col=iso.cols, asp=1, xlim=range(x)*1.05, ylim=range(y)*1.05,
        axes=FALSE, frame=FALSE, ann=FALSE)
  if (cont) contour(x, y, im, nlevels=iso.levels, add=TRUE, drawlabels=FALSE, col=cont.col)
  if (shp) {
    coo.draw(fr, border=shp.border[1], col=shp.col[1], lwd=shp.lwd[1], lty=shp.lty[1])
    coo.draw(to, border=shp.border[2], col=shp.col[2], lwd=shp.lwd[2], lty=shp.lty[2])}}

# 10. Datasets documentation ----------------------------------------------------
#' Outline coordinates of 20 beer and 20 whisky bottles.
#' 
#' @docType data
#' @name bot
#' @keywords datasets
#' @format An Out object containing the outlines coordinates and a grouping factor
#' for 20 beer and 20 whisky bottles
#' @source  Images have been grabbed on the internet and prepared by the package's
#' authors. No particular choice has been made on the dimension of the original
#' images or the brands cited here.
NULL

#' Outline coordinates of 50 cephalic outlines of trilobite
#' 
#' @docType data
#' @name trilo
#' @keywords datasets
#' @format An Out object 64 coordinates of 50 cephalic outlines from different
#' ontogenetic stages of trilobite.
#' @source  Arranged from: \url{http://folk.uio.no/ohammer/past/outlines.dat}.
#' The original data included 51 outlines and 5 ontogenetic stages, 
#' but one of them has just a single outline thas has been removed.

NULL


#' Outline coordinates of 126 mosquito wings.
#' 
#' @docType data
#' @name mosquito
#' @keywords datasets
#' @format An Out object with the 126 mosquito wing outlines outlines
#' used Rohlf and Archie (1984).
#' @source Rohlf F, Archie J. 1984. A comparison of Fourier methods for the
#' description of wing shape in mosquitoes (Diptera: Culicidae). \emph{Systematic Biology}: 302-317.
#' Arranged from: \url{http://life.bio.sunysb.edu/morph/data/RohlfArchieWingOutlines.nts}.
NULL

#' Outline coordinates of 240 hand-drawn hearts
#' 
#' @docType data
#' @name hearts
#' @keywords datasets
#' @format An Out object with the outline coordinates of 240 hand-drawn hearts
#' by 8 different persons, with 4 landmarks.
#' @source We thank the fellows of the Ecology Department of the French Institute
#' of Pondicherry that drawn the hearts, that then have been smoothed, scaled, centered, and reduced to 80 coordinates per outline.
NULL

# #' Outline coordinates of 50 date seeds (Phoenix dactylifera), with 2 views
# #' 
# #' @docType data
# #' @name phoenix
# #' @keywords datasets
# #' @format An Out object with the outline coordinates of 50 date seeds
# #' (Phoenix dactylifera), with dorsal and lateral views
# #' @source We thank Jean-Frédéric Terral and Sarah Ivorral (UMR CBAE, Montpellier, France)
# #' from allowing us to share the data.
# NULL

