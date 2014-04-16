# 1. Simple bridges between R classes ---------------------------------------------
l2m  <- function(l) {
  m <- cbind(l$x, l$y)
  colnames(m) <- c("x", "y")
  return(m)}

m2l  <- function(m) {return(list(x=m[,1], y=m[,2]))}

l2a  <- 
  function(l){return(array(unlist(l), dim=c(nrow(l[[1]]), ncol(l[[1]]), length(l))))}
a2l <- function(a){
  if (!is.array(a)) stop("An array of dimension 3 must be provided")
  k <- dim(a)[3]
  l <- list()
  for (i in 1:k) {l[[i]] <- a[,,i]}
  return(l)}


# Import raw data --------------------------------------------------------------

#todo : work either from a path or from a char vect
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


# Manipulate raw data inside R--------------------------------------------------


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

