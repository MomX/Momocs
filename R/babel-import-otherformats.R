
# Import/Export morphometrics formats ------------------------------------------

#' Convert (x; y) coordinates to chaincoded coordinates
#' 
#' May be useful to convert (x; y) coordinates to chain-coded coordinates.
#' @param coo (x; y) coordinates passed as a matrix
#' @seealso \link{chc2pix}
#' @references Kuhl, F. P., & Giardina, C. R. (1982).
#' Elliptic Fourier features of a closed contour. 
#' Computer Graphics and Image Processing, 18(3), 236-258.
#' @keywords babel
#' @examples
#' data(shapes)
#' pix2chc(shapes[1])
#' @export
pix2chc <- function(coo) {
  if (is.list(coo)) {
    coo <- l2m(coo)}
  if (is.matrix(coo) & ncol(coo)!=2) {
    stop("A 2 col matrix must be provided")}
  coo.d <- apply(coo, 2, diff)
  if (!all(coo.d %in% -1:1)) {
    stop("Matrix must contain only entire pixels indices")}
  if (any(apply(coo.d, 1, function(x) all(x==rep(0, 2))))) {
    stop("At least two succesive coordinates don't code for a displacement")}
  m   <- as.matrix(expand.grid(-1:1, -1:1))[-5,]
  g   <- c(5, 6, 7, 4, 0, 3, 2, 1)
  chc <- g[apply(coo.d, 1, function(x) which(x[1]==m[, 1] & x[2]==m[, 2]))] #dirty
  return(chc)}

#' Convert chain-coded coordinates to (x; y) coordinates
#' 
#' May be useful to convert (prehistoric?) chain-coded coordinates
#' to (x; y) coordinates. The first point is set at the origin.
#' @param chc chain-coded coordinates
#' @seealso \link{pix2chc}
#' @references Kuhl, F. P., & Giardina, C. R. (1982).
#' Elliptic Fourier features of a closed contour. 
#' Computer Graphics and Image Processing, 18(3), 236-258.
#' @keywords babel
#' @examples
#' data(shapes)
#' x <- pix2chc(shapes[1])
#' coo.plot(chc2pix(x))
#' @export
chc2pix <- function(chc){
  if (!all(chc %in% 0:7)) {
    stop("chc string must only contain integers between 0 and 7")}
  m <- matrix(c(1, 0, 1, 1, 0, 1, -1, 1,
                -1, 0, -1, -1, 0, -1, 1, -1), ncol=2, byrow=TRUE)
  pix <- apply(m[chc+1,], 2, cumsum)
  return(pix)}

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

#' From .nef to Coe objects
#' 
#' Useful to convert .nef files into Coe objects.
#' It returns a matrix of coefficients that can be passed to \link{Coe}.
#' @param nef.path the path to the .nef file
#' @details I'm not very familiar to other morphometric formats.
#' So if you have troubles importing your datasets, please contact me.
#' @keywords babel
#' @export
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

#' From .tps to Coo objects
#' 
#' Useful to convert .tps files into Coo objects.
#' It returns a list of matrices of coordinates that can be passed to \link{Coo}.
#' @param tps.path the path to the .tps file
#' @param sep the separator between data
#' @details I'm not very familiar to other morphometric formats.
#' So if you have troubles importing your datasets, please contact me.
#' @keywords babel
#' @export
tps2Coo <- function(tps.path, sep=" "){
  # we read all lines of the file
  tps <- readLines(tps.path)
  # we detect the beginning of every individual
  tps.pos <- cbind(grep(pattern="lm=", x=tps, ignore.case=TRUE),
                 c(grep(pattern="lm=", x=tps, ignore.case=TRUE)[-1]-1, length(tps)))
  # we prepare a vector and a list to host the data
  img.names   <- character()
  coo.list      <- list()
  # and we loop over individuals
  for (i in 1:nrow(tps.pos)) {
    # first we pick one of the individuals
    tps.i         <- tps[tps.pos[i, 1] : tps.pos[i, 2]]
    # and we grab and clean the image name information
    img.i         <- tps.i[grep("image", tps.i, ignore.case=TRUE)]
    img.i         <- gsub("image=" , "", img.i, ignore.case=TRUE)
    img.names[i]  <- gsub(".jpg"   , "", img.i, ignore.case=TRUE)
    # here we exclude every line that start with a letter
    coo.i         <- tps.i[-grep(pattern="[[:alpha:]]", tps.i)]
    # and convert it as a matrix of coordinates
    coo.i         <- unlist(strsplit(coo.i, sep))
    coo.list[[i]] <- matrix(as.numeric(coo.i), ncol=2, byrow=TRUE)
  }
  coo.list <- lapply(coo.list, function(x) colnames())
  names(coo.list) <- img.names
  return(coo.list)}

#' From .nts to Coo objects
#' 
#' Useful to convert .nts files into Coo objects. For .nts provided as rows, use
#' ntsrow2Coo; for .nts provided as columns of coordinates, try ntscol2Coo. It
#' returns a list of matrices of coordinates that can be passed to \link{Coo}.
#' @aliases ntscol2Coo ntsrow2Coo
#' @param nts.path the path to the .nts file
#' @param sep the separator between data
#' @details I'm not very familiar to other morphometric formats and these
#'   functions are (highly) experimental. So if you have troubles importing your
#'   datasets, please contact me.
#' @keywords babel
#' @examples
#' \dontrun{
#' # That's how wings dataset was created
#' # made a local copy from http://life.bio.sunysb.edu/morph/data/RohlfSlice1990Mosq.nts
#' # then :
#' coo.list  <- ntscol2Coo("~/Desktop/mosquitowings.nts)
#' fac       <- data.frame(fac=factor(substr(names(coo.list), 1, 2)))
#' wings <- Ldk(coo.list, fac=fac) 
#' }
#' @export
ntsrow2Coo <- function(nts.path, sep="\t"){
  # we read all lines and remove the first one
  nts <- readLines(nts.path, warn=FALSE)
  comments <- grep(pattern="\"", nts)
  nts <- nts[-comments]
  # we prepare a vector and a list to store the data
  img.i <- character()
  coo.list <- list()
  # we loop over every individual
  for (i in 1:length(nts)){
    # we pick every individual
    ind.i <- unlist(strsplit(nts[i], sep))
    # the first element is the name
    img.i[i] <- ind.i[1]
    # then we convert the rest as a matrix
    coo.list[[i]] <- matrix(as.numeric(ind.i[-1]), ncol=2, byrow=TRUE)}
  # we rename list components with image names
  names(coo.list) <- img.i
  return(coo.list)}

#' @export
ntscol2Coo <- function(nts.path, sep="\t"){
  # candidate for the most ugly function ever?
  # we read all lines and remove the skip one
  nts <- readLines(nts.path, warn=FALSE)
  comments <- grep(pattern="\"", nts)
  nts <- nts[-comments]
  nb.ldk <- as.numeric(strsplit(nts[1], " ")[[1]][3])
  nts <- nts[-1]
  nts <- unlist(strsplit(nts, " "))
  nts <- nts[-which(nchar(nts)==0)]
  nb.nts <- length(nts) / (nb.ldk+1)
  # we prepare a vector and a list to store the data
  names.id <- 1+(nb.ldk+1)*(0:(nb.nts-1))
  start.id <- names.id + 1
  end.id <- start.id + nb.ldk - 1
  img.i <- nts[names.id]
  coo.list <- list()
  # we loop over every individual
  for (i in 1:nb.nts){
    coo.list[[i]] <- matrix(as.numeric(nts[start.id[i]:end.id[i]]), ncol=2, byrow=TRUE)}
  # we rename list components with image names
  names(coo.list) <- img.i
  return(coo.list)}
