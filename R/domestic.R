
# 00. Package documentation and NAMESPACE import ---------------------------

#' Momocs
#' 
#'
#' Morphometrics using R
#' @references Bonhomme et al. JSS
#' @import ape
#' @importFrom jpeg readJPEG
#' @importFrom sp spsample Polygon
#' @importFrom spdep tri2nb
#' @importFrom shapes procGPA
#' @importFrom methods showDefault
#' @importFrom MASS ginv
#' @docType package
#' @name Momocs
#' @keywords Abtract
NULL

# 0. Datasets documentation ----------------------------------------------------
#' Data: Outline coordinates of 20 beer and 20 whisky bottles.
#' 
#' @docType data
#' @name bot
#' @rdname data_bot
#' @keywords datasets
#' @format An Out object containing the outlines coordinates and a grouping factor
#' for 20 beer and 20 whisky bottles
#' @source  Images have been grabbed on the internet and prepared by the package's
#' authors. No particular choice has been made on the dimension of the original
#' images or the brands cited here.
NULL

#' Data: Outline coordinates of 50 cephalic outlines of trilobite
#' 
#' @docType data
#' @name trilo
#' @rdname data_trilo
#' @keywords datasets
#' @format An Out object 64 coordinates of 50 cephalic outlines from different
#' ontogenetic stages of trilobite.
#' @source  Arranged from: \url{http://folk.uio.no/ohammer/past/outlines.dat}.
#' The original data included 51 outlines and 5 ontogenetic stages, 
#' but one of them has just a single outline thas has been removed.
NULL


#' Data: Outline coordinates of 126 mosquito wings.
#' 
#' @docType data
#' @name mosquito
#' @rdname data_mosquito
#' @keywords datasets
#' @format An Out object with the 126 mosquito wing outlines outlines
#' used Rohlf and Archie (1984).
#' @source Rohlf F, Archie J. 1984. A comparison of Fourier methods for the
#' description of wing shape in mosquitoes (Diptera: Culicidae). \emph{Systematic Biology}: 302-317.
#' Arranged from: \url{http://life.bio.sunysb.edu/morph/data/RohlfArchieWingOutlines.nts}.
NULL

#' Data: Outline coordinates of 240 hand-drawn hearts
#' 
#' @docType data
#' @name hearts
#' @rdname data_hearts
#' @keywords datasets
#' @format An Out object with the outline coordinates of 240 hand-drawn hearts
#' by 8 different persons, with 4 landmarks.
#' @source We thank the fellows of the Ecology Department of the French Institute
#' of Pondicherry that drawn the hearts, that then have been smoothed, scaled, centered, and reduced to 80 coordinates per outline.
NULL

# #' Data: Outline coordinates of 50 date seeds (Phoenix dactylifera), with 2 views
# #' 
# #' @docType data
# #' @name phoenix
# #'@rdname data_phoenix
# #' @keywords datasets
# #' @format An Out object with the outline coordinates of 50 date seeds
# #' (Phoenix dactylifera), with dorsal and lateral views
# #' @source We thank Jean-Frédéric Terral and Sarah Ivorral (UMR CBAE, Montpellier, France)
# #' from allowing us to share the data.
# NULL

# 1. Morphospace functions -----------------------------------------------------
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

# 2. Domestic functions -------------------------------------------------------------
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

.refactor <- function(df){
  w <- sapply(df, is.factor)
  df[w] <- lapply(df[w], factor)
  df}







