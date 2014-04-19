# 1. Domestic functions -------------------------------------------------------------
# Placed here so far

#' Calculates euclidean distance between two points.
#' 
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates.
#' 
#' @export
#' @usage ed(pt1, pt2)
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm.nearest}, \link{dist}.
#' @examples
#' ed(c(0,1), c(1,0))
ed <- function(pt1, pt2){return(sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2))}

#' Calculates euclidean intermediate between two points.
#' 
#' \code{edi} simply calculates coordinates of a points at the relative
#' distance \code{r} on the \code{pt1-pt2} defined by their (x; y) coordinates.
#' This function is used internally but may be of interest for other analyses.
#' 
#' @export edi
#' @usage edi(pt1, pt2, r = 0.5)
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @param r the relative distance from \code{pt1} to \code{pt2}.
#' @return Returns the (x; y) interpolated coordinates.
#' @seealso \link{ed}, \link{edm}.
#' @keywords Utilities
#' @examples
#' edi(c(0,1), c(1,0), r = 0.5)
edi <- function(pt1, pt2, r=0.5){
  return(r*(pt2-pt1) + pt1) }

#' Calculates euclidean distance every pairs of points in two matrices.
#' 
#' \code{edm} returns the euclidean distances between points \deqn{1 -> n} of
#' two 2-col matrices of the same dimension. This function is used internally
#' but may be of interest for other analyses.
#' 
#' If one wishes to align two (or more shapes) Procrustes surimposition may
#' provide a better solution.
#' @export edm
#' @usage edm(m1, m2)
#' @param m1 The first \code{matrix} of coordinates.
#' @param m2 The second \code{matrix} of coordinates.
#' @return Returns a \code{vector} of euclidean distances between pairwise
#' coordinates in the two matrices.
#' @seealso \link{ed}, \link{edm.nearest}, \link{dist}.
#' @keywords Utilities
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm(x, x)
#' edm(x, x+1)
edm            <- function(m1, m2){
  return(sqrt((m1[, 1] - m2[, 1])^2 + (m1[, 2] - m2[, 2])^2))}

#' Calculates the shortest euclidean distance found for every point of one
#' matrix among those of a second.
#' 
#' 
#' \code{edm.nearest} calculates the shortest euclidean distance found for
#' every point of one matrix among those of a second. In other words, if
#' \code{m1, m2} have \code{n} rows, the result will be the shortest distance
#' for the first point of \code{m1} to any point of \code{m2} and so on,
#' \code{n} times. This function is used internally but may be of interest for
#' other analyses.
#' 
#' So far this function is quite time consumming since it performs \deqn{ n
#' \times n } euclidean distance computation.  If one wishes to align two (or
#' more shapes) Procrustes surimposition may provide a better solution.
#' @export edm.nearest
#' @usage edm.nearest(m1, m2, full=FALSE)
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
#' @keywords Utilities
#' @examples
#' 
#' x <- matrix(1:10, nc=2)
#' edm.nearest(x, x+rnorm(10))
#' edm.nearest(x, x+rnorm(10), full=TRUE)
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

