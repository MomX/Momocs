# 0. Coo (Opn/Out/Ldk) domestics -----------------------------------------------

#' Coo class
#' 
#' \code{Coo} class is a super class for \link{Opn} and \link{Out} classes,
#' \bold{open outlines} and \bold{closed outlines}, respectively.
#' 
#' It allows to recycle most of the methods, since both of them apply on
#' \eqn{(x; y)} coordinates. In other words, \code{Opn} and \code{Out} classes
#' are all, primarily, \code{Coo} objects on which we define generic \emph{and} 
#' specific methods.
#' 
#' If you used Momocs before version <1.0, or if you have read the JSS paper,
#' you are probably looking for \link{Out}.
#' 
#' More, generally, see \link{Opn} and \link{Out} for documentation on these classes.
#'
#'@param ... anything and, anyway, this function will simply returns a message.
#' @keywords Coo
#' @export
Coo <- function(...){
  cat(" * Coo constructor has been deprecated. See ?Coo")}

#' Coe class
#' 
#' \code{Coe} class is a super class for \link{OutCoe} and \link{OpnCoe} classes,
#' matrices of coefficients, along with other informations, obtained with
#' morphometrics methods on \code{\link{Out}} and \code{\link{Opn}} objects.
#' 
#' It allows to recycle most of the methods, since both of them apply on
#' matrices of coefficients. In other words, \code{OutCoe} and \code{OpnCoe}
#' classes are all, primarily, \code{Coe} objects on which we define generic 
#' \emph{and} specific methods.
#' 
#' More, generally, see \link{Opn} and \link{Out} for documentation on these classes.
#'
#' @param ... anything and, anyway, this function will simply returns a message.
#' @keywords Coe
#' @export
Coe <- function(...){
  cat(" * Coe constructor does not exist alone. See ?Coe")}

# allows to maintain the tradition str() behaviour
# actually useless but dont remember why/where
#' @export
str.Coo <- function(object, ...){
  Coo <- object
  ls.str(Coo)}
#' @export
str.Coe <- function(object, ...){
  Coe <- object
  ls.str(Coe)}

# Coo can be indexing both to [ ] and [[ ]]
# and returns the corresponding coordinate(s)
# We define some getters

#' @export
"[.Coo" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

#' @export
"[[.Coo" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

# length on an Coo return the length of Coo$coo, ie the number of coordinates
#' @export
length.Coo <- function(x) {
  Coo <- x
  return(length(Coo$coo))}
#' @export
dim.Coe <- function(x){
  return(dim(x$coe))}


# names() on a Coo retrieves the names of the Coo$coo
#' @export
names.Coo <- function(x){
  Coo <- x
  return(names(Coo$coo))}
#' @export
names.Coe <- function(x){
  Coe <- x
  return(rownames(Coe$coe))}

# which can in return may be named using names(Coo) <- 
#' @export
"names<-.Coo" <- function(x, value){
  names(x$coo) <- value
  return(x)}
#' @export
"names<-.Coe" <- function(x, value){
  rownames(x$coe) <- value
  return(x)}

#' Create subsets of Coo objects
#' 
#' Pretty useful in morphometrics. Imagine you have a \link{Out} or a \link{Opn}
#' object that combines several different \emph{groups}, 
#' whatever \emph{groups} are : species, views, etc. You may be interested in
#' doing separated analyses (even if you could combine them later), then this
#' function will ease the process. See the examples below.
#' @aliases subset
#' @param x a \code{Coo} object, i.e. \link{Out} or \link{Opn}
#' @param subset logical taken from the $fac slot, or indices. See examples.
#' @param ... useless here but maintains consistence with the generic subset.
#' @keywords Opn Coo
#' @examples
#' data(bot)
#' bot$fac
#' beers <- subset(bot, type=="beer")
#' whisk <- subset(bot, type=="whisky")
#' # or you may prefere indices
#' subset(bot, c(1, 13, 34, 37))
#' # and you can combine them :
#' data(olea)
#' olea$fac
#' subset(olea, domes=="cult" & view=="VL")
#' @export
# bloody dirty #todo
subset.Coo <- function(x, subset, ...){
  Coo <- x
  e <- substitute(subset)
  retain <- eval(e, Coo$fac, parent.frame())
  Coo2 <- Coo
  Coo2$coo <- Coo$coo[retain]
  if (length(Coo$ldk)>0) Coo2$ldk <- Coo$ldk[retain]
  if (ncol(Coo$fac)>0) {
    Coo2$fac <- Coo$fac
    Coo2$fac <- as.data.frame(Coo2$fac[retain, ])
    names(Coo2$fac) <- names(Coo$fac)
    Coo2$fac <- .refactor(Coo2$fac)
  }
  return(Coo2)}

#' @export
subset.OutCoe <- function(x, subset, ...){
  OutCoe <- x
  e <- substitute(subset)
  retain <- eval(e, OutCoe$fac, parent.frame())
  OutCoe2 <- OutCoe
  OutCoe2$coe <- OutCoe$coe[retain, ]
  if (ncol(OutCoe$fac)>0) {
    OutCoe2$fac <- OutCoe$fac
    OutCoe2$fac <- as.data.frame(OutCoe2$fac[retain, ])
    names(OutCoe2$fac) <- names(OutCoe$fac)
    OutCoe2$fac <- .refactor(OutCoe2$fac)
  }
  return(OutCoe2)}

# 0. Datasets documentation ----------------------------------------------------
#' Data: Outline coordinates of 20 beer and 20 whisky bottles.
#' 
#' @docType data
#' @name bot
#' @rdname data_bot
#' @keywords datasets
#' @format A \link{Out} object containing the outlines coordinates and a grouping factor
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
#' @format A \link{Out} object 64 coordinates of 50 cephalic outlines from different
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
#' @format A \link{Out} object with the 126 mosquito wing outlines outlines
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
#' @format A \link{Out} object with the outline coordinates of 240 hand-drawn hearts
#' by 8 different persons, with 4 landmarks.
#' @source We thank the fellows of the Ecology Department of the French Institute
#' of Pondicherry that drawn the hearts, that then have been smoothed, scaled, centered, and degraded to 80 coordinates per outline.
NULL

#' Data: Outline coordinates of 210 olive seeds oopen outlines.
#' 
#' @docType data
#' @name olea
#' @rdname data_olea
#' @keywords datasets
#' @format An \link{Opn} object with the outline coordinates of olive seeds.
#' @source We thank Jean-Frederic Terral and Sarah Ivorra (UMR CBAE, Montpellier, France)
#' from allowing us to share the data.
#' 
#' You can have a look to the original paper:
#' Terral J-F, Alonso N, Capdevila RB i, Chatti N, Fabre L, Fiorentino G, 
#' Marinval P, Jorda GP, Pradat B, Rovira N, et al. 2004. 
#' Historical biogeography of olive domestication (\emph{Olea europaea} L.) 
#' as revealed by geometrical morphometry applied to biological and 
#' archaeological material. \emph{Journal of Biogeography} \bold{31}: 63-77.
NULL

#' Data: Outline coordinates of 210 olive seeds oopen outlines.
#' 
#' @docType data
#' @name shapes
#' @rdname data_shapes
#' @keywords datasets
#' @format An \link{Out} object with the outline coordinates of some various shapes.
#' @source Borrowed from default shapes from (c) Adobe Photoshop.
NULL

#' Data: Outline coordinates of 20 beer and 20 whisky bottles.
#' 
#' @docType data
#' @name wings
#' @rdname data_wings
#' @keywords datasets
#' @format A \link{Ldk} object containing 18 (x; y) landmarks  from 127 mosquito wings, from 
#' @source Rohlf and Slice 1990 and \url{http://life.bio.sunysb.edu/morph/data/RohlfSlice1990Mosq.nts}
NULL

# 00. Package documentation and NAMESPACE import ---------------------------

#' Momocs
#' 
#' Morphometrics using R
#' 
#' @seealso
#' Momocs' homepage : \url{http://www.vincentbonhomme.fr/Momocs} with tutorials
#' and hotline.
#' 
#' Momocs' GitHub repo : \url{https://github.com/vbonhomme/Momocs} to contribute,
#' among other things.
#' 
#' @references Bonhomme V, Picq S, Gaucherel C, Claude J. 2014. Momocs: Outline Analysis Using R. 
#' \emph{Journal of Statistical Software} \bold{56}. \url{http://www.jstatsoft.org/v56/i13}.
#' 
#' Claude J. 2008. \emph{Morphometrics with R}. Springer-Verlag, New-York.
#' @details
#' We are very grateful to  Cedric Gaucherel, Sarah Ivorra, Ricardo Kriebel, Neus
#' Martinez, Marcelo Reginato, Evan Saitta, Norbert Telmon, Bill Venables, Asher Wishkerman for
#' their helpful contributions, ideas, bug reports, and much more.
#' @import ape
#' @import sp
#' @importFrom jpeg readJPEG
#' @importFrom shapes procGPA
#' @importFrom methods showDefault
#' @importFrom MASS ginv
#' @importFrom MASS lda
#' @importFrom MASS kde2d
#' @importFrom graphics boxplot
#' @importFrom utils stack
#' @docType package
#' @name Momocs
#' @keywords Abtract
NULL
