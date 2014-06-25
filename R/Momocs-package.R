##### Package documentation and NAMESPACE import

#' Momocs
#' 
#' Momocs is intended to ease and to popularize morphometrics, 
#' the quantitative analysis of form. It aims at gathering the most common approaches: 
#' traditional morphometrics, global descriptors, open outlines, closed outlines 
#' and configuration of landmarks into the same, open-source, environment. 
#' It hinges on the core functions developed in the book \emph{Morphometrics with R} 
#' by Julien Claude (2008), extend them graphically and statistically, 
#' using the same, unified and extendable, grammar. From import from other 
#' morphometrics format or data acquisition, to display, handling, 
#' calibration and calculations on morphometrics datasets on which to apply 
#' an array of statistical analyses, Momocs intends to provides a complete, 
#' convenient and extendable toolkit to specialists within every field that are, 
#' or may be, interested in integrating shape in their researches.
#' 
#' To cite Momocs in publications: \code{citation("Momocs")}.
#' 
#' @seealso
#' \describe{
#'  \item{Homepage}{\url{http://www.vincentbonhomme.fr/Momocs} with tutorials
#' and hotline}
#'  \item{GitHub repo}{\url{https://github.com/vbonhomme/Momocs} to contribute,
#' get the last version, etc.}
#' }
#' @references
#' \itemize{
#' \item Bonhomme V, Picq S, Gaucherel C, Claude J. 2014. Momocs: Outline Analysis Using R. 
#' \emph{Journal of Statistical Software} \bold{56}. \url{http://www.jstatsoft.org/v56/i13}.
#' \item Claude J. 2008. \emph{Morphometrics with R}. Springer-Verlag, New-York.
#' }
#' @section Cheers:
#' We are very grateful to  Cedric Gaucherel, Sarah Ivorra, Ricardo Kriebel, Neus
#' Martinez, Marcelo Reginato, Evan Saitta, Norbert Telmon, Bill Venables, Asher Wishkerman for
#' their helpful contributions, ideas, bug reports, and much more.
#' @import ape
#' @import sp
#' @import gpclib
#' @importFrom jpeg readJPEG
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
