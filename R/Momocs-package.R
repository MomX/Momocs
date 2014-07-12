##### Package documentation and NAMESPACE import

#' Momocs
#' 
#' Momocs is intended to ease and to popularize morphometrics, the quantitative analysis
#' of form. It aims, eventually, at gathering the most common approaches: traditional 
#' morphometrics, global descriptors, open outlines, closed outlines and configuration of 
#' landmarks into the same, open-source, environment.
#' It hinges on the core functions  developed in the book \emph{Morphometrics with R} 
#' by Julien Claude (2008), extend them graphically and statistically, using the same, 
#' unified and extendable, grammar.
#' From import from other morphometrics format or data acquisition, to display, handling, 
#' calibration and calculations on morphometrics datasets on which to apply an array of 
#' statistical analyses, Momocs intends to provides a complete, convenient and extendable 
#' toolkit to specialists within every field that are, or may be, interested in integrating
#'  shape in their researches.
#' 
#' To cite Momocs in publications: \code{citation("Momocs")}.
#' 
#' @seealso
#' \itemize{
#'  \item \bold{Demos}: \code{demo(package="Momocs")} for the list of demos, e.g. \code{demo(eft)}
#'  \item \bold{Homepage}: \url{http://www.vincentbonhomme.fr/Momocs} with tutorials
#' and hotline
#'  \item \bold{GitHub repo}: \url{https://github.com/vbonhomme/Momocs} to contribute,
#' get the last version, etc.
#' \item \bold{Hotline, questions, requests, etc.}: Feel free to send me an email, should you need, report or
#' request something about Momocs.
#' }
#' 
#' @references
#' \itemize{
#' \item Bonhomme V, Picq S, Gaucherel C, Claude J. 2014. Momocs: Outline Analysis Using R. 
#' \emph{Journal of Statistical Software} \bold{56}. \url{http://www.jstatsoft.org/v56/i13}.
#' \item Claude J. 2008. \emph{Morphometrics with R}. Springer-Verlag, New-York.
#' }
#' @section Cheers:
#' We are very grateful to Cedric Gaucherel, Sarah Ivorra, Ricardo Kriebel, Remi Laffont, Neus
#' Martinez, Marcelo Reginato, Evan Saitta, David Siddons, Norbert Telmon, Bill Venables, 
#' Daniele Ventura and Asher Wishkerman for their helpful contributions, ideas, 
#' bug reports, and much more.
#' @import ape
#' @import sp
#' @importFrom MASS lda ginv kde2d
#' @importFrom geometry delaunayn
#' @importFrom jpeg readJPEG
#' @importFrom methods showDefault
#' @importFrom graphics boxplot
#' @importFrom utils stack
#' @docType package
#' @name Momocs
#' @keywords Abtract
NULL

# .onAttach <- function(lib, pkg)  {
#   packageStartupMessage("This is Momocs ",
#                         utils::packageDescription("Momocs",
#                                                   field="Version"),
#                         appendLF = TRUE)
# }

