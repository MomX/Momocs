# run this one to update DESCRIPTION with no pain
# source("~/Research/Momocs/update.R")

##### We include magrittr pipes when loading Momocs

#' @export
`%>%` <- dplyr::`%>%`

#' @importFrom magrittr %<>%
#' @export
`%<>%` <- magrittr::`%<>%`

#' @importFrom magrittr %$%
#' @export
`%$%` <- magrittr::`%$%`

#' @importFrom magrittr %T>%
#' @export
`%T>%` <- magrittr::`%T>%`



##### Package documentation and NAMESPACE import

#' Momocs
#'
#' Momocs aims to provide a complete and convenient
#' toolkit for morphometrics. It is intended for scientists interested in
#' describing quantitatively the shape, and its variations, of the objects they
#' study. In the last decade, R has become the open-source lingua franca
#' for statistics, and morphometrics known its so-called 'revolution'.
#' Nevertheless, morphometric analyses still have to be carried out using
#' various software packages, for which source code is mostly unavailable
#' or copyrighted. Moreover, existing software packages cannot be extended
#' and their bugs are hard to detect and thus correct. This situation
#' is detrimental to morphometrics; time is wasted, analyses are restricted
#' to available methods, and last but not least, are poorly reproducible.
#' This impedes collaborative effort both in software development and
#' in morphometric studies. By gathering the common morphometric approaches
#' in an open-source environment and welcoming contributions, Momocs is an
#' (work-in-progress) attempt  to solve this twofold problem and to push morphometrics
#' one step further. It hinges on the core functions published
#' in the book \emph{Morphometrics using R} by one of us (Claude, 2008), but has been further
#' extended to allow other shape description systems. So far, configurations
#' of landmarks, outlines and open outline analyses, along with some facilities
#' for traditional morphometrics have been implemented. Prior to analysis,
#' Momocs can be used to acquire and manipulate data or to import/export
#' from/to other formats. Momocs also has the facility for a wide range of
#' multivariate analyses and production of the companion graphics.
#' Thus a researcher will find that just a few lines of code will provide
#' initial results, but the methods implemented can be finely tuned
#' and extended according to the user's needs.
#'
#' To cite Momocs in publications: \code{citation('Momocs')}.
#'
#' @seealso
#' \itemize{
#'  \item \bold{Tutorial}: \code{browseVignettes("Momocs")} (work in progress...)
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
#' We are very grateful to (in alphabetical order): Laurent Bouby, Simon Crameri, April Dinwiddie,
#' Carl Lipo, Cedric Gaucherel, Sarah Ivorra, Ricardo Kriebel, Remi Laffont, Fabien Lafuma,
#' Neus Martinez, Marcelo Reginato, Evan Saitta, David Siddons, Eleanor Stillman,
#' Theodore Stammer, Norbert Telmon, Jean-Frederic Terral, Bill Venables, Daniele Ventura, Michael Wallace,
#' Asher Wishkerman, John Wood.
#' for their helpful ideas and bug reports.
#' @import ape
#' @importFrom plyr ldply ddply
#' @import dplyr
#' @import sp
#' @import reshape2
#' @import ggplot2
#' @importFrom geometry delaunayn convhulln
#' @importFrom graphics boxplot
#' @importFrom jpeg readJPEG
#' @importFrom MASS lda ginv kde2d cov.trob
#' @importFrom utils stack
#' @docType package
#' @name Momocs
#' @keywords Abtract
NULL

# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("x", "y", "f", "xend", "yend", "shp1", "ddply",
                  "x_c", "x_d", "y_c", "y_d", ".id",
                  "node", "label", "angle", "hjust", "Freq",
                  "locus", "name"))

#' Install and load the last version of Momocs
#'
#' Download the last version of Momocs from its GitHub account
#' from \code{http://www.github.com/vbonhomme/Momocs}), install it and load it (\code{library(Momocs)})
#'
#' You need devtools, but it is checked anyway.
#' @examples
#' \dontrun{
#' # use it with:
#' update_Momocs()
#' }
#' @export
update_Momocs <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools needed for this function to work. Please install it with install.packages('devtools')",
         call. = FALSE)
  }
  devtools::install_github("vbonhomme/Momocs")
  library(Momocs)
  cat("Last version of Momocs has been installed from GitHub and loaded into R.\n")
}

# .onAttach <- function(lib, pkg) {
# packageStartupMessage('This is Momocs ',
# utils::packageDescription('Momocs', field='Version'),
# appendLF = TRUE) }


#' Browse Momocs online doc
#'
#' Launch a browser to an online version of the manual
#' @param topic the function name to access. If not specified the homepage of
#' the online manual is accessed.
#' @export
Momocs_help <- function(topic=NULL){
  url <- "http://vbonhomme.github.io/Momocs/"
  if (!is.null(topic)) url <- paste0(url, topic, ".html")
  browseURL(url)
}


