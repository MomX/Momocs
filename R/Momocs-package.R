
##### We include magrittr pipes when loading Momocs

#' @export
dplyr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`

##### Package documentation and NAMESPACE import

#' Momocs
#'
#' A complete toolkit for morphometrics, from data extraction to multivariate analyses.
#' Most common 2D morphometrics approaches are included:
#' outlines, open outlines, configurations of landmarks, traditional morphometrics,
#' and facilities for data preparation, manipulation and visualization
#' with a consistent grammar throughout.
#' Momocs allows reproducible, complex morphometric analyses,
#' paves the way for a pure open-source workflow in R,
#' and other morphometrics approaches should be easy to plug in,
#' or develop from, on top of this canvas.
#'
#' To cite Momocs in publications: \code{citation("Momocs")}.
#'
#' @seealso
#' \itemize{
#'  \item \bold{Homepage}: \url{https://github.com/vbonhomme/Momocs}
#'  \item \bold{Issues}: \url{https://github.com/vbonhomme/Momocs/issues}
#'  \item \bold{Tutorial}: \code{browseVignettes("Momocs")}
#'  \item \bold{Email}: \code{bonhomme.vincent@gmail.com} to contribute to dev, ask for something, share your data, etc.
#' }
#'
#' @references
#' \itemize{
#' \item Bonhomme V, Picq S, Gaucherel C, Claude J. 2014. Momocs: Outline Analysis Using R.
#' \emph{Journal of Statistical Software} \bold{56}. \url{http://www.jstatsoft.org/v56/i13}.
#' \item Claude J. 2008. \emph{Morphometrics with R}. Springer-Verlag, New-York.
#' }
#' @section Cheers:
#' We are very grateful to (in alphabetical order): Sean Asselin, Laurent Bouby, Simon Crameri, April Dinwiddie,
#' Carl Lipo, Cedric Gaucherel, Sarah Ivorra, Glynis Jones, Nathalie Keller, Ricardo Kriebel, Remi Laffont, Fabien Lafuma,
#' Stas Malavin, Neus Martinez, Marcelo Reginato, Evan Saitta, David Siddons, Eleanor Stillman,
#' Theodore Stammer, Norbert Telmon, Jean-Frederic Terral, Bill Venables, Daniele Ventura, Michael Wallace,
#' Asher Wishkerman, John Wood for their helpful ideas and bug reports.
#' @import ape
#' @importFrom plyr ldply ddply
#' @import dplyr
#' @import sp
#' @import reshape2
#' @import ggplot2
#' @importFrom geometry delaunayn convhulln
#' @importFrom geomorph gpagen
#' @importFrom graphics boxplot
#' @importFrom jpeg readJPEG
#' @importFrom MASS lda ginv kde2d cov.trob
#' @importFrom utils stack browseURL combn ls.str modifyList
#' packageVersion read.table setTxtProgressBar
#' txtProgressBar write.table
#' @importFrom grDevices chull
#' @importFrom grDevices chull
#' @importFrom graphics abline arrows axis barplot box
#' contour hist image layout legend lines locator
#' par points polygon rasterImage rect rug
#' segments strheight strwidth text title
#' @importFrom stats cor cov cov.wt df dist dnorm
#' hclust kmeans lm manova median na.omit
#' poly prcomp predict qf qnorm rnorm
#' runif sd symnum terms var

#' @docType package
#' @name Momocs
NULL

# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("x", "y", "f", "xend", "yend", "shp1", "ddply",
                  "x_c", "x_d", "y_c", "y_d", ".id",
                  "node", "label", "angle", "hjust", "Freq",
                  "locus", "name", "."))


.onAttach <- function(lib, pkg) {
  packageStartupMessage('This is Momocs ',
                        utils::packageDescription('Momocs', field='Version'),
                        appendLF = TRUE) }





