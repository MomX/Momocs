
##### Package documentation and NAMESPACE import

#' Momocs
#'
#' The goal of Momocs is to provide a complete, convenient,
#' reproducible and open-source toolkit for 2D morphometrics.
#' It includes most common 2D morphometrics approaches on outlines,
#' open outlines, configurations of landmarks, traditional
#' morphometrics, and facilities for data preparation,
#' manipulation and visualization with a consistent grammar throughout.
#' It allows reproducible, complex morphometric analyses
#' and other morphometrics approaches should be easy to
#' plug in, or develop from, on top of this canvas.
#'
#' To cite Momocs in publications: \code{citation("Momocs")}.
#'
#' @seealso
#' \itemize{
#'  \item \bold{Homepage}: \url{https://github.com/MomX/Momocs}
#'  \item \bold{Issues}: \url{https://github.com/MomX/Momocs/issues}
#'  \item \bold{Tutorial}: \code{browseVignettes("Momocs")} or \url{http://momx.github.io/Momocs/}
#'  \item \bold{Email}: \code{bonhomme.vincent@gmail.com} to contribute to dev,
#'  ask for something, propose collaboration, share your data, etc.
#' }
#'
#' @references
#' \itemize{
#' \item Bonhomme V, Picq S, Gaucherel C, Claude J. 2014. Momocs: Outline Analysis Using R.
#' \emph{Journal of Statistical Software} \bold{56}. \url{https://www.jstatsoft.org/v56/i13}.
#' \item Claude J. 2008. \emph{Morphometrics with R}. Springer-Verlag, New-York.
#' }
#' @section Cheers:
#' We are very grateful to (in alphabetical order): Sean Asselin, Laurent Bouby, Matt Bulbert, Simon Crameri, Julia Cooke, April Dinwiddie,
#' Carl Lipo, Cedric Gaucherel, Catherine Girard, QGouil (GitHub), Christian Steven Hoggard,
#' Sarah Ivorra, Glynis Jones, Nathalie Keller, Ricardo Kriebel, Remi Laffont, Fabien Lafuma,
#' Matthias Mace, Stas Malavin, Neus Martinez, Ben Marwick, Sabrina Renaud, Marcelo Reginato,
#' Evan Saitta, Bill Sellers, David Siddons, Eleanor Stillman,
#' Theodore Stammer, Tom Stubbs, Norbert Telmon, Jean-Frederic Terral,
#' Bill Venables, Daniele Ventura, Michael Wallace,
#' Asher Wishkerman, John Wood for their helpful ideas and bug reports.
#' @return nothing
#' @import ggplot2
#' @importFrom utils stack
#' @importFrom graphics abline arrows axis barplot box boxplot
#' contour hist image layout legend lines locator
#' par plot points polygon rasterImage rect rug
#' segments strheight strwidth text title
# #' @importFrom RColorBrewer brewer.pal brewer.pal.info
# #' @importFrom rgeos gIntersection
#' @importFrom stats cor cov cov.wt df dist dnorm fft
#' hclust kmeans lm manova median na.omit
#' poly prcomp predict qf qnorm rnorm
#' runif sd symnum terms var
#' @docType package
#' @name Momocs
NULL


##### We include magrittr pipes when loading Momocs
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`


# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("x", "y", "f", "xend", "yend", "shp1", "ddply",
                  "x_c", "x_d", "y_c", "y_d", ".id", "id", "count",
                  "node", "label", "angle", "hjust", "Freq",
                  "locus", "name", ".", "actual", "classified", "predicted", "coo", "actual2",
                  "coeff", "key", "value", "ldk",
                  # KMEDOIDS:
                 "cluster", "desc", "n", "neighbor", "sil_width", "width"
                  ))

# welcome message
# .onAttach <- function(lib, pkg) {
#   packageStartupMessage("
#   Momocs is now retired and will no longer be maintained.
#   It is superseded by Momocs2 and more generally MomX ecosystem:
#
#              <   https://momx.github.io   >",
#   appendLF = TRUE)
# }

#on load add Momocs' options
.onLoad <- function(libname, pkgname){
  op <- options()
  op.Momocs <- list(
    Momocs_verbose = TRUE
    # Momocs_message = TRUE,
    # Momocs_pal_qual_default=pal_qual_Set2,
    # Momocs_pal_seq_default=pal_seq_viridis,
    # Momocs_pal_div_default=pal_div_RdBu
  )
  toset <- !(names(op.Momocs) %in% names(op))
  if(any(toset)) options(op.Momocs[toset])

  invisible()
}
# pal_qual_default <- options("Momocs_pal_qual_default")[[1]]
