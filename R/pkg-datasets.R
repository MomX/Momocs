##### Dataset documentation and declaration

#' Data: Outline coordinates of beer and whisky bottles.
#'
#' @docType data
#' @name bot
#' @rdname data_bot
#' @format A \link{Out} object containing the outlines coordinates and a grouping factor
#' for 20 beer and 20 whisky bottles
#' @source  Images have been grabbed on the internet and prepared by the package's
#' authors. No particular choice has been made on the dimension of the original
#' images or the brands cited here.
#' @family datasets
NULL

#' Data: Outline coordinates of cephalic outlines of trilobite
#'
#' @docType data
#' @name trilo
#' @rdname data_trilo
#' @format A \link{Out} object 64 coordinates of 50 cephalic outlines from different
#' ontogenetic stages of trilobite.
#' @source  Arranged from: \url{ https://folk.universitetetioslo.no/} (used to be in \code{ohammer} website but
#' seems to be deprecated now).
#' The original data included 51 outlines and 5 ontogenetic stages,
#' but one of them has just a single outline thas has been removed.
#' @family datasets
NULL

#' Data: Outline coordinates of mosquito wings.
#'
#' @docType data
#' @name mosquito
#' @rdname data_mosquito
#' @format A \link{Out} object with the 126 mosquito wing outlines outlines
#' used Rohlf and Archie (1984). Note that the links defined here are quite approximate.
#' @source Rohlf F, Archie J. 1984. A comparison of Fourier methods for the
#' description of wing shape in mosquitoes (Diptera: Culicidae). \emph{Systematic Biology}: 302-317.
#' @family datasets
NULL

#' Data: Outline coordinates of hand-drawn hearts
#'
#' @docType data
#' @name hearts
#' @rdname data_hearts
#' @format A \link{Out} object with the outline coordinates of 240 hand-drawn hearts
#' by 8 different persons, with 4 landmarks.
#' @source We thank the fellows of the Ecology Department of the French Institute
#' of Pondicherry that drawn the hearts, that then have been smoothed, scaled, centered,
#' and downsampled to 80 coordinates per outline.
#' @family datasets
NULL

#' Data: Outline coordinates of olive seeds open outlines.
#'
#' @docType data
#' @name olea
#' @rdname data_olea
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
#' @family datasets
NULL

#' Data: Outline coordinates of various shapes
#'
#' @docType data
#' @name shapes
#' @rdname data_shapes
#' @format An \link{Out} object with the outline coordinates of some various shapes.
#' @source Borrowed default shapes from (c) Adobe Photoshop. Do not send me to jail.
#' @family datasets
NULL

#' Data: Landmarks coordinates of mosquito wings
#'
#' @docType data
#' @name wings
#' @rdname data_wings
#' @format A \link{Ldk} object containing 18 (x; y) landmarks  from 127 mosquito wings, from
#' @source Rohlf and Slice 1990.
#' @family datasets
NULL

#' Data: Configuration of landmarks of oak leaves
#'
#' From Viscosi and Cardini (2001).
#' @docType data
#' @name oak
#' @rdname data_oak
#' @format A \link{Ldk} object containing 11 (x; y) landmarks  from 176 oak leaves wings, from
#' @source Viscosi, V., & Cardini, A. (2011). Leaf morphology, taxonomy and
#' geometric morphometrics: a simplified protocol for beginners.
#' PloS One, 6(10), e25630. doi:10.1371/journal.pone.0025630
#' @family datasets
NULL

#' Data: Outline coordinates of 360 molars
#'
#' Courtesy of Julien Corny and Florent Detroit.
#' @docType data
#' @name molars
#' @rdname data_molars
#' @format A \link{Out} object containing 79 equilinearly spaced (x; y)
#' coordinates for 360 crown outlines, of modern human molars,
#' along with their type (\code{$type}) - 90 first upper molars (UM1), 90 second upper molars (UM2),
#' 90 first lower molars (LM1), 90 second lower molars (LM2) -
#' and the individual (\code{ind}) they come from (the data of the 360 molars are taken
#' from 180 individuals).
#' @source Corny, J., & Detroit, F. (2014). Technical Note: Anatomic
#' identification of isolated modern human molars: testing Procrustes aligned
#' outlines as a standardization procedure for elliptic fourier analysis.
#' \emph{American Journal of Physical Anthropology}, 153(2), 314-22. doi:10.1002/ajpa.22428
#' \url{ https://onlinelibrary.wiley.com/doi/abs/10.1002/ajpa.22428}
#' @family datasets
NULL

#' Data: Measurement of iris flowers
#'
#' @docType data
#' @name flower
#' @rdname data_flower
#' @format A \code{TraCoe} object with 150 measurements of 4 variables
#' (petal + sepal) x (length x width) on 3 species of iris. This dataset is
#' the classical \link{iris} formatted for Momocs.
#' @source see link{iris}
#' @family datasets
NULL

#' Data: Landmark and semilandmark coordinates on cereal glumes
#'
#' @docType data
#' @name chaff
#' @rdname data_chaff
#' @format An \link{Ldk} object with 21 configurations of landmarks and semi-landmarks (4 partitions)
#' sampled on cereal glumes
#' @source Research support was provided by the European Research Council
#' (Evolutionary Origins of Agriculture (grant no. 269830-EOA)
#' PI: Glynis Jones, Dept of Archaeology, Sheffield, UK. Data collected by Emily Forster.
#' @family datasets
NULL

#' Data: Outline coordinates from an experimental charring on cereal grains
#'
#' @docType data
#' @name charring
#' @rdname data_charring
#' @format An \link{Out} object with 18 grains, 3 views on each, for 2 cereal species, charred at
#' different temperatures for 6 hours (0C (no charring), 230C and 260C).
#' @source Research support was provided by the European Research Council
#' (Evolutionary Origins of Agriculture (grant no. 269830-EOA)
#' PI: Glynis Jones, Dept of Archaeology, Sheffield, UK. Data collected by Emily Forster.
#' @family datasets
NULL

#' Data: Outline coordinates of mouse molars
#'
#' @docType data
#' @name mouse
#' @rdname data_mouse
#' @format A \link{Out} object 64 coordinates of 30 wood molar outlines.
#' @source  Renaud S, Dufour AB, Hardouin EA, Ledevin R, Auffray JC (2015):
#' Once upon multivariate analyses: When they tell several stories about biological evolution.
#' \emph{PLoS One} 10:1-18 \url{ https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0132801}
#' @family datasets
NULL

#' Data: Outline coordinates of Apodemus (wood mouse) mandibles
#'
#' @docType data
#' @name apodemus
#' @rdname data_apodemus
#' @format A \link{Out} object 64 coordinates of 30 wood molar outlines.
#' @source  Renaud S, Pale JRM, Michaux JR (2003): Adaptive latitudinal trends in
#'  the mandible shape of \emph{Apodemus} wood mice. \emph{Journal of Biogeography} 30:1617-1628.
#' \url{https://onlinelibrary.wiley.com/doi/full/10.1046/j.1365-2699.2003.00932.x}
#' @family datasets
NULL

#' Data: Outline coordinates of North Sea fishes
#'
#' @docType data
#' @name nsfishes
#' @rdname data_nsfishes
#' @format A \link{Out} object containing the outlines coordinates for 218 fishes from
#' the North Sea along with taxonomical cofactors.
#' @source Caillon F, Frelat R, Mollmann C, Bonhomme V (submitted)
#' @family datasets
NULL
