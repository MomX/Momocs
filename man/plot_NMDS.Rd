% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr-NMDS.R
\name{plot_NMDS}
\alias{plot_NMDS}
\alias{plot_MDS}
\title{NMDS plot unsing grindr layers}
\usage{
plot_NMDS(
  x,
  f = NULL,
  axes = c(1, 2),
  points = TRUE,
  points_transp = 1/4,
  chull = TRUE,
  chullfilled = FALSE,
  labelgroups = FALSE,
  legend = TRUE,
  title = "",
  box = TRUE,
  axesnames = TRUE,
  palette = pal_qual
)

plot_MDS(
  x,
  f = NULL,
  axes = c(1, 2),
  points = TRUE,
  points_transp = 1/4,
  chull = TRUE,
  chullfilled = FALSE,
  labelgroups = FALSE,
  legend = TRUE,
  title = "",
  box = TRUE,
  axesnames = TRUE,
  palette = pal_qual
)
}
\arguments{
\item{x}{the result of \link{MDS} or \link{NMDS}}

\item{f}{factor specification to feed \link{fac_dispatcher}}

\item{axes}{\code{numeric} of length two to select PCs to use
(\code{c(1, 2)} by default)}

\item{points}{\code{logical} whether to draw this with \link{layer_points}}

\item{points_transp}{\code{numeric} to feed \link{layer_points} (default:0.25)}

\item{chull}{\code{logical} whether to draw this with \link{layer_chull}}

\item{chullfilled}{\code{logical} whether to draw this with \link{layer_chullfilled}}

\item{labelgroups}{\code{logical} whether to draw this with \link{layer_labelgroups}}

\item{legend}{\code{logical} whether to draw this with \link{layer_legend}}

\item{title}{\code{character} if specified, fee \link{layer_title} (default to \code{""})}

\item{box}{\code{logical} whether to draw this using \link{layer_box}}

\item{axesnames}{\code{logical} whether to draw this using \link{layer_axesnames}}

\item{palette}{\code{color palette} to use \code{col_summer} by default}
}
\value{
a plot
}
\description{
Quickly vizualise \link{MDS} and \link{NMDS} objects and build customs plots
using the \link{layers}. See examples.
}
\examples{
### First prepare an NMDS object
x <- bot \%>\% efourier \%>\% NMDS

plot_NMDS(x)
plot_NMDS(x, ~type) \%>\% layer_stars() \%>\% layer_labelpoints()

### Same on MDS object
x <- bot \%>\% efourier \%>\% MDS

plot_MDS(x)
plot_MDS(x, ~type) \%>\% layer_stars() \%>\% layer_labelpoints()
}
\seealso{
Other grindr: 
\code{\link{drawers}},
\code{\link{layers_morphospace}},
\code{\link{layers}},
\code{\link{mosaic_engine}()},
\code{\link{papers}},
\code{\link{pile}()},
\code{\link{plot_LDA}()},
\code{\link{plot_PCA}()}
}
\concept{grindr}
