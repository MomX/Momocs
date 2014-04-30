\name{hqual}
\alias{hqual}
\title{Graphical calibration for Out objects}
\usage{
hqual(Out, method = c("efourier", "rfourier", "tfourier"), id,
  harm.range = c(1, 2, 4, 8, 16, 32), smooth.it = 0, scale = TRUE,
  center = TRUE, align = TRUE, plot.method = c("panel", "stack")[1],
  legend = TRUE, legend.title = "Nb of harmonics", palette = col.india,
  shp.col = NA, shp.border = "#1A1A1A", ...)
}
\arguments{
  \item{Out}{the \code{Out} object on which to hqual}

  \item{method}{any method from \code{c("efourier",
  "rfourier", "tfourier")}}

  \item{id}{the shape on which to perform hqual}

  \item{harm.range}{vector of harmonics on which to perform
  hqual}

  \item{smooth.it}{numeric, number of smoothing iterations}

  \item{scale}{logical whether to scale the shape}

  \item{center}{logical whether to center the shape}

  \item{align}{logical whether to align th shape}

  \item{plot.method}{either \code{"\link{panel}"} or
  \code{"\link{stack}"}}

  \item{legend}{logical whether to plot a legend}

  \item{legend.title}{if TRUE above, its title}

  \item{palette}{a color \link{palette}}

  \item{shp.col}{a color for the shape (\code{NA} by
  default)}

  \item{shp.border}{a color for the border of the shape}

  \item{...}{additional parameters to fed \link{coo.plot}}
}
\description{
Calculate and displays reconstructed shapes using a range
of harmonic number.
}
\examples{
data(bot)
hqual(bot)
}
\keyword{Out}

