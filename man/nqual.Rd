\name{nqual}
\alias{nqual}
\title{Graphical calibration for Opn objects}
\usage{
nqual(Opn, method = c("rawPolynomials", "orthoPolynomials"), id,
  n.range = c(2, 3, 4, 6, 8, 10), smooth.it = 0, baseline1 = c(-1, 0),
  baseline2 = c(1, 0), plot.method = c("panel", "stack")[1],
  legend = TRUE, legend.title = "Degree", palette = col.india,
  shp.border = "#1A1A1A")
}
\arguments{
  \item{Opn}{the \code{Opn} object on which to nqual}

  \item{method}{any method from \code{c("rawPolynomials",
  "orthoPolynomials")}}

  \item{id}{the shape on which to perform nqual}

  \item{n.range}{vector of polynomial degrees on which to
  perform nqual}

  \item{smooth.it}{numeric, number of smoothing iterations}

  \item{baseline1}{\eqn{(x; y)} coordinates for the first
  point of the baseline}

  \item{baseline2}{\eqn{(x; y)} coordinates for the second
  point of the baseline}

  \item{plot.method}{either \code{"\link{panel}"} or
  \code{"\link{stack}"}}

  \item{legend}{logical whether to plot a legend}

  \item{legend.title}{if TRUE above, its title}

  \item{palette}{a color \link{palette}}

  \item{shp.border}{a color for the border of the shape}
}
\description{
Calculate and displays reconstructed shapes using a range
of polynomial degrees.
}
\examples{
data(olea)
nqual(olea)
}
\keyword{Opn}

