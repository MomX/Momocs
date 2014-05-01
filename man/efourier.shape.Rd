\name{efourier.shape}
\alias{efourier.shape}
\title{Calculates and draw "efourier" shapes.}
\usage{
efourier.shape(an, bn, cn, dn, nb.h, nb.pts = 60, alpha = 2, plot = TRUE)
}
\arguments{
  \item{an}{\code{numeric}. The \eqn{a_n} Fourier
  coefficients on which to calculate a shape.}

  \item{bn}{\code{numeric}. The \eqn{b_n} Fourier
  coefficients on which to calculate a shape.}

  \item{cn}{\code{numeric}. The \eqn{c_n} Fourier
  coefficients on which to calculate a shape.}

  \item{dn}{\code{numeric}. The \eqn{d_n} Fourier
  coefficients on which to calculate a shape.}

  \item{nb.h}{\code{integer}. The number of harmonics to
  use.}

  \item{nb.pts}{\code{integer}. The number of points to
  calculate.}

  \item{alpha}{\code{numeric}. The power coefficient
  associated with the (usually decreasing) amplitude of the
  Fourier coefficients (see \bold{Details}).}

  \item{plot}{\code{logical}. Whether to plot or not the
  shape.}
}
\value{
A list with components: \item{x }{\code{vector} of
\code{x}-coordinates.} \item{y }{\code{vector} of
\code{y}-coordinates.}
}
\description{
\code{efourier.shape} calculates a "Fourier elliptical
shape" given Fourier coefficients (see \code{Details}) or
can generate some "efourier" shapes.
}
\details{
\code{efourier.shape} can be used by specifying \code{nb.h}
and \code{alpha}. The coefficients are then sampled in an
uniform distribution \eqn{(-\pi ; \pi)} and this amplitude
is then divided by \eqn{harmonicrank^alpha}. If
\code{alpha} is lower than 1, consecutive coefficients will
thus increase. See \link{efourier} for the mathematical
background.
}
\examples{
data(bot)
ef <- efourier(bot[1], 24)
efourier.shape(ef$an, ef$bn, ef$cn, ef$dn) # equivalent to efourier.i(ef)
efourier.shape() # is autonomous

panel(Out(a2l(replicate(100,
efourier.shape(nb.h=6, alpha=2.5, plot=FALSE))))) # Bubble family
}
\references{
Claude, J. (2008) \emph{Morphometrics with R}, Use R!
series, Springer 316 pp.

Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape
variation of two-dimensional outlines. \emph{Systematic
Biology} \bold{34}: 59-68.
}
\seealso{
\link{efourier.i}.
}
\keyword{coreMorpho}

