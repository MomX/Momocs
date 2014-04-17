\name{tfourier}
\alias{tfourier}
\title{Calculates tangent angle Fourier analysis.}
\usage{
tfourier(coo, nb.h, smooth.it=0, norm = FALSE, silent = TRUE)
}
\arguments{
  \item{coo}{A list or matrix of coordinates}

  \item{nb.h}{\code{integer}. The number of harmonics to
  calculate/use}

  \item{smooth.it}{\code{integer}. The number of smoothing
  iterations to perform}

  \item{norm}{\code{logical}. Whether to scale and register
  new coordinates so that the first point used is sent on
  the origin.}

  \item{verbose}{\code{logical}. Whether to display
  diagnosis messages.}
}
\value{
A list with these components: \item{ao }{\code{ao} Harmonic
coefficient.} \item{an }{\code{vector} of \eqn{a_{1->n}}
harmonic coefficients.} \item{bn }{\code{vector} of
\eqn{b_{1->n}} harmonic coefficients.} \item{phi
}{\code{vector} of variation of the tangent angle.} \item{t
}{\code{vector} of distance along the perimeter expressed
in radians.} \item{perimeter }{\code{numeric}. The
perimeter of the outline.} \item{thetao }{\code{numeric}.
The first tangent angle.} \item{x1 }{The x-coordinate of
the first point.} \item{y1 }{The y-coordinate of the first
point.}
}
\description{
\code{tfourier} computes tangent angle Fourier analysis
from a matrix or a list of coordinates.
}
\details{
Given a closed outline which the outline has been scaled to
\eqn{2\pi}, \eqn{\phi(t)} can be expressed as follows:
\deqn{ \phi(t) = \theta(t) - \theta(0) - t } where \eqn{t}
is the distance along the outline, \eqn{\theta(t)} the
angle of the tangent vector at \eqn{t} and \eqn{\theta(0)}
the angle of the tangent vector taken for the first point.
It can be removed for normalizing the coefficients
obtained. Two coefficients per harmonics can be estimated
as follow:

\deqn{ a_n = \frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\cos n
\theta_i } \deqn{ b_n =
\frac{2}{p}\sum\limits_{n=1}^{p}\phi(t)\sin n \theta_i }
with \deqn{ a_0 =
\sqrt{\frac{2}{p}}\sum\limits_{n=1}^{p}\phi(t) }
}
\examples{
data(bot)
coo <- bot[1]
coo.plot(coo)
tf  <- tfourier(coo, 12)
tf
tfi <- tfourier.i(tf)
coo.draw(tfi, border="red", col=NA) # the outline is not closed...
coo.draw(tfourier.i(tf, force2close=TRUE), border="blue", col=NA) # we force it to close.
}
\references{
Zahn CT, Roskies RZ. 1972. Fourier Descriptors for Plane
Closed Curves. \emph{IEEE Transactions on Computers}
\bold{C-21}: 269-281.

Claude, J. (2008) \emph{Morphometrics with R}, Use R!
series, Springer 316 pp.
}
\seealso{
\link{efourier}, \link{rfourier} for the other members of
the Fourier's family.
}
\keyword{coreMorpho}

