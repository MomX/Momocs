\name{rfourier}
\alias{rfourier}
\title{Calculates radii variation Fourier analysis.}
\usage{
rfourier(coo, nb.h, smooth.it = 0, norm = FALSE, verbose=FALSE)
}
\arguments{
  \item{coo}{A \code{list} or \code{matrix} of
  coordinates.}

  \item{nb.h}{\code{integer}. The number of harmonics to
  calculate/use.}

  \item{smooth.it}{\code{integer}. The number of smoothing
  iterations to perform.}

  \item{norm}{\code{logical}. Whether to scale the outlines
  so that the mean length of the radii used equals 1.}

  \item{verbose}{\code{logical}. Whether to display
  diagnosis messages.}
}
\value{
A list with these components: \item{an }{\code{vector} of
\eqn{a_{1->n}} harmonic coefficients.} \item{bn
}{\code{vector} of \eqn{b_{1->n}} harmonic coefficients.}
\item{ao }{\code{ao} Harmonic coefficient.} \item{r
}{\code{vector} of radii lengths.}
}
\description{
\code{rfourier} computes radii variation Fourier analysis
from a matrix or a list of coordinates.
}
\details{
Given a closed outline, the radius \eqn{r}, taken as the
distance from the outline barycentre and a given point of
the outline, can be expressed as a periodic function of the
angle \eqn{\theta}. Harmonics from \eqn{0} to \eqn{k}
approximate the function \eqn{r(\theta)}:

\deqn{r(\theta)= \frac{1}{2}a_0 +
\sum\limits_{n=1}^{k}a_k\cos(w_k\theta +
b_k\sin(w_k\theta)} with: \deqn{ a_n =
\frac{2}{p}\sum\limits_{n=1}^{p}r_i\cos(n\theta_i) } \deqn{
b_n = \frac{2}{p}\sum\limits_{n=1}^{p}r_i\sin(n\theta_i) }
with \deqn{ a_0 =
\sqrt{\frac{2}{p}}\sum\limits_{n=1}^{p}r_i }

The \eqn{a_n} and \eqn{b_n} harmonic coefficients,
extracted for every individual shape, are then used for
multivariate analyses.
}
\examples{
data(bot)
coo <- coo.center(bot[1]) # centering is almost mandatory for rfourier family
coo.plot(coo)
rf  <- rfourier(coo, 12)
rf
rfi <- rfourier.i(rf)
coo.draw(rfi, border="red", col=NA)
}
\references{
Claude, J. (2008) \emph{Morphometrics with R}, Use R!
series, Springer 316 pp.
}
\seealso{
\link{rfourier.i} for the inverse operation,
\link{rfourier.shape}. \link{efourier}, \link{tfourier} for
the other members of the Fourier's family.
}

