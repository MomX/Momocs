\name{efourier}
\alias{efourier}
\title{Calculates elliptical Fourier analysis.}
\usage{
efourier(coo, nb.h, smooth.it = 0, verbose = TRUE)
}
\arguments{
  \item{coo}{A \code{list} or a \code{matrix} of
  coordinates.}

  \item{nb.h}{\code{integer}. The number of harmonics to
  use}

  \item{smooth.it}{\code{integer}. The number of smoothing
  iterations to perform.}

  \item{verbose}{\code{logical}. Whether to print or not
  diagnosis messages.}
}
\value{
A list with these components: \item{an }{\code{vector} of
\eqn{a_{1->n}} harmonic coefficients.} \item{bn
}{\code{vector} of \eqn{b_{1->n}} harmonic coefficients.}
\item{cn }{\code{vector} of \eqn{c_{1->n}} harmonic
coefficients.} \item{dn }{\code{vector} of \eqn{d_{1->n}}
harmonic coefficients.} \item{ao }{\code{ao} Harmonic
coefficient.} \item{co }{\code{co} Harmonic coefficient.}
}
\description{
\code{efourier} computes elliptical Fourier analysis from a
matrix or a list of coordinates.
}
\details{
These functions and their mathematical background detailed
below are here detailed to ease their use in new methods
but are used internally by methods on \code{Out}-objects.

Elliptic Fourier analysis and normalization are calculated
as follows. Let \eqn{T} be the perimeter of a given closed
outline, here considered as the period of the signal. One
sets \eqn{\omega = 2\pi/T} to be the pulse. Then, the
curvilinear abscissa, \eqn{t} varies from \eqn{0} to
\eqn{T}. One can express \eqn{x(t)} and \eqn{y(t)} as:
\deqn{ x(t) = \frac{a_0}{2}+\sum\limits_{n=1}^{+\infty}
a_n\cos n\omega t + b_n\sin n\omega t } with \deqn{ a_n =
\frac{2}{T}+ \int\limits_{0}^{T} x(t)\cos (n\omega t)
\mathrm{d} t } \deqn{ b_n = \frac{2}{T}+
\int\limits_{0}^{T} x(t)\sin (n\omega t) \mathrm{d} t }

similarly, \deqn{ y(t) =
\frac{c_0}{2}+\sum\limits_{n=1}^{+\infty} c_n\cos n\omega t
+ d_n\sin n\omega t } with \deqn{ c_n = \frac{2}{T}+
\int\limits_{0}^{T} y(t)\cos (n\omega t) \mathrm{d} t }
\deqn{d_n = \frac{2}{T}+ \int\limits_{0}^{T} y(t)\sin
(n\omega t) \mathrm{d} t }

Since the outline contains a \eqn{k} finite number of
points, one can therefore calculate discrete estimators for
every harmonic coefficient of the \eqn{n^{th}} harmonics:
\deqn{ a_n=\frac{T}{2\pi^2n^2}\sum\limits_{p=1}^k
\frac{\Delta x_p}{\Delta t_p}(\cos\frac{2\pi
nt_p}{T}-\cos\frac{2\pi nt_{p-1}}{T}) } \deqn{
b_n=\frac{T}{2\pi^2n^2}\sum\limits_{p=1}^k \frac{\Delta
x_p}{\Delta t_p}(\sin\frac{2\pi nt_p}{T}-\sin\frac{2\pi
nt_{p-1}}{T}) }

\eqn{\Delta x_1=x_1-x_k} and \eqn{c_n} and \eqn{d_n} are
calculated similarly. \eqn{a_0} and \eqn{c_0} correspond to
the estimate of the coordinates of the centroid of original
outline and are estimated by: \deqn{
a_0=\frac{2}{T}\sum\limits_{i=1}^p x_i } and \deqn{
c_0=\frac{2}{T}\sum\limits_{i=1}^p y_i }

Intuitively, for all positive integers \eqn{n}, the sum of
a cosine curve and a sine curve represent the \eqn{n^{th}}
harmonic content of the \eqn{x} and \eqn{y} projections of
the \eqn{k}-edged polygon, and for any \eqn{n}, these two
curves define an ellipse in the plane. Ferson and
colleagues noticed that in the "time" it takes the
\eqn{n^{th}} harmonic to traverse its ellipse \eqn{n}
times, the \eqn{(n+1)^{th}} harmonic has traversed its own
ellipse \eqn{n+1} times. The reconstruction of the original
polygon is done by vector adding these ellipses for all
harmonics, which echoes astronomical Ptolemy's epicycles
(see \link{Ptolemy}), and the reconstruction obtained from
\eqn{N} harmonics is the best possible fit in a
least-squares sense.
}
\examples{
data(bot)
coo <- bot[1]
coo.plot(coo)
ef <- efourier(coo, 12)
ef
efi <- efourier.i(ef)
coo.draw(efi, border="red", col=NA)
}
\references{
Claude, J. (2008) \emph{Morphometrics with R}, Use R!
series, Springer 316 pp.

Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape
variation of two-dimensional outlines. \emph{Systematic
Biology} \bold{34}: 59-68.
}
\seealso{
\link{efourier.i} for the reverse operation and
\link{eFourier} the method for \code{Out} objects.
\link{Ptolemy} for an implementation of the Ptolemaic
ellipses. \link{rfourier}, \link{tfourier} for the other
members of the Fourier's family.
}
\keyword{coreMorpho}

