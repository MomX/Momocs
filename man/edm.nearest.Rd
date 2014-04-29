\name{edm.nearest}
\alias{edm.nearest}
\title{Calculates the shortest euclidean distance found for every point of one
matrix among those of a second.}
\usage{
edm.nearest(m1, m2, full=FALSE)
}
\arguments{
  \item{m1}{The first \code{list} or \code{matrix} of
  coordinates.}

  \item{m2}{The second \code{list} or \code{matrix} of
  coordinates.}

  \item{full}{\code{logical}. Whether to returns a
  condensed version of the results.}
}
\value{
If \code{full} is \code{TRUE}, returns a \code{list} with
two components: \code{d} which is for every point of
\code{m1} the shortest distance found between it and any
point in \code{m2}, and \code{pos} the (\code{m2}) row
indices of these points. Otherwise returns \code{d} as a
numeric vector of the shortest distances.
}
\description{
\code{edm.nearest} calculates the shortest euclidean
distance found for every point of one matrix among those of
a second. In other words, if \code{m1, m2} have \code{n}
rows, the result will be the shortest distance for the
first point of \code{m1} to any point of \code{m2} and so
on, \code{n} times. This function is used internally but
may be of interest for other analyses.
}
\details{
So far this function is quite time consumming since it
performs \deqn{ n \times n } euclidean distance
computation.  If one wishes to align two (or more shapes)
Procrustes surimposition may provide a better solution.
}
\examples{
x <- matrix(1:10, nc=2)
edm.nearest(x, x+rnorm(10))
edm.nearest(x, x+rnorm(10), full=TRUE)
}
\seealso{
\link{ed}, \link{edm}, \link{dist}.
}
\keyword{domestic}

