\name{ed}
\alias{ed}
\title{Calculates euclidean distance between two points.}
\usage{
ed(pt1, pt2)
}
\arguments{
  \item{pt1}{(x; y) coordinates of the first point.}

  \item{pt2}{(x; y) coordinates of the second point.}
}
\value{
Returns the euclidean distance between the two points.
}
\description{
\code{ed} simply calculates euclidean distance between two
points defined by their (x; y) coordinates. This function
is used internally but may be of interest for other
analyses.
}
\examples{
ed(c(0,1), c(1,0)) # sqrt 2
}
\seealso{
\link{edm}, \link{edm.nearest}, \link{dist}.
}
\keyword{Utilities}

