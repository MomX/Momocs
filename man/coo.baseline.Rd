\name{coo.baseline}
\alias{coo.baseline}
\title{Register new baselines}
\usage{
coo.baseline(coo, ldk1, ldk2, t1, t2)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{ldk1}{the id of the first point of the new
  baseline}

  \item{ldk2}{the id of the second point of the new
  baseline}

  \item{t1}{numeric the (x,y) coordinates of the 1st point
  of the new baseline}

  \item{t2}{numeric the (x,y) coordinates of the 2nd point
  of the new baseline}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
A non-exact baseline registration on t1 and t2 coordinates,
for the ldk1-th and ldk2-th points. By default it returns
Bookstein's coordinates.
}
\examples{
data(hearts)
stack(hearts)
# note the scale
stack(coo.baseline(hearts, 2, 4, c(-1, 0), c(1, 0)))
}
\keyword{coo_utilities}

