\name{coo.slide}
\alias{coo.slide}
\title{Slides coordinates}
\usage{
coo.slide(coo, id1)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{id1}{the id of the point that will become the new
  first point.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Slides the coordinates so that the id1-th point become the
first one.
}
\examples{
data(hearts)
stack(hearts)
stack(coo.slide(hearts, 1))
coo.plot(hearts[4])
coo.plot(coo.slide(hearts[4], 50))
}
\keyword{coo_utilities}

