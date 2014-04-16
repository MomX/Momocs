\name{coo.rev}
\alias{coo.rev}
\title{Reverses coordinates}
\usage{
coo.rev(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Returns the reverse suite of coordinates, i.e. change
shape's orientation
}
\examples{
data(bot)
b <- coo.sample(bot[4], 4)
b
coo.rev(b)
}
\keyword{coo_utilities}

