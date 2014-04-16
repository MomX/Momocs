\name{coo.ldk}
\alias{coo.ldk}
\title{Defines interactively landmarks
Allows to interactively define a "nb.ldk" number of landarks on a shape.
Used in other facilities to acquire/manipulate data.}
\usage{
coo.ldk(coo, nb.ldk)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}

  \item{nb.ldk}{integer, the number of landmarks to define}
}
\value{
a numeric that corresponds to the closest ids, on the
shape, from cliked points.
}
\description{
Defines interactively landmarks Allows to interactively
define a "nb.ldk" number of landarks on a shape. Used in
other facilities to acquire/manipulate data.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 4)
# coo.ldk(b, 3) # run this, and click 3 times.
}
\keyword{coo_utilities}

