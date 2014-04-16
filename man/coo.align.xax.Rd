\name{coo.align.xax}
\alias{coo.align.xax}
\title{Aligns shapes along the x-axis
Align the longest axis of a shape along the x-axis}
\usage{
\method{coo.align}{xax}(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Aligns shapes along the x-axis Align the longest axis of a
shape along the x-axis
}
\examples{
data(bot)
b <- bot[4]
coo.plot(b)
coo.plot(coo.align.xax(b))
}
\keyword{coo_utilities}

