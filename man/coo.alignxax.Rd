\name{coo.alignxax}
\alias{coo.alignxax}
\title{Aligns shapes along the x-axis
Align the longest axis of a shape along the x-axis}
\usage{
coo.alignxax(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Aligns shapes along the x-axis Align the longest axis of a
shape along the x-axis
}
\examples{
\dontrun{
data(bot)
b <- bot[4]
coo.plot(b)
coo.plot(coo.alignxax(b))
}
}
\keyword{coo_utilities}

