\name{coo.force2close}
\alias{coo.force2close}
\title{Forces shapes to close}
\usage{
coo.force2close(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
An exotic function that distribute the distance between the
first and the last points of unclosed shapes, so that they
become closed. May be useful (?) e.g. for t/rfourier
methods where reconstructed shapes may not be closed.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 64)
b <- b[1:40,]
coo.plot(b)
coo.plot(coo.force2close(b))
}
\keyword{coo_utilities}

