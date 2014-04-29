\name{coo.sample}
\alias{coo.sample}
\title{Sample coordinates (among points)}
\usage{
coo.sample(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{n}{an integer, the number fo points to sample.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Sample n coordinates among existing points
}
\examples{
data(bot)
stack(bot)
stack(coo.sample(bot, 12))
coo.plot(bot[4])
coo.plot(coo.sample(bot[4], 12))
}
\keyword{coo_utilities}

