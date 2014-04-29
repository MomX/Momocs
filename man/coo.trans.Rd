\name{coo.trans}
\alias{coo.trans}
\title{Translates coordinates}
\usage{
coo.trans(coo, x = 0, y = 0)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{x}{the translation on the x dimension.}

  \item{y}{the translation on the y dimension.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Translates the coordinatesby a 'x' and 'y' value
}
\examples{
data(bot)
stack(bot)
stack(coo.trans(bot, 10, 10))
coo.plot(bot[4])
coo.plot(coo.trans(bot[4], 10, 10))
}
\seealso{
Other res: \code{\link{Coo}}, \code{\link{Out}};
\code{\link{Opn}}
}
\keyword{coo_utilities}

