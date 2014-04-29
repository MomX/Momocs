\name{coo.ldk}
\alias{coo.ldk}
\title{Defines interactively landmarks
Allows to interactively define a "nb.ldk" number of landarks on a shape.
Used in other facilities to acquire/manipulate data.}
\usage{
coo.ldk(coo, nb.ldk)
}
\arguments{
  \item{coo}{a \code{matrix} or a list of \eqn{(x; y)}
  coordinates.}

  \item{nb.ldk}{integer, the number of landmarks to define}
}
\value{
\code{numeric} that corresponds to the closest ids, on the
shape, from cliked points.
}
\description{
Defines interactively landmarks Allows to interactively
define a "nb.ldk" number of landarks on a shape. Used in
other facilities to acquire/manipulate data.
}
\examples{
\dontrun{
data(bot)
b <- bot[1]
coo.ldk(b, 3) # run this, and click 3 times
coo.ldk(bot, 2) # this also works on Out
}
}
\keyword{coo_utilities}

