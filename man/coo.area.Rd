\name{coo.area}
\alias{coo.area}
\title{Calculates the area}
\usage{
coo.area(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
\code{numeric}, the area.
}
\description{
Calculates the area for any non-crossing polygon.
}
\examples{
data(bot)
coo.area(bot[1])
hist(sapply(bot$coo, coo.area), breaks=10)
}
\keyword{coo_descriptors}

