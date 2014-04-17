\name{lf.structure}
\alias{lf.structure}
\title{Extract structure from filenames}
\usage{
lf.structure(lf, names = character(), split = "_", trim.extension = TRUE)
}
\arguments{
  \item{lf}{a list filenames, as characters, typically such
  as those obtained with \link{list.files}.}

  \item{names}{the names of the groups.}

  \item{split}{character, the spliiting factor used for the
  file names.}

  \item{trim.extension}{logical. Whether to remove the last
  for characters in filenames, typically their extension,
  eg. ".jpg".}
}
\value{
data.frame with, for every individual, the corresponding
label for every group.
}
\description{
If filenames are consistently named: eg
'speciesI_siteA_ind1_dorsalview', returns a data.frame from
it that can be passed to Out, Opn, Ldk, objects.
}
\details{
#todo
}
\examples{
data(bot)
coo.area(bot[4])
}
\keyword{import}

