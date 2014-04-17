\name{import.txt}
\alias{import.txt}
\title{Import coordinates from a .txt file}
\usage{
import.txt(txt.list, ...)
}
\arguments{
  \item{txt.list}{a vector of paths corresponding to the
  .txt files to import}

  \item{...}{arguments to be passed to \link{read.table},
  eg. 'skip', 'dec', etc.}
}
\value{
a list of matrix(ces) of (x; y) coordinates that can be
passed to Out, Opn, Ldk, etc.
}
\description{
A wrapper around read.table. May be used to import
outline/landmark coordinates By default it assumes that the
columns are not named in the .txt files. You can tune this
using the '...' argument.
}
\keyword{import}

