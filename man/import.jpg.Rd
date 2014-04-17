\name{import.jpg}
\alias{import.jpg}
\title{Extract outline coordinates from multiple .jpg files}
\usage{
import.jpg(jpg.paths, auto.notcentered = FALSE, threshold = 0.5)
}
\arguments{
  \item{jpg.paths}{a vector of paths corresponding to the
  .jpg files to import}

  \item{auto.notcentered}{logical if TRUE random locations
  will be used until one of them is (assumed) to be within
  the shape (because of a black pixel); if FALSE a
  \link{locator} will be called, and you will have to click
  on a point within the shape.}

  \item{threshold}{the threshold value use to binarize the
  images. Above, pixels are turned to 1, below to 0.}

  \item{...}{arguments to be passed to \link{read.table},
  eg. 'skip', 'dec', etc.}
}
\value{
a list of matrices of (x; y) coordinates that can be passed
to Out
}
\description{
This function is used to import outline coordinates and is
built around \link{import.jpg1}
}
\details{
see \link{import.jpg1} and \link{import.Conte}.
}

