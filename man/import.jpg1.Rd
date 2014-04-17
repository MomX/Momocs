\name{import.jpg1}
\alias{import.jpg1}
\title{Extract outline coordinates from a single .jpg file}
\usage{
import.jpg1(jpg.path, auto.notcentered = FALSE, threshold = 0.5)
}
\arguments{
  \item{jpg.path}{a vector of paths corresponding to the
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
a matrix of (x; y) coordinates that can be passed to Out
}
\description{
Used to import outline coordinates from .jpg files. This
function is used for single images and is the core function
of \link{import.jpg}
}
\details{
jpegs can be provided either as RVB or as 8-bit greylevels
or monochrome. The function binarizes pixels values using
the 'threshold' argument. It will try to start apply the
\link{import.Conte} algortih of outline extraction from the
center of the image and 'loinking' downwards for the first
black -> white 'frontier' in the pixels. This point will be
the first of the outlines and it may be useful if you align
manually the images and if you want to retain this
information in the consequent morphometric analyses. If the
point at the center of the image is not within the shape,
ie is "white" you have two choices defined by the
'auto.notcentered' argument. If it's TRUE, some random
starting points will be tried until on of them is "black"
and within the shape; if FALSE you will be asked to click
on a point within the shape.
}

