\name{color-palettes}
\alias{col.gallus}
\alias{col.solarized}
\alias{col.summer}
\alias{col.summer2}
\alias{color-palettes}
\alias{palette}
\title{Some color palettes.}
\usage{
col.summer(n)
col.summer2(n)
col.solarized(n)
col.gallus(n)
col.blackgallus(n)
col.hot(n)
col.cold(n)
col.sari(n)
col.india(n)
col.bw(n)
}
\arguments{
  \item{n}{the number of colors to generate from the color
  palette}
}
\value{
color codes (hexadecimal format)
}
\description{
Some color palettes.
}
\examples{
barplot(1:10, col=col.summer(10), main="col.summer")
barplot(1:10, col=col.summer2(10), main="col.summer2")
barplot(1:10, col=col.solarized(10), main="col.solarized")
barplot(1:10, col=col.gallus(10), main="col.gallus")
barplot(1:10, col=col.blackgallus(10), main="col.blackgallus")
barplot(1:10, col=col.hot(10), main="col.hot")
barplot(1:10, col=col.cold(10), main="col.cold")
barplot(1:10, col=col.sari(10), main="col.sari")
barplot(1:10, col=col.india(10), main="col.india")
barplot(1:10, col=col.bw(10), main="col.bw")
}
\keyword{graphics}

