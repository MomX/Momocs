# Ptolemaic ellipses and illustration of efourier

Calculate and display Ptolemaic ellipses which illustrates intuitively
the principle behing elliptical Fourier analysis.

## Usage

``` r
Ptolemy(
  coo,
  t = seq(0, 2 * pi, length = 7)[-1],
  nb.h = 3,
  nb.pts = 360,
  palette = col_heat,
  zoom = 5/4,
  legend = TRUE,
  ...
)
```

## Arguments

- coo:

  a matrix of (x; y) coordinates

- t:

  A `vector` af angles (in radians) on which to display ellipses

- nb.h:

  `integer`. The number of harmonics to display

- nb.pts:

  `integer`. The number of points to use to display shapes

- palette:

  a color palette

- zoom:

  numeric a zoom factor for
  [coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md)

- legend:

  `logical`. Whether to plot the legend box

- ...:

  additional parameters to feed
  [coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md)

## Value

a drawing on the last plot

## References

This method has been inspired by the figures found in the followings
papers. Kuhl FP, Giardina CR. 1982. Elliptic Fourier features of a
closed contour. *Computer Graphics and Image Processing* **18**:
236-258. Crampton JS. 1995. Elliptical Fourier shape analysis of fossil
bivalves: some practical considerations. *Lethaia* **28**: 179-186.

## See also

An intuitive explanation of elliptic Fourier analysis can be found in
the **Details** section of the
[efourier](http://momx.github.io/Momocs/reference/efourier.md) function.

exemplifying functions

## Examples

``` r
cat <- shapes[4]
Ptolemy(cat, main="An EFT cat")
```
