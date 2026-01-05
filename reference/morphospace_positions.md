# Calculates nice positions on a plane for drawing shapes

Calculates nice positions on a plane for drawing shapes

## Usage

``` r
morphospace_positions(
  xy,
  pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
  nb.shp = 12,
  nr.shp = 6,
  nc.shp = 5,
  circle.r.shp
)
```

## Arguments

- xy:

  a matrix of points typically from a PCA or other multivariate method
  on which morphospace can be calculated

- pos.shp:

  how shapes should be positionned: `range` of xy, `full` extent of the
  plane, `circle` as a rosewind, on `xy` values provided, `range_axes`
  on the range of xy but on the axes, `full_axes` same thing but on
  (0.85) range of the axes. You can also directly pass a matrix (or a
  data.frame) with columns named `("x", "y")`.

- nb.shp:

  the total number of shapes

- nr.shp:

  the number of rows to position shapes

- nc.shp:

  the number of cols to position shapes

- circle.r.shp:

  if circle, its radius

## Value

a data.frame of positions

## Details

See [plot.PCA](http://momx.github.io/Momocs/reference/plot.PCA.md) for
self-speaking examples
