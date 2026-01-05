# Deformation grids using Thin Plate Splines

`tps_grid` calculates and plots deformation grids between two
configurations.

## Usage

``` r
tps_grid(
  fr,
  to,
  amp = 1,
  over = 1.2,
  grid.size = 15,
  grid.col = "grey80",
  poly = TRUE,
  shp = TRUE,
  shp.col = rep(NA, 2),
  shp.border = col_qual(2),
  shp.lwd = c(1, 1),
  shp.lty = c(1, 1),
  legend = TRUE,
  legend.text,
  ...
)
```

## Arguments

- fr:

  the reference \\(x; y)\\ coordinates

- to:

  the target \\(x; y)\\ coordinates

- amp:

  an amplification factor of differences between `fr` and `to`

- over:

  `numeric` that indicates how much the thin plate splines extends over
  the shapes

- grid.size:

  `numeric` to specify the number of grid cells on the longer axis on
  the outlines

- grid.col:

  color for drawing the grid

- poly:

  whether to draw polygons (for outlines) or points (for landmarks)

- shp:

  `logical`. Whether to draw shapes

- shp.col:

  Two colors for filling the shapes

- shp.border:

  Two colors for drawing the borders

- shp.lwd:

  Two `lwd` for drawing shapes

- shp.lty:

  Two `lty` fro drawing the shapes

- legend:

  logical whether to plot a legend

- legend.text:

  some text for the legend

- ...:

  additional arguments to feed
  [coo_draw](http://momx.github.io/Momocs/reference/coo_draw.md)

## Value

Nothing

## See also

Other thin plate splines:
[`tps2d()`](http://momx.github.io/Momocs/reference/tps2d.md),
[`tps_arr()`](http://momx.github.io/Momocs/reference/tps_arr.md),
[`tps_iso()`](http://momx.github.io/Momocs/reference/tps_iso.md),
[`tps_raw()`](http://momx.github.io/Momocs/reference/tps_raw.md)

## Examples

``` r
botF <- efourier(bot)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
x <- MSHAPES(botF, 'type', nb.pts=80)$shp
fr <- x$beer
to <- x$whisky
tps_grid(fr, to, amp=3, grid.size=10)
```
