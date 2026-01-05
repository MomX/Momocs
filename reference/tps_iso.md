# Deformation isolines using Thin Plate Splines.

`tps_iso` calculates deformations between two configurations and map
them with or without isolines.

## Usage

``` r
tps_iso(
  fr,
  to,
  amp = 1,
  grid = FALSE,
  over = 1.2,
  palette = col_spring,
  iso.nb = 1000,
  iso.levels = 12,
  cont = TRUE,
  cont.col = "black",
  poly = TRUE,
  shp = TRUE,
  shp.border = col_qual(2),
  shp.lwd = c(2, 2),
  shp.lty = c(1, 1),
  legend = TRUE,
  legend.text,
  ...
)
```

## Arguments

- fr:

  The reference \\(x; y)\\ coordinates

- to:

  The target \\(x; y)\\ coordinates

- amp:

  An amplification factor of differences between `fr` and `to`

- grid:

  whether to calculate and plot changes across the graphical window
  `TRUE` or just within the starting shape (`FALSE`)

- over:

  A `numeric` that indicates how much the thin plate splines extends
  over the shapes

- palette:

  A color palette such those included in Momocs or produced with
  [colorRampPalette](https://rdrr.io/r/grDevices/colorRamp.html)

- iso.nb:

  A `numeric`. The number of points to use for the calculation of
  deformation

- iso.levels:

  `numeric`. The number of levels for mapping the deformations

- cont:

  `logical`. Whether to draw contour lines

- cont.col:

  A color for drawing the contour lines

- poly:

  whether to draw polygons (for outlines) or points (for landmarks)

- shp:

  `logical`. Whether to draw shapes

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

No returned value

## See also

Other thin plate splines:
[`tps2d()`](http://momx.github.io/Momocs/reference/tps2d.md),
[`tps_arr()`](http://momx.github.io/Momocs/reference/tps_arr.md),
[`tps_grid()`](http://momx.github.io/Momocs/reference/tps_grid.md),
[`tps_raw()`](http://momx.github.io/Momocs/reference/tps_raw.md)

## Examples

``` r
botF <- efourier(bot)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
x <- MSHAPES(botF, 'type', nb.pts=80)$shp
fr <- x$beer
to <- x$whisky
tps_iso(fr, to, iso.nb=200, amp=3)

tps_iso(fr, to, iso.nb=200, amp=3, grid=TRUE)
```
