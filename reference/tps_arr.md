# Deformation 'vector field' using Thin Plate Splines

`tps_arr`(ows) calculates deformations between two configurations and
illustrate them using arrows.

## Usage

``` r
tps_arr(
  fr,
  to,
  amp = 1,
  grid = TRUE,
  over = 1.2,
  palette = col_summer,
  arr.nb = 200,
  arr.levels = 100,
  arr.len = 0.1,
  arr.ang = 20,
  arr.lwd = 0.75,
  arr.col = "grey50",
  poly = TRUE,
  shp = TRUE,
  shp.col = rep(NA, 2),
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

  the reference \\(x; y)\\ coordinates

- to:

  the target \\(x; y)\\ coordinates

- amp:

  an amplification factor of differences between `fr` and `to`

- grid:

  whether to calculate and plot changes across the graphical window
  `TRUE` or just within the starting shape (`FALSE`)

- over:

  `numeric` that indicates how much the thin plate splines extends over
  the shapes

- palette:

  a color palette such those included in Momocs or produced with
  [colorRampPalette](https://rdrr.io/r/grDevices/colorRamp.html)

- arr.nb:

  `numeric` The number of arrows to calculate

- arr.levels:

  `numeric`. The number of levels for the color of arrows

- arr.len:

  `numeric` for the length of arrows

- arr.ang:

  `numeric` for the angle for arrows' heads

- arr.lwd:

  `numeric` for the `lwd` for drawing arrows

- arr.col:

  if `palette` is not used the color for arrows

- poly:

  whether to draw polygons (for outlines) or points (for landmarks)

- shp:

  `logical`. whether to draw shapes

- shp.col:

  two colors for filling the shapes

- shp.border:

  two colors for drawing the borders

- shp.lwd:

  two `lwd` for drawing shapes

- shp.lty:

  two `lty` fro drawing the shapes

- legend:

  logical whether to plot a legend

- legend.text:

  some text for the legend

- ...:

  additional arguments to feed
  [coo_draw](http://momx.github.io/Momocs/reference/coo_draw.md)

## Value

Nothing.

## See also

Other thin plate splines:
[`tps2d()`](http://momx.github.io/Momocs/reference/tps2d.md),
[`tps_grid()`](http://momx.github.io/Momocs/reference/tps_grid.md),
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
tps_arr(fr, to, arr.nb=200, palette=col_sari, amp=3)

tps_arr(fr, to, arr.nb=200, palette=col_sari, amp=3, grid=FALSE)
```
