# Plots a single shape

A simple wrapper around
[plot](https://rdrr.io/r/graphics/plot.default.html) for plotting
shapes. Widely used in Momocs in other graphical functions, in methods,
etc.

## Usage

``` r
coo_plot(
  coo,
  xlim,
  ylim,
  border = "#333333",
  col = NA,
  lwd = 1,
  lty = 1,
  points = FALSE,
  first.point = TRUE,
  cex.first.point = 0.5,
  centroid = TRUE,
  xy.axis = TRUE,
  pch = 1,
  cex = 0.5,
  main = NA,
  poly = TRUE,
  plot.new = TRUE,
  plot = TRUE,
  zoom = 1,
  ...
)

ldk_plot(coo, ...)
```

## Arguments

- coo:

  A `list` or a `matrix` of coordinates.

- xlim:

  If `coo_plot` is called and `coo` is missing, then a vector of length
  2 specifying the `ylim` of the ploting area.

- ylim:

  If `coo_plot` is called and `coo` is missing, then a vector of length
  2 specifying the `ylim` of the ploting area.

- border:

  A color for the shape border.

- col:

  A color to fill the shape polygon.

- lwd:

  The `lwd` for drawing shapes.

- lty:

  The `lty` for drawing shapes.

- points:

  `logical`. Whether to display points. If missing and number of points
  is \< 100, then points are plotted.

- first.point:

  `logical` whether to plot or not the first point.

- cex.first.point:

  `numeric` size of this first point

- centroid:

  `logical`. Whether to display centroid.

- xy.axis:

  `logical`. Whether to draw the xy axis.

- pch:

  The `pch` for points.

- cex:

  The `cex` for points.

- main:

  `character`. A title for the plot.

- poly:

  logical whether to use
  [polygon](https://rdrr.io/r/graphics/polygon.html) and
  [lines](https://rdrr.io/r/graphics/lines.html) to draw the shape, or
  just [points](https://rdrr.io/r/graphics/points.html). In other words,
  whether the shape should be considered as a configuration of landmarks
  or not (eg a closed outline).

- plot.new:

  `logical` whether to plot or not a new frame.

- plot:

  logical whether to plot something or just to create an empty plot.

- zoom:

  a numeric to take your distances.

- ...:

  further arguments for use in coo_plot methods. See examples.

## Value

a plot

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

## Examples

``` r
b <- bot[1]
coo_plot(b)
coo_plot(bot[2], plot.new=FALSE) # equivalent to coo_draw(bot[2])

coo_plot(b, zoom=2)

coo_plot(b, border='blue')

coo_plot(b, first.point=FALSE, centroid=FALSE)

coo_plot(b, points=TRUE, pch=20)

coo_plot(b, xy.axis=FALSE, lwd=2, col='#F2F2F2')
```
