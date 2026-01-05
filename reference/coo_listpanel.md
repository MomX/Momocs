# Plots sets of shapes.

`coo_listpanel` plots a list of shapes if passed with a list of
coordinates. Mainly used by
[panel.Coo](http://momx.github.io/Momocs/reference/panel.Coo.md)
functions. If used outside the latter, shapes must be "templated", see
[coo_template](http://momx.github.io/Momocs/reference/coo_template.md).
If you want to reorder shapes according to a factor, use
[arrange](http://momx.github.io/Momocs/reference/arrange.md).

## Usage

``` r
coo_listpanel(
  coo.list,
  dim,
  byrow = TRUE,
  fromtop = TRUE,
  cols,
  borders,
  poly = TRUE,
  points = FALSE,
  points.pch = 3,
  points.cex = 0.2,
  points.col = "#333333",
  ...
)
```

## Arguments

- coo.list:

  A `list` of coordinates

- dim:

  A `vector` of the form `(nb.row, nb.cols)` to specify the panel
  display. If missing, shapes are arranged in a square.

- byrow:

  `logical`. Whether to draw successive shape by row or by col.

- fromtop:

  `logical`. Whether to display shapes from the top of the plotting
  region.

- cols:

  A `vector` of colors to fill shapes.

- borders:

  A `vector` of colors to draw shape borders.

- poly:

  logical whether to use polygon or lines to draw shapes. mainly for use
  for outlines and open outlines.

- points:

  logical if poly is set to FALSE whether to add points

- points.pch:

  if points is TRUE, a pch for these points

- points.cex:

  if points is TRUE, a cex for these points

- points.col:

  if points is TRUE, a col for these points

- ...:

  additional arguments to feed generic `plot`

## Value

Returns (invisibly) a `data.frame` with position of shapes that can be
used for other sophisticated plotting design.

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
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
coo_listpanel(bot$coo) # equivalent to panel(bot)
```
