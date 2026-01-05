# Adds a shape to the current plot

`coo_draw` is simply a
[coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md) with
`plot.new=FALSE`, ie that adds a shape on the active plot.

## Usage

``` r
coo_draw(coo, ...)
```

## Arguments

- coo:

  a `list` or a `matrix` of coordinates.

- ...:

  optional parameters for
  [coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md)

## Value

a drawing on the last plot

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
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
b1 <- bot[4]
b2 <- bot[5]
coo_plot(b1)
coo_draw(b2, border='red') # all coo_plot arguments will work for coo_draw
```
