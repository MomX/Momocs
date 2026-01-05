# Draws convex hulls around landmark positions

A wrapper that uses
[coo_chull](http://momx.github.io/Momocs/reference/coo_chull.md)

## Usage

``` r
ldk_chull(ldk, col = "grey40", lty = 1)
```

## Arguments

- ldk:

  an array (or a list) of landmarks

- col:

  a color for drawing the convex hull

- lty:

  an lty for drawing the convex hulls

## Value

a drawing on the last plot

## See also

[coo_chull](http://momx.github.io/Momocs/reference/coo_chull.md),
[chull](https://rdrr.io/r/grDevices/chull.html),
[ldk_confell](http://momx.github.io/Momocs/reference/ldk_confell.md),
[ldk_contour](http://momx.github.io/Momocs/reference/ldk_contour.md)

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

Other ldk plotters:
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md)

## Examples

``` r
coo_plot(MSHAPES(wings))
ldk_chull(wings$coo)
```
