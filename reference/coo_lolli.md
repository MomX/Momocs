# Plots (lollipop) differences between two configurations

Draws 'lollipops' between two configurations.

## Usage

``` r
coo_lolli(coo1, coo2, pch = NA, cex = 0.5, ...)
```

## Arguments

- coo1:

  A `list` or a `matrix` of coordinates.

- coo2:

  A `list` or a `matrix` of coordinates.

- pch:

  a pch for the points (default to NA)

- cex:

  a cex for the points

- ...:

  optional parameters to fed
  [points](https://rdrr.io/r/graphics/points.html) and
  [segments](https://rdrr.io/r/graphics/segments.html).

## Value

a drawing on the last plot

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
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
coo_lolli(coo_sample(olea[3], 50), coo_sample(olea[6], 50))
title("A nice title !")
```
