# Plots (lollipop) differences between two configurations

Draws 'arrows' between two configurations.

## Usage

``` r
coo_arrows(coo1, coo2, length = coo_centsize(coo1)/15, angle = 20, ...)
```

## Arguments

- coo1:

  A `list` or a `matrix` of coordinates.

- coo2:

  A `list` or a `matrix` of coordinates.

- length:

  a length for the arrows.

- angle:

  an angle for the arrows

- ...:

  optional parameters to fed
  [arrows](https://rdrr.io/r/graphics/arrows.html).

## Value

a plot

## See also

Other plotting functions:
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
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
coo_arrows(coo_sample(olea[3], 50), coo_sample(olea[6], 50))
#> Warning: zero-length arrow is of indeterminate angle and so skipped
title("Hi there !")
```
