# Draws kernel density contours around landmark

Using kde2d in the MASS package.

## Usage

``` r
ldk_contour(ldk, nlevels = 5, grid.nb = 50, col = "grey60")
```

## Arguments

- ldk:

  an array (or a list) of landmarks

- nlevels:

  the number of contour lines

- grid.nb:

  the grid.nb

- col:

  a color for drawing the contour lines

## Value

a drawing on the last plot

## See also

kde2d,
[ldk_confell](http://momx.github.io/Momocs/reference/ldk_confell.md),
[ldk_chull](http://momx.github.io/Momocs/reference/ldk_chull.md)

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

Other ldk plotters:
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md)

## Examples

``` r
coo_plot(MSHAPES(wings))
ldk_contour(wings$coo)
```
