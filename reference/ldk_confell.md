# Draws confidence ellipses for landmark positions

Draws confidence ellipses for landmark positions

## Usage

``` r
ldk_confell(
  ldk,
  conf = 0.5,
  col = "grey40",
  ell.lty = 1,
  ax = TRUE,
  ax.lty = 2
)
```

## Arguments

- ldk:

  an array (or a list) of landmarks

- conf:

  the confidence level (normal quantile, 0.5 by default)

- col:

  the color for the ellipse

- ell.lty:

  an lty for the ellipse

- ax:

  logical whether to draw ellipses axes

- ax.lty:

  an lty for ellipses axes

## Value

a drawing on the last plot

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

Other ldk plotters:
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md)

## Examples

``` r
coo_plot(MSHAPES(wings))
ldk_confell(wings$coo)
```
