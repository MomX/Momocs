# Add landmarks labels

Add landmarks labels

## Usage

``` r
ldk_labels(ldk, d = 0.05, cex = 2/3, ...)
```

## Arguments

- ldk:

  a matrix of (x; y) coordinates: where to plot the labels

- d:

  how far from the coordinates, on a (centroid-landmark) segment

- cex:

  the cex for the label

- ...:

  additional parameters to fed
  [text](https://rdrr.io/r/graphics/text.html)

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
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

Other ldk plotters:
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md)

## Examples

``` r
coo_plot(wings[1])
ldk_labels(wings[1])

# closer and smaller
coo_plot(wings[1])
ldk_labels(wings[1], d=0.05, cex=0.5)
```
