# Plots differences as (colored) segments aka a ruban

Useful to display differences between shapes

## Usage

``` r
coo_ruban(coo, dev, palette = col_heat, normalize = TRUE, ...)
```

## Arguments

- coo:

  a shape, typically a mean shape

- dev:

  numeric a vector of distances or anythinh relevant

- palette:

  the color palette to use or any palette

- normalize:

  logical whether to normalize (TRUE by default) distances

- ...:

  other parameters to fed segments, eg lwd (see examples)

## Value

a plot

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

## Examples

``` r
ms <- MSHAPES(efourier(bot , 10), "type")
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
b <- ms$shp$beer
w <- ms$shp$whisky
# we obtain the mean shape, then euclidean distances between points
m <- MSHAPES(list(b, w))
d <- edm(b, w)
# First plot
coo_plot(m, plot=FALSE)
coo_draw(b)
coo_draw(w)
coo_ruban(m, d, lwd=5)


#Another example
coo_plot(m, plot=FALSE)
coo_ruban(m, d, palette=col_summer2, lwd=5)


#If you want linewidth rather than color
coo_plot(m, plot=FALSE)
coo_ruban(m, d, palette=col_black)
```
