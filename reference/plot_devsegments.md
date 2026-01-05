# Draws colored segments from a matrix of coordinates.

Given a matrix of (x; y) coordinates, draws segments between every
points defined by the row of the matrix and uses a color to display an
information.

## Usage

``` r
plot_devsegments(coo, cols, lwd = 1)
```

## Arguments

- coo:

  A matrix of coordinates.

- cols:

  A vector of color of `length = nrow(coo)`.

- lwd:

  The `lwd` to use for drawing segments.

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
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)

## Examples

``` r
# we load some data
guinness <- coo_sample(bot[9], 100)

# we calculate the diff between 48 harm and one with 6 harm.
out.6    <- efourier_i(efourier(guinness, nb.h=6), nb.pts=120)

# we calculate deviations, you can also try 'edm'
dev <- edm_nearest(out.6, guinness) / coo_centsize(out.6)

# we prepare the color scale
d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
cols  <- paste0(col_summer(20)[d.cut], 'CC')

# we draw the results
coo_plot(guinness, main='Guiness fitted with 6 harm.', points=FALSE)
par(xpd=NA)
plot_devsegments(out.6, cols=cols, lwd=4)
coo_draw(out.6, lty=2, points=FALSE, col=NA)

par(xpd=FALSE)
```
