# NMDS plot unsing grindr layers

Quickly vizualise [MDS](http://momx.github.io/Momocs/reference/MDS.md)
and [NMDS](http://momx.github.io/Momocs/reference/NMDS.md) objects and
build customs plots using the
[layers](http://momx.github.io/Momocs/reference/layers.md). See
examples.

## Usage

``` r
plot_NMDS(
  x,
  f = NULL,
  axes = c(1, 2),
  points = TRUE,
  points_transp = 1/4,
  chull = TRUE,
  chullfilled = FALSE,
  labelgroups = FALSE,
  legend = TRUE,
  title = "",
  box = TRUE,
  axesnames = TRUE,
  palette = pal_qual
)

plot_MDS(
  x,
  f = NULL,
  axes = c(1, 2),
  points = TRUE,
  points_transp = 1/4,
  chull = TRUE,
  chullfilled = FALSE,
  labelgroups = FALSE,
  legend = TRUE,
  title = "",
  box = TRUE,
  axesnames = TRUE,
  palette = pal_qual
)
```

## Arguments

- x:

  the result of [MDS](http://momx.github.io/Momocs/reference/MDS.md) or
  [NMDS](http://momx.github.io/Momocs/reference/NMDS.md)

- f:

  factor specification to feed
  [fac_dispatcher](http://momx.github.io/Momocs/reference/fac_dispatcher.md)

- axes:

  `numeric` of length two to select PCs to use (`c(1, 2)` by default)

- points:

  `logical` whether to draw this with
  [layer_points](http://momx.github.io/Momocs/reference/layers.md)

- points_transp:

  `numeric` to feed
  [layer_points](http://momx.github.io/Momocs/reference/layers.md)
  (default:0.25)

- chull:

  `logical` whether to draw this with
  [layer_chull](http://momx.github.io/Momocs/reference/layers.md)

- chullfilled:

  `logical` whether to draw this with
  [layer_chullfilled](http://momx.github.io/Momocs/reference/layers.md)

- labelgroups:

  `logical` whether to draw this with
  [layer_labelgroups](http://momx.github.io/Momocs/reference/layers.md)

- legend:

  `logical` whether to draw this with
  [layer_legend](http://momx.github.io/Momocs/reference/layers.md)

- title:

  `character` if specified, fee
  [layer_title](http://momx.github.io/Momocs/reference/layers.md)
  (default to `""`)

- box:

  `logical` whether to draw this using
  [layer_box](http://momx.github.io/Momocs/reference/layers.md)

- axesnames:

  `logical` whether to draw this using
  [layer_axesnames](http://momx.github.io/Momocs/reference/layers.md)

- palette:

  `color palette` to use `col_summer` by default

## Value

a plot

## See also

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`layers_morphospace`](http://momx.github.io/Momocs/reference/layers_morphospace.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)

## Examples

``` r
### First prepare an NMDS object
x <- bot %>% efourier %>% NMDS
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
#> 'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
#> Warning: results may be meaningless because data have negative entries
#>                  in method “bray”
#> Run 0 stress 0.07227125 
#> Run 1 stress 0.07227125 
#> ... Procrustes: rmse 8.460091e-06  max resid 3.090637e-05 
#> ... Similar to previous best
#> Run 2 stress 0.07227125 
#> ... New best solution
#> ... Procrustes: rmse 5.108024e-06  max resid 1.882959e-05 
#> ... Similar to previous best
#> Run 3 stress 0.1558547 
#> Run 4 stress 0.1653725 
#> Run 5 stress 0.07227125 
#> ... Procrustes: rmse 1.464749e-06  max resid 5.407828e-06 
#> ... Similar to previous best
#> Run 6 stress 0.1566038 
#> Run 7 stress 0.07227125 
#> ... Procrustes: rmse 1.490112e-06  max resid 3.743155e-06 
#> ... Similar to previous best
#> Run 8 stress 0.07227125 
#> ... Procrustes: rmse 5.69994e-06  max resid 2.264085e-05 
#> ... Similar to previous best
#> Run 9 stress 0.07227125 
#> ... Procrustes: rmse 3.375566e-06  max resid 1.412437e-05 
#> ... Similar to previous best
#> Run 10 stress 0.07227125 
#> ... Procrustes: rmse 1.702584e-06  max resid 7.024297e-06 
#> ... Similar to previous best
#> Run 11 stress 0.07227125 
#> ... Procrustes: rmse 4.198101e-06  max resid 1.568591e-05 
#> ... Similar to previous best
#> Run 12 stress 0.07227125 
#> ... Procrustes: rmse 2.782198e-06  max resid 9.31499e-06 
#> ... Similar to previous best
#> Run 13 stress 0.07227125 
#> ... Procrustes: rmse 6.140461e-06  max resid 2.422899e-05 
#> ... Similar to previous best
#> Run 14 stress 0.07227125 
#> ... Procrustes: rmse 2.371183e-06  max resid 6.726446e-06 
#> ... Similar to previous best
#> Run 15 stress 0.07227125 
#> ... Procrustes: rmse 4.036543e-06  max resid 1.59661e-05 
#> ... Similar to previous best
#> Run 16 stress 0.07227125 
#> ... Procrustes: rmse 5.818498e-06  max resid 2.334124e-05 
#> ... Similar to previous best
#> Run 17 stress 0.07227125 
#> ... Procrustes: rmse 1.884869e-06  max resid 7.993225e-06 
#> ... Similar to previous best
#> Run 18 stress 0.07227125 
#> ... Procrustes: rmse 9.897688e-06  max resid 3.87602e-05 
#> ... Similar to previous best
#> Run 19 stress 0.1595332 
#> Run 20 stress 0.07227125 
#> ... Procrustes: rmse 6.944369e-06  max resid 2.757848e-05 
#> ... Similar to previous best
#> *** Best solution repeated 15 times

plot_NMDS(x)
#> Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
plot_NMDS(x, ~type) %>% layer_stars() %>% layer_labelpoints()
#> Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet

### Same on MDS object
x <- bot %>% efourier %>% MDS
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)

plot_MDS(x)
#> Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
plot_MDS(x, ~type) %>% layer_stars() %>% layer_labelpoints()
#> Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
```
