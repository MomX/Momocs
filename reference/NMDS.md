# Non metric multidimensional scaling

A wrapper around
[vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html).

## Usage

``` r
NMDS(x, distance = "bray", k = 2, try = 20, trymax = 20, ...)
```

## Arguments

- x:

  any [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

- distance:

  a dissiminarity index to feed
  [vegan::vegdist](https://vegandevs.github.io/vegan/reference/vegdist.html)
  (default: `bray`)

- k:

  `numeric` number of dimensions to feed
  [vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)
  (default: 2)

- try:

  `numeric` minimum number of random starts to feed
  [vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)
  (default: 20)

- trymax:

  `numeric` minimum number of random starts to feed
  [vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)
  (default: 20)

- ...:

  additional parameters to feed
  [vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)

## Value

what is returned by
[vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)
plus `$fac`. And prepend `NMDS` class to it.

## Details

For Details, see
[vegan::metaMDS](https://vegandevs.github.io/vegan/reference/metaMDS.html)

## See also

Other multivariate:
[`CLUST()`](http://momx.github.io/Momocs/reference/CLUST.md),
[`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md),
[`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md),
[`LDA()`](http://momx.github.io/Momocs/reference/LDA.md),
[`MANOVA()`](http://momx.github.io/Momocs/reference/MANOVA.md),
[`MANOVA_PW()`](http://momx.github.io/Momocs/reference/MANOVA_PW.md),
[`MDS()`](http://momx.github.io/Momocs/reference/MDS.md),
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md),
[`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)

## Examples

``` r
x <- bot %>% efourier %>% NMDS
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
#> 'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
#> Warning: results may be meaningless because data have negative entries
#>                  in method “bray”
#> Run 0 stress 0.07227125 
#> Run 1 stress 0.07227125 
#> ... Procrustes: rmse 8.334796e-06  max resid 3.047993e-05 
#> ... Similar to previous best
#> Run 2 stress 0.07227125 
#> ... New best solution
#> ... Procrustes: rmse 5.865888e-06  max resid 2.065795e-05 
#> ... Similar to previous best
#> Run 3 stress 0.07227125 
#> ... Procrustes: rmse 6.50041e-06  max resid 2.560239e-05 
#> ... Similar to previous best
#> Run 4 stress 0.07227126 
#> ... Procrustes: rmse 1.973897e-05  max resid 8.1637e-05 
#> ... Similar to previous best
#> Run 5 stress 0.07227125 
#> ... Procrustes: rmse 1.611046e-06  max resid 5.206746e-06 
#> ... Similar to previous best
#> Run 6 stress 0.07227125 
#> ... Procrustes: rmse 4.38684e-06  max resid 1.707128e-05 
#> ... Similar to previous best
#> Run 7 stress 0.07227125 
#> ... Procrustes: rmse 3.098235e-06  max resid 1.266529e-05 
#> ... Similar to previous best
#> Run 8 stress 0.07227125 
#> ... Procrustes: rmse 7.27379e-06  max resid 2.741795e-05 
#> ... Similar to previous best
#> Run 9 stress 0.07227125 
#> ... Procrustes: rmse 2.350355e-06  max resid 9.21327e-06 
#> ... Similar to previous best
#> Run 10 stress 0.07227125 
#> ... Procrustes: rmse 8.276153e-06  max resid 3.231576e-05 
#> ... Similar to previous best
#> Run 11 stress 0.1733868 
#> Run 12 stress 0.07227125 
#> ... Procrustes: rmse 5.64529e-06  max resid 2.294965e-05 
#> ... Similar to previous best
#> Run 13 stress 0.07227125 
#> ... New best solution
#> ... Procrustes: rmse 1.826634e-06  max resid 6.121634e-06 
#> ... Similar to previous best
#> Run 14 stress 0.1709375 
#> Run 15 stress 0.07227125 
#> ... Procrustes: rmse 6.363625e-06  max resid 2.436065e-05 
#> ... Similar to previous best
#> Run 16 stress 0.07227125 
#> ... Procrustes: rmse 2.991654e-06  max resid 1.240109e-05 
#> ... Similar to previous best
#> Run 17 stress 0.07227125 
#> ... Procrustes: rmse 6.666184e-06  max resid 2.63522e-05 
#> ... Similar to previous best
#> Run 18 stress 0.07227125 
#> ... Procrustes: rmse 8.082234e-06  max resid 3.110865e-05 
#> ... Similar to previous best
#> Run 19 stress 0.07227125 
#> ... Procrustes: rmse 5.577537e-06  max resid 2.153054e-05 
#> ... Similar to previous best
#> Run 20 stress 0.07227125 
#> ... Procrustes: rmse 2.058946e-06  max resid 7.515172e-06 
#> ... Similar to previous best
#> *** Best solution repeated 7 times

# Shepard diagram # before a Momocs wrapper
# vegan::stressplot(x)
```
