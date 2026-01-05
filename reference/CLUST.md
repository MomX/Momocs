# Hierarchical clustering

Performs hierarchical clustering through
[dist](https://rdrr.io/r/stats/dist.html) and
[hclust](https://rdrr.io/r/stats/hclust.html). So far it is mainly a
wrapper around these two functions, plus plotting using the `dendextend`
package facilities.

## Usage

``` r
CLUST(x, ...)

# Default S3 method
CLUST(x, ...)

# S3 method for class 'Coe'
CLUST(
  x,
  fac,
  type = c("horizontal", "vertical", "fan")[1],
  k,
  dist_method = "euclidean",
  hclust_method = "complete",
  retain = 0.99,
  labels,
  lwd = 1/4,
  cex = 1/2,
  palette = pal_qual,
  ...
)
```

## Arguments

- x:

  a [Coe](http://momx.github.io/Momocs/reference/Coe.md) or
  [PCA](http://momx.github.io/Momocs/reference/PCA.md) object

- ...:

  useless here

- fac:

  factor specification for
  [fac_dispatcher](http://momx.github.io/Momocs/reference/fac_dispatcher.md)

- type:

  `character` one of `c("horizontal", "vertical", "fan")` (default:
  `horizontal`)

- k:

  `numeric` if provided and greater than 1, cut the tree into this
  number of groups

- dist_method:

  to feed [dist](https://rdrr.io/r/stats/dist.html)'s `method` argument,
  that is one of `euclidean` (default), `maximum`, `manhattan`,
  `canberra`, `binary` or `minkowski`.

- hclust_method:

  to feed [hclust](https://rdrr.io/r/stats/hclust.html)'s `method`
  argument, one of `ward.D`, `ward.D2`, `single`, `complete` (default),
  `average`, `mcquitty`, `median` or `centroid`.

- retain:

  number of axis to retain if a
  [PCA](http://momx.github.io/Momocs/reference/PCA.md) object is passed.
  If a number \< 1 is passed, then the number of PCs retained will be
  enough to capture this proportion of variance via
  [scree_min](http://momx.github.io/Momocs/reference/scree.md)

- labels:

  factor specification for labelling tips and to feed
  [fac_dispatcher](http://momx.github.io/Momocs/reference/fac_dispatcher.md)

- lwd:

  for branches (default: `0.25`)

- cex:

  for labels (default: `1`)

- palette:

  one of available
  [palettes](http://momx.github.io/Momocs/reference/palettes.md)

## Value

a `ggplot` plot

## See also

Other multivariate:
[`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md),
[`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md),
[`LDA()`](http://momx.github.io/Momocs/reference/LDA.md),
[`MANOVA()`](http://momx.github.io/Momocs/reference/MANOVA.md),
[`MANOVA_PW()`](http://momx.github.io/Momocs/reference/MANOVA_PW.md),
[`MDS()`](http://momx.github.io/Momocs/reference/MDS.md),
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md),
[`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)

## Examples

``` r
# On Coe
bf <- bot %>% efourier(6)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
CLUST(bf)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the dendextend package.
#>   Please report the issue at <https://github.com/talgalili/dendextend/issues>.

# with a factor and vertical
CLUST(bf, ~type, "v")

# with some cutting and different dist/hclust methods
CLUST(bf,
      dist_method="maximum", hclust_method="average",
      labels=~type, k=3, lwd=1, cex=1, palette=pal_manual(c("green", "yellow", "red")))


# On PCA
bf %>% PCA %>% CLUST

```
