# (Metric) multidimensional scaling

A wrapper around
[stats::cmdscale](https://rdrr.io/r/stats/cmdscale.html).

## Usage

``` r
MDS(x, method = "euclidean", k = 2, ...)
```

## Arguments

- x:

  any [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

- method:

  a dissiminarity index to feed `method` in
  [stats::dist](https://rdrr.io/r/stats/dist.html) (default:
  `euclidean`)

- k:

  `numeric` number of dimensions to feed
  [stats::cmdscale](https://rdrr.io/r/stats/cmdscale.html) (default: 2)

- ...:

  additional parameters to feed
  [stats::cmdscale](https://rdrr.io/r/stats/cmdscale.html)

## Value

what is returned by [stats::dist](https://rdrr.io/r/stats/dist.html)
plus `$fac`. And prepend `MDS` class to it.

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
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md),
[`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)

## Examples

``` r
x <- bot %>% efourier %>% MDS
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
x
#> $x
#>                       [,1]          [,2]
#> brahma         -0.05277530 -0.0230987121
#> caney          -0.03692797  0.0072496676
#> chimay          0.08091319 -0.0047905849
#> corona         -0.06936747  0.0022942672
#> deusventrue    -0.01117242  0.0475689844
#> duvel           0.11514484 -0.0169960574
#> franziskaner   -0.04425459 -0.0185989836
#> grimbergen      0.02874996  0.0099554795
#> guiness         0.01231138  0.0006027416
#> hoegardeen     -0.04579716  0.0056378311
#> jupiler        -0.05431287  0.0072952179
#> kingfisher     -0.03821463 -0.0034548056
#> latrappe        0.13300133 -0.0384264345
#> lindemanskriek -0.03540248  0.0147002348
#> nicechouffe    -0.02097431  0.0133514818
#> pecheresse     -0.05277659  0.0083010974
#> sierranevada    0.03905169 -0.0068665050
#> tanglefoot      0.07741376 -0.0020813543
#> tauro          -0.05456357  0.0073871372
#> westmalle      -0.05000066  0.0065364611
#> amrut          -0.04851067  0.0003233973
#> ballantines     0.12125872 -0.0671656955
#> bushmills      -0.03619504 -0.0540035080
#> chivas          0.07579382  0.0395367090
#> dalmore         0.10196157  0.0535941215
#> famousgrouse   -0.03460400 -0.0243909943
#> glendronach    -0.05266032 -0.0003142188
#> glenmorangie   -0.06127392  0.0037530633
#> highlandpark    0.08332158 -0.0437952867
#> jackdaniels     0.01064050 -0.0005682657
#> jb             -0.04043031  0.0088167995
#> johnniewalker  -0.03553374 -0.0387787659
#> magallan       -0.07106980 -0.0303786761
#> makersmark      0.06020727  0.0476203806
#> oban           -0.06297482  0.0061352696
#> oldpotrero      0.03373439  0.0616822707
#> redbreast       0.06482559  0.0494227507
#> tamdhu         -0.04715671 -0.0061720938
#> wildturkey     -0.01548202  0.0145967715
#> yoichi          0.03410175 -0.0364811930
#> 
#> $fac
#> # A tibble: 40 × 2
#>    type   fake 
#>    <fct>  <fct>
#>  1 whisky a    
#>  2 whisky a    
#>  3 whisky a    
#>  4 whisky a    
#>  5 whisky a    
#>  6 whisky a    
#>  7 whisky a    
#>  8 whisky a    
#>  9 whisky a    
#> 10 whisky a    
#> # ℹ 30 more rows
#> 
#> attr(,"class")
#> [1] "MDS"  "list"

```
