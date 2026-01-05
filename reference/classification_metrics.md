# Calculate classification metrics on a confusion matrix

In some cases, the class correctness or the proportion of correctly
classified individuals is not enough, so here are more detailed metrics
when working on classification.

## Usage

``` r
classification_metrics(x)
```

## Arguments

- x:

  a `table` or an [LDA](http://momx.github.io/Momocs/reference/LDA.md)
  object

## Value

a list with the following components is returned:

1.  `accuracy` the fraction of instances that are correctly classified

2.  `macro_prf` data.frame containing `precision` (the fraction of
    correct predictions for a certain class); `recall`, the fraction of
    instances of a class that were correctly predicted; `f1` the
    harmonic mean (or a weighted average) of precision and recall.

3.  `macro_avg`, just the average of the three `macro_prf` indices

4.  `ova` a list of one-vs-all confusion matrices for each class

5.  `ova_sum` a single of all ova matrices

6.  `kappa` measure of agreement between the predictions and the actual
    labels

## See also

The pages below are of great interest to understand these metrics. The
code used is partley derived from the Revolution Analytics blog post
(with their authorization). Thanks to them!

1.  <https://en.wikipedia.org/wiki/Precision_and_recall>

2.  `https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html`

Other multivariate:
[`CLUST()`](http://momx.github.io/Momocs/reference/CLUST.md),
[`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md),
[`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md),
[`LDA()`](http://momx.github.io/Momocs/reference/LDA.md),
[`MANOVA()`](http://momx.github.io/Momocs/reference/MANOVA.md),
[`MANOVA_PW()`](http://momx.github.io/Momocs/reference/MANOVA_PW.md),
[`MDS()`](http://momx.github.io/Momocs/reference/MDS.md),
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md)

## Examples

``` r
# some morphometrics on 'hearts'
hearts %>% fgProcrustes(tol=1) %>%
coo_slide(ldk=1) %>% efourier(norm=FALSE) %>% PCA() %>%
# now the LDA and its summary
LDA(~aut) %>% classification_metrics()
#> iteration:  1    gain: 30322 
#> iteration:  2    gain: 1.2498 
#> iteration:  3    gain: 0.34194 
#> 'nb.h' set to 7 (99% harmonic power)
#> 11 PC retained
#> $accuracy
#> [1] 0.7666667
#> 
#> $macro_prf
#> # A tibble: 8 × 3
#>   precision recall    f1
#>       <dbl>  <dbl> <dbl>
#> 1     0.839  0.867 0.852
#> 2     0.75   0.8   0.774
#> 3     0.542  0.433 0.481
#> 4     0.893  0.833 0.862
#> 5     0.893  0.833 0.862
#> 6     0.812  0.867 0.839
#> 7     0.871  0.9   0.885
#> 8     0.529  0.6   0.562
#> 
#> $macro_avg
#> # A tibble: 1 × 3
#>   avg_precision avg_recall avg_f1
#>           <dbl>      <dbl>  <dbl>
#> 1         0.766      0.767  0.765
#> 
#> $ova
#> $ova$ced
#>         classified
#> actual   ced others
#>   ced     26      4
#>   others   5    205
#> 
#> $ova$jeya
#>         classified
#> actual   jeya others
#>   jeya     24      6
#>   others    8    202
#> 
#> $ova$mat
#>         classified
#> actual   mat others
#>   mat     13     17
#>   others  11    199
#> 
#> $ova$ponnu
#>         classified
#> actual   ponnu others
#>   ponnu     25      5
#>   others     3    207
#> 
#> $ova$remi
#>         classified
#> actual   remi others
#>   remi     25      5
#>   others    3    207
#> 
#> $ova$rom
#>         classified
#> actual   rom others
#>   rom     26      4
#>   others   6    204
#> 
#> $ova$ruks
#>         classified
#> actual   ruks others
#>   ruks     27      3
#>   others    4    206
#> 
#> $ova$vince
#>         classified
#> actual   vince others
#>   vince     18     12
#>   others    16    194
#> 
#> 
#> $ova_sum
#>           classified
#> actual     relevant others
#>   relevant      184     56
#>   others         56   1624
#> 
#> $kappa
#> [1] 0.7333333
#> 
```
