# Pairwise Multivariate analyses of variance

A wrapper for pairwise
[MANOVA](http://momx.github.io/Momocs/reference/MANOVA.md)s on
[Coe](http://momx.github.io/Momocs/reference/Coe.md) objects. Calculates
a MANOVA for every pairwise combination of the factor provided.

## Usage

``` r
MANOVA_PW(x, ...)

# S3 method for class 'PCA'
MANOVA_PW(x, fac, retain = 0.99, ...)
```

## Arguments

- x:

  a [PCA](http://momx.github.io/Momocs/reference/PCA.md) object

- ...:

  more arguments to feed
  [MANOVA](http://momx.github.io/Momocs/reference/MANOVA.md)

- fac:

  a name (or its id) of a grouping factor in `$fac` or a factor or a
  formula.

- retain:

  the number of PC axis to retain (1:retain) or the proportion of
  variance to capture (0.99 par default).

## Value

a list with the following components is returned (invisibly because
\$manovas may be very long, see examples):

- manovas a list containing all the raw manovas

- summary

- stars.tab a table with 'significance stars', discutable but largely
  used: '***' if Pr(\>F) \< 0.001; '**' of \< 0.01; '*' if \< 0.05; '.'
  if \< 0.10 and '-' if above.

## Note

Needs a review and should be considered as experimental. If the fac
passed has only two levels, there is only pair and it is equivalent to
[MANOVA](http://momx.github.io/Momocs/reference/MANOVA.md).
`MANOVA_PW.PCA` works with the regular
[manova](https://rdrr.io/r/stats/manova.html).

## See also

[MANOVA](http://momx.github.io/Momocs/reference/MANOVA.md),
[manova](https://rdrr.io/r/stats/manova.html).

Other multivariate:
[`CLUST()`](http://momx.github.io/Momocs/reference/CLUST.md),
[`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md),
[`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md),
[`LDA()`](http://momx.github.io/Momocs/reference/LDA.md),
[`MANOVA()`](http://momx.github.io/Momocs/reference/MANOVA.md),
[`MDS()`](http://momx.github.io/Momocs/reference/MDS.md),
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md),
[`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)

## Examples

``` r
# we create a fake factor with 4 levels
bot$fac$fake <- factor(rep(letters[1:4], each=10))
bot.p <- PCA(efourier(bot, 8))
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
MANOVA_PW(bot.p, 'fake') # or MANOVA_PW(bot.p, 2)
#> PC axes 1 to 6 were retained
#> ab
#> ac
#> ad
#> bc
#> bd
#> cd
#> $stars.tab
#>   a b c  d 
#> a   - ** **
#> b     *  * 
#> c        - 
#> 
#> $summary (see also $manovas)
#>       Df Pillai approx F num Df den Df   Pr(>F)
#> a - b  1 0.1848   0.4912      6     13 0.803857
#> a - c  1 0.7785   7.6167      6     13 0.001157
#> a - d  1 0.6865   4.7449      6     13 0.009007
#> b - c  1 0.6634   4.2700      6     13 0.013537
#> b - d  1 0.5793   2.9830      6     13 0.046573
#> c - d  1 0.3489   1.1611      6     13 0.383292

# an example on open outlines
op <- PCA(npoly(olea))
#> 'nb.pts' missing and set to: 91
#> 'degree' missing and set to: 5
MANOVA_PW(op, 'domes')
#> PC axes 1 to 2 were retained
#> cultwild
#> $stars.tab
#>      cult wild
#> cult      *** 
#> 
#> $summary (see also $manovas)
#>             Df Pillai approx F num Df den Df    Pr(>F)
#> cult - wild  1 0.2721    38.69      2    207 5.315e-15
# to get the results
res <- MANOVA_PW(op, 'domes')
#> PC axes 1 to 2 were retained
#> cultwild
#> $stars.tab
#>      cult wild
#> cult      *** 
#> 
#> $summary (see also $manovas)
#>             Df Pillai approx F num Df den Df    Pr(>F)
#> cult - wild  1 0.2721    38.69      2    207 5.315e-15
res$manovas
#> $`cult - wild`
#>            Df  Pillai approx F num Df den Df    Pr(>F)    
#> fac.i       1 0.27208   38.686      2    207 5.315e-15 ***
#> Residuals 208                                             
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
res$stars.tab
#>      cult wild
#> cult      *** 
res$summary
#>             Df    Pillai approx F num Df den Df       Pr(>F)
#> cult - wild  1 0.2720825 38.68644      2    207 5.315003e-15
```
