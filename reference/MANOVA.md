# Multivariate analysis of (co)variance on Coe objects

Performs multivariate analysis of variance on
[PCA](http://momx.github.io/Momocs/reference/PCA.md) objects.

## Usage

``` r
MANOVA(x, fac, test = "Hotelling", retain, drop)

# S3 method for class 'OpnCoe'
MANOVA(x, fac, test = "Hotelling", retain, drop)

# S3 method for class 'OutCoe'
MANOVA(x, fac, test = "Hotelling", retain, drop)

# S3 method for class 'PCA'
MANOVA(x, fac, test = "Hotelling", retain = 0.99, drop)
```

## Arguments

- x:

  a [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

- fac:

  a name of a colum in the `$fac` slot, or its id, or a formula

- test:

  a test for [manova](https://rdrr.io/r/stats/manova.html)
  (`'Hotelling'` by default)

- retain:

  how many harmonics (or polynomials) to retain, for PCA the highest
  number of PC axis to retain, or the proportion of the variance to
  capture.

- drop:

  how many harmonics (or polynomials) to drop

## Value

a list of matrices of (x,y) coordinates.

## Details

Performs a MANOVA/MANCOVA on PC scores. Just a wrapper around
[manova](https://rdrr.io/r/stats/manova.html). See examples for
multifactorial manova and
[summary.manova](https://rdrr.io/r/stats/summary.manova.html) for more
details and examples.

## Note

Needs a review and should be considered as experimental. Silent message
and progress bars (if any) with `options("verbose"=FALSE)`.

## See also

Other multivariate:
[`CLUST()`](http://momx.github.io/Momocs/reference/CLUST.md),
[`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md),
[`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md),
[`LDA()`](http://momx.github.io/Momocs/reference/LDA.md),
[`MANOVA_PW()`](http://momx.github.io/Momocs/reference/MANOVA_PW.md),
[`MDS()`](http://momx.github.io/Momocs/reference/MDS.md),
[`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md),
[`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md),
[`PCA()`](http://momx.github.io/Momocs/reference/PCA.md),
[`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)

## Examples

``` r
# MANOVA
bot.p <- PCA(efourier(bot, 12))
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
MANOVA(bot.p, 'type')
#> PC axes 1 to 7 were retained
#>           Df Hotelling-Lawley approx F num Df den Df    Pr(>F)    
#> fac        1           2.7631   12.631      7     32 1.202e-07 ***
#> Residuals 38                                                      
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

op <- PCA(npoly(olea, 5))
#> 'nb.pts' missing and set to: 91
MANOVA(op, 'domes')
#> PC axes 1 to 2 were retained
#>            Df Hotelling-Lawley approx F num Df den Df    Pr(>F)    
#> fac         1          0.37378   38.686      2    207 5.315e-15 ***
#> Residuals 208                                                      
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 m <- manova(op$x[, 1:5] ~  op$fac$domes * op$fac$var)
 summary(m)
#>               Df  Pillai approx F num Df den Df    Pr(>F)    
#> op$fac$domes   1 0.38594  25.3915      5    202 < 2.2e-16 ***
#> op$fac$var     2 0.34192   8.3723     10    406 2.069e-12 ***
#> Residuals    206                                             
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 summary.aov(m)
#>  Response PC1 :
#>               Df  Sum Sq Mean Sq F value  Pr(>F)   
#> op$fac$domes   1   11.79  11.790  1.2623 0.26251   
#> op$fac$var     2  109.81  54.903  5.8784 0.00329 **
#> Residuals    206 1924.02   9.340                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>  Response PC2 :
#>               Df  Sum Sq Mean Sq F value    Pr(>F)    
#> op$fac$domes   1  62.486  62.486  93.511 < 2.2e-16 ***
#> op$fac$var     2  34.489  17.244  25.806  9.97e-11 ***
#> Residuals    206 137.654   0.668                      
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>  Response PC3 :
#>               Df  Sum Sq  Mean Sq F value  Pr(>F)   
#> op$fac$domes   1  0.2345 0.234541  3.9476 0.04826 * 
#> op$fac$var     2  0.5998 0.299918  5.0479 0.00724 **
#> Residuals    206 12.2393 0.059414                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>  Response PC4 :
#>               Df  Sum Sq  Mean Sq F value   Pr(>F)   
#> op$fac$domes   1 0.12558 0.125582  8.5246 0.003894 **
#> op$fac$var     2 0.08698 0.043490  2.9521 0.054442 . 
#> Residuals    206 3.03476 0.014732                    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>  Response PC5 :
#>               Df   Sum Sq    Mean Sq F value Pr(>F)
#> op$fac$domes   1 0.000140 0.00014009  0.7838 0.3770
#> op$fac$var     2 0.000299 0.00014964  0.8372 0.4344
#> Residuals    206 0.036819 0.00017873               
#> 

 # MANCOVA example
 # we create a numeric variable, based on centroid size
 bot %<>% mutate(cs=coo_centsize(.))
 # same pipe
 bot %>% efourier %>% PCA %>% MANOVA("cs")
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
#> PC axes 1 to 7 were retained
#>           Df Hotelling-Lawley approx F num Df den Df Pr(>F)
#> fac        1          0.38135   1.7433      7     32 0.1341
#> Residuals 38                                               
```
