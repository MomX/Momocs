# Calculates the Haralick's circularity of a shape

`coo_circularity` calculates the 'circularity measure'. Also called
'compactness' and 'shape factor' sometimes. `coo_circularityharalick`
calculates Haralick's circularity which is less sensible to
digitalization noise than `coo_circularity`. `coo_circularitynorm`
calculates 'circularity', also called compactness and shape factor, but
normalized to the unit circle.

## Usage

``` r
coo_circularity(coo)

# Default S3 method
coo_circularity(coo)

# S3 method for class 'Coo'
coo_circularity(coo)

coo_circularityharalick(coo)

# Default S3 method
coo_circularityharalick(coo)

# S3 method for class 'Coo'
coo_circularityharalick(coo)

coo_circularitynorm(coo)

# Default S3 method
coo_circularitynorm(coo)

# S3 method for class 'Coo'
coo_circularitynorm(coo)
```

## Source

Rosin PL. 2005. Computing global shape measures. Handbook of Pattern
Recognition and Computer Vision. 177-196.

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

## Value

`numeric` for single shapes, `list` for `Coo` of the corresponding
circularity measurement.

## See also

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md),
[`coo_eccentricity`](http://momx.github.io/Momocs/reference/coo_eccentricity.md),
[`coo_elongation()`](http://momx.github.io/Momocs/reference/coo_elongation.md),
[`coo_length()`](http://momx.github.io/Momocs/reference/coo_length.md),
[`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md),
[`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md),
[`coo_rectilinearity()`](http://momx.github.io/Momocs/reference/coo_rectilinearity.md),
[`coo_scalars()`](http://momx.github.io/Momocs/reference/coo_scalars.md),
[`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md),
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
# coo_circularity
bot[1] %>% coo_circularity()
#> [1] 26.26463
bot %>%
    slice(1:5) %>% # for speed sake only
    coo_circularity
#> $brahma
#> [1] 26.26463
#> 
#> $caney
#> [1] 25.60553
#> 
#> $chimay
#> [1] 20.83278
#> 
#> $corona
#> [1] 27.61134
#> 
#> $deusventrue
#> [1] 25.75573
#> 

# coo_circularityharalick
bot[1] %>% coo_circularityharalick()
#> [1] 2.320493
bot %>%
    slice(1:5) %>% # for speed sake only
    coo_circularityharalick
#> $brahma
#> [1] 2.320493
#> 
#> $caney
#> [1] 2.374045
#> 
#> $chimay
#> [1] 2.935174
#> 
#> $corona
#> [1] 2.261573
#> 
#> $deusventrue
#> [1] 2.397828
#> 

# coo_circularitynorm
bot[1] %>% coo_circularitynorm()
#> [1] 2.090073
bot %>%
    slice(1:5) %>% # for speed sake only
    coo_circularitynorm
#> $brahma
#> [1] 2.090073
#> 
#> $caney
#> [1] 2.037623
#> 
#> $chimay
#> [1] 1.65782
#> 
#> $corona
#> [1] 2.197241
#> 
#> $deusventrue
#> [1] 2.049576
#> 
```
