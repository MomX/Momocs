# Calculates the eccentricity of a shape

`coo_eccentricityeigen` uses the ratio of the eigen values (inertia axes
of coordinates). `coo_eccentricityboundingbox` uses the width/length
ratio (see [coo_lw](http://momx.github.io/Momocs/reference/coo_lw.md)).

## Usage

``` r
coo_eccentricityeigen(coo)

# Default S3 method
coo_eccentricityeigen(coo)

# S3 method for class 'Coo'
coo_eccentricityeigen(coo)

coo_eccentricityboundingbox(coo)

# Default S3 method
coo_eccentricityboundingbox(coo)

# S3 method for class 'Coo'
coo_eccentricityboundingbox(coo)
```

## Source

Rosin PL. 2005. Computing global shape measures. Handbook of Pattern
Recognition and Computer Vision. 177-196.

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

## Value

`numeric` for single shapes, `list` for `Coo`.

## See also

coo_eccentricityboundingbox

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md),
[`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md),
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
# coo_eccentricityeigen
bot[1] %>% coo_eccentricityeigen()
#> [1] 0.09292547
bot %>%
    slice(1:3) %>% # for speed sake only
    coo_eccentricityeigen()
#> $brahma
#> [1] 0.09292547
#> 
#> $caney
#> [1] 0.100634
#> 
#> $chimay
#> [1] 0.1813198
#> 

# coo_eccentricityboundingbox
bot[1] %>% coo_eccentricityboundingbox()
#> [1] 0.2555899
bot %>%
    slice(1:3) %>% # for speed sake only
    coo_eccentricityboundingbox()
#> $brahma
#> [1] 0.2555899
#> 
#> $caney
#> [1] 0.2617262
#> 
#> $chimay
#> [1] 0.3744498
#> 
```
