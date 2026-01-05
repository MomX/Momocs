# Calculates the convexity of a shape

Calculated using a ratio of the eigen values (inertia axis)

## Usage

``` r
coo_convexity(coo)
```

## Source

Rosin PL. 2005. Computing global shape measures. Handbook of Pattern
Recognition and Computer Vision. 177-196.

## Arguments

- coo:

  a `matrix` of (x; y) coordinates.

## Value

`numeric` for a single shape, `list` for a `Coo`

## See also

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md),
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
coo_convexity(bot[1])
#> [1] 0.8003675
bot %>%
    slice(1:3) %>% # for speed sake only
    coo_convexity()
#> $brahma
#> [1] 0.8003675
#> 
#> $caney
#> [1] 0.9409434
#> 
#> $chimay
#> [1] 0.9454935
#> 
```
