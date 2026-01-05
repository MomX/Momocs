# Calculates the rectilinearity of a shape

As proposed by Zunic and Rosin (see below). May need some
testing/review.

## Usage

``` r
coo_rectilinearity(coo)
```

## Source

Zunic J, Rosin PL. 2003. Rectilinearity measurements for polygons. IEEE
Transactions on Pattern Analysis and Machine Intelligence 25: 1193-1200.

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

## Value

`numeric` for a single shape, `list` for `Coo`

## Note

due to the laborious nature of the algorithm (in nb.pts^2), and of its
implementation, it may be very long to compute.

## See also

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md),
[`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md),
[`coo_eccentricity`](http://momx.github.io/Momocs/reference/coo_eccentricity.md),
[`coo_elongation()`](http://momx.github.io/Momocs/reference/coo_elongation.md),
[`coo_length()`](http://momx.github.io/Momocs/reference/coo_length.md),
[`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md),
[`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md),
[`coo_scalars()`](http://momx.github.io/Momocs/reference/coo_scalars.md),
[`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md),
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
bot[1] %>%
    coo_sample(32) %>% # for speed sake only
    coo_rectilinearity
#> [1] 0.3539899

bot %>%
    slice(1:3) %>% coo_sample(32) %>% # for speed sake only
    coo_rectilinearity
#> $brahma
#> [1] 0.3539899
#> 
#> $caney
#> [1] 0.3751378
#> 
#> $chimay
#> [1] 0.3597856
#> 
```
