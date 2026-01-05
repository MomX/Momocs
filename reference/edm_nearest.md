# Calculates the shortest euclidean distance found for every point of one matrix among those of a second.

`edm_nearest` calculates the shortest euclidean distance found for every
point of one matrix among those of a second. In other words, if `m1, m2`
have `n` rows, the result will be the shortest distance for the first
point of `m1` to any point of `m2` and so on, `n` times. This function
is used internally but may be of interest for other analyses.

## Usage

``` r
edm_nearest(m1, m2, full = FALSE)
```

## Arguments

- m1:

  The first `list` or `matrix` of coordinates.

- m2:

  The second `list` or `matrix` of coordinates.

- full:

  `logical`. Whether to returns a condensed version of the results.

## Value

If `full` is `TRUE`, returns a `list` with two components: `d` which is
for every point of `m1` the shortest distance found between it and any
point in `m2`, and `pos` the (`m2`) row indices of these points.
Otherwise returns `d` as a numeric vector of the shortest distances.

## Details

So far this function is quite time consumming since it performs \\ n
\times n \\ euclidean distance computation. If one wishes to align two
(or more shapes) Procrustes surimposition may provide a better solution.

## See also

[ed](http://momx.github.io/Momocs/reference/ed.md),
[edm](http://momx.github.io/Momocs/reference/edm.md),
[dist](https://rdrr.io/r/stats/dist.html).

## Examples

``` r
x <- matrix(1:10, nc=2)
edm_nearest(x, x+rnorm(10))
#> [1] 1.3227275 0.9962565 1.2300098 1.8592129 1.2580101
edm_nearest(x, x+rnorm(10), full=TRUE)
#> $d
#> [1] 0.7887792 0.5191523 1.2266653 0.8739136 0.3660873
#> 
#> $pos
#> [1] 2 1 3 4 5
#> 
```
