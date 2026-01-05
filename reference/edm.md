# Calculates euclidean distance every pairs of points in two matrices.

`edm` returns the euclidean distances between points \\1 -\> n\\ of two
2-col matrices of the same dimension. This function is used internally
but may be of interest for other analyses.

## Usage

``` r
edm(m1, m2)
```

## Arguments

- m1:

  The first `matrix` of coordinates.

- m2:

  The second `matrix` of coordinates.

## Value

Returns a `vector` of euclidean distances between pairwise coordinates
in the two matrices.

## Details

If one wishes to align two (or more shapes) Procrustes surimposition may
provide a better solution.

## See also

[ed](http://momx.github.io/Momocs/reference/ed.md),
[edm_nearest](http://momx.github.io/Momocs/reference/edm_nearest.md),
[dist](https://rdrr.io/r/stats/dist.html).

## Examples

``` r
x <- matrix(1:10, nc=2)
edm(x, x)
#> [1] 0 0 0 0 0
edm(x, x+1)
#> [1] 1.414214 1.414214 1.414214 1.414214 1.414214
```
