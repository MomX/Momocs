# Calculates a shape from Bezier coefficients

Calculates a shape from Bezier coefficients

## Usage

``` r
bezier_i(B, nb.pts = 120)
```

## Arguments

- B:

  a matrix of Bezier vertices, such as those produced by
  [bezier](http://momx.github.io/Momocs/reference/bezier.md)

- nb.pts:

  the number of points to sample along the curve.

## Value

a matrix of (x; y) coordinates

## Note

Directly borrowed for Claude (2008), and called `beziercurve` there. Not
implemented for open outlines but may be useful for other purposes.

## References

Claude, J. (2008) *Morphometrics with R*, Use R! series, Springer 316
pp.

## See also

Other bezier functions:
[`bezier()`](http://momx.github.io/Momocs/reference/bezier.md)

## Examples

``` r
set.seed(34)
x <- coo_sample(efourier_shape(), 5)

plot(x, ylim=c(-3, 3), asp=1, type='b', pch=20)
b <- bezier(x)
bi <- bezier_i(b$B)
lines(bi, col='red')
```
