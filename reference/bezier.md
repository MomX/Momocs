# Calculates Bezier coefficients from a shape

Calculates Bezier coefficients from a shape

## Usage

``` r
bezier(coo, n)
```

## Arguments

- coo:

  a matrix or a list of (x; y) coordinates

- n:

  the degree, by default the number of coordinates.

## Value

a list with components:

- `$J` matrix of Bezier coefficients

- `$B` matrix of Bezier vertices.

## Note

Directly borrowed for Claude (2008), and also called `bezier` there. Not
implemented for open outlines but may be useful for other purposes.

## References

Claude, J. (2008) *Morphometrics with R*, Use R! series, Springer 316
pp.

## See also

Other bezier functions:
[`bezier_i()`](http://momx.github.io/Momocs/reference/bezier_i.md)

## Examples

``` r
set.seed(34)
x <- coo_sample(efourier_shape(), 5)

plot(x, ylim=c(-3, 3), asp=1, type='b', pch=20)
b <- bezier(x)
bi <- bezier_i(b$B)
lines(bi, col='red')
```
