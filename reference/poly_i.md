# Calculates shape from a polynomial model

Returns a matrix of (x; y) coordinates when passed with a list obtained
with [opoly](http://momx.github.io/Momocs/reference/opoly.md) or
[npoly](http://momx.github.io/Momocs/reference/npoly.md).

## Usage

``` r
opoly_i(pol, nb.pts = 120, reregister = TRUE)

npoly_i(pol, nb.pts = 120, reregister = TRUE)
```

## Arguments

- pol:

  a pol list such as created by
  [npoly](http://momx.github.io/Momocs/reference/npoly.md) or
  [opoly](http://momx.github.io/Momocs/reference/opoly.md)

- nb.pts:

  the number of points to predict. By default (and cannot be higher) the
  number of points in the original shape.

- reregister:

  logical whether to reregister the shape with the original baseline.

## Value

a matrix of (x; y) coordinates.

## See also

Other polynomials:
[`npoly()`](http://momx.github.io/Momocs/reference/npoly.md),
[`opoly()`](http://momx.github.io/Momocs/reference/opoly.md)

## Examples

``` r
data(olea)
o <- olea[5]
coo_plot(o)
for (i in 2:7){
x <- opoly_i(opoly(o, i))
coo_draw(x, border=col_summer(7)[i], points=FALSE)  }
```
