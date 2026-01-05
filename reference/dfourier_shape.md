# Calculates and draws 'dfourier' shapes

Calculates shapes based on 'Discrete cosine transforms' given harmonic
coefficients (see
[dfourier](http://momx.github.io/Momocs/reference/dfourier.md)) or can
generate some random 'dfourier' shapes. Mainly intended to generate
shapes and/or to understand how dfourier works.

## Usage

``` r
dfourier_shape(A, B, nb.h, nb.pts = 60, alpha = 2, plot = TRUE)
```

## Arguments

- A:

  vector of harmonic coefficients

- B:

  vector of harmonic coefficients

- nb.h:

  if `A` and/or `B` are not provided, the number of harmonics to
  generate

- nb.pts:

  if `A` and/or `B` are not provided, the number of points to use to
  reconstruct the shapes

- alpha:

  The power coefficient associated with the (usually decreasing)
  amplitude of the harmonic coefficients (see
  [efourier_shape](http://momx.github.io/Momocs/reference/efourier_shape.md))

- plot:

  logical whether to plot the shape

## Value

a list of shapes or a plot

## See also

Other dfourier:
[`dfourier()`](http://momx.github.io/Momocs/reference/dfourier.md),
[`dfourier_i()`](http://momx.github.io/Momocs/reference/dfourier_i.md)

## Examples

``` r
# some signatures
panel(coo_align(Opn(replicate(48, dfourier_shape(alpha=0.5, nb.h=6)))))

















































# some worms
panel(coo_align(Opn(replicate(48, dfourier_shape(alpha=2, nb.h=6)))))
















































```
