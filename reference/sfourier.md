# Radii variation Fourier transform (equally spaced curvilinear abscissa)

`sfourier` computes radii variation Fourier analysis from a matrix or a
list of coordinates where points are equally spaced aong the curvilinear
abscissa.

## Usage

``` r
sfourier(x, nb.h)

# Default S3 method
sfourier(x, nb.h)

# S3 method for class 'Out'
sfourier(x, nb.h)

# S3 method for class 'list'
sfourier(x, nb.h)
```

## Arguments

- x:

  A `list` or `matrix` of coordinates or an `Out` object

- nb.h:

  `integer`. The number of harmonics to use. If missing, 12 is used on
  shapes; 99 percent of harmonic power on Out objects, both with
  messages.

## Value

A list with following components:

- `an` vector of \\a\_{1-\>n}\\ harmonic coefficients

- `bn` vector of \\b\_{1-\>n}\\ harmonic coefficients

- `ao` ao harmonic coefficient

- `r` vector of radii lengths

## Note

The implementation is still quite experimental (as of Dec. 2016)

## References

Renaud S, Michaux JR (2003): Adaptive latitudinal trends in the mandible
shape of *Apodemus* wood mice. *J Biogeogr* 30:1617-1628.

## See also

Other sfourier:
[`sfourier_i()`](http://momx.github.io/Momocs/reference/sfourier_i.md),
[`sfourier_shape()`](http://momx.github.io/Momocs/reference/sfourier_shape.md)

## Examples

``` r
molars[4] %>%
coo_center %>% coo_scale %>% coo_interpolate(1080) %>%
coo_slidedirection("right") %>%
   coo_sample(360) %T>% coo_plot(zoom=2) %>%
   sfourier(16) %>%
   sfourier_i() %>%
   coo_draw(bor="red", points=TRUE)
```
