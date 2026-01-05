# Calculates harmonic power given a list from e/t/rfourier

Given a list with `an, bn (and eventually cn and dn)`, returns the
harmonic power.

## Usage

``` r
harm_pow(xf)
```

## Arguments

- xf:

  A list with an, bn (and cn, dn) components, typically from a
  e/r/tfourier passed on coo\_

## Value

Returns a `vector` of harmonic power

## Examples

``` r
ef <- efourier(bot[1], 24)
rf <- efourier(bot[1], 24)
harm_pow(ef)
#>           H1           H2           H3           H4           H5           H6 
#> 1.299790e+05 2.593195e+02 1.376114e+03 1.792188e+02 2.594795e+02 2.675959e+01 
#>           H7           H8           H9          H10          H11          H12 
#> 3.103764e+01 3.422527e+00 6.584799e-01 8.255293e+00 3.010409e+00 6.433551e+00 
#>          H13          H14          H15          H16          H17          H18 
#> 1.082413e+00 1.171377e+00 1.929022e-01 4.769194e-01 2.059250e-01 1.971375e-01 
#>          H19          H20          H21          H22          H23          H24 
#> 1.099667e-01 1.586647e-01 4.222544e-02 1.447763e-01 5.618937e-02 1.570677e-01 
harm_pow(rf)
#>           H1           H2           H3           H4           H5           H6 
#> 1.299790e+05 2.593195e+02 1.376114e+03 1.792188e+02 2.594795e+02 2.675959e+01 
#>           H7           H8           H9          H10          H11          H12 
#> 3.103764e+01 3.422527e+00 6.584799e-01 8.255293e+00 3.010409e+00 6.433551e+00 
#>          H13          H14          H15          H16          H17          H18 
#> 1.082413e+00 1.171377e+00 1.929022e-01 4.769194e-01 2.059250e-01 1.971375e-01 
#>          H19          H20          H21          H22          H23          H24 
#> 1.099667e-01 1.586647e-01 4.222544e-02 1.447763e-01 5.618937e-02 1.570677e-01 

plot(cumsum(harm_pow(ef)[-1]), type='o',
  main='Cumulated harmonic power without the first harmonic',
  ylab='Cumulated harmonic power', xlab='Harmonic rank')

```
