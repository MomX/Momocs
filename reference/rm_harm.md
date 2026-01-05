# Removes harmonics from Coe objects

Useful to drop harmonics on Coe objects. Should only work for
Fourier-based approached since it looks for `[A-D][1-drop]` pattern.

## Usage

``` r
rm_harm(x, drop = 1)
```

## Arguments

- x:

  Coe object

- drop:

  numeric number of harmonics to drop

## Value

a Momocs object of same class

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
[`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md),
[`fac_dispatcher()`](http://momx.github.io/Momocs/reference/fac_dispatcher.md),
[`filter()`](http://momx.github.io/Momocs/reference/filter.md),
[`mutate()`](http://momx.github.io/Momocs/reference/mutate.md),
[`rename()`](http://momx.github.io/Momocs/reference/rename.md),
[`rescale()`](http://momx.github.io/Momocs/reference/rescale.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
data(bot)
bf <- efourier(bot)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'nb.h' set to 10 (99% harmonic power)
colnames(rm_harm(bf, 1)$coe)
#>  [1] "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9" "B2" "B3" "B4" "B5" "B6" "B7" "B8"
#> [16] "B9" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" "D2" "D3" "D4" "D5" "D6" "D7"
#> [31] "D8" "D9"
```
