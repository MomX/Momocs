# Subset based on positions

Select rows by position, based on `$fac`. See examples and
[`?dplyr::slice`](https://dplyr.tidyverse.org/reference/slice.html).

## Usage

``` r
slice(.data, ...)
```

## Arguments

- .data:

  a `Coo`, `Coe`, `PCA` object

- ...:

  logical conditions

## Value

a Momocs object of the same class.

## Details

dplyr verbs are maintained.

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
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
olea
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 210 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VL    O10  
#> 3 Aglan cult  VD    O11  
#> 4 Aglan cult  VL    O11  
#> 5 Aglan cult  VD    O12  
#> 6 Aglan cult  VL    O12  
#> # ℹ 204 more rows
#>   - also: $ldk
slice(olea, 1) # if you only want the coordinates, try bot[1]
#> Opn (curves)
#>   - 1 curves, 99 +/- NA coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 1 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#>   - also: $ldk
slice(olea, 1:20)
#> Opn (curves)
#>   - 20 curves, 99 +/- 4 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 20 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VL    O10  
#> 3 Aglan cult  VD    O11  
#> 4 Aglan cult  VL    O11  
#> 5 Aglan cult  VD    O12  
#> 6 Aglan cult  VL    O12  
#> # ℹ 14 more rows
#>   - also: $ldk
slice(olea, 21:30)
#> Opn (curves)
#>   - 10 curves, 100 +/- 4 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 10 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O1   
#> 2 Aglan cult  VL    O1   
#> 3 Aglan cult  VD    O20  
#> 4 Aglan cult  VL    O20  
#> 5 Aglan cult  VD    O21  
#> 6 Aglan cult  VL    O21  
#> # ℹ 4 more rows
#>   - also: $ldk
```
