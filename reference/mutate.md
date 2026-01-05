# Add new variables

Add new variables to the `$fac`. See examples and
[`?dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
mutate(.data, ...)
```

## Arguments

- .data:

  a `Coo`, `Coe`, `PCA` object

- ...:

  comma separated list of unquoted expressions

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
[`rename()`](http://momx.github.io/Momocs/reference/rename.md),
[`rescale()`](http://momx.github.io/Momocs/reference/rescale.md),
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
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
olea
#> Opn (curves)
#>   - 210 curves, 99 +/- 3 coords (in $coo)
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
mutate(olea, id=factor(1:length(olea)))
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 5 classifiers (in $fac): 
#> # A tibble: 210 × 5
#>   var   domes view  ind   id   
#>   <fct> <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10   1    
#> 2 Aglan cult  VL    O10   2    
#> 3 Aglan cult  VD    O11   3    
#> 4 Aglan cult  VL    O11   4    
#> 5 Aglan cult  VD    O12   5    
#> 6 Aglan cult  VL    O12   6    
#> # ℹ 204 more rows
#>   - also: $ldk
```
