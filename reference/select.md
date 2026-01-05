# Select columns by name

Select variables by name, from the `$fac`. Selected variables can also
be renamed on the fly. See examples and
[`?dplyr::select`](https://dplyr.tidyverse.org/reference/select.html).

## Usage

``` r
select(.data, ...)
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
[`mutate()`](http://momx.github.io/Momocs/reference/mutate.md),
[`rename()`](http://momx.github.io/Momocs/reference/rename.md),
[`rescale()`](http://momx.github.io/Momocs/reference/rescale.md),
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
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
select(olea, var, view) # drops domes and ind
#> Opn (curves)
#>   - 210 curves, 98 +/- 4 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 210 × 2
#>   var   view 
#>   <fct> <fct>
#> 1 Aglan VD   
#> 2 Aglan VL   
#> 3 Aglan VD   
#> 4 Aglan VL   
#> 5 Aglan VD   
#> 6 Aglan VL   
#> # ℹ 204 more rows
#>   - also: $ldk
select(olea, variety=var, domesticated_status=domes, view)
#> Opn (curves)
#>   - 210 curves, 98 +/- 4 coords (in $coo)
#>   - 3 classifiers (in $fac): 
#> # A tibble: 210 × 3
#>   variety domesticated_status view 
#>   <fct>   <fct>               <fct>
#> 1 Aglan   cult                VD   
#> 2 Aglan   cult                VL   
#> 3 Aglan   cult                VD   
#> 4 Aglan   cult                VL   
#> 5 Aglan   cult                VD   
#> 6 Aglan   cult                VL   
#> # ℹ 204 more rows
#>   - also: $ldk
# combine with filter with magrittr pipes
# only dorsal views, and 'var' and 'domes' columns
filter(olea, view=="VD") %>% select(var, domes)
#> Opn (curves)
#>   - 120 curves, 99 +/- 3 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 120 × 2
#>   var   domes
#>   <fct> <fct>
#> 1 Aglan cult 
#> 2 Aglan cult 
#> 3 Aglan cult 
#> 4 Aglan cult 
#> 5 Aglan cult 
#> 6 Aglan cult 
#> # ℹ 114 more rows
#>   - also: $ldk
head(olea$fac)
#> # A tibble: 6 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VL    O10  
#> 3 Aglan cult  VD    O11  
#> 4 Aglan cult  VL    O11  
#> 5 Aglan cult  VD    O12  
#> 6 Aglan cult  VL    O12  
# select some columns
select(olea, domes, view)
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 210 × 2
#>   domes view 
#>   <fct> <fct>
#> 1 cult  VD   
#> 2 cult  VL   
#> 3 cult  VD   
#> 4 cult  VL   
#> 5 cult  VD   
#> 6 cult  VL   
#> # ℹ 204 more rows
#>   - also: $ldk
# remove some columns
select(olea, -ind)
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 3 classifiers (in $fac): 
#> # A tibble: 210 × 3
#>   var   domes view 
#>   <fct> <fct> <fct>
#> 1 Aglan cult  VD   
#> 2 Aglan cult  VL   
#> 3 Aglan cult  VD   
#> 4 Aglan cult  VL   
#> 5 Aglan cult  VD   
#> 6 Aglan cult  VL   
#> # ℹ 204 more rows
#>   - also: $ldk
# rename on the fly and select some columns
select(olea, foo=domes)
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 1 classifiers (in $fac): 
#> # A tibble: 210 × 1
#>   foo  
#>   <fct>
#> 1 cult 
#> 2 cult 
#> 3 cult 
#> 4 cult 
#> 5 cult 
#> 6 cult 
#> # ℹ 204 more rows
#>   - also: $ldk
```
