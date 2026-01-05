# Retain groups with at least n shapes

Examples are self-speaking.

## Usage

``` r
at_least(x, fac, N)
```

## Arguments

- x:

  any Momocs object

- fac:

  the id of name of the \$fac column

- N:

  minimal number of individuals to retain the group

## Value

a Momocs object of same class

## Note

if N is too ambitious the original object is returned with a message

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
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
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
table(trilo$onto)
#> 
#>  a  b  c  d 
#>  7 16 18  9 
at_least(trilo, "onto", 9)
#> Out (outlines)
#>   - 43 outlines, 64 +/- 0 coords (in $coo)
#>   - 1 classifiers (in $fac): 
#> # A tibble: 43 × 1
#>   onto 
#>   <fct>
#> 1 b    
#> 2 b    
#> 3 b    
#> 4 b    
#> 5 b    
#> 6 b    
#> # ℹ 37 more rows
#>   - also: $ldk
at_least(trilo, "onto", 16)
#> Out (outlines)
#>   - 34 outlines, 64 +/- 0 coords (in $coo)
#>   - 1 classifiers (in $fac): 
#> # A tibble: 34 × 1
#>   onto 
#>   <fct>
#> 1 b    
#> 2 b    
#> 3 b    
#> 4 b    
#> 5 b    
#> 6 b    
#> # ℹ 28 more rows
#>   - also: $ldk
at_least(trilo, "onto", 2000) # too ambitious !
#> no group with at least 2000 indidivuals
#> empty Out
```
