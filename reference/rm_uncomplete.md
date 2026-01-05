# Remove shapes with incomplete slices

Imagine you take three views of every object you study. Then, you can
[slice](http://momx.github.io/Momocs/reference/slice.md),
[filter](http://momx.github.io/Momocs/reference/filter.md) or
[chop](http://momx.github.io/Momocs/reference/chop.md) your entire
dataset, do morphometrics on it, then want to
[combine](http://momx.github.io/Momocs/reference/combine.md) it. But if
you have forgotten one view, or if it was impossible to obtain, for one
or more objects, combine will not work. This function helps you to
remove those ugly ducklings. See examples

## Usage

``` r
rm_uncomplete(x, id, by)
```

## Arguments

- x:

  the object on which to remove uncomplete "by"

- id:

  of the objects, within the \$fac slot

- by:

  which column of the \$fac should objects have complete views

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
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
# we load olea
data(olea)
# we select the var Aglan since it is the only one complete
ol <- filter(olea, var == "Aglan")
# everything seems fine
table(ol$view, ol$ind)
#>     
#>      O1 O10 O11 O12 O13 O14 O15 O16 O17 O18 O19 O2 O20 O21 O22 O23 O24 O25 O26
#>   VD  1   1   1   1   1   1   1   1   1   1   1  1   1   1   1   1   1   1   1
#>   VL  1   1   1   1   1   1   1   1   1   1   1  1   1   1   1   1   1   1   1
#>     
#>      O27 O28 O29 O3 O30 O4 O5 O6 O7 O8 O9
#>   VD   1   1   1  1   1  1  1  1  1  1  1
#>   VL   1   1   1  1   1  1  1  1  1  1  1
# indeed
rm_uncomplete(ol, id="ind", by="view")
#> all ids have 2 slices
#> Opn (curves)
#>   - 60 curves, 98 +/- 4 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 60 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VL    O10  
#> 3 Aglan cult  VD    O11  
#> 4 Aglan cult  VL    O11  
#> 5 Aglan cult  VD    O12  
#> 6 Aglan cult  VL    O12  
#> # ℹ 54 more rows
#>   - also: $ldk

# we mess the ol object by removing a single shape
ol.pb <- slice(ol, -1)
table(ol.pb$view, ol.pb$ind)
#>     
#>      O1 O10 O11 O12 O13 O14 O15 O16 O17 O18 O19 O2 O20 O21 O22 O23 O24 O25 O26
#>   VD  1   0   1   1   1   1   1   1   1   1   1  1   1   1   1   1   1   1   1
#>   VL  1   1   1   1   1   1   1   1   1   1   1  1   1   1   1   1   1   1   1
#>     
#>      O27 O28 O29 O3 O30 O4 O5 O6 O7 O8 O9
#>   VD   1   1   1  1   1  1  1  1  1  1  1
#>   VL   1   1   1  1   1  1  1  1  1  1  1
# the counterpart has been removed with a notice
ol.ok <- rm_uncomplete(ol.pb, "ind", "view")
#> those shapes did not have 2 slices and has been removed: O10
# now you can combine them
table(ol.ok$view, ol.ok$ind)
#>     
#>      O1 O10 O11 O12 O13 O14 O15 O16 O17 O18 O19 O2 O20 O21 O22 O23 O24 O25 O26
#>   VD  0   0   0   0   0   0   0   0   0   0   0  0   0   0   0   0   0   0   0
#>   VL  0   0   0   0   0   0   0   0   0   0   0  0   0   0   0   0   0   0   0
#>     
#>      O27 O28 O29 O3 O30 O4 O5 O6 O7 O8 O9
#>   VD   0   0   0  0   0  0  0  0  0  0  0
#>   VL   0   0   0  0   0  0  0  0  0  0  0
```
