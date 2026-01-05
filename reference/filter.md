# Subset based on conditions

Return shapes with matching conditions, from the `$fac`. See examples
and
[`?dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).

## Usage

``` r
filter(.data, ...)
```

## Arguments

- .data:

  a `Coo`, `Coe`, `PCA` object

- ...:

  logical conditions

## Value

a Momocs object of the same class.

## Details

dplyr verbs are maintained. You should probbaly not filter on PCA
objects. The latter are calculated using all individuals and filtering
may lead to false conclusions. If you want to highlith some individuals,
see examples in
[plot_PCA](http://momx.github.io/Momocs/reference/plot_PCA.md).

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
[`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md),
[`fac_dispatcher()`](http://momx.github.io/Momocs/reference/fac_dispatcher.md),
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
# we retain on dorsal views
filter(olea, view=="VD")
#> Opn (curves)
#>   - 120 curves, 100 +/- 2 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 120 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VD    O11  
#> 3 Aglan cult  VD    O12  
#> 4 Aglan cult  VD    O13  
#> 5 Aglan cult  VD    O14  
#> 6 Aglan cult  VD    O15  
#> # ℹ 114 more rows
#>   - also: $ldk
# only dorsal views and Aglan+PicMa varieties
filter(olea, view=="VD", var %in% c("Aglan", "PicMa"))
#> Opn (curves)
#>   - 60 curves, 100 +/- 2 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 60 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VD    O11  
#> 3 Aglan cult  VD    O12  
#> 4 Aglan cult  VD    O13  
#> 5 Aglan cult  VD    O14  
#> 6 Aglan cult  VD    O15  
#> # ℹ 54 more rows
#>   - also: $ldk
# we create an id column and retain the 120 first shapes
olea %>% mutate(id=1:length(olea)) %>% filter(id > 120)
#> Opn (curves)
#>   - 90 curves, 99 +/- 4 coords (in $coo)
#>   - 5 classifiers (in $fac): 
#> # A tibble: 90 × 5
#>   var   domes view  ind      id
#>   <fct> <fct> <fct> <fct> <int>
#> 1 PicMa cult  VD    O24     121
#> 2 PicMa cult  VL    O24     122
#> 3 PicMa cult  VD    O25     123
#> 4 PicMa cult  VL    O25     124
#> 5 PicMa cult  VD    O26     125
#> 6 PicMa cult  VL    O26     126
#> # ℹ 84 more rows
#>   - also: $ldk
```
