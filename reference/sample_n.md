# Sample n shapes

Sample n shapes from a Momocs object. See examples and
[`?dplyr::sample_n`](https://dplyr.tidyverse.org/reference/sample_n.html).

## Usage

``` r
sample_n(tbl, size, replace, fac, ...)
```

## Arguments

- tbl:

  a Momocs object (Coo, Coe)

- size:

  numeric how many shapes should we sample

- replace:

  logical whether sample should be done with ot without replacement

- fac:

  a column name if a `$fac` is defined; size is then applied within
  levels of this factor

- ...:

  additional arguments to dplyr::sample_n and to maintain generic
  compatibility

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
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
# samples 5 bottles no matter their type
sample_n(bot, 5)
#> Out (outlines)
#>   - 5 outlines, 162 +/- 30 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 5 × 2
#>   type   fake 
#>   <fct>  <fct>
#> 1 whisky b    
#> 2 beer   d    
#> 3 beer   d    
#> 4 whisky a    
#> 5 whisky b    
#>   - also: $ldk
# 5 bottles of beer and of whisky
table(sample_n(bot, 5, fac="type")$type)
#> 
#>   beer whisky 
#>      5      5 
# many repetitions
table(names(sample_n(bot, 400, replace=TRUE)))
#> 
#>          amrut    ballantines         brahma      bushmills          caney 
#>              6              7              6             11             10 
#>         chimay         chivas         corona        dalmore    deusventrue 
#>             13             16             14             17             10 
#>          duvel   famousgrouse   franziskaner    glendronach   glenmorangie 
#>             14             15              8             12             12 
#>     grimbergen        guiness   highlandpark     hoegardeen    jackdaniels 
#>              7              5              8             10              7 
#>             jb  johnniewalker        jupiler     kingfisher       latrappe 
#>              7             14              9             13             10 
#> lindemanskriek       magallan     makersmark    nicechouffe           oban 
#>             11             11             11              9              8 
#>     oldpotrero     pecheresse      redbreast   sierranevada         tamdhu 
#>             10             10             14              8             10 
#>     tanglefoot          tauro      westmalle     wildturkey         yoichi 
#>              7              6              8             11              5 
```
