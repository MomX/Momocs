# Sample a fraction of shapes

Sample a fraction of shapes from a Momocs object. See examples and
[`?dplyr::sample_n`](https://dplyr.tidyverse.org/reference/sample_n.html).

## Usage

``` r
sample_frac(tbl, size, replace, fac, ...)
```

## Arguments

- tbl:

  a Momocs object (Coo, Coe)

- size:

  numeric (0 \< numeric \<= 1) the fraction of shapes to select

- replace:

  logical whether sample should be done with ot without replacement

- fac:

  a column name if a `$fac` is defined; size is then applied within
  levels of this factor

- ...:

  additional arguments to dplyr::sample_frac and to maintain generic
  compatibility

## Value

a Momocs object of same class

## Note

the resulting fraction is rounded with
[ceiling](https://rdrr.io/r/base/Round.html).

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
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
# samples 50% of the bottles no matter their type
sample_frac(bot, 0.5)
#> Out (outlines)
#>   - 20 outlines, 159 +/- 23 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 20 × 2
#>   type   fake 
#>   <fct>  <fct>
#> 1 beer   d    
#> 2 beer   d    
#> 3 beer   d    
#> 4 whisky a    
#> 5 beer   c    
#> 6 beer   d    
#> # ℹ 14 more rows
#>   - also: $ldk
# 80% bottles of beer and of whisky
table(sample_frac(bot, 0.8, fac="type")$fac)
#>         fake
#> type     a b c d
#>   beer   0 0 8 8
#>   whisky 8 8 0 0
# bootstrap the same number of bootles of each type but with replacement
table(names(sample_frac(bot, 1, replace=TRUE)))
#> 
#>        amrut  ballantines    bushmills        caney       chimay       corona 
#>            1            1            2            1            2            1 
#>      dalmore        duvel famousgrouse franziskaner  glendronach   grimbergen 
#>            2            1            1            4            1            1 
#>      guiness highlandpark           jb      jupiler   kingfisher     latrappe 
#>            2            2            1            1            1            2 
#>     magallan         oban   oldpotrero   pecheresse sierranevada    westmalle 
#>            4            1            2            2            1            1 
#>   wildturkey       yoichi 
#>            1            1 
```
