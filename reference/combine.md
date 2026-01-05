# Combine several objects

Combine `Coo` objects after a slicing, either manual or using
[slice](http://momx.github.io/Momocs/reference/slice.md) or
[chop](http://momx.github.io/Momocs/reference/chop.md). Note that on Coo
object, it combines row-wise (ie, merges shapes as a `c` would do) ; but
on Coe it combines column-wise (merges coefficients). In the latter
case, Coe must have the same number of shapes (not necessarily the same
number of coefficients). Also the `$fac` of the first Coe is retrieved.
A separate version may come at some point.

## Usage

``` r
combine(...)
```

## Arguments

- ...:

  a list of Out(Coe), Opn(Coe), Ldk objects (but of the same class)

## Value

a Momocs object of same class

## Note

Note that the order of shapes or their coefficients is not checked, so
anything with the same number of rows will be merged.

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
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
w <- filter(bot, type=="whisky")
b <- filter(bot, type=="beer")
combine(w, b)
#> Out (outlines)
#>   - 40 outlines, 162 +/- 21 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 40 × 2
#>   type   fake 
#>   <fct>  <fct>
#> 1 whisky a    
#> 2 whisky a    
#> 3 whisky a    
#> 4 whisky a    
#> 5 whisky a    
#> 6 whisky a    
#> # ℹ 34 more rows
#>   - also: $ldk
# or, if you have many levels
bot_s <- chop(bot, ~type)
bot_s$whisky
#> Out (outlines)
#>   - 20 outlines, 158 +/- 23 coords (in $coo)
#>   - 2 classifiers (in $fac): 
#> # A tibble: 20 × 2
#>   type   fake 
#>   <fct>  <fct>
#> 1 whisky a    
#> 2 whisky a    
#> 3 whisky a    
#> 4 whisky a    
#> 5 whisky a    
#> 6 whisky a    
#> # ℹ 14 more rows
#>   - also: $ldk
# note that you can apply something (single function or a more
# complex pipe) then combine everyone, since combine also works on lists
# eg:
# bot_s2 <- efourier(bot_s, 10) # equivalent to lapply(bot_s, efourier, 10)
# bot_sf <- combine(bot_s2)

# pipe style
efourier(bot_s, 10) %>% combine()
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> An OutCoe object [ combined: efourier + efourier analyses ]
#> --------------------
#>  - $coe: 20 outlines described, and (total) 80 coefficients
#> # A tibble: 20 × 2
#>   type  fake 
#>   <fct> <fct>
#> 1 beer  c    
#> 2 beer  c    
#> 3 beer  c    
#> 4 beer  c    
#> 5 beer  c    
#> 6 beer  c    
#> # ℹ 14 more rows
```
