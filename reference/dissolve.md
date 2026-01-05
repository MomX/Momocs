# Dissolve Coe objects

the opposite of combine, typically used after it. Note that the `$fac`
slot may be wrong since combine...well combines... this `$fac`. See
examples.

## Usage

``` r
dissolve(x, retain)
```

## Arguments

- x:

  a Coe object

- retain:

  the partition id to retain. Or their name if the partitions are named
  (see x\$method) eg after a chop

## Value

a Momocs object of same class

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
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
data(bot)
w <- filter(bot, type=="whisky")
b <- filter(bot, type=="beer")
wf <- efourier(w, 10)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bf <- efourier(b, 10)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
wbf <- combine(wf, bf)
dissolve(wbf, 1)
#> An OutCoe object [ elliptical Fourier analysis ]
#> --------------------
#>  - $coe: 20 outlines described, 10 harmonics
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
dissolve(wbf, 2)
#> An OutCoe object [ elliptical Fourier analysis ]
#> --------------------
#>  - $coe: 20 outlines described, 10 harmonics
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

# or using chop (yet combine here makes no sense)
bw <- bot %>% chop(~type) %>% lapply(efourier, 10) %>% combine
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bw %>% dissolve(1)
#> An OutCoe object [ elliptical Fourier analysis ]
#> --------------------
#>  - $coe: 20 outlines described, 10 harmonics
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
bw %>% dissolve(2)
#> An OutCoe object [ elliptical Fourier analysis ]
#> --------------------
#>  - $coe: 20 outlines described, 10 harmonics
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
