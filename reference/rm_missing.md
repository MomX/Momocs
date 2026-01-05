# Remove shapes with missing data in fac

Any row (or within a given column if `by` is specified) containing `NA`
in `$fac` and the corresponding shapes in `$coo`, lines in `$coe` or
other objects will also be dropped.

## Usage

``` r
rm_missing(x, by)
```

## Arguments

- x:

  the object on which to NA

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
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
bot$fac$type[3] <- NA
bot$fac$fake[9] <- NA

bot %>% length()
#> [1] 40
bot %>% rm_missing() %>% length
#> [1] 38
bot %>% rm_missing("fake") %>% length()
#> [1] 39
```
