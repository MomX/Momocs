# Split to several objects based on a factor

Rougher slicing that accepts a classifier ie a column name from the
`$fac` on Momocs classes. Returns a named (after every level) list that
can be lapply-ed and combined. See examples.

## Usage

``` r
chop(.data, fac)
```

## Arguments

- .data:

  a `Coo` or `Coe` object

- fac:

  a column name from the `$fac`

## Value

a named list of `Coo` or `Coe` objects

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
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
 olea %>%
      filter(var == "Aglan") %>% # to have a balanced nb of 'view'
      chop(~view) %>%    # split into a list of 2
      npoly %>%          # separately apply npoly
                         # strict equivalent to lapply(npoly)
      combine %>%       # recombine
      PCA %>% plot      # an illustration of the 2 views
#> 'nb.pts' missing and set to: 95
#> 'degree' missing and set to: 5
#> 'nb.pts' missing and set to: 91
#> 'degree' missing and set to: 5
#> will be deprecated soon, see ?plot_PCA

      # treated separately
```
