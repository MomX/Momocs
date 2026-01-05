# Brew and serve fac from Momocs object

Ease various specifications for fac specification when passed to Momocs
objects. Intensively used (internally).

## Usage

``` r
fac_dispatcher(x, fac)
```

## Arguments

- x:

  a Momocs object (any `Coo`, `Coe`, `PCA`, etc.)

- fac:

  a specification to extract from `fac`

## Value

a prepared `factor` (or a `numeric`). See examples

## Details

`fac` can be:

- a factor, passed on the fly

- a column id from `$fac`

- a column name from `fac`; if not found, return `NULL` with a message

- a formula in the form: `~column_name` (from `$fac`, no quotes). It
  expresses more in a concise way. Also allows interacting on the fly.
  See examples.

- a `NULL` returns a `NULL`, with a message

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
[`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md),
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
bot <- mutate(bot, s=rnorm(40), fake=factor(rep(letters[1:4], 10)))

# factor, on the fly
fac_dispatcher(bot, factor(rep(letters[1:4], 10)))
#>  [1] a b c d a b c d a b c d a b c d a b c d a b c d a b c d a b c d a b c d a b
#> [39] c d
#> Levels: a b c d

# column id
fac_dispatcher(bot, 1)
#>  type1  type2  type3  type4  type5  type6  type7  type8  type9 type10 type11 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky 
#> type12 type13 type14 type15 type16 type17 type18 type19 type20 type21 type22 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky   beer   beer 
#> type23 type24 type25 type26 type27 type28 type29 type30 type31 type32 type33 
#>   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer 
#> type34 type35 type36 type37 type38 type39 type40 
#>   beer   beer   beer   beer   beer   beer   beer 
#> Levels: beer whisky

# column name
fac_dispatcher(bot, "type")
#>  type1  type2  type3  type4  type5  type6  type7  type8  type9 type10 type11 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky 
#> type12 type13 type14 type15 type16 type17 type18 type19 type20 type21 type22 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky   beer   beer 
#> type23 type24 type25 type26 type27 type28 type29 type30 type31 type32 type33 
#>   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer 
#> type34 type35 type36 type37 type38 type39 type40 
#>   beer   beer   beer   beer   beer   beer   beer 
#> Levels: beer whisky
# same, numeric case
fac_dispatcher(bot, "s")
#>          s1          s2          s3          s4          s5          s6 
#> -1.79913645  1.08295568 -0.35085362 -1.40349008 -0.20179666 -0.12677816 
#>          s7          s8          s9         s10         s11         s12 
#>  1.05920687 -1.16739603 -0.55764363  1.48811993  1.35866577  1.16321454 
#>         s13         s14         s15         s16         s17         s18 
#>  1.66152395  0.20403098 -0.58188369  0.55520406  1.05872313  2.41363327 
#>         s19         s20         s21         s22         s23         s24 
#> -1.96498233  0.27323570  0.65479458 -0.05459866 -1.55782225  0.74150089 
#>         s25         s26         s27         s28         s29         s30 
#> -0.77908574  0.50586150  0.90755171  1.28395701 -1.55786380  1.08174185 
#>         s31         s32         s33         s34         s35         s36 
#> -0.75698136 -1.28901947  1.31432067  1.14625997 -0.24258327  0.75954071 
#>         s37         s38         s39         s40 
#> -0.86032574 -0.15103158 -0.09372323 -0.28074005 

# formula interface
fac_dispatcher(bot, ~type)
#>  type1  type2  type3  type4  type5  type6  type7  type8  type9 type10 type11 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky whisky 
#> type12 type13 type14 type15 type16 type17 type18 type19 type20 type21 type22 
#> whisky whisky whisky whisky whisky whisky whisky whisky whisky   beer   beer 
#> type23 type24 type25 type26 type27 type28 type29 type30 type31 type32 type33 
#>   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer   beer 
#> type34 type35 type36 type37 type38 type39 type40 
#>   beer   beer   beer   beer   beer   beer   beer 
#> Levels: beer whisky

# formula interface + interaction on the fly
fac_dispatcher(bot, ~type+fake)
#>  [1] whisky_a whisky_b whisky_c whisky_d whisky_a whisky_b whisky_c whisky_d
#>  [9] whisky_a whisky_b whisky_c whisky_d whisky_a whisky_b whisky_c whisky_d
#> [17] whisky_a whisky_b whisky_c whisky_d beer_a   beer_b   beer_c   beer_d  
#> [25] beer_a   beer_b   beer_c   beer_d   beer_a   beer_b   beer_c   beer_d  
#> [33] beer_a   beer_b   beer_c   beer_d   beer_a   beer_b   beer_c   beer_d  
#> Levels: beer_a beer_b beer_c beer_d whisky_a whisky_b whisky_c whisky_d

# when passing NULL or non existing column
fac_dispatcher(42, NULL)
#> NULL
fac_dispatcher(bot, "loser")
#> not a valid column specification, returning NULL
#> NULL
```
