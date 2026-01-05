# Renames levels on Momocs objects

rw_fac stands for 'rewriting rule'. Typically useful to correct typos at
the import, or merge some levels within covariates. Drops levels
silently.

## Usage

``` r
rw_fac(x, fac, from, to)
```

## Arguments

- x:

  any Momocs object

- fac:

  the id of the name of the \$fac column to look for
  ([fac_dispatcher](http://momx.github.io/Momocs/reference/fac_dispatcher.md)
  not yet supported)

- from:

  which level(s) should be renamed; passed as a single or several
  characters

- to:

  which name should be used to rename this/these levels

## Value

a Momocs object of the same class

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
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
# single renaming
rw_fac(bot, "type", "whisky", "agua_de_fuego")$type # 1 instead of "type" is fine too
#>         type1         type2         type3         type4         type5 
#>          beer          beer          beer          beer          beer 
#>         type6         type7         type8         type9        type10 
#>          beer          beer          beer          beer          beer 
#>        type11        type12        type13        type14        type15 
#>          beer          beer          beer          beer          beer 
#>        type16        type17        type18        type19        type20 
#>          beer          beer          beer          beer          beer 
#>        type21        type22        type23        type24        type25 
#> agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego 
#>        type26        type27        type28        type29        type30 
#> agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego 
#>        type31        type32        type33        type34        type35 
#> agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego 
#>        type36        type37        type38        type39        type40 
#> agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego agua_de_fuego 
#> Levels: agua_de_fuego beer
# several renaming
bot2 <- mutate(bot, fake=factor(rep(letters[1:4], 10)))
rw_fac(bot2, "fake", c("a", "e"), "ae")$fake
#>  fake1  fake2  fake3  fake4  fake5  fake6  fake7  fake8  fake9 fake10 fake11 
#>     ae     ae      b      c     ae     ae      b      c     ae     ae      b 
#> fake12 fake13 fake14 fake15 fake16 fake17 fake18 fake19 fake20 fake21 fake22 
#>      c     ae     ae      b      c     ae     ae      b      c     ae     ae 
#> fake23 fake24 fake25 fake26 fake27 fake28 fake29 fake30 fake31 fake32 fake33 
#>      b      c     ae     ae      b      c     ae     ae      b      c     ae 
#> fake34 fake35 fake36 fake37 fake38 fake39 fake40 
#>     ae      b      c     ae     ae      b      c 
#> Levels: ae b c
```
