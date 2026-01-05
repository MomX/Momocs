# Builds an OpnCoe object

In Momocs, `OpnCoe` classes objects are wrapping around lists of
morphometric coefficients, along with other informations, on which
generic methods such as plotting methods (e.g.
[boxplot](https://rdrr.io/r/graphics/boxplot.html)) and specific methods
can be applied. `OpnCoe` objects are primarily
[`Coe`](http://momx.github.io/Momocs/reference/Coe.md) objects.

## Usage

``` r
OpnCoe(
  coe = matrix(),
  fac = dplyr::tibble(),
  method = character(),
  baseline1 = numeric(),
  baseline2 = numeric(),
  mod = list(),
  r2 = numeric()
)
```

## Arguments

- coe:

  `matrix` of morphometric coefficients

- fac:

  (optionnal) a `data.frame` of factors, specifying the grouping
  structure

- method:

  used to obtain these coefficients

- baseline1:

  \\(x; y)\\ coordinates of the first baseline point

- baseline2:

  \\(x; y)\\ coordinates of the second baseline point

- mod:

  an R [lm](https://rdrr.io/r/stats/lm.html) object, used to reconstruct
  shapes

- r2:

  numeric, the r-squared from every model

## Value

an `OpnCoe` object

## See also

Other classes: [`Coe()`](http://momx.github.io/Momocs/reference/Coe.md),
[`Coo()`](http://momx.github.io/Momocs/reference/Coo.md),
[`Ldk()`](http://momx.github.io/Momocs/reference/Ldk.md),
[`Opn()`](http://momx.github.io/Momocs/reference/Opn.md),
[`Out()`](http://momx.github.io/Momocs/reference/Out.md),
[`OutCoe()`](http://momx.github.io/Momocs/reference/OutCoe.md),
[`TraCoe()`](http://momx.github.io/Momocs/reference/TraCoe.md)

## Examples

``` r
# all OpnCoe classes
methods(class='OpnCoe')
#> [1] MANOVA  MSHAPES PCA     boxplot combine print  
#> see '?methods' for accessing help and source code
```
