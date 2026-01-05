# Builds an OutCoe object

In Momocs, `OutCoe` classes objects are wrapping around lists of
morphometric coefficients, along with other informations, on which
generic methods such as plotting methods (e.g.
[boxplot](https://rdrr.io/r/graphics/boxplot.html)) and specific methods
can be applied. `OutCoe` objects are primarily
[`Coe`](http://momx.github.io/Momocs/reference/Coe.md) objects.

## Usage

``` r
OutCoe(coe = matrix(), fac = dplyr::tibble(), method, norm)
```

## Arguments

- coe:

  `matrix` of harmonic coefficients

- fac:

  (optional) a `data.frame` of factors, specifying the grouping
  structure

- method:

  used to obtain these coefficients

- norm:

  the normalisation used to obtain these coefficients

## Value

an `OutCoe` object

## Details

These methods can be applied on `Out` objects:

## See also

Other classes: [`Coe()`](http://momx.github.io/Momocs/reference/Coe.md),
[`Coo()`](http://momx.github.io/Momocs/reference/Coo.md),
[`Ldk()`](http://momx.github.io/Momocs/reference/Ldk.md),
[`Opn()`](http://momx.github.io/Momocs/reference/Opn.md),
[`OpnCoe()`](http://momx.github.io/Momocs/reference/OpnCoe.md),
[`Out()`](http://momx.github.io/Momocs/reference/Out.md),
[`TraCoe()`](http://momx.github.io/Momocs/reference/TraCoe.md)

## Examples

``` r
# all OutCoe methods
methods(class='OutCoe')
#>  [1] MANOVA   MSHAPES  PCA      boxplot  combine  hcontrib print    rm_asym 
#>  [9] rm_sym   symmetry
#> see '?methods' for accessing help and source code
```
