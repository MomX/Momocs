# Traditional morphometrics class

Defines the builder for traditional measurement class in Momocs. Is is
intended to ease calculations, data handling and multivariate statistics
just ad the other Momocs' classes

## Usage

``` r
TraCoe(coe = matrix(), fac = dplyr::tibble())
```

## Arguments

- coe:

  a matrix of measurements

- fac:

  a data.frame for covariates

## Value

a list of class TraCoe

## See also

Other classes: [`Coe()`](http://momx.github.io/Momocs/reference/Coe.md),
[`Coo()`](http://momx.github.io/Momocs/reference/Coo.md),
[`Ldk()`](http://momx.github.io/Momocs/reference/Ldk.md),
[`Opn()`](http://momx.github.io/Momocs/reference/Opn.md),
[`OpnCoe()`](http://momx.github.io/Momocs/reference/OpnCoe.md),
[`Out()`](http://momx.github.io/Momocs/reference/Out.md),
[`OutCoe()`](http://momx.github.io/Momocs/reference/OutCoe.md)

## Examples

``` r
# let's (more or less) rebuild the flower dataset
fl <- TraCoe(iris[, 1:4], dplyr::tibble(sp=iris$Species))
fl %>% PCA() %>% plot("sp")
#> will be deprecated soon, see ?plot_PCA
```
