# Boxplot of morphometric coefficients

Explores the distribution of coefficient values.

## Usage

``` r
# S3 method for class 'OutCoe'
boxplot(x, ...)
```

## Arguments

- x:

  the [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

- ...:

  useless here

## Value

a ggplot2 object

## See also

Other Coe_graphics:
[`hcontrib()`](http://momx.github.io/Momocs/reference/harm.contrib.md)

## Examples

``` r
# on OutCoe
bot %>% efourier(9) %>% rm_harm(1) %>% boxplot()
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details


data(olea)
op <- opoly(olea)
#> 'nb.pts' missing and set to 91
#> 'degree' missing and set to 5
boxplot(op)
```
