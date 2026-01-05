# Boxplot on PCA objects

Boxplot on PCA objects

## Usage

``` r
# S3 method for class 'PCA'
boxplot(x, fac = NULL, nax, ...)
```

## Arguments

- x:

  `PCA`, typically obtained with
  [PCA](http://momx.github.io/Momocs/reference/PCA.md)

- fac:

  factor, or a name or the column id from the \$fac slot

- nax:

  the range of PC to plot (1 to 99pc total variance by default)

- ...:

  useless here

## Value

a ggplot object

## Examples

``` r
bot.f <- efourier(bot, 12)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bot.p <- PCA(bot.f)
boxplot(bot.p)
#> `prop` not provided. All axes returned
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the Momocs package.
#>   Please report the issue at <https://github.com/MomX/Momocs/issues>.

p <- boxplot(bot.p, 1)
#> `prop` not provided. All axes returned
#p +  theme_minimal() + scale_fill_grey()
#p + facet_wrap(~PC, scales = "free")
```
