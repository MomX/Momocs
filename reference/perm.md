# Permutes and breed Coe (and others) objects

This methods applies permutations column-wise on the `coe` of any
[Coe](http://momx.github.io/Momocs/reference/Coe.md) object but relies
on a function that can be used on any matrix. For a Coe object, it uses
[sample](https://rdrr.io/r/base/sample.html) on every column (or row)
with (or without) replacement.

## Usage

``` r
perm(x, ...)

# Default S3 method
perm(x, margin = 2, size, replace = TRUE, ...)

# S3 method for class 'Coe'
perm(x, size, replace = TRUE, ...)
```

## Arguments

- x:

  the object to permute

- ...:

  useless here

- margin:

  numeric whether 1 or 2 (rows or columns)

- size:

  numeric the required size for the final object, same size by default.

- replace:

  logical, whether to use [sample](https://rdrr.io/r/base/sample.html)
  with replacement

## Value

a Coe object of same class

## See also

Other farming:
[`breed()`](http://momx.github.io/Momocs/reference/breed.md)

## Examples

``` r
m <- matrix(1:12, nrow=3)
m
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    7   10
#> [2,]    2    5    8   11
#> [3,]    3    6    9   12
perm(m, margin=2, size=5)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    9   12
#> [2,]    3    5    9   11
#> [3,]    3    5    7   10
#> [4,]    1    5    8   12
#> [5,]    1    6    7   11
perm(m, margin=1, size=10)
#>       [,1] [,2] [,3]
#>  [1,]    1   11    9
#>  [2,]    7    8   12
#>  [3,]   10   11    6
#>  [4,]   10    5    6
#>  [5,]    4   11    6
#>  [6,]   10    5    9
#>  [7,]    1    8    6
#>  [8,]   10    2   12
#>  [9,]   10    2    3
#> [10,]    4    2   12

bot.f <- efourier(bot, 12)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bot.m <- perm(bot.f, 80)
bot.m
#> An OutCoe object [ elliptical Fourier analysis ]
#> --------------------
#>  - $coe: 80 outlines described, 12 harmonics
#> # A tibble: 0 × 0
```
