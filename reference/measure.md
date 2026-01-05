# Measures shape descriptors

Calculates shape descriptors on Coo and other objects. Any function that
returns a scalar when fed coordinates can be passed and naturally those
of Momocs (pick some there `apropos("coo_")`). Functions without
arguments (eg
[coo_area](http://momx.github.io/Momocs/reference/coo_area.md)) have to
be passed without brackets but functions with arguments (eg
[d](http://momx.github.io/Momocs/reference/d.md)) have to be passed
"entirely". See examples.

## Usage

``` r
measure(x, ...)
```

## Arguments

- x:

  any `Coo` object, or a list of shapes, or a shape as a matrix.

- ...:

  a list of functions. See examples.

## Value

a [TraCoe](http://momx.github.io/Momocs/reference/TraCoe.md) object, or
a raw data.frame

## See also

Other premodern:
[`coo_truss()`](http://momx.github.io/Momocs/reference/coo_truss.md)

## Examples

``` r
bm <- measure(bot, coo_area, coo_perim)
bm
#> A TraCoe object --------------------
#>  - $coe: 40 shapes described with 2 variables
#> # A tibble: 40 × 2
#>   type   fake 
#>   <fct>  <fct>
#> 1 whisky a    
#> 2 whisky a    
#> 3 whisky a    
#> 4 whisky a    
#> 5 whisky a    
#> 6 whisky a    
#> # ℹ 34 more rows
bm$coe
#> # A tibble: 40 × 2
#>       area perim
#>      <dbl> <dbl>
#>  1 234515  2482.
#>  2 201056. 2269.
#>  3 119460. 1578.
#>  4 119568. 1817.
#>  5 165736. 2066.
#>  6 114015  1487.
#>  7 149503  1954.
#>  8 147642. 1826.
#>  9 130178. 1751.
#> 10 219548  2399.
#> # ℹ 30 more rows

# how to use arguments, eg with the d() function
measure(wings, coo_area, d(1, 3), d(4, 5))
#> A TraCoe object --------------------
#>  - $coe: 127 shapes described with 3 variables
#> # A tibble: 127 × 1
#>   group
#>   <fct>
#> 1 AN   
#> 2 AN   
#> 3 AN   
#> 4 AN   
#> 5 AN   
#> 6 AN   
#> # ℹ 121 more rows

# alternatively, to get a data_frame
measure(bot$coo, coo_area, coo_perim)
#> # A tibble: 40 × 2
#>       area perim
#>      <dbl> <dbl>
#>  1 234515  2482.
#>  2 201056. 2269.
#>  3 119460. 1578.
#>  4 119568. 1817.
#>  5 165736. 2066.
#>  6 114015  1487.
#>  7 149503  1954.
#>  8 147642. 1826.
#>  9 130178. 1751.
#> 10 219548  2399.
#> # ℹ 30 more rows

# and also, to get a data_frame (one row)
measure(bot[1], coo_area, coo_perim)
#> # A tibble: 1 × 2
#>     area perim
#>    <dbl> <dbl>
#> 1 234515 2482.
```
