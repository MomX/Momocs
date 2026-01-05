# Validates Coo objects

No validation for S3 objects, so this method is a (cheap) attempt at
checking [Coo](http://momx.github.io/Momocs/reference/Coo.md) objects,
[Out](http://momx.github.io/Momocs/reference/Out.md),
[Opn](http://momx.github.io/Momocs/reference/Opn.md) and
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md) objects.

## Usage

``` r
verify(Coo)
```

## Arguments

- Coo:

  any Coo object

## Value

a Coo object.

## Details

Implemented before all morphometric methods and handling verbs. To see
what is checked, try eg `Momocs:::verify.Coo`

## Examples

``` r
verify(bot)
#> Out (outlines)
#>   - 40 outlines, 162 +/- 21 coords (in $coo)
#>   - 2 classifiers (in $fac): 
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
#>   - also: $ldk
bot[12] <- NA
# you would not use try, but here we cope with R CMD CHECK standards
plop <- try(verify(bot), silent=TRUE)
class(plop)
#> [1] "try-error"

verify(hearts)
#> Out (outlines)
#>   - 240 outlines, 80 +/- 0 coords (in $coo)
#>   - 1 classifiers (in $fac): 
#> # A tibble: 240 × 1
#>   aut  
#>   <fct>
#> 1 ced  
#> 2 ced  
#> 3 ced  
#> 4 ced  
#> 5 ced  
#> 6 ced  
#> # ℹ 234 more rows
#>   - also: $ldk
hearts$ldk[[4]] <- c(1, 2)
# same remark
plop2 <- try(verify(hearts), silent=TRUE)
class(plop2)
#> [1] "try-error"
```
