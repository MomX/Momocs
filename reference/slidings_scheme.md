# Extracts partitions of sliding coordinates

Helper function that deduces (likely to be a reminder) partition scheme
from `$slidings` of `Ldk` objects.

## Usage

``` r
slidings_scheme(Coo)
```

## Arguments

- Coo:

  an Ldk object

## Value

a list with two components: `n` the number of partition; `id` their
position. Or a NULL if no slidings are defined

## See also

Other ldk/slidings methods:
[`add_ldk()`](http://momx.github.io/Momocs/reference/add_ldk.md),
[`def_ldk()`](http://momx.github.io/Momocs/reference/def_ldk.md),
[`def_slidings()`](http://momx.github.io/Momocs/reference/def_slidings.md),
[`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md),
[`get_slidings()`](http://momx.github.io/Momocs/reference/get_slidings.md),
[`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md)

## Examples

``` r
# no slidings defined a NULL is returned with a message
slidings_scheme(wings)
#> no sliding defined
#> NULL

# slidings defined
slidings_scheme(chaff)
#> $n
#> [1] 4
#> 
#> $id
#>            start end
#> partition1    13  52
#> partition2    53  92
#> partition3    93 132
#> partition4   133 172
#> 
```
