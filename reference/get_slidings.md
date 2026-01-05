# Extracts sliding landmarks coordinates

From an [Ldk](http://momx.github.io/Momocs/reference/Ldk.md) object.

## Usage

``` r
get_slidings(Coo, partition)
```

## Arguments

- Coo:

  an Ldk object

- partition:

  numeric which one(s) to get.

## Value

a list of list(s) of coordinates.

## See also

Other ldk/slidings methods:
[`add_ldk()`](http://momx.github.io/Momocs/reference/add_ldk.md),
[`def_ldk()`](http://momx.github.io/Momocs/reference/def_ldk.md),
[`def_slidings()`](http://momx.github.io/Momocs/reference/def_slidings.md),
[`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md),
[`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md),
[`slidings_scheme()`](http://momx.github.io/Momocs/reference/slidings_scheme.md)

## Examples

``` r
# for each example below a list with partition containign shapes is returned
# extracts the first partition
get_slidings(chaff, 1) %>% names()
#> [1] "partition1"
# the first and the fourth
get_slidings(chaff, c(1, 4)) %>%  names()
#> [1] "partition1" "partition4"
# all of them
get_slidings(chaff) %>%  names
#> [1] "partition1" "partition2" "partition3" "partition4"
# here we want to see it
get_slidings(chaff, 1)[[1]] %>%  Ldk %>% stack
```
