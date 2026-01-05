# Define tips as new landmarks

On [Opn](http://momx.github.io/Momocs/reference/Opn.md) objects, this
can be used before
[coo_slice](http://momx.github.io/Momocs/reference/coo_slice.md). See
examples.

## Usage

``` r
def_ldk_tips(coo)
```

## Arguments

- coo:

  `Opn` object

## Value

a Momocs object of same class

## Note

any existing ldk will be preserved.

## Examples

``` r
is_ldk(olea) # no ldk for olea
#> [1] FALSE
olea %>%
slice(1:3) %>% #for the sake of speed
def_ldk_tips %>%
def_ldk_angle(3*pi/4) %>% def_ldk_angle(pi/4) %T>% stack %>%
coo_slice(ldk=1:4) -> oleas

stack(oleas[[1]])

stack(oleas[[2]]) # etc.
```
