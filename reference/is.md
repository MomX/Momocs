# Class and component testers

Class testers test if any of the classes of an object *is* of a given
class. For instance `is_PCA` on a
[PCA](http://momx.github.io/Momocs/reference/PCA.md) object (of classes
`PCA` and `prcomp`) will return `TRUE`. Component testers check if
*there_is* a particular component (eg `$fac`, etc.) in an object.

## Usage

``` r
is_Coo(x)

is_PCA(x)

is_LDA(x)

is_Out(x)

is_Opn(x)

is_Ldk(x)

is_Coe(x)

is_OutCoe(x)

is_OpnCoe(x)

is_LdkCoe(x)

is_TraCoe(x)

is_shp(x)

is_fac(x)

is_ldk(x)

is_slidings(x)

is_links(x)
```

## Arguments

- x:

  the object to test

## Value

`logical`

## Examples

``` r
is_Coo(bot)
#> [1] TRUE
is_Out(bot)
#> [1] TRUE
is_Ldk(bot)
#> [1] FALSE
is_ldk(hearts) # mind the capitals!
#> [1] TRUE
```
