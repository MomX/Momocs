# Builds an Out object

In Momocs, `Out`-classes objects are lists of closed **out**lines, with
optional components, and on which generic methods such as plotting
methods (e.g.
[stack](http://momx.github.io/Momocs/reference/stack.Coo.md)) and
specific methods (e.g.
[efourier](http://momx.github.io/Momocs/reference/efourier.md) can be
applied. `Out` objects are primarily
[`Coo`](http://momx.github.io/Momocs/reference/Coo.md) objects.

## Usage

``` r
Out(x, fac = dplyr::tibble(), ldk = list())
```

## Arguments

- x:

  a `list` of matrices of `(x; y)` coordinates, or an array or an Out
  object or an Ldk object, or a data.frame (and friends)

- fac:

  (optional) a `data.frame` of factors and/or numerics specifying the
  grouping structure

- ldk:

  (optional) `list` of landmarks as row number indices

## Value

an `Out` object

## See also

Other classes: [`Coe()`](http://momx.github.io/Momocs/reference/Coe.md),
[`Coo()`](http://momx.github.io/Momocs/reference/Coo.md),
[`Ldk()`](http://momx.github.io/Momocs/reference/Ldk.md),
[`Opn()`](http://momx.github.io/Momocs/reference/Opn.md),
[`OpnCoe()`](http://momx.github.io/Momocs/reference/OpnCoe.md),
[`OutCoe()`](http://momx.github.io/Momocs/reference/OutCoe.md),
[`TraCoe()`](http://momx.github.io/Momocs/reference/TraCoe.md)

## Examples

``` r
methods(class=Out)
#>  [1] add_ldk           combine           coo_bookstein     coo_down         
#>  [5] coo_left          coo_right         coo_sample        coo_sample_prop  
#>  [9] coo_slice         coo_up            d                 def_ldk          
#> [13] def_ldk_angle     def_ldk_direction efourier          fgProcrustes     
#> [17] get_ldk           mosaic            panel             pile             
#> [21] rearrange_ldk     rfourier          sfourier          tfourier         
#> see '?methods' for accessing help and source code
```
