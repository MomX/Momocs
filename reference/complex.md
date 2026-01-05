# Convert complex to/from cartesian coordinates

Convert complex to/from cartesian coordinates

## Usage

``` r
cpx2coo(Z)

coo2cpx(coo)
```

## Arguments

- Z:

  coordinates expressed in the complex form

- coo:

  coordinates expressed in the cartesian form

## Value

coordinates expressed in the cartesian/complex form

## See also

Other bridges functions:
[`as_df()`](http://momx.github.io/Momocs/reference/as_df.md),
[`bridges`](http://momx.github.io/Momocs/reference/bridges.md),
[`export()`](http://momx.github.io/Momocs/reference/export.md)

## Examples

``` r
shapes[4] %>%            # from cartesian
    coo_sample(24) %>%
    coo2cpx() %T>%       # to complex
    cpx2coo()            # and back
#>  [1] 200+ 62i 205+ 43i 176+ 43i 146+ 44i 156+ 23i 186+ 20i 202+ 15i 172+  9i
#>  [9] 143+ 16i 130+ 45i 135+ 74i 133+104i 142+134i 165+160i 191+182i 203+210i
#> [17] 225+226i 239+204i 238+178i 237+150i 227+120i 221+ 91i 224+ 62i 219+ 45i
```
