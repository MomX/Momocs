# Quantitative r2 calibration for Opn objects

Estimates the r2 to calibrate the degree for
[npoly](http://momx.github.io/Momocs/reference/npoly.md) and
[opoly](http://momx.github.io/Momocs/reference/opoly.md) methods. Also
returns a plot

## Usage

``` r
calibrate_r2()

calibrate_r2_opoly(
  Opn,
  id = 1:length(Opn),
  degree.range = 1:8,
  thresh = c(0.9, 0.95, 0.99, 0.999),
  plot = TRUE,
  ...
)

calibrate_r2_npoly(
  Opn,
  id = 1:length(Opn),
  degree.range = 1:8,
  thresh = c(0.9, 0.95, 0.99, 0.999),
  plot = TRUE,
  ...
)
```

## Arguments

- Opn:

  an Opn object

- id:

  the ids of shapes on which to calculate r2 (all by default)

- degree.range:

  on which to calculate r2

- thresh:

  the threshold to return diagnostic

- plot:

  logical whether to print the plot

- ...:

  useless here

## Value

a ggpot2 object

## Details

May be long, so you can estimate it on a sample either with id here, or
one of [sample_n](http://momx.github.io/Momocs/reference/sample_n.md) or
[sample_frac](http://momx.github.io/Momocs/reference/sample_frac.md)

## Note

Silent message and progress bars (if any) with
`options("verbose"=FALSE)`.

## See also

Other calibration:
[`calibrate_deviations()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md),
[`calibrate_harmonicpower()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md),
[`calibrate_reconstructions`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)

## Examples

``` r
olea %>% slice(1:5) %>% #for the sake of spped
    calibrate_r2_opoly(degree.range=1:5, thresh=c(0.9, 0.99))

#> $gg

#> 
#> $q
#>                        degree1   degree2   degree3   degree4   degree5
#> 0001-cAglan_O10VD 0.0004287127 0.9821251 0.9831469 0.9986415 0.9987220
#> 0001-cAglan_O10VL 0.0011742448 0.9838978 0.9841793 0.9955520 0.9972836
#> 0001-cAglan_O11VD 0.0123113197 0.9706618 0.9906431 0.9965470 0.9965690
#> 0001-cAglan_O11VL 0.0151312838 0.9459654 0.9796698 0.9958102 0.9963290
#> 0001-cAglan_O12VD 0.0002310795 0.9673982 0.9674270 0.9912600 0.9938820
#> 
#> $mind
#>  0.9 0.99 
#>    2    4 
#> 

olea %>% slice(1:5) %>% #for the sake of spped
    calibrate_r2_npoly(degree.range=1:5, thresh=c(0.9, 0.99))

#> $gg

#> 
#> $q
#>                        degree1   degree2   degree3   degree4   degree5
#> 0001-cAglan_O10VD 0.0004287127 0.9821251 0.9831469 0.9986415 0.9987220
#> 0001-cAglan_O10VL 0.0011742448 0.9838978 0.9841793 0.9955520 0.9972836
#> 0001-cAglan_O11VD 0.0123113197 0.9706618 0.9906431 0.9965470 0.9965690
#> 0001-cAglan_O11VL 0.0151312838 0.9459654 0.9796698 0.9958102 0.9963290
#> 0001-cAglan_O12VD 0.0002310795 0.9673982 0.9674270 0.9912600 0.9938820
#> 
#> $mind
#>  0.9 0.99 
#>    2    4 
#> 
```
