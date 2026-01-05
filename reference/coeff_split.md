# Converts a numerical description of harmonic coefficients to a named list.

`coeff_split` returns a named list of coordinates from a vector of
harmonic coefficients. For instance, harmonic coefficients are arranged
in the `$coe` slot of `Coe`-objects in that way: \\A_1, \dots, A_n, B_1,
\dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n\\ after an elliptical
Fourier analysis (see
[efourier](http://momx.github.io/Momocs/reference/efourier.md) and
[efourier](http://momx.github.io/Momocs/reference/efourier.md)) while
\\C_n and D_n\\ harmonic are absent for radii variation and tangent
angle approaches (see
[rfourier](http://momx.github.io/Momocs/reference/rfourier.md) and
[tfourier](http://momx.github.io/Momocs/reference/tfourier.md)
respectively). This function is used internally but might be of interest
elwewhere.

## Usage

``` r
coeff_split(cs, nb.h = 8, cph = 4)
```

## Arguments

- cs:

  A `vector` of harmonic coefficients.

- nb.h:

  `numeric`. The maximum harmonic rank.

- cph:

  `numeric`. Must be set to 2 for `rfourier` and `tfourier` were used.

## Value

Returns a named list of coordinates.

## Examples

``` r
coeff_split(1:128, nb.h=32, cph=4) # efourier
#> $an
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32
#> 
#> $bn
#>  [1] 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57
#> [26] 58 59 60 61 62 63 64
#> 
#> $cn
#>  [1] 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89
#> [26] 90 91 92 93 94 95 96
#> 
#> $dn
#>  [1]  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
#> [20] 116 117 118 119 120 121 122 123 124 125 126 127 128
#> 
coeff_split(1:64, nb.h=32, cph=2)  # t/r fourier
#> $an
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32
#> 
#> $bn
#>  [1] 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57
#> [26] 58 59 60 61 62 63 64
#> 
```
