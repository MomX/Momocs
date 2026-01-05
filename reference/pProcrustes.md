# Partial Procrustes alignment between two shapes

Directly borrowed from Claude (2008), and called `pPsup` there.

## Usage

``` r
pProcrustes(coo1, coo2)
```

## Arguments

- coo1:

  Configuration matrix to be superimposed onto the centered preshape of
  coo2.

- coo2:

  Reference configuration matrix.

## Value

a list with components

- `coo1` superimposed centered preshape of coo1 onto the centered
  preshape of coo2

- `coo2` centered preshape of coo2

- `rotation` rotation matrix

- `DP` partial Procrustes distance between coo1 and coo2

- `rho` trigonometric Procrustes distance.

## References

Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.

## See also

Other procrustes functions:
[`fProcrustes()`](http://momx.github.io/Momocs/reference/fProcrustes.md),
[`fgProcrustes()`](http://momx.github.io/Momocs/reference/fgProcrustes.md),
[`fgsProcrustes()`](http://momx.github.io/Momocs/reference/fgsProcrustes.md)
