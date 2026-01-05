# Full Procrustes alignment between two shapes

Directly borrowed from Claude (2008), called there the `fPsup` function.

## Usage

``` r
fProcrustes(coo1, coo2)
```

## Arguments

- coo1:

  configuration matrix to be superimposed onto the centered preshape of
  coo2.

- coo2:

  reference configuration matrix.

## Value

a list with components:

- `coo1` superimposed centered preshape of coo1 onto the centered
  preshape of coo2

- `coo2` centered preshape of coo2

- `rotation` rotation matrix

- `scale` scale parameter

- `DF` full Procrustes distance between coo1 and coo2.

## References

Claude, J. (2008). Morphometrics with R. Analysis (p. 316). Springer.

## See also

Other procrustes functions:
[`fgProcrustes()`](http://momx.github.io/Momocs/reference/fgProcrustes.md),
[`fgsProcrustes()`](http://momx.github.io/Momocs/reference/fgsProcrustes.md),
[`pProcrustes()`](http://momx.github.io/Momocs/reference/pProcrustes.md)
