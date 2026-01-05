# Nearest intersection between a shape and a segment specified with an angle

Take a shape, and segment starting on the centroid and having a
particular angle, which point is the nearest where the segment
intersects with the shape?

## Usage

``` r
coo_intersect_angle(coo, angle = 0)

coo_intersect_direction(coo, direction = c("down", "left", "up", "right")[4])

# Default S3 method
coo_intersect_direction(coo, direction = c("down", "left", "up", "right")[4])

# S3 method for class 'Coo'
coo_intersect_direction(coo, direction = c("down", "left", "up", "right")[4])
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

- angle:

  `numeric` an angle in radians (0 by default).

- direction:

  `character` one of `"down", "left", "up", "right"` ("right" by
  default)

## Value

`numeric` the id of the nearest point or a `list` for `Coo` See
examples.

## Note

shapes are always centered before this operation. If you need a simple
direction such as `(down, left, up, right)ward`, then use
coo_intersect_direction which does not need to find an intersection but
relies on coordinates and is about 1000.

## See also

Other coo\_ intersect:
[`coo_intersect_segment()`](http://momx.github.io/Momocs/reference/coo_intersect_segment.md)

## Examples

``` r
coo <- bot[1] %>% coo_center %>% coo_scale
coo_plot(coo)
coo %>% coo_intersect_angle(pi/7) %>%
   coo[., , drop=FALSE] %>% points(col="red")


 # many angles
 coo_plot(coo)
 sapply(seq(0, pi, pi/12),
       function(x) coo %>% coo_intersect_angle(x)) -> ids
 coo[ids, ] %>% points(col="blue")

 coo %>%
 coo_intersect_direction("down") %>%
 coo[.,, drop=FALSE] %>% points(col="orange")

```
