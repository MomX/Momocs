# Add new landmarks based on angular positions

A wrapper on
[coo_intersect_angle](http://momx.github.io/Momocs/reference/coo_intersect_angle.md)
and
[coo_intersect_direction](http://momx.github.io/Momocs/reference/coo_intersect_angle.md)
for [Out](http://momx.github.io/Momocs/reference/Out.md) and
[Opn](http://momx.github.io/Momocs/reference/Opn.md) objects.

## Usage

``` r
def_ldk_angle(coo, angle)

def_ldk_direction(coo, direction = c("down", "left", "up", "right")[4])

# Default S3 method
def_ldk_direction(coo, direction = c("down", "left", "up", "right")[4])

# S3 method for class 'Out'
def_ldk_direction(coo, direction = c("down", "left", "up", "right")[4])

# S3 method for class 'Opn'
def_ldk_direction(coo, direction = c("down", "left", "up", "right")[4])
```

## Arguments

- coo:

  a `Out` or `Opn` object

- angle:

  `numeric` an angle in radians (0 by default).

- direction:

  `character` one of `"down", "left", "up", "right"` ("right" by
  default)

## Value

a Momocs object of same class

## Note

any existing ldk will be preserved.

## See also

Typically used before
[coo_slice](http://momx.github.io/Momocs/reference/coo_slice.md) and
[coo_slide](http://momx.github.io/Momocs/reference/coo_slide.md). See
[def_ldk_tips](http://momx.github.io/Momocs/reference/def_ldk_tips.md)
as well.

## Examples

``` r
# adds a new landmark towards south east
hearts %>%
   slice(1:5) %>% # for speed purpose only
   def_ldk_angle(-pi/6) %>%
stack()


# on Out and towards NW and NE here
olea %>%
   slice(1:5) %>% #for speed purpose only
   def_ldk_angle(3*pi/4) %>%
   def_ldk_angle(pi/4) %>%
   stack

```
