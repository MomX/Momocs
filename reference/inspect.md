# Graphical inspection of shapes

Allows to plot shapes, individually, for
[Coo](http://momx.github.io/Momocs/reference/Coo.md)
([Out](http://momx.github.io/Momocs/reference/Out.md),
[Opn](http://momx.github.io/Momocs/reference/Opn.md) or
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md)) objects.

## Usage

``` r
inspect(x, id, ...)
```

## Arguments

- x:

  the [Coo](http://momx.github.io/Momocs/reference/Coo.md) object

- id:

  the id of the shape to plot, if not provided a random shape is
  plotted. If passed with `'all'` all shapes are plotted, one by one.

- ...:

  further arguments to be passed to
  [coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md)

## Value

an interactive plot

## See also

Other Coo_graphics:
[`panel()`](http://momx.github.io/Momocs/reference/panel.Coo.md),
[`stack()`](http://momx.github.io/Momocs/reference/stack.Coo.md)

## Examples

``` r
if (FALSE) { # \dontrun{
inspect(bot, 5)
inspect(bot)
inspect(bot, 5, pch=3, points=TRUE) # an example of '...' use
} # }
```
