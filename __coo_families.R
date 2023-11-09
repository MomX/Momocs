library(tidyverse)
library(Momocs)
cool <- apropos("coo_")


eval_f <- function(x) paste0(x, "(bot[1])") %>% parse(text = .) %>% eval %>% class
safe_eval_f <- safely(eval_f)
cool_classes <- map(cool, safe_eval_f)  %>% map_chr("result")
names(cool_classes) <- cool


# coo_geometry

coo_align
coo_aligncalliper
coo_alignminradius
coo_alignxax
coo_baseline
coo_bookstein
coo_center
coo_centre # alias
coo_close
coo_down
coo_dxy # a way to set the 1st point on 0 0
coo_extract
coo_flipx
coo_flipy
coo_force2close # a bit rough
coo_interpolate
coo_jitter
coo_left
coo_rev
coo_right
coo_rotate
coo_rotatecenter
coo_sample
coo_sample_prop
coo_samplerr
coo_scale
coo_scalex
coo_scaley
coo_shearx
coo_sheary
coo_slice
coo_slide
coo_slidedirection
coo_slidegap
coo_smooth
coo_smoothcurve
coo_template_relatively # perhaps in coo_template?
coo_trim # combine with an "top, bottom, both" arg?
coo_trimbottom
coo_trimtop
coo_unclose
coo_untiltx
coo_up

# coo_descriptors scalar
coo_area
coo_calliper
coo_centsize
coo_circularity
coo_circularityharalick
coo_circularitynorm
coo_convexity
coo_eccentricityboundingbox
coo_eccentricityeigen
coo_elongation
coo_length
coo_perim
coo_rectangularity
coo_rectilinearity
coo_solidity
coo_width


# coo_descriptors along
coo_centdist
coo_perimcum
coo_perimpts



# coo_descriptors non-scalars
coo_boundingbox
coo_centpos # should be a df?
coo_chull
coo_chull_onion
coo_diffrange # coo_range_diff
coo_lw
coo_truss

coo_range
coo_range_enlarge

# coo_drawers
coo_arrows
coo_draw
coo_draw_rads
coo_listpanel
coo_lolli
coo_oscillo # deprecate for a proper oscillo
coo_plot
coo_ruban

# coo_others
coo_angle_edges
coo_angle_tangent

coo_intersect_angle
coo_intersect_direction
coo_intersect_segment

# coo_testers
coo_is_closed
coo_likely_anticlockwise
coo_likely_clockwise

# helpers
coo_check
coo_nb

coo_ldk



