#' Graphical pile of shapes
#'
#' Pile all shapes in the same graphical window. Useful to check
#' their normalization in terms of size, position, rotation, first point, etc.
#' It is, essentially, a shortcut around `paper + drawers` of the grindr family.
#'
#' @param coo a single shape or any  [Coo] object
#' @param f factor specification
#' @param `paper_fun` a [papers] function (default: `paper`)
#' @param `draw_fun` one of [drawers] for `pile.list`
#' @param pal palette among [palettes] (default: pal_qual)
#' @param transp `numeric` for transparency (default:adjusted, min:0, max=0)
#' @param ... more arguments to feed the core drawer, depending on the object
#' @family grindr
#' @note A variation of this plot was called `stack` before `Momocs 1.2.5`
#' @examples
#' # all Coo are supported wtih sensible defaults
#' pile(bot)    # outlines
#' pile(olea, ~var, pal=pal_qual_Dark2, paper_fun=paper_grid)   # curves
#' pile(wings)  # landmarks
#'
#' # you can continue the pipe with compatible drawers
#' pile(bot, trans=0.9) %>% draw_centroid
#'
#' # if you are not happy with this, build your own !
#' # eg see Momocs::pile.Out
#'
#' my_pile <- function(x, col_labels="red", transp=0.5){
#'     x %>% paper_chess(n=100) %>%
#'           draw_landmarks(transp=transp) %>%
#'           draw_labels(col=col_labels)
#' }
#' # using it
#' wings %>% my_pile(transp=3/4)
#'
#'  # and as gridr functions propagate, you can even continue:
#'  wings %>% my_pile() %>% draw_centroid(col="blue", cex=5)
#'
#'  # method on lists
#'  bot$coo %>% pile
#'
#'  # it can be tuned when we have a list of landmarks with:
#'  wings$coo %>% pile(draw_fun=draw_landmarks)
#'
#'  # or on arrays (turn for draw_landmarks)
#'  wings$coo %>% l2a %>% #we now have an array
#'      pile
#' @name pile
#' @rdname pile
#' @export
pile <- function(coo, ...){
  UseMethod("pile")
}

#' @name pile
#' @rdname pile
#' @export
pile.default <- function(coo, ...){
  message("only defined for Coo classes")
}

#' @name pile
#' @rdname pile
#' @export
pile.list <- function(coo, f, pal=pal_qual, paper_fun=paper,
                      draw_fun=draw_curves, transp=0, ...){
  coo <- coo %>% paper_fun
  if (!missing(f))
    coo %>% draw_fun(f=f, pal=pal, transp=transp, ...)
  else
    coo %>% draw_fun(transp=transp, ...)
}

#' @name pile
#' @rdname pile
#' @export
pile.array <- function(coo, f, pal=pal_qual, paper_fun=paper,
                      draw_fun=draw_landmarks, transp=0, ...){
  coo <- coo %>% a2l %>%  paper_fun
  if (!missing(f))
    coo %>% draw_fun(f=f, pal=pal, transp=transp, ...)
  else
    coo %>% draw_fun(transp=transp, ...)
}

#' @name pile
#' @rdname pile
#' @export
pile.Out <- function(coo, f, pal=pal_qual,
                     paper_fun=paper, transp=0, ...){
  coo <- coo %>% paper_fun
  if (!missing(f))
    coo %>% draw_outlines(f=f, pal=pal, transp=transp, ...) %>% draw_firstpoint(transp=transp)
  else
    coo %>% draw_outlines(transp=transp, ...) %>% draw_firstpoint(transp=transp)
}

#' @name pile
#' @rdname pile
#' @export
pile.Opn <- function(coo, f, pal=pal_qual,
                     paper_fun=paper, transp=0, ...){
  coo <- coo %>% paper_fun
  if (!missing(f))
    coo %>% draw_curves(f=f, pal=pal, transp=transp, ...) %>% draw_firstpoint(transp=transp)
  else
    coo %>% draw_curves(transp=transp, ...) %>% draw_firstpoint(transp=transp)

}

#' @name pile
#' @rdname pile
#' @export
pile.Ldk <- function(coo, f, pal=pal_qual,
                     paper_fun=paper, transp=0, ...){
  coo <- coo %>% paper_fun
  if (!missing(f))
    coo %>% draw_landmarks(f=f, pal=pal, transp=transp, ...)
  else
    coo %>% draw_landmarks(transp=transp, ...)

}


