#' Graphical pile of shapes
#'
#' Pile all shapes in the same graphical window. Useful to check
#' their normalization in terms of size, position, rotation, first point, etc.
#' It is, essentially, a shortcut around `paper + drawers` of the grindr family.
#'
#' @param coo a single shape or any  [Coo] object
#' @param ... more arguments to feed the core drawer, depending on the object
#' @family grindr
#'
#' @note A variation of this plot was called `stack` before `Momocs 1.2.5`
#' @examples
#' # all Coo are supported wtih sensible defaults
#' pile(bot)    # outlines
#' pile(olea)   # curves
#' pile(wings)  # landmarks
#'
#' # you can continue the pipe with compatible drawers
#' pile(bot, trans=0.9) %>% draw_centroid
#'
#' # if you are not happy with this, build your own !
#' # eg see Momocs::pile.Out
#'
#' my_pile <- function(x, transp=0.5){
#'     x %>% paper_chess(n=100) %>%
#'           draw_outlines(transp=transp) %>%
#'           draw_centroid(col="red", pch=20, trans=transp) %>%
#'           draw_firstpoint(transp=transp)
#' }
#'
#' bot %>% my_pile(transp=1/4)
#'
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
pile.Out <- function(coo, ...){
  coo %>% paper_white() %>% draw_axes() %>%
    draw_firstpoint() %>% draw_outlines(...)
}

#' @name pile
#' @rdname pile
#' @export
pile.Opn <- function(coo, ...){
  coo %>% paper_white() %>% draw_axes() %>%
    draw_firstpoint() %>% draw_curves(...)
}

#' @name pile
#' @rdname pile
#' @export
pile.Ldk <- function(coo, ...){
  coo %>% paper_white() %>% draw_axes() %>%
    draw_landmarks(...)
}


