#' Plots mosaics of shapes.
#'
#' Will soon replace [panel]. See examples and vignettes.
#'
#' @param coo_list `list` of shapes
#' @param x any [Coo] object
#' @param f factor specification to feed [fac_dispatcher]
#' @param dim `numeric` of length 2, the desired dimensions for rows and columns
#' @param asp `numeric` the yx ratio used to calculate `dim` (1 by default).
#' @param byrow `logical` whether to order shapes by rows
#' @param fromtop `logical` whether to order shapes from top
#' @param sample `numeric` number of points to [coo_sample]
#' @param `palette` from [palettes]
#' @param ... additional arguments to feed the main drawer
#' if the number of shapes is > 1000 (default: 64).
#' If non-numeric (eg `FALSE`) do not sample.
#' @param relatively `logical` if `TRUE` use [coo_template_relatively]
#' or, if `FALSE`(by default) [coo_template].
#' In other words, whether to preserve size or not.
#' @param paper_fun a [papers] function (default: `paper`)
#' @param draw_fun one of [drawers] for `pile.list`
#' @param legend `logical` whether to draw a legend (will be improved in further versions)
#' @param template_size `numeric` to feed `coo_template(_relatively)`.
#' Only useful to add padding around shapes when the default value (0.95) is lowered.
#' @return a list of templated and translated shapes
#' @family grindr
#' @examples
#'
#' # On Out ---
#' bot %>% mosaic
#' bot %>% mosaic(~type)
#'
#' # As with other grindr functions you can continue the pipe
#' bot %>% mosaic(~type, asp=0.5) %>% draw_firstpoint
#'
#' # On Opn ---- same grammar
#' olea %>% mosaic(~view+var, paper_fun=paper_dots)
#'
#'  # On Ldk
#'  mosaic(wings, ~group, pal=pal_qual_Dark2, pch=3)
#'
#'  # On Out with different sizes
#'  # would work on other Coo too
#' shapes2 <- shapes
#' sizes <- runif(30, 1, 2)
#' shapes2 %>% mosaic(relatively=FALSE)
#' shapes2 %>% mosaic(relatively=TRUE) %>% draw_centroid()

#' @rdname mosaic
#' @export
mosaic_engine <- function(coo_list,
                          dim, asp=1, byrow = TRUE, fromtop = TRUE,
                          sample=60,
                          relatively=FALSE,
                          template_size=0.92) {
  # some preliminary checks and deductions --
  .check(is.list(coo_list), "'coo_list' must be a list")
  .check(template_size <= 1, "'template_size' must be <= 1")
  # number of shapes
  n <- length(coo_list)
  # downsample if asked
  if (n>200 && is.numeric(sample))
    coo_list <- lapply(coo_list, coo_sample, sample)
  # check feasability of dim, if provided
  if (missing(dim) || is.na(dim)) {
    # if not provided, calculate it
    nc <- ceiling(sqrt(n*asp))
    nr <- ceiling(n/nc)
    dim <- c(nr, nc)
  } else {
    .check(prod(dim)>=n, "dim[1]*dim[2] must be >= length(coo_list)")
    .check(all(dim>=1),  "dim[1] and dim[2] must be >= 1")
  }
  # matrix of positions --
  m_pos <- matrix(1:prod(dim), dim[1], dim[2], byrow=byrow)
  # if fromtop, reverse rows
  if (fromtop & dim[1] > 1) {
    m_pos <- m_pos[dim[1]:1, ]
  }
  # prepare the panel with no margins
  # old <- par(mar=c(rep(0, 3), 4), oma=rep(0, 4))
  # on.exit(par(old))
  # # empty plot of correct dimensions
  # plot(NA, asp = 1,
  #      xlim = c(0, dim[2]+(1-template_size)),
  #      ylim = c(0, dim[1]+(1-template_size)),
  #      ann=FALSE, frame=FALSE, axes=FALSE,
  #      xaxs = "i", yaxs = "i")
  # template shapes
  if (relatively)
    cool <- coo_template_relatively(coo_list, size = template_size)
  else
    cool <- coo_template(coo_list, size = template_size)
  # deduce translation values from position in m
  # and store it as a data.frame
  df_pos <- lapply(1:n, function(i) which(m_pos==i, arr.ind=TRUE)-0.5) %>%
    do.call("rbind", .) %>% as.data.frame()
  # # first row has y=0.5
  # df_pos$row <- df_pos$row - 1
  # translate shapes
  res <-
    lapply(seq_along(cool),
           function(i) coo_trans(cool[[i]], x=df_pos[i, 2], y=df_pos[i, 1]))
  # copy names or dummy them
  if (is.null(names(coo_list)))
    names(res) <- 1:n
  else
    names(res) <- names(coo_list)
  # return this beauty
  return(res)
}

#' @rdname mosaic
#' @export
mosaic <- function(x, ...){
  UseMethod("mosaic")
}

#' @rdname mosaic
#' @export
mosaic.Out <- function(x, f, relatively=FALSE,
                       pal=pal_qual, sample=60,
                       paper_fun=paper_white, draw_fun=draw_outlines, legend=TRUE,
                       dim=NA, asp=1, byrow = TRUE, fromtop = TRUE, ...){
  x$coo %>%
    mosaic_engine(dim=dim, asp=asp,
                  byrow=byrow, fromtop=fromtop,
                  relatively=relatively, sample=sample) %>%
    paper_fun() -> coos

  if (!missing(f)){
    f <- fac_dispatcher(x, f)
    if (legend){
    colors_groups <- pal(nlevels(f))
    list(f=f, colors_groups=colors_groups) %>% layer_legend()
    }
    coos %>% draw_fun(fac_dispatcher(x, f), pal=pal, ...)
  } else {
    coos %>% draw_fun(pal=pal, ...)
  }
}

#' @rdname mosaic
#' @export
mosaic.Opn <- function(x, f, relatively=FALSE,
                       pal=pal_qual, sample=60,
                       paper_fun=paper_white, draw_fun=draw_curves, legend=TRUE,
                       dim=NA, asp=1, byrow = TRUE, fromtop = TRUE, ...){
  x$coo %>%
    mosaic_engine(dim=dim, asp=asp,
                  byrow=byrow, fromtop=fromtop,
                  relatively=relatively, sample=sample) %>%
    paper_fun() -> coos

  if (!missing(f)){
    f <- fac_dispatcher(x, f)
    if (legend){
      colors_groups <- pal(nlevels(f))
      list(f=f, colors_groups=colors_groups) %>% layer_legend()
    }
    coos %>% draw_fun(fac_dispatcher(x, f), pal=pal, ...)
  } else {
    coos %>% draw_fun(pal=pal, ...)
  }
}

#' @rdname mosaic
#' @export
mosaic.Ldk <- function(x, f, relatively=FALSE,
                       pal=pal_qual, sample=60,
                       paper_fun=paper_white, draw_fun=draw_landmarks, legend=TRUE,
                       dim=NA, asp=1, byrow = TRUE, fromtop = TRUE, ...){
  x$coo %>%
    mosaic_engine(dim=dim, asp=asp,
                  byrow=byrow, fromtop=fromtop,
                  relatively=relatively, sample=sample) %>%
    paper_fun() -> coos

  if (!missing(f)){
    f <- fac_dispatcher(x, f)
    if (legend){
      colors_groups <- pal(nlevels(f))
      list(f=f, colors_groups=colors_groups) %>% layer_legend()
    }
    coos %>% draw_fun(fac_dispatcher(x, f), pal=pal, ...)
  } else {
    coos %>% draw_fun(pal=pal, ...)
  }
}
