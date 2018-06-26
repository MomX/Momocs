
#' Morphospace layers
#'
#' Used internally by [plot_PCA], [plot_LDA], etc. but may be useful elsewhere.
#'
#' @param x layered [PCA] or [LDA]. Typically, the object returned by [plot_PCA] and [plot_LDA]
#' @param position one of `range, full, circle, xy, range_axes, full_axes` to feed [morphospace_positions] (default: `range`)
#' @param nb `numeric` total number of shapes when `position="circle"` (default: `12`)
#' @param nr `numeric` number of rows to position shapes (default: `6`)
#' @param nc `numeric` number of columns to position shapes (default `5`)
#' @param rotate `numeric` angle (in radians) to rotate shapes when displayed on the morphospace (default: `0`)
#' @param size `numeric` size to use to feed [coo_template] (default: `0.9`)
#' @param col color to draw shapes (default: `#999999`)
#' @param flipx `logical` whether to flip shapes against the x-axis (default: `FALSE`)
#' @param flipy `logical` whether to flip shapes against the y-axis (default: `FALSE`)
#' @param draw `logical` whether to draw shapes (default: `TRUE`)
#' @rdname layers_morphospace
#' @name layers_morphospace
#' @family grindr
#' @export
layer_morphospace_PCA <- function(x,
                                  position=c("range", "full", "circle",
                                             "xy", "range_axes", "full_axes")[1],
                                  nb=12, nr=6, nc=5,
                                  rotate=0, size=0.9,
                                  col="#999999",
                                  flipx=FALSE, flipy=FALSE, draw=TRUE){
  # shortcut for useful components
  xy <- x$xy
  rot <- x$rotation
  mshape <- x$mshape
  method <- x$method

  # number of methods must be <=4
  # message and propagate anyway
  if (is.null(method) || length(method)>4) {
    message("layer_morphospace_PCA needs a $method of length <= 4")
    invisible(x)
  }

  # position of shapes to reconstruct
  pos <- morphospace_positions(xy=xy, pos.shp = position,
                               nb.shp = nb, nr.shp = nr, nc.shp = nc)

  # according to the type of morphometric method,
  # switch the inverse method and the way we draw shapes
  # (ie draw_outline, draw_curve, draw_landmarks for Out, Opn, Ldk, respectively).
  #
  # when the object combines different morphometric approaches (up to 4)
  # their size is divided by 2 and the shapes and translated (of d)
  # from the (x; y) coordinates from pos.shp,
  # of a distance d below and towards the appropriate direction

  # deduce the number of methods and
  # and recycle shorter arguments
  ml <- length(method)
  if (length(rotate) != ml)
    rotate <- rep(rotate[1], ml)
  if (length(flipx) != ml)
    flipx <- rep(flipx[1], ml)
  if (length(flipy) != ml)
    flipy <- rep(flipy[1], ml)
  if (length(size) != ml)
    size <- rep(size[1], ml)

  # calculate the final templated size for shapes
  # divide by two if arranged, eg if more than one shape
  wdw <- .wdw() %>% max()
  size <- (size*wdw/14) / ifelse(ml==1, 1, 2)

  # defines the x and y translation distances
  # for every sub-morphoshape
  d <- mean(size) / 2 # gap distance
  if (ml==1){
    dx <- 0
    dy <- 0
  }
  if (ml==2){ #met1 on top of met2 - h center
    dx <- c(0, 0)
    dy <- c(d, -d)
  }
  if (ml==3){ #podium arrangement
    dx <- c(0, -d, d)
    dy <- c(d, -d, -d)
  }
  if (ml==4){ #form top left, clockwise
    dx <- c(-d, d, -d, d)
    dy <- c(d, d, -d, -d)
  }

  # indices of coe partition to successively use
  if (ml==1){
    col.start <- 1
    col.end   <- length(mshape)
  } else {
    col.start <- cumsum(x$cuts) - x$cuts + 1
    col.end   <- cumsum(x$cuts)
  }

  # to store the shapes
  SHP <- vector("list", length(method)) %>% `names<-`(method)

  ### loop over method and calculate reconstructed shapes
  for (i in seq_along(method)){
    # to store local results
    shp <- vector("list", nrow(pos))
    ids <- col.start[i]:col.end[i]
    mi <- method[i]

    # deduce the plotting method
    if (grepl("(e|r|s|t)fourier", mi))
      draw_method <- draw_outline
    if (grepl("(dfourier)|(poly)", mi))
      draw_method <- draw_curve
    if (grepl("procrustes", mi))
      draw_method <- draw_landmarks

    # method dispatch
    # efourier
    if (mi == "efourier")
      shp <- PCA2shp_efourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # rfourier
    if (mi == "rfourier")
      shp <- PCA2shp_rfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # sfourier
    if (mi == "sfourier")
      shp <- PCA2shp_sfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # tfourier
    if (mi == "tfourier")
      shp <- PCA2shp_tfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # dfourier
    if (mi == "dfourier")
      shp <- PCA2shp_dfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids])
    # opoly
    if (mi == "opoly")
      shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 ortho = TRUE,
                                 baseline1 = x$baseline1[1:2 + (i-1)*2],
                                 baseline2 = x$baseline2[1:2 + (i-1)*2])
    # npoly
    if (mi == "npoly")
      shp <- PCA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 ortho = FALSE,
                                 baseline1 = x$baseline1[1:2 + (i-1)*2],
                                 baseline2 = x$baseline2[1:2 + (i-1)*2])
    # landmarks
    if (mi == "procrustes")
      shp <- PCA2shp_procrustes(pos = pos, rot = rot[ids, ], mshape=mshape[ids])

    # reconstructed shapes are now waiting in shp
    # for templating, translating and friends
    shp %<>%
      # template to the final size
      lapply(coo_template, size = size[i]) %>%
      # coo_template does not center shapes but the bounding box
      lapply(coo_center) %>%
      # rotate shapes
      lapply(coo_rotate, rotate[i])
    # flip (if required)
    if (flipx[i]) shp %<>% lapply(coo_flipx)
    if (flipy[i]) shp %<>% lapply(coo_flipy)
    # finally translate shapes
    if (draw) { # to translate only for morphospace PCA, not PCcontrib, etc.
      shp %<>%
        seq_along() %>%
        lapply(function(.) coo_trans(shp[[.]], pos[., 1] + dx[i], pos[., 2] + dy[i]))
    }
    SHP[[i]] <- shp

    # finally draw the morphospace
    if (draw){
      lapply(shp, draw_method, col=col)
# special case when links
      if (is_links(x)){
        lapply(shp, function(.x) draw_links(.x, links=x$links, col=col))
      }
    }
    # if (!is.null(PCA$links)) lapply(shp, function(x) ldk_links(x, PCA$links, col="grey90"))
  }
  # propagate
  if (draw)
    invisible(x)
  else
    return(SHP)
}

#' @rdname layers_morphospace
#' @name layers_morphospace
#' @family grindr
#' @export
layer_morphospace_LDA <- function(x,
                                  position=c("range", "full", "circle",
                                             "xy", "range_axes", "full_axes")[1],
                                  nb=12, nr=6, nc=5,
                                  rotate=0, size=0.9,
                                  col="#999999",
                                  flipx=FALSE, flipy=FALSE, draw=TRUE){
  message("* layer_morphospace_LDA is back, but experimental")

  # shortcut for useful components
  xy <- x$xy
  rot <- x$rotation
  mshape <- x$mshape
  method <- x$method

  # number of methods must be <=4
  # message and propagate anyway
  if (is.null(method) || length(method)>4) {
    message("layer_morphospace_LDA needs a $method of length <= 4")
    invisible(x)
  }

  # position of shapes to reconstruct
  pos <- morphospace_positions(xy=xy, pos.shp = position,
                               nb.shp = nb, nr.shp = nr, nc.shp = nc)

  # according to the type of morphometric method,
  # switch the inverse method and the way we draw shapes
  # (ie draw_outline, draw_curve, draw_landmarks for Out, Opn, Ldk, respectively).
  #
  # when the object combines different morphometric approaches (up to 4)
  # their size is divided by 2 and the shapes and translated (of d)
  # from the (x; y) coordinates from pos.shp,
  # of a distance d below and towards the appropriate direction

  # deduce the number of methods and
  # and recycle shorter arguments
  ml <- length(method)
  if (length(rotate) != ml)
    rotate <- rep(rotate[1], ml)
  if (length(flipx) != ml)
    flipx <- rep(flipx[1], ml)
  if (length(flipy) != ml)
    flipy <- rep(flipy[1], ml)
  if (length(size) != ml)
    size <- rep(size[1], ml)

  # calculate the final templated size for shapes
  # divide by two if arranged, eg if more than one shape
  wdw <- .wdw() %>% max()
  size <- (size*wdw/14) / ifelse(ml==1, 1, 2)

  # defines the x and y translation distances
  # for every sub-morphoshape
  d <- mean(size) / 2 # gap distance
  if (ml==1){
    dx <- 0
    dy <- 0
  }
  if (ml==2){ #met1 on top of met2 - h center
    dx <- c(0, 0)
    dy <- c(d, -d)
  }
  if (ml==3){ #podium arrangement
    dx <- c(0, -d, d)
    dy <- c(d, -d, -d)
  }
  if (ml==4){ #form top left, clockwise
    dx <- c(-d, d, -d, d)
    dy <- c(d, d, -d, -d)
  }

  # indices of coe partition to successively use
  if (ml==1){
    col.start <- 1
    col.end   <- length(mshape)
  } else {
    col.start <- cumsum(x$cuts) - x$cuts + 1
    col.end   <- cumsum(x$cuts)
  }

  # to store the shapes
  SHP <- vector("list", length(method)) %>% `names<-`(method)

  ### loop over method and calculate reconstructed shapes
  for (i in seq_along(method)){
    # to store local results
    shp <- vector("list", nrow(pos))
    ids <- col.start[i]:col.end[i]
    mi <- method[i]

    # deduce the plotting method
    if (grepl("(e|r|s|t)fourier", mi))
      draw_method <- draw_outline
    if (grepl("(dfourier)|(poly)", mi))
      draw_method <- draw_curve
    if (grepl("procrustes", mi))
      draw_method <- draw_landmarks

    # method dispatch
    # efourier
    if (mi == "efourier")
      shp <- LDA2shp_efourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # rfourier
    if (mi == "rfourier")
      shp <- LDA2shp_rfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # sfourier
    if (mi == "sfourier")
      shp <- LDA2shp_sfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # tfourier
    if (mi == "tfourier")
      shp <- LDA2shp_tfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids]) %>% lapply(coo_close)
    # dfourier
    if (mi == "dfourier")
      shp <- LDA2shp_dfourier(pos = pos, rot = rot[ids, ], mshape = mshape[ids])
    # opoly
    if (mi == "opoly")
      shp <- LDA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 ortho = TRUE,
                                 baseline1 = x$baseline1[1:2 + (i-1)*2],
                                 baseline2 = x$baseline2[1:2 + (i-1)*2])
    # npoly
    if (mi == "npoly")
      shp <- LDA2shp_polynomials(pos = pos, rot = rot[ids, ], mshape = mshape[ids],
                                 ortho = FALSE,
                                 baseline1 = x$baseline1[1:2 + (i-1)*2],
                                 baseline2 = x$baseline2[1:2 + (i-1)*2])
    # landmarks
    if (mi == "procrustes")
      shp <- LDA2shp_procrustes(pos = pos, rot = rot[ids, ])

    # reconstructed shapes are now waiting in shp
    # for templating, translating and friends
    shp %<>%
      # template to the final size
      lapply(coo_template, size = size[i]) %>%
      # coo_template does not center shapes but the bounding box
      lapply(coo_center) %>%
      # rotate shapes
      lapply(coo_rotate, rotate[i])
    # flip (if required)
    if (flipx[i]) shp %<>% lapply(coo_flipx)
    if (flipy[i]) shp %<>% lapply(coo_flipy)
    # finally translate shapes
    if (draw) { # to translate only for morphospace PCA, not PCcontrib, etc.
      shp %<>%
        seq_along() %>%
        lapply(function(.) coo_trans(shp[[.]], pos[., 1] + dx[i], pos[., 2] + dy[i]))
    }
    SHP[[i]] <- shp

    # finally draw the morphospace
    if (draw)
      lapply(shp, draw_method, col=col)
    # if (!is.null(PCA$links)) lapply(shp, function(x) ldk_links(x, PCA$links, col="grey90"))
  }
  # propagate
  if (draw)
    invisible(x)
  else
    return(SHP)
}
