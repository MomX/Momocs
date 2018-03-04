# Everything palettes --------------------------------------
# A function to bypass RColorBrewer limitations in terms of number of colors
# eg it is not happy with less than 3,
# and more than maxcolors as defined in RColorBrewer::brewer.pal.info
# here for less than 3 or more than the corresponding value for each palette,
# we simply create a palette on the fly and return the required colors
# It is used to recreate all RColorBrewer palettes below
.pal_brewer <- function(n, name){
  # RColorBrewer always want at least 3 levels
  if (n<3){
    return(colorRampPalette(RColorBrewer::brewer.pal(3, name))(n))
  }
  # But no more than a given value given in brewer.pal.info
  mc <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
  if (n>mc){
    return(colorRampPalette(RColorBrewer::brewer.pal(mc, name))(n))
  } else {
    # If between 3 and max allowed, simply brewer.pal
    return(RColorBrewer::brewer.pal(n, name))
  }
}

#' Color palettes
#'
#' All colorblind friendly RColorBrewer palettes
#' recreated without the number of colors limitation
#' and with transparency support thanks to `pal_alpha` that can be used alone.
#'
#' @param n `numeric` number of colors
#' @param cols color(s) as hexadecimal values
#' @param transp `numeric` between 0 and 1 (0, eg opaque, by default)
#' @note RColorBrewer palettes are not happy when `n` is lower than 3 and above
#' a given number for each palette. If this is the case, these functions will
#' create a color palette with [colorRampPalette] and return colors even so.
#'
#' @aliases pal palette
#' @examples
#' pal_div_BrBG(5) %>% barplot(rep(1, 5), col=.)
#' pal_div_BrBG(5, 0.5) %>% barplot(rep(1, 5), col=.)
#' @name palettes
#' @rdname palettes
#' @export
pal_alpha <- function(cols, transp = 0) {
  alpha.int <- as.integer((1 - transp) * 255)
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex) < 2] <- paste0("0", alpha.hex[nchar(alpha.hex) < 2])
  alpha.hex <- toupper(alpha.hex) # pure cosmetics
  return(paste0(cols, alpha.hex))
}

# # below, generated with
# df <- RColorBrewer::brewer.pal.info %>%
#   mutate(name=rownames(.)) %>%
#   filter(colorblind==TRUE) %>%
#   select(category, name)
# paste0("#' @rdname palettes\n",
#        "#' @export\n",
#        "pal", "_", df$category, "_", df$name,
#        " <- function(n, transp=0){
#           .pal_brewer(n, '", df$name, "') %>%
#            pal_alpha(transp=transp)
# }", "\n") %>%
#   cat(sep="\n")

#' @rdname palettes
#' @export
pal_div_BrBG <- function(n, transp=0){
  .pal_brewer(n, 'BrBG') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PiYG <- function(n, transp=0){
  .pal_brewer(n, 'PiYG') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PRGn <- function(n, transp=0){
  .pal_brewer(n, 'PRGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PuOr <- function(n, transp=0){
  .pal_brewer(n, 'PuOr') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_RdBu <- function(n, transp=0){
  .pal_brewer(n, 'RdBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_RdYlBu <- function(n, transp=0){
  .pal_brewer(n, 'RdYlBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Dark2 <- function(n, transp=0){
  .pal_brewer(n, 'Dark2') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Paired <- function(n, transp=0){
  .pal_brewer(n, 'Paired') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Set2 <- function(n, transp=0){
  .pal_brewer(n, 'Set2') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Blues <- function(n, transp=0){
  .pal_brewer(n, 'Blues') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_BuGn <- function(n, transp=0){
  .pal_brewer(n, 'BuGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_BuPu <- function(n, transp=0){
  .pal_brewer(n, 'BuPu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_GnBu <- function(n, transp=0){
  .pal_brewer(n, 'GnBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Greens <- function(n, transp=0){
  .pal_brewer(n, 'Greens') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Greys <- function(n, transp=0){
  .pal_brewer(n, 'Greys') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Oranges <- function(n, transp=0){
  .pal_brewer(n, 'Oranges') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_OrRd <- function(n, transp=0){
  .pal_brewer(n, 'OrRd') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuBu <- function(n, transp=0){
  .pal_brewer(n, 'PuBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuBuGn <- function(n, transp=0){
  .pal_brewer(n, 'PuBuGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuRd <- function(n, transp=0){
  .pal_brewer(n, 'PuRd') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Purples <- function(n, transp=0){
  .pal_brewer(n, 'Purples') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_RdPu <- function(n, transp=0){
  .pal_brewer(n, 'RdPu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Reds <- function(n, transp=0){
  .pal_brewer(n, 'Reds') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlGn <- function(n, transp=0){
  .pal_brewer(n, 'YlGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlGnBu <- function(n, transp=0){
  .pal_brewer(n, 'YlGnBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlOrBr <- function(n, transp=0){
  .pal_brewer(n, 'YlOrBr') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlOrRd <- function(n, transp=0){
  .pal_brewer(n, 'YlOrRd') %>%
    pal_alpha(transp=transp)
}

#TODO
# col_grey
# col_solarized()
# col_viridis() viridisLite::viridis.map
