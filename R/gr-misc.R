##### graphics cosmetics

# Palettes -------------------------------------------------
#' Some color palettes
#'
#' Colors, colors, colors.
#' @aliases Palettes
#' @param n the number of colors to generate from the color palette
#' @return colors (hexadecimal format)
#' @note
#' Among available color palettes, \code{col_solarized} is based on Solarized: \url{https://ethanschoonover.com/solarized/};
#' \code{col_div}, \code{col_qual}, \code{col_heat}, \code{col_cold}
#' and \code{col_gallus} are based on on ColorBrewer2: \url{https://colorbrewer2.org/}.
#'
#' @examples
#'
#' wheel <- function(palette, n=10){
#'  op <- par(mar=rep(0, 4)) ; on.exit(par(op))
#'  pie(rep(1, n), col=palette(n), labels=NA, clockwise=TRUE)}
#'
#'  # Qualitative
#'  wheel(col_qual)
#'  wheel(col_solarized)
#'  wheel(col_summer)
#'  wheel(col_summer2)
#'  wheel(col_spring)
#'  wheel(col_autumn)
#'
#'  # Divergent
#'  wheel(col_gallus)
#'  wheel(col_india)
#'
#'  # Sequential
#'  wheel(col_heat)
#'  wheel(col_hot)
#'  wheel(col_cold)
#'  wheel(col_sari)
#'  wheel(col_bw)
#'  wheel(col_grey)
#'
#'  # Black only for pubs
#'  wheel(col_black)
#' @name color_palettes
#' @rdname color_palettes
#' @export
col_summer <- grDevices::colorRampPalette(c(
  "#4876FF",
  "#FFFF00",
  "#FF3030"
))

#' @rdname color_palettes
#' @export
col_summer2 <- grDevices::colorRampPalette(c(
  "#781C81",
  "#413B93",
  "#4065B1",
  "#488BC2",
  "#55A1B1",
  "#63AD99",
  "#7FB972",
  "#B5BD4C",
  "#D9AD3C",
  "#E68E34",
  "#E6642C",
  "#D92120"
))

#' @rdname color_palettes
#' @export
col_spring <- grDevices::colorRampPalette(c(
  "#a3baff",
  "#ffff7f",
  "#ff9797"
))

#' @rdname color_palettes
#' @export
col_autumn <- grDevices::colorRampPalette(c(
  "#3353b3",
  "#b1b100",
  "#b32222"
))

#' @rdname color_palettes
#' @export
col_black <- grDevices::colorRampPalette(c(
  "#000000",
  "#000000"
))

### solarized
#' @rdname color_palettes
#' @export
col_solarized <- grDevices::colorRampPalette(c(
  "#dc322f",
  "#d33682",
  "#6c71c4",
  "#268bd2",
  "#2aa198",
  "#859900"
))

### colorspace
#' @rdname color_palettes
#' @export
col_gallus <- grDevices::colorRampPalette(c(
  "#d7191c",
  "#fdae61",
  "#ffffbf",
  "#abd9e9",
  "#2c7bb6"
))

#' @rdname color_palettes
#' @export
col_qual <- grDevices::colorRampPalette(c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f",
  "#ff7f00",
  "#cab2d6",
  "#6a3d9a"))

#' @rdname color_palettes
#' @export
col_heat <- grDevices::colorRampPalette(c(
  "#ffffb2",
  "#fecc5c",
  "#fd8d3c",
  "#f03b20",
  "#bd0026"
))

#' @rdname color_palettes
#' @export
col_hot <- grDevices::colorRampPalette(c(
  "#fee5d9",
  "#fcae91",
  "#fb6a4a",
  "#de2d26",
  "#a50f15"))

#' @rdname color_palettes
#' @export
col_cold <- grDevices::colorRampPalette(c(
  "#f2f0f7",
  "#cbc9e2",
  "#9e9ac8",
  "#756bb1",
  "#54278f"
))

#' @rdname color_palettes
#' @export
col_sari <- grDevices::colorRampPalette(c(
  "#551A8B",
  "#47A23E",
  "#FF7F00"
))
#' @rdname color_palettes
#' @export
col_india <- grDevices::colorRampPalette(c(
  "#FF9933",
  "#FFFFFF",
  "#138808"
))
#' @rdname color_palettes
#' @export
col_bw <- grDevices::colorRampPalette(c(
  "#000000",
  "#FFFFFF"
))
#' @rdname color_palettes
#' @export
col_grey <- grDevices::colorRampPalette(c(
  "#B3B3B3",
  "#4D4D4D"
))

#' Transparency helpers and palettes
#'
#' To ease transparency handling.
#' @param n the number of colors to generate
#' @param col a color in hexadecimal format on which to generate levels of transparency
#' @param ceiling the maximal opacity (from 0 to 1)
#' @param cols on or more colors, provided as hexadecimal values
#' @param transp numeric between 0 and 1, the value of the transparency to obtain
#' @rdname color_transparency
#' @return colors
#' @examples
#' x <- col_transp(10, col='#000000')
#' x
#' barplot(1:10, col=x, main='a transparent black is grey')
#'
#' summer10 <- col_summer(10)
#' summer10
#' summer10.transp8 <- col_alpha(summer10, 0.8)
#' summer10.transp8
#' summer10.transp2 <- col_alpha(summer10, 0.8)
#' summer10.transp2
#' x <- 1:10
#' barplot(x, col=summer10.transp8)
#' barplot(x/2, col=summer10.transp2, add=TRUE)
#' @export
col_transp <- function(n, col = "#000000", ceiling = 1) {
  alpha.int <- as.integer(seq(0, 255 * ceiling, length = n))
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex) < 2] <- paste0("f", alpha.hex[nchar(alpha.hex) < 2])
  return(paste0(col, alpha.hex))
}

#' @rdname color_transparency
#' @export
col_alpha <- function(cols, transp = 0) {
  alpha.int <- as.integer((1 - transp) * 255)
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex) < 2] <- paste0("0", alpha.hex[nchar(alpha.hex) <
                                                             2])
  return(paste0(cols, alpha.hex))
}

# utils ----------------------------------------------------
# is_palette <- function(x){
#   any(class(x)=="palette")
# }
#
# as_palette <- function(x){
#   class(x) <- unique(c(class(x), "palette"))
#   x
# }

.cex <- function(x) {
  3/(log(x + 1) + 1)
}

.pch <- function() {
  c(2, 6, 1, 3, 4, 5, 8, 7, 9, 10)}


##### end colors graphics
