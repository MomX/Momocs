##### graphics cosmetics

#' Colors, colors, colors.
#' @name col.summer
#' @title Some color palettes.
#' @rdname color_palettes
#' @aliases Palettes
#' @param n the number of colors to generate from the color palette
#' @return colors (hexadecimal format)
#' @note 
#' \itemize{
#' \item \code{col.solarized} is based on Solarized: \url{http://ethanschoonover.com/solarized}; 
#' \item \code{col.div} on ColorBrewer2: \url{http://colorbrewer2.org/};
#' \item \code{col.qual}, \code{col.heat}, \code{col.cold} and \code{col.gallus} use the colorspace package.
#' }
#' @keywords Graphics
#' @examples
#' 
#' wheel <- function(palette, n=10){
#'  op <- par(mar=rep(0, 4)) ; on.exit(par(op))
#'  pie(rep(1, n), col=palette(n), labels=NA, clockwise=TRUE)}
#'  
#'  # Qualitative
#'  wheel(col.qual)
#'  wheel(col.solarized)
#'  wheel(col.summer)
#'  wheel(col.summer2)
#'  wheel(col.spring)
#'  wheel(col.autumn)
#'  
#'  # Divergent
#'  wheel(col.gallus)
#'  wheel(col.india)
#'  
#'  # Sequential
#'  wheel(col.heat)
#'  wheel(col.hot)
#'  wheel(col.cold)
#'  wheel(col.sari)
#'  wheel(col.bw)
#'  wheel(col.grey)
#'  
#'  # Black only for pubs
#'  wheel(col.black)
#' @export
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
#' @rdname color_palettes
#' @export
col.summer2 <- colorRampPalette(c("#781C81", "#413B93", "#4065B1", 
    "#488BC2", "#55A1B1", "#63AD99", "#7FB972", "#B5BD4C", "#D9AD3C", 
    "#E68E34", "#E6642C", "#D92120"))
#' @rdname color_palettes
#' @export
col.spring <- colorRampPalette(c("#a3baff", "#ffff7f", "#ff9797"))
#' @rdname color_palettes
#' @export
col.autumn <- colorRampPalette(c("#3353b3", "#b1b100", "#b32222"))

#' @rdname color_palettes
#' @export
col.black <- colorRampPalette(c("#000000", "#000000"))

### solarized
#' @rdname color_palettes
#' @export
col.solarized <- colorRampPalette(c(
# "#002b36", 
# "#073642", 
# "#586e75", 
# "#657b83", 
# "#839496", 
# "#93a1a1", 
# "#eee8d5", 
# "#fdf6e3", 
# "#b58900",
#"#cb4b16", 
"#dc322f", 
"#d33682", 
"#6c71c4", 
"#268bd2", 
"#2aa198", 
"#859900"))
  
### colorspace
#' @rdname color_palettes
#' @export
col.gallus <- function(n) {
    return(diverge_hcl(n))
}
#' @rdname color_palettes
#' @export
col.qual <- function(n) {
  rainbow_hcl(n, c = 90)}

#' @rdname color_palettes
#' @export
col.heat <- function(n) {
  return(rev(heat_hcl(n, h=c(10, 90), c.=c(160, 40))))
}
#' @rdname color_palettes
#' @export
col.hot <- colorRampPalette(c("#FFFFFF", "#8E063B"))
#' @rdname color_palettes
#' @export
col.cold <- colorRampPalette(c("#FFFFFF", "#023FA5"))
#' @rdname color_palettes
#' @export
col.sari <- colorRampPalette(c("#551A8B", "#47A23E", "#FF7F00"))
#' @rdname color_palettes
#' @export
col.india <- colorRampPalette(c("#FF9933", "#FFFFFF", "#138808"))
#' @rdname color_palettes
#' @export
col.bw <- colorRampPalette(c("#000000", "#FFFFFF"))
#' @rdname color_palettes
#' @export
col.grey <- colorRampPalette(c("#B3B3B3", "#4D4D4D"))

#' Transparency helpers and palettes
#' 
#' To ease transparency handling.
#' @param n the number of colors to generate
#' @param col a color in hexadecimal format on which to generate levels of transparency
#' @param ceiling the maximal opacity (from 0 to 1)
#' @param cols on or more colors, provided as hexadecimal values
#' @param transp numeric between 0 and 1, the value of the transparency to obtain
#' @rdname colors_transp
#' @keywords Graphics
#' @examples
#' x <- col.transp(10, col='#000000')
#' x
#' barplot(1:10, col=x, main='a transparent black is grey')
#' 
#' summer10 <- col.summer(10)
#' summer10
#' summer10.transp8 <- .transp(summer10, 0.8)
#' summer10.transp8
#' summer10.transp2 <- .transp(summer10, 0.8)
#' summer10.transp2
#' x <- 1:10
#' barplot(x, col=summer10.transp8)
#' barplot(x/2, col=summer10.transp2, add=TRUE)
#' @export
col.transp <- function(n, col = "#000000", ceiling = 1) {
    alpha.int <- as.integer(seq(0, 255 * ceiling, length = n))
    alpha.hex <- as.character(as.hexmode(alpha.int))
    alpha.hex[nchar(alpha.hex) < 2] <- paste0("f", alpha.hex[nchar(alpha.hex) < 2])
    return(paste0(col, alpha.hex))
}
#' @rdname colors_transp
#' @export
.transp <- function(cols, transp = 0) {
    alpha.int <- as.integer((1 - transp) * 255)
    alpha.hex <- as.character(as.hexmode(alpha.int))
    alpha.hex[nchar(alpha.hex) < 2] <- paste0("0", alpha.hex[nchar(alpha.hex) < 
        2])
    return(paste0(cols, alpha.hex))
}
#' @rdname colors_transp
#' @export
col.alpha <- .transp

#' @export
.cex <- function(x) {
    3/(log(x + 1) + 1)
}

#' @export
.pch <- function() { 
  c(2, 6, 1, 3, 4, 5, 8, 7, 9, 10)}
                  
##### end colors graphics 
