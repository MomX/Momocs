##### graphics cosmetics

#' Colors, colors, colors.
#' @name col.summer
#' @title Some color palettes.
#' @rdname color_palettes
#' @aliases Palettes
#' @param n the number of colors to generate from the color palette
#' @return colors (hexadecimal format)
#' @note \code{col.solarized} is based on Solarized: \url{http://ethanschoonover.com/solarized}; 
#' \code{col.div} on ColorBrewer2: \url{http://colorbrewer2.org/}; \code{col.gallus2} is based on
#' Sron \url{http://www.sron.nl/~pault/colourschemes.pdf}
#' @keywords Graphics
#' @examples
#' barplot(1:20, col=col.summer(20), main="col.summer")
#' barplot(1:20, col=col.summer2(20), main="col.summer2")
#' barplot(1:20, col=col.spring(20), main="col.spring")
#' barplot(1:20, col=col.autumn(20), main="col.autumn")
#' barplot(1:20, col=col.solarized(20), main="col.solarized")
#' barplot(1:20, col=col.gallus(20), main="col.gallus")
#' barplot(1:20, col=col.gallus2(20), main="col.gallus2")
#' barplot(1:20, col=col.blackgallus(20), main="col.blackgallus")
#' barplot(1:20, col=col.hot(20), main="col.hot")
#' barplot(1:20, col=col.cold(20), main="col.cold")
#' barplot(1:20, col=col.sari(20), main="col.sari")
#' barplot(1:20, col=col.india(20), main="col.india")
#' barplot(1:20, col=col.bw(20), main="col.bw")
#' barplot(1:20, col=col.div(20), main="col.div")
#' @export
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
#' @rdname color_palettes
#' @export
col.spring <- colorRampPalette(c("#a3baff", "#ffff7f", "#ff9797"))
#' @rdname color_palettes
#' @export
col.autumn <- colorRampPalette(c("#3353b3", "#b1b100", "#b32222"))
#' @rdname color_palettes
#' @export
col.solarized <- colorRampPalette(c("#b58900", "#cb4b16", "#dc322f", "#d33682",
                                    "#6c71c4", "#268bd2", "#2aa198", "#859900"))
#' @rdname color_palettes
#' @export
col.gallus <- colorRampPalette(c("#0000FF", "#FFFFFF", "#CC0000"))
#' @rdname color_palettes
#' @export
col.blackgallus <- colorRampPalette(c("#0000FF", "#272727", "#CC0000"))
#' @rdname color_palettes
#' @export
col.hot <- colorRampPalette(c("#F2F2F2","#CC0000"))
#' @rdname color_palettes
#' @export
col.cold <- colorRampPalette(c("#F2F2F2","#0000FF"))
#' @rdname color_palettes
#' @export
col.sari <- colorRampPalette(c("#551A8B", "#FF7F00"))
#' @rdname color_palettes
#' @export
col.india <- colorRampPalette(c("#FF9933", "#138808"))
#' @rdname color_palettes
#' @export
col.bw <- colorRampPalette(c("#000000", "#FFFFFF"))
#' @rdname color_palettes
#' @export
col.gw <- colorRampPalette(c("#E5E5E5", "#1A1A1A"))
#' @rdname color_palettes
#' @export
col.div <- colorRampPalette(c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                              "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"))
#' @rdname color_palettes
#' @export
col.gallus2 <- colorRampPalette(c("#3D52A1", "#3A89C9", "#77B7E5", "#B4DDF7",
                                  "#E6F5FE", "#FFFAD2", "#FFE3AA", "#F9BD7E",
                                  "#ED875E", "#D24D3E", "#AE1C3E")) 
#' @rdname color_palettes
#' @export
col.summer2 <- colorRampPalette(c("#781C81", "#413B93", "#4065B1", "#488BC2",
                                  "#55A1B1", "#63AD99", "#7FB972", "#B5BD4C",
                                  "#D9AD3C", "#E68E34", "#E6642C", "#D92120"))

#' Transparency helpers and palettes
#' 
#' To ease transparency handling.
#' @param n the number of colors to generate
#' @param col a color in hexadecimal format on which to generate levels of transparency
#' @param ceiling the maximal opacity (from 0 to 1)
#' @param cols on or more colors, provided as hexadecimal values
#' @param transp numeric between 0 and 1, the value of the transparency to obtain
#' @rdname colors_transp
#' @examples
#' x <- col.transp(10, col="#000000")
#' x
#' barplot(1:10, col=x, main="a transparent black is grey")
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
col.transp <- function(n, col="#000000", ceiling=1){
  alpha.int <- as.integer(seq(0, 255*ceiling, length=n))
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex)<2] <- paste0("f", alpha.hex[nchar(alpha.hex)<2])
  return(paste0(col, alpha.hex))}
#' @rdname colors_transp
#' @export
.transp <- function(cols, transp=0){
  alpha.int <- as.integer((1-transp)*255)
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex)<2] <- paste0("0", alpha.hex[nchar(alpha.hex)<2])
  return(paste0(cols, alpha.hex))}

#' @export
.cex <- function(x){3/(log(x+1)+1)}
##### end colors graphics