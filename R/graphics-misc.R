
# 0. Color palettes ------------------------------------------------------------

#' Colors, colors, colors.
#' @name col.summer
#' @title Some color palettes.
#' @usage col.summer(n)
#' col.spring(n)
#' col.autumn(n)
#' col.solarized(n)
#' col.gallus(n)
#' col.blackgallus(n)
#' col.hot(n)
#' col.cold(n)
#' col.sari(n)
#' col.india(n)
#' col.bw(n)
#' @aliases col.spring col.summer col.autumn col.solarized col.gallus col.blackgallus col.hot col.cold col.sari col.india col.bw
#' @param n the number of colors to generate from the color palette
#' @return color codes (hexadecimal format)
#' @keywords graphics
#' @examples
#' barplot(1:10, col=col.summer(10), main="col.summer")
#' barplot(1:10, col=col.spring(10), main="col.spring")
#' barplot(1:10, col=col.autumn(10), main="col.autumn")
#' barplot(1:10, col=col.solarized(10), main="col.solarized")
#' barplot(1:10, col=col.gallus(10), main="col.gallus")
#' barplot(1:10, col=col.blackgallus(10), main="col.blackgallus")
#' barplot(1:10, col=col.hot(10), main="col.hot")
#' barplot(1:10, col=col.cold(10), main="col.cold")
#' barplot(1:10, col=col.sari(10), main="col.sari")
#' barplot(1:10, col=col.india(10), main="col.india")
#' barplot(1:10, col=col.bw(10), main="col.bw")
#' @export
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
#' @export
col.spring <- colorRampPalette(c("#a3baff", "#ffff7f", "#ff9797"))
#' @export
col.autumn <- colorRampPalette(c("#3353b3", "#b1b100", "#b32222"))
#' @export
col.solarized <- colorRampPalette(c("#b58900", "#cb4b16", "#dc322f", "#d33682",
                                    "#6c71c4", "#268bd2", "#2aa198", "#859900"))
#' @export
col.gallus <- colorRampPalette(c("#025D8C", "#FFFFFF", "#A80000"))
#' @export
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
#' @export
col.hot <- colorRampPalette(c("#F2F2F2","#A80000"))
#' @export
col.cold <- colorRampPalette(c("#F2F2F2","#025D8C"))
#' @export
col.sari <- colorRampPalette(c("#551A8B", "#FF7F00"))
#' @export
col.india <- colorRampPalette(c("#FF9933", "#138808"))
#' @export
col.bw <- colorRampPalette(c("#FFFFFF", "#000000"))