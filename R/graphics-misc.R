##### graphics cosmetics

#' Colors, colors, colors.
#' @name col.summer
#' @title Some color palettes.
#' @rdname color_palettes
#' @param n the number of colors to generate from the color palette
#' @return colors (hexadecimal format)
#' @keywords Graphics
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
col.gallus <- colorRampPalette(c("#025D8C", "#FFFFFF", "#A80000"))
#' @rdname color_palettes
#' @export
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
#' @rdname color_palettes
#' @export
col.hot <- colorRampPalette(c("#F2F2F2","#A80000"))
#' @rdname color_palettes
#' @export
col.cold <- colorRampPalette(c("#F2F2F2","#025D8C"))
#' @rdname color_palettes
#' @export
col.sari <- colorRampPalette(c("#551A8B", "#FF7F00"))
#' @rdname color_palettes
#' @export
col.india <- colorRampPalette(c("#FF9933", "#138808"))
#' @rdname color_palettes
#' @export
col.bw <- colorRampPalette(c("#FFFFFF", "#000000"))

##### end colors graphics