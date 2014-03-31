
################################################################################
# 1. distance calculations
# --------------
################################################################################
ed             <- function(pt1, pt2){return(sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2))}

edi <- function(pt1, pt2, r=0.5){
  return(r*(pt2-pt1) + pt1) }

edm            <- function(m1, m2){return(sqrt((m1[, 1] - m2[, 1])^2 + (m1[, 2] - m2[, 2])^2))}

edm.nearest <- function(m1, m2, full=FALSE){
  if (!is.matrix(m1) | !is.matrix(m2)) stop("Matrices must be provided")
  if (ncol(m1)!=2    | ncol(m2)!=2)    stop("2-cols matrices must be provided")
  nr <- nrow(m1)
  pos <- d  <- numeric(nr)
  for (i in 1:nr){
    m1.i   <- m1[i, ]
    di     <- apply(m2, 1, function(x) sqrt(sum((x - m1.i)^2)))
    d[i]   <- min(di)
    pos[i] <- which.min(di)}
  if (full) return(list(d=d, pos=pos)) else return(d) }

# Color Palettes ---------------------------------------------------------------

col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
col.summer2 <- colorRampPalette(c("#66c2a5", "#fc8d62", "#8da0cb",
                                  "#e78ac3", "#a6d854", "#ebad1f"))
col.solarized <- colorRampPalette(c("#b58900", "#cb4b16", "#dc322f", "#d33682",
                                    "#6c71c4", "#268bd2", "#2aa198", "#859900"))
col.gallus <- colorRampPalette(c("#000080", "#FFFFFF", "#EE0000"))
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
col.sari   <- colorRampPalette(c("#551A8B", "#FF7F00"))
col.india  <- colorRampPalette(c("#FF9933", "#138808"))
col.bw     <- colorRampPalette(c("#FFFFFF", "#000000"))
col.wcol   <- function(col.hex) colorRampPalette(c("#FFFFFF", col.hex))
col.bcol   <- function(col.hex) colorRampPalette(c("#000000", col.hex))
