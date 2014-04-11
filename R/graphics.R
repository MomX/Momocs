
# 3. graphical functions and utilities  ---------------------------------------

coo.plot <- function(coo, xlim, ylim, border="#333333", col="#33333322", lwd=1, lty=1,
                     points=FALSE, first.point=TRUE, centroid=TRUE, xy.axis=TRUE,
                     pch=1, cex=0.5, main, plot.new=TRUE){
  coo <- coo.check(coo)
  if (plot.new) {
    # we setup coo.plot graphical parameters
    op <- par(mar=c(3, 3, 2, 1))
    on.exit(par(op))
    if (!missing(xlim) | !missing(ylim)) {
      if (missing(xlim)){ xlim <- ylim } else {ylim <- xlim }
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE,
           xlim=xlim, ylim=ylim)
    } else {
      plot(coo, type="n", asp=1,  las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)}
    if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}}
  polygon(coo, col=col, border=NA)
  lines(coo, col=border, lwd=lwd, lty=lty)
  # we handle coordinate points
  if (missing(points)) { if (nrow(coo)<=100) points(coo, pch=pch, cex=cex, col=border)}
  if (points) { points(coo, pch=pch, cex=cex, col=border) }
  if (first.point) {points(coo[1, 1], coo[1, 2], col = border, pch=20)}
  if (centroid) {
    cent <- coo.centpos(coo)
    points(cent[1], cent[2], pch=3, col=border, cex=cex)}
  if (!missing(main)) title(main=main)} 

coo.draw <- function(coo, ...){
  coo.plot(coo, plot.new=FALSE, ...)}

#very prototypic
coo.lolliplot <- function(coo1, coo2, type=c("lolli", "arrow")[1]){
  wdw <- apply(rbind(coo1, coo2), 2, function(x) max(abs(x)))
  plot(NA, xlim=c(-wdw[1], wdw[1]), ylim=c(-wdw[2], wdw[2]), asp=1)
  for (i in 1:nrow(coo1)){
    segments(coo1[i, 1], coo1[i, 2], coo2[i, 1], coo2[i, 2])
  }
  points(coo2, pch=20, cex=0.8)}

coo.template   <- function(coo, size=1) {
  # only for matrices
  coo      <- coo * min(size/apply(coo, 2, function(x) diff(range(x))))
  expected <- apply(coo, 2, function(x) diff(range(x)))/2
  observed <- apply(coo, 2, range)[2, ]
  shift    <-  expected - observed
  return(coo.trans(coo, shift[1], shift[2]))}

coo.list.panel <- function(coo.list, dim, byrow=TRUE,
                           fromtop=TRUE, mar=rep(0, 4),
                           cols, borders){
  coo.list <- lapply(coo.list, coo.check)
  # if dim is missing, we define a square
  n <- length(coo.list)
  if(missing(dim)) {
    nc  <- ceiling(sqrt(n))
    nr  <- ceiling(n/nc)
    dim <- c(nr, nc)}
  k   <- dim[1]*dim[2]
  if (k < n) stop("dim[1]*dim[2] must be >= the length of coo.list")
  pos <- matrix(1:k, dim[1], dim[2], byrow=byrow)
  if (fromtop & dim[1]>1) { pos <- pos[dim[1]:1,] }
  # we prepare the panel
  op <- par("mar", "oma")
  on.exit(par(op))
  par(mar=mar, oma=rep(0.2, 4))
  plot(NA, asp=1,
       xlim=c(0, dim[2]),
       ylim=c(0, dim[1]),
       xaxs="i", yaxs="i", frame=FALSE, ann=FALSE, axes=FALSE)
  # we template and plot shapes
  coo.tp  <- lapply(coo.list, coo.template, size=0.95)
  if (missing(cols))    { cols      <- rep("grey80", n) }
  if (missing(borders)) { borders   <- rep("grey20", n) }
  res <- data.frame(pos.x=numeric(), pos.y=numeric())
  for (i in 1:n){
    trans <- which(pos==i, arr.ind=TRUE) - 0.5
    res[i, ] <- c(trans[2], trans[1])
    polygon(coo.tp[[i]][, 1] + trans[2],
            coo.tp[[i]][, 2] + trans[1],
            col=cols[i], border=borders[i])}
  invisible(res)}

coo.oscillo <- function(coo, rug=TRUE, legend=TRUE, cols=col.gallus(2), nb.pts=12){
  coo <- coo.check(coo)
  nr <- nrow(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]
  
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  layout(matrix(1:2, ncol=2), widths=c(1, 2))
  par(mar=c(3, 3, 2 , 1))
  coo.plot(coo, points=FALSE, first.point=FALSE)
  box()
  refs <- round(seq(1, nr, length=nb.pts+1)[-(nb.pts+1)])
  text(coo[refs, 1], coo[refs, 2], labels=as.character(1:nb.pts), cex=0.7)
  ry <- max(abs(range(c(dx, dy))))
  par(mar=c(3, 3, 2 , 1))
  plot(NA, xlim=c(1, nr), xlab="",
       ylim=c(-ry, ry)*1.1,   ylab="Deviation",
       las=1, frame=FALSE, axes=FALSE)
  axis(2, cex.axis=2/3, las=1)
  lines(dx, col=cols[1])
  lines(dy, col=cols[2])
  text((1:nr)[refs], dx[refs], labels=as.character(1:nb.pts), cex=0.7, col=cols[1])
  text((1:nr)[refs], dy[refs], labels=as.character(1:nb.pts), cex=0.7, col=cols[2])
  mtext("Deviation", side=2, line=1.5)
  box()
  if (legend) {
    legend("bottomright", legend = c(expression(x[i] - x[0]), expression(y[i] - y[0])),
           col = cols, bg="#FFFFFFCC", 
           cex=0.7, lty = 1, lwd=1, inset=0.05, bty="n")}}

dev.plot       <- function(mat, dev, cols, x=1:ncol(mat), 
                           lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
                           plot=FALSE, main="Deviation plot", xlab="", ylab="Deviations") {
  # we prepare and check a bit
  r <- nrow(mat)
  if (!missing(dev)){
    if (any(dim(mat)!=dim(dev))) {
      stop("mat and dev must be of the same dimension")}}
  if (missing(cols))   {cols <- rep("#000000", r)}
  if (length(cols)!=r) {cols <- rep("#000000", r)}
  # we call a new plot if required
  if (plot) {
    if (missing(dev)) {
      ylim <- range(mat)
    } else { ylim <- c(min(mat+dev), max(mat+dev)) }
    plot(NA, xlim=range(x), ylim=ylim, main=main, xlab=xlab, ylab=ylab, las=1, xaxs="i", yaxs="i")
    axis(1, at=1:ncol(mat))}
  # if a deviation matrix is provided
  if (!missing(dev)){
    for (i in 1:r){
      # if required, we draw the background polygons
      if (poly) {
        polygon(x = c(x, rev(x)),
                y = c(mat[i, ] - dev[i, ], rev(c(mat[i, ] + dev[i, ]))),
                col=paste0(cols[i], "55"), border=NA)}
      # if required we draw the dev segments
      if (segments) {
        segments(x,    mat[i, ] - dev[i, ], x, mat[i, ]    + dev[i, ], col=cols[i], lwd=0.5)
        segments(x-bw, mat[i, ] - dev[i, ], x+bw, mat[i, ] - dev[i, ], col=cols[i], lwd=0.5)
        segments(x-bw, mat[i, ] + dev[i, ], x+bw, mat[i, ] + dev[i, ], col=cols[i], lwd=0.5)}}}
  # if a dev matrix is not provided, we simply draw lines
  if (lines) {
    for (i in 1:nrow(mat)) {
      if (lines) {
        lines(x, mat[i, ], col=cols[i], type="o", cex=0.25, pch=20)}}}}



dev.segments <-function(coo, cols, lwd=1){
  nr <- nrow(coo)
  coo <- rbind(coo, coo[1, ])
  for (i in 1:nr) {
    segments(coo[i, 1], coo[i, 2], coo[i+1, 1], coo[i+1, 2],
             col=cols[i], lwd=lwd)}}


# from Claude
conf.ell <- function(x, y, conf=0.95, nb.pts = 60){
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]}
  centroid <- apply(cbind(x,y), 2, mean)
  theta.i <- seq(0, 2*pi, length = nb.pts + 1)[-c(nb.pts+1)]
  z <- cbind(cos(theta.i), sin(theta.i))
  rad <- qnorm((1 - conf)/2, mean=0, sd=1, lower.tail=FALSE)
  vcvxy <- var(cbind(x,y))
  r <- cor(x, y)
  M1 <- matrix(c(1,1,-1,1), nrow=2, ncol=2)
  M2 <- matrix(c(var(x), var(y)), nrow=2, ncol=2)
  M3 <- matrix(c(1+r, 1-r), nrow=2, ncol=2, byrow=TRUE)
  ellpar <- M1 * sqrt(M2 * M3/2)
  t(centroid + rad * ellpar %*% t(z))}

.frame <- function(xy, center.origin=FALSE, zoom=1){
  if (center.origin) {
    w <- zoom*max(abs(xy))
    plot(NA, xlim=c(-w, w), ylim=c(-w, w),  asp=1, axes=FALSE, frame=TRUE) 
  } else {
    w <- zoom*apply(abs(xy), 2, max)
    plot(xy, xlim=c(-w[1], w[1]), ylim=c(-w[2], w[2]),
         type="n", asp=1,  axes=FALSE, frame=TRUE)}}

#grid layer
.grid <- function(xy, nb.grids=3){
  m <- max(abs(xy))
  g <- seq(0, m, length=nb.grids)
  g <- c(g[-1]*-1, g[-1])
  abline(h=g, v=g, col="grey90", lty=2)
  abline(h=0, v=0, col="grey80")}

#rug
.rug <- function(xy, fac, col){
  if (is.null(fac)) {
    rug(xy[, 1], ticksize=0.015, side=1, col="black")
    rug(xy[, 2], ticksize=0.015, side=2, col="black")
  } else {
    for (i in seq(along=levels(fac))) {
      rug(xy[fac==levels(fac)[i], 1], ticksize=0.015, lwd=1, side=1, col=col[i])
      rug(xy[fac==levels(fac)[i], 2], ticksize=0.015, lwd=1,  side=2, col=col[i])}}}

# convertir en vrai morphospace, à base de plotnew=TRUE/FALSE
.morphospace <- function(xy, pos.shp, rot, mshape, amp.shp=1,
                         size.shp=15, border.shp="#00000055", col.shp="#00000011", ...){
  pos <- pos.shapes(xy, pos.shp=pos.shp)
  shp <- pca2shp.efourier(pos=pos, rot=rot, mshape=mshape, amp=amp.shp, trans=TRUE)
  width <- (par("usr")[4] - par("usr")[3]) / size.shp
  shp <- lapply(shp, coo.scale, 1/width)
  burp <- lapply(shp, polygon, border=border.shp, col=col.shp)}

.ellipses <- function(xy, fac, conf=0.5, col){
  for (i in seq(along=levels(fac))) {
    pts.i <- xy[fac==levels(fac)[i], ]
    ell.i <- conf.ell(x=pts.i, conf=conf)
    lines(coo.close(ell.i), col=col[i])
    points(coo.centpos(pts.i)[1], coo.centpos(pts.i)[2], pch=3, col=col[i])
  }}

.chull <- function(coo, fac, col){
  for (i in seq(along=levels(fac))) {
    chull.i <- coo.chull(coo[fac==levels(fac)[i], ])
    lines(coo.close(chull.i), col=col[i])}}

.labels <- function(xy, fac, col){
  for (i in seq(along=levels(fac))) {
    cent.i <- coo.centpos(xy[fac==levels(fac)[i], ])
    text(cent.i[1], cent.i[2], labels=levels(fac)[i], col=col[i], pos=3)}}

.stars <- function(xy, fac, col){
  col.i <- paste0(col, "55")
  for (i in seq(along=levels(fac))) {
    pts.i  <- xy[fac==levels(fac)[i], ]
    cent.i <- coo.centpos(pts.i)
    for (j in 1:nrow(pts.i)){
      segments(cent.i[1], cent.i[2], pts.i[j, 1], pts.i[j, 2], col=col.i[i])}}}

.eigen <- function(ev, xax, yax, ratio=0.12){
  plt0 <- par("plt")
  on.exit(par(plt = plt0))
  g <- 0.015
  w <- min(c(plt0[2]-plt0[1]), plt0[4]-plt0[3])*ratio
  par(plt = c(plt0[2]-w-g, plt0[2]-g, plt0[3]+g*1.5, plt0[3]+w+g*1.5), xpd=NA, new = TRUE)
  cols <- rep("grey80", 5)
  cols[c(xax,yax)] <- "grey40"
  var <- ev^2
  cs.var <- cumsum(var)/sum(var)
  k <- ifelse(max(c(xax, yax))>5, max(c(xax, yax)), 5)
  barplot(var[1:k], axes=FALSE, col=cols, border=NA)
  text(-0.7, par("usr")[3], labels="Eigenvalues", pos=4, cex=0.6, srt=90, col="grey40")
  
}

.axisnames <- function(xax, yax){
  gx <- strwidth("PCN")/1.75
  gy <- strheight("PCN")/1.8
  text(par("usr")[2]-gx, gy, col="grey40", cex=0.8,
       labels=paste0("PC", xax))
  text(-gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0("PC", yax), srt=90)}

.axisvar <- function(ev, xax, yax){
  var <- ev^2
  var <- signif(100*var/sum(var), 3)
  gx <- strwidth("00.0%")/1.75
  gy <- strheight("00.0%")/1.8
  text(par("usr")[2]-gx, -gy, col="grey40", cex=0.8,
       labels=paste0(var[xax], "%"))
  text(+gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0(var[yax], "%"), srt=90)}

.title <- function(title){
  pos <- par("usr")
  text(pos[1], pos[3]+ strheight(title), labels=title, pos=4)}

# 4. color palettes ----------------------------------------------------------
col.summer <- colorRampPalette(c("#4876FF", "#FFFF00", "#FF3030"))
col.summer2 <- colorRampPalette(c("#66c2a5", "#fc8d62", "#8da0cb",
                                  "#e78ac3", "#a6d854", "#ebad1f"))
col.solarized <- colorRampPalette(c("#b58900", "#cb4b16", "#dc322f", "#d33682",
                                    "#6c71c4", "#268bd2", "#2aa198", "#859900"))
col.gallus <- colorRampPalette(c("#025D8C", "#FFFFFF", "#A80000"))
col.hot  <- colorRampPalette(c("#F2F2F2","#A80000"))
col.cold  <- colorRampPalette(c("#F2F2F2","#025D8C"))
col.blackgallus <- colorRampPalette(c("#000080", "#000000", "#EE0000"))
col.sari   <- colorRampPalette(c("#551A8B", "#FF7F00"))
col.india  <- colorRampPalette(c("#FF9933", "#138808"))
col.bw     <- colorRampPalette(c("#FFFFFF", "#000000"))
col.wcol   <- function(col.hex) colorRampPalette(c("#FFFFFF", col.hex))
col.bcol   <- function(col.hex) colorRampPalette(c("#000000", col.hex))
