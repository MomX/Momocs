
# 3. Graphics subfunctions -----------------------------------------------------------------------
#create an empty frame
#' @export
.frame <- function(xy, center.origin=FALSE, zoom=1){
  if (center.origin) {
    w <- zoom*max(abs(xy))
    plot(NA, xlim=c(-w, w), ylim=c(-w, w),  asp=1, axes=FALSE, frame=TRUE) 
  } else {
    w <- zoom*apply(abs(xy), 2, max)
    plot(xy, xlim=c(-w[1], w[1]), ylim=c(-w[2], w[2]),
         type="n", asp=1,  axes=FALSE, frame=TRUE)}}

#grid layer
#' @export
.grid <- function(nb.grids=3){
  m <- max(abs(par("usr")))
  g <- seq(0, m, length=nb.grids)
  g <- c(g[-1]*-1, g[-1])
  abline(h=g, v=g, col="grey90", lty=3)
  abline(h=0, v=0, col="grey80")}

#rug
#' @export
.rug <- function(xy, fac, col){
  if (is.null(fac)) {
    rug(xy[, 1], ticksize=0.015, side=1, col="black")
    rug(xy[, 2], ticksize=0.015, side=2, col="black")
  } else {
    for (i in seq(along=levels(fac))) {
      rug(xy[fac==levels(fac)[i], 1], ticksize=0.015, lwd=1, side=1, col=col[i])
      rug(xy[fac==levels(fac)[i], 2], ticksize=0.015, lwd=1,  side=2, col=col[i])}}}

#confidence ellipses
#' @export
.ellipses <- function(xy, fac, conf, col){
  for (i in seq(along=levels(fac))) {
    pts.i <- xy[fac==levels(fac)[i], ]
    if (is.matrix(pts.i)) {
      if (nrow(pts.i)>1) {
        ell.i <- conf.ell(x=pts.i, conf=conf)$ell
        lines(coo.close(ell.i), col=col[i])
        points(coo.centpos(pts.i)[1], coo.centpos(pts.i)[2], pch=3, col=col[i])}}}}

#confidence ellipses
#' @export
.ellipsesax <- function(xy, fac, conf, col, lty){
  for (i in seq(along=levels(fac))) {
    pts.i <- xy[fac==levels(fac)[i], ]
    if (is.matrix(pts.i)) {
      if (nrow(pts.i)>1) {
        seg.i <- conf.ell(x=pts.i, conf=conf, nb.pts=720)$seg
        segments(seg.i[1, 1], seg.i[1, 2],
                 seg.i[2, 1], seg.i[2, 2], lty=lty, col=col[i])
        segments(seg.i[3, 1], seg.i[3, 2],
                 seg.i[4, 1], seg.i[4, 2], lty=lty, col=col[i])}}}}

#convex hulls
#' @export
.chull <- function(coo, fac, col, lty){
  for (i in seq(along=levels(fac))) {
    coo.i <- coo[fac==levels(fac)[i], ] 
    if (is.matrix(coo.i)) {
      if (nrow(coo.i)>1) {
        chull.i <- coo.chull(coo.i)
        lines(coo.close(chull.i), col=col[i], lty=lty)}}}}

#add labels
#' @export
.labels <- function(xy, fac, col){
  for (i in seq(along=levels(fac))) {
    cent.i <- xy[fac==levels(fac)[i], ]
    if (is.matrix(cent.i)){
      cent.i <- coo.centpos(xy[fac==levels(fac)[i], ])}
    text(cent.i[1], cent.i[2], labels=levels(fac)[i], col=col[i], pos=3)}}

#add 'stars'
#' @export
.stars <- function(xy, fac, col){
  col.i <- paste0(col, "55")
  for (i in seq(along=levels(fac))) {
    pts.i  <- xy[fac==levels(fac)[i], ]
    cent.i <- coo.centpos(pts.i)
    for (j in 1:nrow(pts.i)){
      segments(cent.i[1], cent.i[2], pts.i[j, 1], pts.i[j, 2], col=col.i[i])}}}

#add loading vectors
#' @export
.loadings <- function(loadings.mat, d=1, d.lab=1.2, col="red"){
  loadings.mat <- loadings.mat*d
  loadings.lab <- loadings.mat*d.lab
  arrows(0, 0, loadings.mat[, 1], loadings.mat[, 2],
          angle=20, length=0.1, col=col)
  text(loadings.lab[, 1], loadings.lab[, 2],
          labels=rownames(loadings.lab), cex=0.8, col=col)}

#add eigen
#' @export
.eigen <- function(ev, xax, yax, ratio=0.12, ev.names){
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
  text(-0.9, par("usr")[3], labels=ev.names,
       pos=4, cex=0.6, srt=90, col="grey40")}

#names axes
#' @export
.axisnames <- function(xax, yax, nax="PC"){
  gx <- strwidth("PCN")/1.75
  gy <- strheight("PCN")/1.8
  text(par("usr")[2]-gx, gy, col="grey40", cex=0.8,
       labels=paste0(nax, xax))
  text(-gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0(nax, yax), srt=90)}

#adds var captured
#' @export
.axisvar <- function(ev, xax, yax){
  var <- ev^2
  var <- signif(100*var/sum(var), 3)
  gx <- strwidth("00.0%")/1.75
  gy <- strheight("00.0%")/1.8
  text(par("usr")[2]-gx, -gy, col="grey40", cex=0.8,
       labels=paste0(var[xax], "%"))
  text(+gy, par("usr")[4]-gx, col="grey40",cex=0.8,
       labels=paste0(var[yax], "%"), srt=90)}

#adds title to pca plots
#' @export
.title <- function(title){
  pos <- par("usr")
  text(pos[1], pos[3]+ strheight(title), labels=title, pos=4)}
