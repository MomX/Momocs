# 4. Coo / Out / Opn plotters --------------------------------------------------
#' Plot on Coo (Out/Opn) objects: quick review
#' 
#' Allows to plot shapes from Coo objects
#' @method plot Coo
#' @param x the Coo object
#' @param id the id of the shape to plot, if not provided a 
#' random shape is plotted
#' @param ... further arguments to be passed to \link{coo.plot}
#' @keywords Coo
#' @export
plot.Coo <- function(x, id, ...){
  Coo <- x
  if (missing(id)) {
    repeat{
      id <- sample(length(Coo), 1)
      coo.plot(Coo$coo[[id]], main=names(Coo)[id], ...)
      readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}
  if (id[1]=="all") { id <- 1:length(Coo)}
  if (is.numeric(id)){
    if (length(id)==1) {
      coo.plot(Coo$coo[[id]], main=names(Coo)[id], ...)
    } else {
      for (i in seq(along=id)) {
        coo.plot(Coo$coo[[id[i]]], main=names(Coo)[id[i]], ...)
        readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}}}

#' Plot on Coo objects: stacks all shapes
#' 
#' Plots all the outlines from a \code{Coo} on the same graph with graphical 
#' options.
#' @method stack Coo
#' @param x The \code{Coo} object to plot.
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param points logical whether to draw or not points
#' @param first.point logical whether to draw or not the first point
#' @param centroid logical whether to draw or not the centroid
#' @param ldk \code{logical}. Whether to display landmarks (if any).
#' @param ldk.pch A \code{pch} for these landmarks
#' @param ldk.col A color for these landmarks
#' @param ldk.cex A \code{cex} fro these landmarks
#' @param ldk.links logical whether to draw links (of the mean shape)
#' @param ldk.confell logical whether to draw conf ellipses
#' @param ldk.contour logical whether to draw contour lines
#' @param ldk.chull logical whether to draw convex hull
#' @param ldk.labels logical whether to draw landmark labels
#' @param xy.axis whether to draw or not the x and y axes
#' @param ... further arguments to be passed to coo.plot
#' @seealso \link{panel}, \link{plot.Coo}.
#' @examples
#' data(mosquito)
#' stack(mosquito, borders="#1A1A1A22", first.point=FALSE)
#' data(hearts)
#' stack(hearts)
#' stack(hearts, ldk=FALSE)
#' stack(hearts, borders="#1A1A1A22", ldk=TRUE, ldk.col=col.summer(4), ldk.pch=20)
#' @export
stack.Coo <- function(x, cols, borders,
                      points=FALSE, first.point=TRUE, centroid=TRUE,
                      ldk=TRUE, ldk.pch=3, ldk.col="#FF000055", ldk.cex=0.5,
                      ldk.links=FALSE, ldk.confell=FALSE, ldk.contour=FALSE,
                      ldk.chull=FALSE, ldk.labels=FALSE,
                      xy.axis=TRUE, ...){
  Coo <- x
  if (missing(cols)) {
    cols     <- rep(NA, length(Coo))}
  if (length(cols)!=length(Coo)) {
    cols     <- rep(cols[1], length(Coo))}
  if (missing(borders)) {
    borders     <- rep("#33333355", length(Coo))}
  if (length(borders)!=length(Coo)) {
    borders     <- rep(borders[1], length(Coo))}
  op <- par(mar=c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Coo$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)
  if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}
  for (i in 1:length(Coo)) {
    coo.draw(Coo$coo[[i]], col=cols[i], border=borders[i],
             points=points, first.point=TRUE, centroid=centroid)
    if (ldk & length(Coo$ldk)!=0) {
      points(Coo$coo[[i]][Coo$ldk[[i]], ], pch=ldk.pch, col=ldk.col, cex=ldk.cex)}}}

#' @export
stack.Ldk <- function(x, cols, borders,
                      first.point=TRUE, centroid=TRUE,
                      ldk=TRUE, ldk.pch=20, ldk.col="#33333333", ldk.cex=0.3,
                      ldk.links=FALSE, ldk.confell=FALSE, ldk.contour=FALSE,
                      ldk.chull=FALSE, ldk.labels=FALSE,
                      xy.axis=TRUE, ...){
  Coo <- x
  if (missing(cols)) {
    cols     <- rep(NA, length(Coo))}
  if (length(cols)!=length(Coo)) {
    cols     <- rep(cols[1], length(Coo))}
  if (missing(borders)) {
    borders     <- rep("#33333355", length(Coo))}
  if (length(borders)!=length(Coo)) {
    borders     <- rep(borders[1], length(Coo))}
  op <- par(mar=c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Coo$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)
  if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}
  for (i in 1:length(Coo)) {
    points(Coo$coo[[i]], pch=ldk.pch, col=ldk.col, cex=ldk.cex)}  
  # Specific to Ldk
  # not very clean below #todo
  A <- l2a(Coo$coo)
  mA <- mshape(A)
  points(mA, pch=20, cex=ifelse(ldk.cex>0.5, ldk.cex*1.5, 1), col="grey20")
  if (ldk.confell) { ldk.confell(A, conf=0.9) }
  if (ldk.contour) { ldk.contour(A, nlevels=3, col="grey20") }
  if (ldk.chull)   { ldk.chull(A) }
  if (ldk.links)    { if (is.matrix(Coo$links)) ldk.links(mshape(A), Coo$links) }
  if (ldk.labels)  { ldk.labels(mshape(A)) }}

#' Plot on Coo objects: family picture
#' 
#' Plots all the outlines from a \code{Coo} side by side
#'
#' @param Coo The \code{Coo (Out/Opn)} object  to plot.
#' @param cols A \code{vector} of colors for drawing the outlines.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param borders A \code{vector} of colors for drawing the borders.
#' Either a single value or of length exactly equals to the number of coordinates.
#' @param fac a factor within the $fac slot for colors
#' @param reorder a factor or a numeric to reorder shapes
#' @param palette a color \link{palette}
#' @param names whether to plot names or not. If TRUE uses shape names, otherwise
#' pass a character for the names of the files
#' @param cex.names a cex for the names
#' @param points logical (for Ldk) whether to draw points
#' @param points.pch (for Ldk) and a pch for these points
#' @param points.cex (for Ldk) and a cex for these points
#' @param points.col (for Ldk) and a col  for these points
#' @param ... further arguments for the generic
#' @seealso \link{stack.Coo}, \link{plot.Coo}.
#' @examples
#' data(mosquito)
#' panel(mosquito, names=TRUE, cex.names=0.5)
#' data(olea)
#' panel(olea)
#' @export
panel <- function(Coo, cols, borders, fac,
                  reorder, palette=col.summer,
                  names=NULL, cex.names=0.6, points=TRUE, points.pch=3,
                  points.cex=0.2, points.col, ...){UseMethod("panel")}
#' @export
panel.Out <- function(Coo, cols, borders,
                      fac, reorder=NULL, palette=col.summer,
                      names=NULL, cex.names=0.6, points=TRUE, points.pch=3,
                      points.cex=0.2, points.col, ...){
  Out <- Coo
  if (!missing(fac)){
    
    if (missing(cols)){
      cols <- palette(nlevels(Coo$fac[, fac]))[Coo$fac[, fac]]
    } else {
      cols <- cols[Coo$fac[, fac]]
    }
  }
  if (missing(cols)) {
    cols     <- rep(NA, length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))} 
  if (!missing(reorder)) reorder <- Out$fac[, reorder]
  pos <- coo.list.panel(Out$coo, cols=cols, borders=borders,
                        reorder=reorder, poly=TRUE)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Out), cex=cex.names)
    } else {    
      if (length(names)!=length(Out)) {
        if (is.null(reorder)) {
          text(pos[,1], pos[,2], labels=Coo$fac[, names], cex=cex.names)
        } else {
          text(pos[,1], pos[,2], labels=Coo$fac[, names][order(reorder)], cex=cex.names)
        }
      } else {
        text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}}     
#' @export
panel.Opn <- function(Coo, cols, borders, fac,
                      reorder=NULL, palette=col.summer,
                      names=NULL, cex.names=0.6, points=TRUE, points.pch=3,
                      points.cex=0.2, points.col, ...){
  Opn <- Coo
  if (!missing(fac)){
    
    if (missing(cols)){
      cols <- palette(nlevels(Coo$fac[, fac]))[Coo$fac[, fac]]
    } else {
      cols <- cols[Coo$fac[, fac]]
    }
  }
  if (missing(cols)) {
    cols     <- rep(NA, length(Opn))}
  if (length(cols)!=length(Opn)) {
    cols     <- rep(cols[1], length(Opn))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Opn))}
  if (length(borders)!=length(Opn)) {
    cols     <- rep(borders[1], length(Opn))} 
  if (!missing(reorder)) reorder <- Opn$fac[, reorder]
  pos <- coo.list.panel(Opn$coo, cols=cols, borders=borders,
                        reorder=reorder, poly=FALSE)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Opn), cex=cex.names)
    } else {    
      if (length(names)!=length(Opn)) {
        if (is.null(reorder)) {
          text(pos[,1], pos[,2], labels=Coo$fac[, names], cex=cex.names)
        } else {
          text(pos[,1], pos[,2], labels=Coo$fac[, names][order(reorder)], cex=cex.names)
        }
      } else {
        text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}}     

#' @export
panel.Ldk <- function(Coo, cols, borders, fac,
                      reorder=NULL, palette=col.summer,
                      names=NULL, cex.names=0.6,
                      points=TRUE, points.pch=3,
                      points.cex=0.2, points.col="#333333", ...){
  Opn <- Coo
  if (!missing(fac)){
    if (missing(borders)){
      borders <- palette(nlevels(Coo$fac[, fac]))[Coo$fac[, fac]]
    } else {
      borders <- borders[Coo$fac[, fac]]
    }
  }
  if (missing(borders)) {
    borders     <- rep("#333333", length(Opn))}
  if (length(borders)!=length(Opn)) {
    borders     <- rep(borders[1], length(Opn))} 
  if (!missing(reorder)) reorder <- Opn$fac[, reorder]
  pos <- coo.list.panel(Opn$coo, cols=cols, borders=borders,
                        reorder=reorder, poly=FALSE,
                        points=points, points.pch=points.pch,
                        points.cex=points.cex, points.col=points.col)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Opn), cex=cex.names)
    } else {    
      if (length(names)!=length(Opn)) {
        if (is.null(reorder)) {
          text(pos[,1], pos[,2], labels=Coo$fac[, names], cex=cex.names)
        } else {
          text(pos[,1], pos[,2], labels=Coo$fac[, names][order(reorder)], cex=cex.names)
        }
      } else {
        text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}}     
