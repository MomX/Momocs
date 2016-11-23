# g is the new plot ----------

g <- function(x, axes_corner=TRUE, ...){
  # dots <- as.list(sys.call())[-(1:2)]
  dots <- Momocs:::cook_dots()
  # return(dots)
  # preliminaries ------------------
  # defines a new par, saves old one and restore it on exit
  op <- par(mai=rep(0, 4), mar=rep(0, 4), xpd=NA)
  on.exit(par(op))
  # bounding box coordinates
  bb <- coo_boundingbox(x)
  # initializes plot window
  plot(NA, xlim=c(bb$x0, bb$x1), ylim=c(bb$y0, bb$y1),
       asp=1, axes=FALSE, ann=FALSE)
  # grabs window actual coordinates
  usr <- par("usr")
  w <- min(.wdw()*(1/100))

  # layers ------------
  do_with_args("chessboard", dots)


  # if coo spans origins, then draw x- and y- axes
  span_origin <- bb$x0<0 & bb$x1>0 & bb$y0 < 0 & bb$y1 > 0
  if (span_origin){
    # abline(h=0, v=0, col="grey80", lty="dotdash", lwd=0.5)
    segments(usr[1], 0, usr[2], 0, col="grey80", lty="dotdash", lwd=0.5)
    segments(0, usr[3], 0, usr[4], col="grey80", lty="dotdash", lwd=0.5)
  }

  # deduced layers ------------------

  do_with_args("axes_corner", dots, with=list(bb=bb, w=w))
  # draws a cross for centroid position
  cp <- coo_centpos(x)
  segments(cp[1]-w/2, cp[2], cp[1]+w/2, cp[2], lwd=0.5, col="grey20")
  segments(cp[1], cp[2]-w/2, cp[1], cp[2]+w/2, lwd=0.5, col="grey20")

  # outline case -------
  # arrow on first point
  x1 <- x[1, ]
  x2 <- x[2, ]
  # theta <- atan2(x2[2] - x1[2], x2[1] - x1[1]) * (180 / pi) - 90
  # text(x1[1], x1[2], labels = "^", cex=1, srt=theta)

  # new arrow
  theta <- atan2(x2[2] - x1[2], x2[1] - x1[1]) * (180 / pi)
  # starts from x1 and do 1/100 of the road to centroid
  a0 <- edi(x1, cp, -1/20)
  a1 <- edi(x2, cp, -1/20)
  a1 <- edi(a0, a1, w*2 / ed(a0, a1))
  arrows(a0[1], a0[2], a1[1], a1[2], lwd=0.5, col="grey20", length=0.05)
  # given the length of the arrow, some trig to deduce a1
  # a1 <- c(a0[1] - cos(theta)*w, a0[2] - sin(theta)*w)
  # arrows(a0[1], a0[2], a1[1], a1[2], lwd=0.5, col="grey20", length=0.05)

  # outline drawing
  lines(x, col="grey75")
  points(x, pch=20, cex=1/5)

  # return(dots)
}
