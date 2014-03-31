#todo Out.check
# Out.nbh check etc. coderepété sur 10 lines
# smooth.it -> nb.s ?

# xFourier methods -------------------------------------------------------------
eFourier     <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  UseMethod("eFourier")}

eFourier.Out <- function(Out, nb.h, smooth.it=0, norm=TRUE, start=FALSE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1 #todo
    warning(" * at least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q,"\n")}
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:4], each = nb.h), rep(1:nb.h, times = 4))
  coe <- matrix(ncol = 4 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) { #todo: vectorize ?
    ef <- efourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, verbose = TRUE)
    if (norm) {
      ef <- efourier.norm(ef, start=start)
      if (ef$A[1] < 0) {
        ef$A <- (-ef$A)
        ef$B <- (-ef$B)
        ef$C <- (-ef$C)
        ef$D <- (-ef$D)
        ef$lnef <- (-ef$lnef)}
      coe[i, ] <- c(ef$A, ef$B, ef$C, ef$D)
    } else {
      coe[i, ] <- c(ef$an, ef$bn, ef$cn, ef$dn)}}
  return(OutCoe(coe=coe, fac=Out$fac, method="eFourier", norm=norm))}
  
rFourier <- function(Out, nb.h = 40, nb.smooth = 0, norm =TRUE){
  UseMethod("rFourier")}

rFourier.Out <- function(Out, nb.h = 40, smooth.it = 0, norm = TRUE) {
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- ifelse(q >= 32, 32, q)
    cat(" * nb.h not provided and set to", nb.h, "\n")}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1 #todo
    warning("* at least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q,"\n")}
  coo <- Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) {
    rf <- rfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, norm=norm, verbose = TRUE) #todo: vectorize
    coe[i, ] <- c(rf$an, rf$bn)}
  return(OutCoe(coe, fac=Out$fac, method="rFourier"))}

tFourier <- function(Out, nb.h = 40, smooth.it = 0, norm =TRUE){
  UseMethod("tFourier")}

tFourier.Out <- function(Out, nb.h=40, smooth.it = 0, norm=TRUE){
  q <- floor(min(sapply(Out$coo, nrow)/2)) - 1
  if (missing(nb.h))  {
    nb.h <- if (q >= 32) { 32 } else { q }
    cat(paste("  * nb.h not provided and set to", nb.h, "\n"))}
  if(nb.h  > (q+1)*2) {
    nb.h <- q # should not be 1
    warning("At least one outline has no more than ", (q+1)*2, " coordinates. 
            'nb.h' has been set to: ", q, "\n")}
  coo <-Out$coo
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coe <- matrix(ncol = 2 * nb.h, nrow = length(coo), dimnames = list(names(coo), col.n))
  for (i in seq(along = coo)) {
    tf <- tfourier(coo[[i]], nb.h = nb.h, smooth.it = smooth.it, norm=norm, verbose=TRUE)
    coe[i, ] <- c(tf$an, tf$bn)}
  return(OutCoe(coe, fac=Out$fac, method="tFourier"))}


# # hqual ##########################################################
# setGeneric(name= "hqual",     def=function(
#   Coo,
#   method = c("efourier", "rfourier", "tfourier"), 
#   id        = sample(Coo@coo.nb, 1),
#   smooth.it = 0,
#   harm.range= c(1, 2, 4, 8, 16, 32),
#   scale = TRUE,
#   center = TRUE,
#   align = TRUE,
#   plot.method = c("stack", "panel")[1],
#   legend = TRUE,
#   legend.title = "# harmonics",
#   palette = col.summer,
#   shp.col="#70809033",
#   shp.border="#708090EE"){standardGeneric("hqual")})
# 
# setMethod(f="hqual", signature="Coo", definition=
#             function(Coo,
#                      method = c("efourier", "rfourier", "tfourier"),
#                      id        = sample(Coo@coo.nb, 1),
#                      smooth.it = 0,
#                      harm.range= c(1, 2, 4, 8, 16, 32),
#                      scale = TRUE,
#                      center = TRUE,
#                      align = TRUE,
#                      plot.method = c("stack", "panel")[1],
#                      legend = TRUE,
#                      legend.title = "# harmonics",
#                      palette = col.summer,
#                      shp.col="#70809033",
#                      shp.border="#708090EE"){
#               # for one single outline
#               if (missing(method)) {
#                 cat(" * Method not provided. efourier is used.\n")
#                 method   <- efourier
#                 method.i <- efourier.i 
#               } else {
#                 p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
#                 if (is.na(p)) { warning(" * Unvalid method. efourier is used.\n")
#                 } else {
#                   method   <- switch(p, efourier,   rfourier,   tfourier)
#                   method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)}}
#               
#               # check for too ambitious harm.range
#               if (max(harm.range) > (min(Coo@coo.len)/2 + 1)) {
#                 harm.range <- floor(seq(1, q/2 - 1, length=6))
#                 cat(" * harm.range was too high and set to: ", harm.range, ".\n")}
#               coo <- Coo@coo[[id]]
#               if (scale)  coo <- coo.scale(coo)
#               if (center) coo <- coo.center(coo)
#               if (align)  coo <- coo.align(coo)
#               res <- list()
#               for (i in seq(along=harm.range)) {
#                 res[[i]] <- method.i(method(coo, nb.h=max(harm.range), smooth.it=smooth.it), nb.h=harm.range[i])}
#               # plotting
#               op <- par(no.readonly = TRUE)
#               on.exit(par(op))
#               cols <- paste(palette(length(harm.range)), "88", sep="")
#               if (plot.method=="stack") {
#                 coo.plot(coo.smooth(coo, smooth.it), main=Coo@names[id], col=shp.col, border=shp.border)
#                 for (i in seq(along=harm.range)) {lines(res[[i]], col=cols[i], lwd=1)}
#                 if (legend) {
#                   legend("topright", legend = as.character(harm.range), bty="o",
#                          col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", cex=0.7,
#                          title = legend.title)}
#                 if (center) {points(0, 0, pch=3, col=shp.col)}
#               } else {
#                 if (plot.method=="panel") {
#                   #par(oma=c(1, 1, 3, 0))
#                   pos <- coo.list.panel(res, cols=cols)
#                   if (legend) {text(x=pos[, 1], y=pos[, 2], as.character(harm.range))}
#                   mtext(Coo@names[id], side=3, line=0, cex=1.3, outer=TRUE)
#                 }}})
# 
# # hquant #########################################################
# setGeneric(name= "hquant", def=function(
#   Coo,
#   method = c("efourier", "rfourier", "tfourier"),
#   id        = 1,
#   smooth.it = 0,
#   harm.range = seq(4, 20, 4),
#   norm.centsize = TRUE,
#   dist.method = edm.nearest,
#   dist.nbpts = 120,
#   plot = TRUE,
#   dev.plot=TRUE,
#   title = "Deviations along the outline",
#   legend = TRUE,
#   legend.title = "# harmonics",
#   palette = col.summer,
#   lineat.y=c(0.5, 0.1, 0.01)){standardGeneric("hquant")})
# 
# setMethod(f="hquant", signature="Coo", definition=
#             function(Coo,
#                      method = c("efourier", "rfourier", "tfourier"),
#                      id        = 1,
#                      smooth.it = 0,
#                      harm.range= seq(4, 20, 4),
#                      norm.centsize = TRUE,
#                      dist.method = edm.nearest,
#                      dist.nbpts = 120,
#                      plot = TRUE,
#                      dev.plot=TRUE,
#                      title = "Deviations along the outline",
#                      legend = TRUE,
#                      legend.title = "# harmonics",
#                      palette = col.summer,
#                      lineat.y=c(0.5, 0.1, 0.01)){
#               if (missing(method)) {
#                 cat("  * Method not provided. efourier is used.\n")
#                 method   <- efourier
#                 method.i <- efourier.i 
#               } else {
#                 p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
#                 if (is.na(p)) { warning("Unvalid method. efourier is used.")
#                 } else {
#                   method   <- switch(p, efourier,   rfourier,   tfourier)
#                   method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)}}
#               # We define the highest possible nb.h along Coo@coo[id]
#               min.nb.pts <- min(unlist(lapply(Coo@coo[id], nrow)))
#               nb.h.best  <- floor(min.nb.pts/2)-1
#               # we handle too ambitious harm.range
#               if (max(harm.range) > nb.h.best) {
#                 harm.range <- floor(seq(4, nb.h.best, length=6))
#                 cat("  * 'harm.range' was too high and set to: ", harm.range, ".\n")}
#               
#               # we prepare the results array
#               nb.pts <- ifelse(dist.nbpts == "max", 2*nb.h.best, dist.nbpts)
#               nr <- length(harm.range)
#               nc <- nb.pts
#               nk <- length(id)
#               res <- array(NA, dim=c(nr, nc, nk),
#                            dimnames=list(paste0("h", harm.range),
#                                          paste("pt", 1:nb.pts),
#                                          Coo@names[id]))
#               # progressbar
#               if (nk > 5) {
#                 pb <- txtProgressBar(1, nk)
#                 t <- TRUE } else {t <- FALSE}
#               # the core loops that will calculate deviations
#               for (ind in seq(along=id)) {
#                 coo <- Coo@coo[[id[ind]]]
#                 # below, the best possible fit
#                 coo.best <- l2m(method.i(method(coo, nb.h=nb.h.best, smooth.it=smooth.it), nb.pts=nb.pts))
#                 for (i in seq(along=harm.range)) {
#                   # for each number of harmonics we calculate deviation with the FUN=method
#                   coo.i <- l2m(method.i(method(coo, nb.h=harm.range[i], smooth.it=smooth.it, silent=TRUE), nb.pts=nb.pts))
#                   res[i, , ind] <- dist.method(coo.best, coo.i)
#                 }
#                 # we normalize by the centroid size
#                 if (norm.centsize) {res[,,ind] <- res[,,ind]/coo.centsize(coo)}
#                 if (t) setTxtProgressBar(pb, ind)}
#               # below we manage for single/several individuals
#               if (nk > 1) { # if more than 1, we calculate median and sd
#                 m <- apply(res, 1:2, median)
#                 d <- apply(res, 1:2, sd)
#               } else {
#                 m <- res[,,1]
#                 d <- NULL}
#               # plotting stuff
#               if (plot) {
#                 cols <- palette(nr)
#                 if (nk > 1) {ylim <- c(0, max(m+d, na.rm=TRUE))} else {ylim <- range(m)}
#                 if (norm.centsize) {
#                   ylab = "Deviation (in % of the centroid size)"
#                 } else {
#                   ylab = "Deviation (in original units)"}
#                 plot(NA, xlim=c(1, nc), ylim=ylim,
#                      xlab="Points sampled along the outline",
#                      ylab=ylab, main=title,
#                      xaxs="i", yaxs="i", axes=FALSE)
#                 axis(1, at=seq(0, dist.nbpts, length=5))
#                 axis(2)
#                 abline(h=lineat.y, lty=2, col="grey90")
#                 # if you want deviations, here they are
#                 if (dev.plot) {
#                   if (nk > 1) {dev.plot(m, d, cols=cols) } else {
#                     for (i in 1:nr) {
#                       lines(1:ncol(m), m[i, ], col=cols[i])}}}
#                 # same for legend
#                 if (legend) {
#                   legend("topright", legend = as.character(harm.range), bty="o",
#                          col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", inset=0.005, cex=0.7,
#                          title = legend.title)}
#                 box() }
#               return(list(res=res, m=m, d=d))})
# 
# # harm.pow ###########################################################
# setGeneric(name= "hpow", def=function(    
#   Coo,
#   method = c("efourier", "rfourier", "tfourier"),
#   id=1:Coo@coo.nb,
#   probs=c(0, 0.5, 1),
#   nb.h = 24,
#   drop   = 1,
#   smooth.it = 0,
#   plot = TRUE,
#   legend = FALSE,
#   title="Fourier power spectrum",
#   lineat.y=c(0.9, 0.95, 0.99, 0.999),
#   bw=0.1){standardGeneric("hpow")})
# 
# setMethod(f="hpow", signature="Coo", definition=
#             function(Coo,
#                      method = c("efourier", "rfourier", "tfourier"),
#                      id=1:Coo@coo.nb,
#                      probs=c(0, 0.5, 1),
#                      nb.h = 24,
#                      drop   = 1,
#                      smooth.it = 0,
#                      plot = TRUE,
#                      legend = FALSE,
#                      title="Fourier power spectrum",
#                      lineat.y=c(0.9, 0.95, 0.99, 0.999),
#                      bw=0.1) {
#               # for one signle outline
#               if (missing(method)) {
#                 cat("  * Method not provided. efourier is used.\n")
#                 method   <- efourier
#               } else {
#                 p <- pmatch(tolower(method), c("efourier", "rfourier", "tfourier"))
#                 if (is.na(p)) { warning("Unvalid method. efourier is used.")
#                 } else {
#                   method   <- switch(p, efourier,   rfourier,   tfourier)}}
#               res <- matrix(nrow=length(id), ncol=(nb.h-drop))
#               x <- (drop+1) : nb.h
#               for (i in seq(along=id)) {
#                 xf  <- method(Coo@coo[[id[i]]], nb.h = nb.h, smooth.it = smooth.it)
#                 pow <- harm.pow(xf)[x]
#                 res[i, ] <-  (cumsum(pow)/sum(pow))}
#               res <- apply(res, 2, quantile, probs=probs)
#               rownames(res) <- paste("q", probs)
#               colnames(res) <- paste("h", x, sep="")
#               if (plot){
#                 if (length(probs)!=3) stop("probs of length != 3 are not (yet) supported")  
#                 plot(NA, xlim = range(x), ylim = c(min(res), 1), las=1, yaxs="i", 
#                      xlab = "Number of harmonics included", ylab = "Cumulative harmonic power",
#                      main=title, sub=paste(length(id), "outlines"), axes=FALSE)
#                 axis(1, at=x) ; axis(2)
#                 abline(h=lineat.y, lty=2, col="grey90")
#                 segments(x,    res[1, ], x,    res[3, ], lwd=0.5)
#                 segments(x-bw, res[1, ], x+bw, res[1, ], lwd=0.5)
#                 segments(x-bw, res[3, ], x+bw, res[3, ], lwd=0.5)
#                 lines(x, res[2, ], type="o", pch=20, cex=0.6) 
#                 if (legend) {
#                   legend("topright", legend = as.character(probs), bty="o", lwd=1,
#                          bg="#FFFFFFCC", cex=0.7, inset = 0.005,
#                          title = "Quantiles")}
#                 box()
#               }
#               return(res)})
# 
# # Ptolemy ############################################################
# setGeneric(name="Ptolemy", def=function(Coo,
#                                         id=1,
#                                         t=seq(0, 2*pi, length=7)[-1],
#                                         nb.h=3,
#                                         nb.pts=360,
#                                         palette=col.sari,
#                                         legend=FALSE){standardGeneric("Ptolemy")}) 
# 
# setMethod(f="Ptolemy", signature="Coo", definition=
#             function(Coo,
#                      id=1,
#                      t=seq(0, 2*pi, length=7)[-1],
#                      nb.h=3,
#                      nb.pts=360,
#                      palette=col.sari,
#                      legend=FALSE) {    # we prepare and deduce
#               op <- par(no.readonly = TRUE)
#               on.exit(par(op))
#               par(xpd=NA)
#               cols <- palette(nb.h)
#               coo <- coo.center(Coo@coo[[id]])
#               #k <- floor(length(coo$x)/4)
#               coo.plot(coo, main=Coo@names[id])
#               # now we calculate for every harmonic
#               coo.ef  <- efourier(coo, nb.h)
#               coo.efi <- efourier.i(coo.ef, nb.h, nb.pts)
#               vect   <- matrix(nrow=nb.h, ncol=2)
#               vect <- rbind(c(0, 0), vect)
#               for (i in seq(along=t)) {
#                 for(j in 1:nb.h) {
#                   vect[j+1, 1] <- coo.ef$an[j] * cos(j * t[i]) + coo.ef$bn[j] * sin(j * t[i])
#                   vect[j+1, 2] <- coo.ef$cn[j] * cos(j * t[i]) + coo.ef$dn[j] * sin(j * t[i])}
#                 vs <- apply(vect, 2, cumsum)
#                 for (j in 1:nb.h){
#                   lh   <- efourier.shape(coo.ef$an[1:j], coo.ef$bn[1:j],
#                                          coo.ef$cn[1:j], coo.ef$dn[1:j],
#                                          nb.h=j, nb.pts=nb.pts, plot=FALSE)
#                   ellh <- efourier.shape(coo.ef$an[j], coo.ef$bn[j],
#                                          coo.ef$cn[j], coo.ef$dn[j],
#                                          nb.h=1, nb.pts=nb.pts, plot=FALSE)
#                   lines(lh, col=paste(cols[j], "22", sep=""), lwd=0.8)
#                   lines(ellh$x + vs[j, 1], ellh$y + vs[j, 2],
#                         col=cols[j], lwd=1)
#                   points(vs[j+1, 1], vs[j+1, 2], col=cols[j], cex=0.8)
#                   arrows(vs[j, 1], vs[j, 2], vs[j+1, 1], vs[j+1, 2],
#                          col=cols[j], angle=10, length=0.05, lwd=1.2)
#                 }
#               }
#               points(0, 0, pch=20, col=cols[1])
#               if (legend) {
#                 legend("topright", legend = as.character(1:nb.h), bty="o",
#                        col = cols, lty = 1, lwd=1, bg="#FFFFFFCC", cex=0.7,
#                        title = "Number of harmonics")}
#             })
# 
# 
# # smooth.qual ########################################################
# setGeneric(name= "smooth.qual",   def=function(Coo, id=1,
#                                                smooth.range = c(10, 50, 200, 500, 1000),
#                                                palette = col.summer){standardGeneric("smooth.qual")})
# setMethod(f="smooth.qual", signature="Coo", definition=
#             function(Coo,
#                      id=1,
#                      smooth.range = c(10, 50, 200, 500, 1000),
#                      palette = col.summer){
#               cont <- Coo@coo[[id]]
#               coo.plot(coo.smooth(cont, 0), main = Coo@names[id])
#               cols <- palette(length(smooth.range))
#               for (i in seq(along = smooth.range)) {
#                 lines(coo.smooth(cont, smooth.range[i]), col = cols[i])
#               }
#               legend("topright", legend = as.character(smooth.range), 
#                      col = cols, lty = 1, bty = "o", bg="#FFFFFFCC", cex=0.7, title = "Smooth iterations")})
