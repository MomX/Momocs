# 5. Coe / OutCoe / OpnCoe plotters
# ------------------------------------------
#' Boxplot on OutCoe matrices of harmonic coefficients
#' 
#' Allows to explore diversity of coefficients from OutCoe objects,
#' typically obtain after a eFourier, tFourier, rFourier on an Out object.
#' 
#' @param x the \link{OutCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param legend logical whether to add a legend
#' @param ... useless here but maintain the consistency with generic boxplot
#' @seealso \link{hist.OutCoe}
#' @rdname boxplot.Coe
#' @keywords Graphics Out Opn Ldk
#' @aliases boxplot.Coe
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' boxplot(bot.f)
#' @export
boxplot.OutCoe <- function(x, retain, drop, palette = col.gallus, 
                           title = "Variation of harmonic coefficients", legend = TRUE, 
                           ...) {
  # we deduce and prepare
  OutCoe <- x
  x <- OutCoe$coe
  nb.h <- ncol(x)/4
  cph <- 4
  if (missing(retain)) 
    retain <- 6
  if (missing(drop)) 
    drop <- 0
  cs <- coeff.sel(retain = retain, drop = drop, nb.h = nb.h, 
                  cph = cph)
  range <- (drop + 1):retain
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  cols <- palette(cph)
  
  mv <- max(abs(range(x[, cs])))
  ylim <- c(-mv, mv)
  
  plot(NA, ylim = ylim, xlim = range(range) + c(-0.6, 0.6), 
       xlab = "Harmonic rank", ylab = "Coefficient value", main = title, 
       axes = FALSE, xaxs = "i", frame = FALSE)
  abline(v = range + 0.4, col = "grey80", lty = 2)
  abline(h = 0, col = "grey80")
  for (i in 1:cph) {
    xi <- x[, (i - 1) * nb.h + range]
    boxplot(xi, range = 0, boxwex = 0.2, at = range - 0.6 + 
              (i * 0.2), col = cols[i], names = FALSE, border = cols[i], 
            axes = FALSE, add = TRUE)
  }
  axis(1, at = range - 0.1, labels = range)
  axis(2)
  if (legend) {
    legend("topright", legend = LETTERS[1:cph], bty = "n", 
           fill = cols, border = NA, bg = "#FFFFFFBB", cex = 0.7, 
           inset = 0.005, title = "Harmonic coefficients")
  }
}

#' @rdname boxplot.Coe
#' @keywords Graphics
#' @export
boxplot.OpnCoe <- function(x, retain, drop, palette = col.gallus, 
                           title = "Variation of polynomials coefficients", ...) {
  # we deduce and prepare OutCoe <- x x <- OutCoe$coe
  x <- x$coe
  degree <- ncol(x)
  if (missing(retain)) 
    retain <- degree
  if (missing(drop)) 
    drop <- 0
  cs <- (drop + 1):retain
  h.names <- colnames(x)[cs]
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  cols <- palette(degree)
  mv <- max(abs(range(x[, cs])))
  ylim <- c(-mv, mv)
  plot(NA, ylim = ylim, xlim = range(cs) + c(-0.6, 0.6), ylab = "Coefficient value", 
       main = title, axes = FALSE, xaxs = "i", frame = FALSE)
  abline(v = cs, col = "grey80", lty = 2)
  abline(h = 0, col = "grey80")
  for (i in seq(along = cs)) {
    boxplot(x[, cs[i]], range = 0, boxwex = 0.2, at = i, 
            col = cols[i], names = FALSE, border = cols[i], axes = FALSE, 
            add = TRUE)
  }
  axis(1, at = cs, labels = h.names)
  axis(2)
}

#' Histogram on OutCoe matrices of harmonic coefficients
#' 
#' Explores the distribution of harmonic coefficient values
#' @method hist OutCoe
#' @param x the \link{OutCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param ... useless here but maintain the consistency with generic hist
#' @seealso \link{boxplot.Coe}, \link{hist.OpnCoe}
#' @keywords Graphics Out
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' hist(bot.f)
#' @export
hist.OutCoe <- function(x, retain, drop, palette = col.gallus, 
                        title = "Variation of harmonic coefficients", ...) {
  # we deduce and prepare OutCoe <- x x <- OutCoe$coe
  x <- x$coe
  nb.h <- ncol(x)/4  #todo: restore rfourier, tfourier
  cph <- 4
  if (missing(retain)) 
    retain <- 4
  if (missing(drop)) 
    drop <- 0
  cs <- coeff.sel(retain = retain, drop = drop, nb.h = nb.h, 
                  cph = cph)
  h.names <- colnames(x)[cs]
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  cols <- palette(cph)
  layout(matrix(1:length(cs), ncol = cph, byrow = TRUE))
  # par(oma=c(2, 2, 5, 2), mar=rep(2, 4)) #former version, also
  # below
  par(oma = c(2, 2, 5, 2), mar = c(2, 0.5, 1.5, 0.5))
  cols <- rep(cols, each = retain - drop)
  for (i in seq(along = cs)) {
    # thx Dufour, Chessel and Lobry
    h <- x[, cs[i]]
    # hist(h, main=h.names[i], col=cols[i], proba=TRUE, xlab='',
    # ylab='', las=1)
    hx <- hist(h, main = h.names[i], freq = FALSE, col = cols[i], 
               axes = FALSE, las = 1)
    if (sd(h) > 1e-10) {
      # eg. when coeff are not normalized
      h0 <- seq(min(h), max(h), len = 50)
      y0 <- dnorm(h0, mean(h), sd(h))
      abline(v = mean(h), lwd = 1, lty = 2)
      lines(h0, y0, col = "black", lwd = 1)
      at <- c(range(hx$mids), mean(hx$mids))
      axis(1, at = at, labels = signif(at, 3), cex.axis = 0.75)
    }
  }
  title(main = title, cex.main = 1.5, outer = TRUE)
  layout(matrix(1))
}

#' Histogram on OpnCoe matrices of polynomials coefficients
#' 
#' Explores the distribution of polynomials coefficient values
#' 
#' @param x the \link{OpnCoe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param palette a color \link{palette}
#' @param title a title for the plot
#' @param hist.per.row numeric the number of histograms per row
#' @param ... useless here but maintain the consistency with generic hist
#' @seealso \link{boxplot.Coe}
#' @keywords Graphics Opn
#' @seealso \link{hist.OutCoe}, \link{boxplot.Coe}
#' @examples
#' data(olea)
#' olea.p <- orthoPolynomials(olea, 5)
#' hist(olea.p)
#' @export
hist.OpnCoe <- function(x, retain = 4, drop, palette = col.gallus, 
                        title = "Variation of polynomials coefficients", hist.per.row = 3, 
                        ...) {
  # we deduce and prepare OpnCoe <- x x <- OpnCoe$coe
  x <- x$coe
  degree <- ncol(x)
  if (missing(retain)) 
    retain <- degree
  if (missing(drop)) 
    drop <- 0
  cs <- (drop + 1):retain
  h.names <- colnames(x)[cs]
  # we save the old par and prepare the plot
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  nc <- length(cs)
  cols <- palette(nc)
  lay.nr <- ceiling(nc/hist.per.row)
  layout(matrix(1:nc, nrow = lay.nr, ncol = hist.per.row, byrow = TRUE))
  par(oma = c(2, 2, 5, 2), mar = c(2, 0.5, 1.5, 0.5))
  for (i in seq(along = cs)) {
    # thx Dufour, Chessel and Lobry
    h <- x[, cs[i]]
    # hist(h, main=h.names[i], col=cols[i], proba=TRUE, xlab='',
    # ylab='', las=1)
    hx <- hist(h, main = h.names[i], freq = FALSE, col = cols[i], 
               axes = FALSE, las = 1)
    if (sd(h) > 1e-10) {
      # eg. when coeff are not normalized
      h0 <- seq(min(h), max(h), len = 50)
      y0 <- dnorm(h0, mean(h), sd(h))
      abline(v = mean(h), lwd = 1, lty = 2)
      lines(h0, y0, col = "black", lwd = 1)
      at <- c(range(hx$mids), mean(hx$mids))
      axis(1, at = at, labels = signif(at, 3), cex.axis = 0.75)
    }
  }
  title(main = title, cex.main = 1.5, outer = TRUE)
  layout(matrix(1))
}

#' Harmonic contribution to shape
#' 
#' Calculates contribution of harmonics to shape. The amplitude of every coefficients
#' of a given harmonic is multiplied by the coefficients provided and the resulting
#' shapes are reconstructed and plotted. Naturally, only works on Fourier-based methods.
#'  @param Coe a \code{\link{Coe}} object (either \code{OutCoe} or (soon) \code{OpnCoe})
#'  @param id the id of a particular shape, otherwise working on the meanshape
#'  @param harm.r range of harmonics on which to explore contributions
#'  @param amp.r a vector of numeric for multiplying coefficients
#'  @param main a title for the plot
#'  @param xlab a title for the x-axis
#'  @param ylab a title for the y-axis
#'  @param ... additional parameter to pass to \code{\link{coo.draw}}
#'  @rdname harm.contrib
#'  @examples
#'  data(bot)
#'  bot.f <- eFourier(bot, 12)
#'  hcontrib(bot.f)
#'  hcontrib(bot.f, harm.r=3:10, amp.r=1:8, col="grey20",
#'     main="A huge panel")
#'  @export
hcontrib <- function(Coe, ...){UseMethod("hcontrib")}

#'  @rdname harm.contrib
#'  @export
hcontrib.OutCoe <- function(Coe,
                            id,
                            harm.r,
                            amp.r   = c(0, 0.5, 1, 2, 5, 10),
                            main="Harmonic contribution to shape",
                            xlab="Harmonic rank",
                            ylab="Amplification factor", ...){
  x <- Coe
  # we handle the method
  p <- pmatch(tolower(x$method), c("efourier", "rfourier", "tfourier"))
  if (is.na(p)) { warning("Unvalid method. efourier is used.")
  } else {
    method.i <- switch(p, efourier.i, rfourier.i, tfourier.i)}
  # we deduce the number of coefficient / harmonic, and their number
  cph <- ifelse(p==1, 4, 2)
  nb.h <- ncol(x$coe)/cph
  # we handle for missing harm.r
  if (missing(harm.r)) harm.r <- 1:ifelse(nb.h > 6, 6, nb.h)
  # if id is provided, we work on it, otherwise, on the average shape
  if (missing(id)){
    coe <- apply(x$coe, 2, mean)
    cat(" * no 'id' provided, working on the meanshape.\n")
  } else {
    coe <- x$coe[id, ]}
  # we prepare a xf to feed the method.i functions
  xf <- coeff.split(coe, nb.h, cph)
  # we prepare a neutral amplification factor
  mult <- rep(1, nb.h)
  # the core below
  shp <- list()
  p <- 1 # kinda dirty
  # we loop over harm.r and amp.r by just multiplying xf
  # by a given vector, all set to 1 except the harmonic that has to
  # amplified
  for (j in seq(along=harm.r)){
    for (i in seq(along=amp.r)){
      mult.loc    <- mult
      mult.loc[harm.r[j]] <- amp.r[i]
      xfi <- lapply(xf, function(x) x*mult.loc)
      shp[[p]] <- method.i(xfi)
      p <- p+1}}
  # graphics start here
  # we borrow this block to PC.contrib
  # except the expand.grid and coo.trans that needed to be "transposed"
  xs <- 1:length(harm.r) - 0.5
  ys <- rev(1:length(amp.r) - 0.5)
  plot(NA, xlim=c(0, length(harm.r)), ylim=c(0, length(amp.r)),
       asp=1, frame=FALSE, axes=FALSE,
       main=main, xlab=xlab, ylab=ylab)
  axis(1, at = xs, labels = harm.r)
  axis(2, at = ys, labels = amp.r, las=1)
  # we template the size of the shapes
  shp <- lapply(shp, coo.close)
  shp <- lapply(shp, coo.template, 0.95)
  # here we prepare and apply the translation values
  trans <- expand.grid(ys, xs)
  colnames(trans) <- c("x", "y")
  for (i in seq(along=shp)){
    shp[[i]] <- coo.trans(shp[[i]], trans[i, 2], trans[i, 1])}
  # we finally plot the shapes
  gc <- lapply(shp, coo.draw, centroid = FALSE, first.point=FALSE, ...)
  invisible(list(shp=shp, trans=trans))}

# hcontrib.Opn (dct) # todo





##### end graphics Coe 
