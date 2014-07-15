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

##### end graphics Coe 
