# 5. Coe / OutCoe / OpnCoe plotters
# ------------------------------------------
#' Boxplot of morphometric coefficients
#' 
#' Explores the distribution of coefficient values.
#' 
#' @param x the \link{Coe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param center.y logical whether to center the y-axis
#' @param ... useless here but maintain the consistency with generic boxplot
#' @return a ggplot2 object
#' @aliases boxplot.Coe
#' @seealso \link{hist.Coe}
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 24)
#' boxplot(bot.f)
#' 
#' data(olea)
#' op <- opoly(olea)
#' boxplot(op)
#' @export
boxplot.OutCoe <- function(x, retain=6, drop=0, center.y = TRUE, ...){
  # we convert to a data.frame
  df <- as_df(x)
  # we retrive coefficient rank and names, and we filter
  df <- df %>%
    mutate_(coeN = "substr(coefficient, 1, 1)",
           coeR = "as.numeric(substr(coefficient, 2, 2))")
  df <- filter_(df, ~ coeR > drop, ~ coeR <= retain)
  # we ggplot
  gg <- ggplot(df, aes_string(x="factor(coeR)", y="value", fill="factor(coeN)")) +
    geom_boxplot(outlier.size = 1) +
    labs(x="Harmonic rank", y="Coefficient amplitude", fill="Coefficient")
  if (center.y) gg <- gg + ylim(.center_range(df$value))
  gg
}

#'@export
boxplot.OpnCoe <- function(x, retain=6, drop=0, center.y = TRUE, ...){
  # xfourier case should be treated as a Out
  if (grepl("fourier", x$method)) {
    gg <- boxplot.OutCoe(x, retain=retain, drop=drop, center.y=center.y, ...)
    return(gg)
  }
  # otherwise...
  # retain drop is easier to do before the as_df
  retain <- ifelse(retain<ncol(x$coe), retain, ncol(x$coe)) 
  x$coe <- x$coe[, drop+1 : retain]
  df <- as_df(x)
  # we ggplot
  gg <- ggplot(df, aes_string(x="coefficient", y="value")) +
    geom_boxplot(outlier.size = 1) +
    labs(x="Coefficient", y="Amplitude")
  if (center.y) gg <- gg + ylim(.center_range(df$value))
  gg
}


#' Histogram of morphometric coefficients
#' 
#' Explores the distribution of coefficient values.
#' 
#' @param x the \link{Coe} object
#' @param retain numeric the number of harmonics to retain
#' @param drop numeric the number of harmonics to drop
#' @param bw the number of bins (range/bw) to display
#' @param ... useless here but maintain the consistency with generic hist
#' @return a ggplot2 object
#' @seealso \link{boxplot.Coe}
#' @aliases hist.Coe
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 24)
#' hist(bot.f)
#' 
#' data(olea)
#' op <- opoly(olea)
#' hist(op)
#' @export
hist.OutCoe <- function(x, retain=4, drop=0, bw=20, ...){
  # we convert to a data.frame
  df <- as_df(x)
  # we retrive coefficient rank and names, and we filter
  df <- df %>%
    mutate(coeN = substr(df$coefficient, 1, 1),
           coeR = as.numeric(substr(df$coefficient, 2, 2)))
  df <- filter_(df, ~ coeR > drop, ~ coeR <= retain)
  # we ggplot
  gg <- ggplot(df, aes_string(x="value")) +
    geom_histogram(binwidth=diff(range(df$value))/bw) +
    facet_grid(coeR ~ coeN) + 
    labs(x="Coefficient amplitude", y="Freq")
  gg
}

#' @export
hist.OpnCoe <- function(x, retain=4, drop=0, bw=20, ...){
  # xfourier case should be treated as a Out
  if (grepl("fourier", x$method)) {
    gg <- hist.OutCoe(x, retain=retain, drop=drop, bw=bw, ...)
    return(gg)
  }
  # otherwise...
  # retain drop is easier to do before the as_df
  retain <- ifelse(retain<ncol(x$coe), retain, ncol(x$coe)) 
  x$coe <- x$coe[, drop+1 : retain]
  df <- as_df(x)
  gg <- ggplot(df, aes_string(x="value")) +
    geom_histogram(binwidth=diff(range(df$value))/bw) +
    facet_grid(coefficient ~ .) + 
    labs(x="Coefficient amplitude", y="Freq")
  gg
}

#' Harmonic contribution to shape
#' 
#' Calculates contribution of harmonics to shape. The amplitude of every coefficients
#' of a given harmonic is multiplied by the coefficients provided and the resulting
#' shapes are reconstructed and plotted. Naturally, only works on Fourier-based methods.
#' @param Coe a \code{\link{Coe}} object (either \code{OutCoe} or (soon) \code{OpnCoe})
#' @param id the id of a particular shape, otherwise working on the meanshape
#' @param harm.r range of harmonics on which to explore contributions
#' @param amp.r a vector of numeric for multiplying coefficients
#' @param main a title for the plot
#' @param xlab a title for the x-axis
#' @param ylab a title for the y-axis
#' @param ... additional parameter to pass to \code{\link{coo_draw}}
#' @rdname harm.contrib
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 12)
#' hcontrib(bot.f)
#' hcontrib(bot.f, harm.r=3:10, amp.r=1:8, col="grey20",
#'    main="A huge panel")
#' @export
hcontrib <- function(Coe, ...){
  UseMethod("hcontrib")
  }

#' @rdname harm.contrib
#' @export
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
  if (is.na(p)) { warning("unvalid method. efourier is used")
  } else {
    method.i <- switch(p, efourier_i, rfourier_i, tfourier_i)}
  # we deduce the number of coefficient / harmonic, and their number
  cph <- ifelse(p==1, 4, 2)
  nb.h <- ncol(x$coe)/cph
  # we handle for missing harm.r
  if (missing(harm.r)) harm.r <- 1:ifelse(nb.h > 6, 6, nb.h)
  # if id is provided, we work on it, otherwise, on the average shape
  if (missing(id)){
    coe <- apply(x$coe, 2, mean)
    message("no 'id' provided, working on the meanshape")
  } else {
    coe <- x$coe[id, ]}
  # we prepare a xf to feed the method.i functions
  xf <- coeff_split(coe, nb.h, cph)
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
  # except the expand.grid and coo_trans that needed to be "transposed"
  xs <- 1:length(harm.r) - 0.5
  ys <- rev(1:length(amp.r) - 0.5)
  plot(NA, xlim=c(0, length(harm.r)), ylim=c(0, length(amp.r)),
       asp=1, frame=FALSE, axes=FALSE,
       main=main, xlab=xlab, ylab=ylab)
  axis(1, at = xs, labels = harm.r)
  axis(2, at = ys, labels = amp.r, las=1)
  # we template the size of the shapes
  shp <- lapply(shp, coo_close)
  shp <- lapply(shp, coo_template, 0.95)
  # here we prepare and apply the translation values
  trans <- expand.grid(ys, xs)
  colnames(trans) <- c("x", "y")
  for (i in seq(along=shp)){
    shp[[i]] <- coo_trans(shp[[i]], trans[i, 2], trans[i, 1])}
  # we finally plot the shapes
  gc <- lapply(shp, coo_draw, centroid = FALSE, first.point=FALSE, ...)
  invisible(list(shp=shp, trans=trans))}

# hcontrib.Opn (dct) # todo





##### end graphics Coe 
