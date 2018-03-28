#' Discrete cosinus transform
#'
#' Calculates discrete cosine transforms, as introduced by Dommergues and colleagues, on
#' a shape (mainly open outlines).
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @param nb.h numeric the number of harmonics to calculate
#' @return a list with the following components:
#' \itemize{
#' \item an the A harmonic coefficients
#' \item bn the B harmonic coefficients
#' \item mod the modules of the points
#' \item arg the arguments of the points
#' }
#' @note This method has been only poorly tested in Momocs and should be considered as
#' experimental. Yet improved by a factor 10, this method is still long to execute.
#' It will be improved in further releases but it should not be so painful right now.
#' It also explains the progress bar. Shapes should be aligned
#' before performing the dct transform.
#'
#' Silent message and progress bars (if any) with `options("verbose"=FALSE)`.
#'
#' @references
#' \itemize{
#'  \item Dommergues, C. H., Dommergues, J.-L., & Verrecchia, E. P. (2007).
#'  The Discrete Cosine Transform, a Fourier-related Method for Morphometric Analysis of Open Contours.
#'  \emph{Mathematical Geology}, 39(8), 749-763. doi:10.1007/s11004-007-9124-6
#'  \item Many thanks to Remi Laffont for the translation in R).
#' }
#'
#' @family dfourier
#' @examples
#' \dontrun{ # because it's long
#' od <- dfourier(olea)
#' od
#' op <- PCA(od)
#' plot(op, 1)
#' }
#' # dfourier and inverse dfourier
#' o <- olea[1]
#' o <- coo_bookstein(o)
#' coo_plot(o)
#' o.dfourier <- dfourier(o, nb.h=12)
#' o.dfourier
#' o.i <- dfourier_i(o.dfourier)
#' o.i <- coo_bookstein(o.i)
#' coo_draw(o.i, border='red')
#'
#' #future calibrate_reconstructions
#' o <- olea[1]
#' h.range <- 2:13
#' coo <- list()
#' for (i in seq(along=h.range)){
#' coo[[i]] <- dfourier_i(dfourier(o, nb.h=h.range[i]))}
#' names(coo) <- paste0('h', h.range)
#' panel(Opn(coo), borders=col_india(12), names=TRUE)
#' title('Discrete Cosine Transforms')
#' @rdname dfourier
#' @export
dfourier <- function(coo, nb.h) {
  UseMethod("dfourier")
}
#' @rdname dfourier
#' @export

dfourier.default <- function(coo, nb.h) {
  # we check a bit
  coo <- coo_check(coo)
  if (missing(nb.h)) {
    nb.h <- 12
    if (.is_verbose()) message("'nb.h' not provided and set to " , nb.h)}
  # preliminaries
  N <- nrow(coo)
  pol <- coo[, 1] + (0+1i) * coo[, 2]
  # dfourier
  c <- rep(sqrt(2/N), N)
  c[1] <- 1/sqrt(N)
  Sv <- S <- rep(NA, N)
  for (k in 0:(N-1)) {
    Sv <- pol * cos(((2 * 0:(N-1)) * k * pi)/(2 * N))
    S[k+1] <- c[k+1] * sum(Sv)
  }
  S <- S[2:(nb.h+1)] #we remove the 1st harmonic
  # return(list(A = Re(S)/N, B = Im(S)/N, mod = Mod(S)/N, phi = Arg(S)))
  return(list(an = Re(S), bn = Im(S), mod = Mod(S), phi = Arg(S)))
}

#' @rdname dfourier
#' @export
dfourier.Opn <- function(coo, nb.h) {
  Opn <- coo
  # verify
  Opn %<>% verify()
  # we set nb.h if missing
  if (missing(nb.h)) {
    nb.h <- 12
    if (.is_verbose()) message("'nb.h' not provided and set to ", nb.h)}
  col.n <- paste0(rep(LETTERS[1:2], each = nb.h), rep(1:nb.h, times = 2))
  coo <- Opn$coo
  nr <- length(coo)
  # we prepare the matrix
  coe <- matrix(ncol = 2 * nb.h, nrow = nr,
                dimnames = list(names(coo), col.n))
  if (.is_verbose()){
    pb <- progress::progress_bar$new(total = nr)
    t <- TRUE
  } else {
    t <- FALSE
  }
  # we loop
  for (i in seq(along=coo)) {
    dfourier_i <- dfourier(coo[[i]], nb.h = nb.h)
    coe[i, ] <- c(dfourier_i$an, dfourier_i$bn)
    # progress bar
    if (t)
      pb$tick()
  }
  # still progress bar: skips a line
  if (t) cat("\n")
  # we prepare the list and return the results
  res <- OpnCoe(coe=coe,
                fac=Opn$fac,
                method = "dfourier",
                baseline1 = Opn$baseline1,
                baseline2 = Opn$baseline2)
  res$cuts <- ncol(res$coe)
  return(res)
}

#' @rdname dfourier
#' @export
dfourier.list <- function(coo, nb.h){
  lapply(coo, dfourier, nb.h)
}

#' @rdname dfourier
#' @export
dfourier.Coo <- function(coo, nb.h) {
  stop("dfourier can only be applied on Opn objects")
}

#' Investe discrete cosinus transform
#'
#' Calculates inverse discrete cosine transforms (see \link{dfourier}), given a list of A and B harmonic coefficients,
#' typically such as those produced by \link{dfourier}.
#' @param df a list with \code{$A} and \code{$B} components, containing harmonic coefficients.
#' @param nb.h a custom number of harmonics to use
#' @param nb.pts numeric the number of pts for the shape reconstruction
#' @return a matrix of (x; y) coordinates
#' @note Only the core functions so far. Will be implemented as an \link{Opn} method soon.
#'
#' @references
#' \itemize{
#'  \item Dommergues, C. H., Dommergues, J.-L., & Verrecchia, E. P. (2007).
#'  The Discrete Cosine Transform, a Fourier-related Method for Morphometric Analysis of Open Contours.
#'  \emph{Mathematical Geology}, 39(8), 749-763. doi:10.1007/s11004-007-9124-6
#'  \item Many thanks to Remi Laffont for the translation in R).
#' }
#'
#' @family dfourier
#' @examples
#' # dfourier and inverse dfourier
#' o <- olea[1]
#' o <- coo_bookstein(o)
#' coo_plot(o)
#' o.dfourier <- dfourier(o, nb.h=12)
#' o.dfourier
#' o.i <- dfourier_i(o.dfourier)
#' o.i <- coo_bookstein(o.i)
#' coo_draw(o.i, border='red')
#'
#' o <- olea[1]
#' h.range <- 2:13
#' coo <- list()
#' for (i in seq(along=h.range)){
#' coo[[i]] <- dfourier_i(dfourier(o, nb.h=h.range[i]))}
#' names(coo) <- paste0('h', h.range)
#' panel(Opn(coo), borders=col_india(12), names=TRUE)
#' title('Discrete Cosine Transforms')
#' @export
dfourier_i <- function(df, nb.h, nb.pts = 60) {
  A <- df$an
  B <- df$bn
  if (missing(nb.h)) {
    nb.h <- length(A) + 1
  }

  c <- rep(sqrt(2/nb.pts), nb.pts)
  c[1] <- 1/sqrt(nb.pts)

  S <- A + (0+1i) * B
  S <- c(0+1i, S)  # we add a trivial harmonic corresponding to (0; 0)

  sv_r <- rep(NA, nb.h)
  s_r <- rep(NA, nb.pts)
  # idfourier pour le nombre d'harmonique specifie
  for (n in 0:(nb.pts - 1)) {
    for (k in 0:(nb.h - 1)) {
      sv_r[k + 1] <- c[k + 1] * S[k + 1] * cos(((2 * n + 1) * k * pi)/(2 * nb.pts))
    }
    s_r[n + 1] <- sum(sv_r)
  }
  shp <- cbind(Re(s_r), Im(s_r))
  return(shp)
}

#' Calculates and draws 'dfourier' shapes
#'
#' Calculates shapes based on 'Discrete cosine transforms' given harmonic coefficients
#' (see \link{dfourier}) or can generate some random 'dfourier' shapes.
#' Mainly intended to generate shapes and/or to understand how dfourier works.
#' @param A vector of harmonic coefficients
#' @param B vector of harmonic coefficients
#' @param nb.h if \code{A} and/or \code{B} are not provided,
#' the number of harmonics to generate
#' @param nb.pts if \code{A} and/or \code{B} are not provided,
#' the number of points to use to reconstruct the shapes
#' @param alpha The power coefficient associated with the (usually decreasing)
#' amplitude of the harmonic coefficients (see \link{efourier_shape})
#' @param plot logical whether to plot the shape
#' @family dfourier
#' @examples
#' # some signatures
#' panel(coo_align(Opn(replicate(48, dfourier_shape(alpha=0.5, nb.h=6)))))
#' # some worms
#' panel(coo_align(Opn(replicate(48, dfourier_shape(alpha=2, nb.h=6)))))
#' @export
dfourier_shape <- function(A, B, nb.h, nb.pts = 60, alpha = 2, plot = TRUE) {
  if (missing(nb.h) & missing(A))
    nb.h <- 3
  if (missing(nb.h) & !missing(A))
    nb.h <- length(A)
  if (missing(A))
    A <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  if (missing(B))
    B <- runif(nb.h, -pi, pi)/(1:nb.h)^alpha
  df <- list(A = A, B = B)
  shp <- dfourier_i(df, nb.pts = nb.pts)
  if (plot)
    coo_plot(shp)
  return(shp)
}

##### end core-dfourier
