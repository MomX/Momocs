#' Discrete cosinus transform
#' 
#' Calculates discrete cosine transforms, as introduced by Dommergues and colleagues, on
#' a shape (mainly open outlines).
#' @param coo a matrix (or a list) of (x; y) coordinates
#' @param nb.h numeric the number of harmonics to calculate
#' @return a list with the following components:
#' \itemize{
#' \item A the A harmonic coefficients
#' \item B the B harmonic coefficients
#' \item mod the modules of the points
#' \item arg the arguments of the points
#' }
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
#' @keywords discreteCosine
#' @examples
#' # dct and inverse dct
#' data(olea)
#' o <- olea[1]
#' o <- coo.bookstein(o)
#' coo.plot(o)
#' o.dct <- dct(o, nb.h=12)
#' o.dct
#' o.i <- dct.i(o.dct)
#' o.i <- coo.bookstein(o.i)
#' coo.draw(o.i, border="red")
#'
#' #future hqual
#' o <- olea[1]
#' h.range <- 2:13
#' coo <- list()
#' for (i in seq(along=h.range)){
#' coo[[i]] <- dct.i(dct(o, nb.h=h.range[i]))}
#' names(coo) <- paste0("h", h.range)
#' panel(Opn(coo), borders=col.india(12), names=TRUE)
#' title("Discrete Cosine Transforms")
#' @export
dct <- function(coo, nb.h){
  # we cjeck a bit
  coo <- coo.check(coo)
  if (missing(nb.h)) {
    nb.h <- 12
    cat(" * 'nb.h' not provided and set to", nb.h, "\n")}
  # preliminaries
  N <- nrow(coo)
  pol <- coo[, 1] + 1i*coo[, 2] 
  # dct
  c    <- rep(sqrt(2/N), N)
  c[1] <- 1/sqrt(N)
  Sv <- S <- rep(NA, N)
  for (k in 0:(N-1)) {
    for (n in 0:(N-1)) {
      Sv[n+1] <- pol[n+1] * cos(((2*n+1)*k*pi)/(2*N))
    }
    S[k+1] <- c[k+1] * sum(Sv)
  }
    S<-S[2:nb.h] #we remove the 1st harmonic
    return(list(A=Re(S)/N, B=Im(S)/N, 
                mod=Mod(S)/N, phi=Arg(S)))}

#' Investe discrete cosinus transform
#' 
#' Calculates inverse discrete cosine transforms (see \link{dct}), given a list of A and B harmonic coefficients,
#' typically such as those produced by \link{dct}.
#' @param df a list with \code{$A} and \code{$B} components, containing harmonic coefficients.
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
#' @keywords discreteCosine
#' @examples
#' # dct and inverse dct
#' data(olea)
#' o <- olea[1]
#' o <- coo.bookstein(o)
#' coo.plot(o)
#' o.dct <- dct(o, nb.h=12)
#' o.dct
#' o.i <- dct.i(o.dct)
#' o.i <- coo.bookstein(o.i)
#' coo.draw(o.i, border="red")
#'
#' o <- olea[1]
#' h.range <- 2:13
#' coo <- list()
#' for (i in seq(along=h.range)){
#' coo[[i]] <- dct.i(dct(o, nb.h=h.range[i]))}
#' names(coo) <- paste0("h", h.range)
#' panel(Opn(coo), borders=col.india(12), names=TRUE)
#' title("Discrete Cosine Transforms")
#' @export
dct.i <- function(df, nb.pts=60){
  A <- df$A
  B <- df$B
  nb.h <- length(A)+1
  
  c <- rep(sqrt(2/nb.pts),nb.pts)
  c[1] <- 1/sqrt(nb.pts)
  
  S <- A+1i*B
  S <- c(0+0*1i, S) # we add a trivial harmonic corresponding to (0; 0)
  
  sv_r <- rep(NA, nb.h)
  s_r <- rep(NA,  nb.pts)
  # idct pour le nombre d'harmonique spécifié
  for (n in 0:(nb.pts-1)){
    for (k in 0:(nb.h-1)){
      sv_r[k+1] <- c[k+1] * S[k+1]*cos(((2*n+1)*k*pi)/(2*nb.pts))
    }
    s_r[n+1]<-sum(sv_r)
  }
  shp <- cbind(Re(s_r), Im(s_r))
  return(shp)}

#' Calculates and draws "dct" shapes
#' 
#' Calculates shapes based on "Discrete cosine transforms" given harmonic coefficients 
#' (see \link{dct}) or can generate some random "dct" shapes. 
#' Mainly intended to generate shapes and/or to understand how dct works.
#' @param A vector of harmonic coefficients
#' @param B vector of harmonic coefficients
#' @param nb.h if \code{A} and/or \code{B} are not provided, 
#' the number of harmonics to generate
#' @param nb.pts if \code{A} and/or \code{B} are not provided, 
#' the number of points to use to reconstruct the shapes 
#' @param alpha tThe power coefficient associated with the (usually decreasing) 
#' amplitude of the harmonic coefficients (see \link{efourier.shape})
#' @param plot logical whether to plot the shape
#' @examples 
#' # some signatures
#' panel(coo.align(Opn(replicate(48, dct.shape(alpha=0.5, nb.h=6)))))
#' # some worms
#' panel(coo.align(Opn(replicate(48, dct.shape(alpha=2, nb.h=6)))))
#' @export
dct.shape <- function(A, B, nb.h, nb.pts=60, alpha=2, plot=TRUE){
  if (missing(nb.h) &  missing(A)) nb.h <- 3
  if (missing(nb.h) & !missing(A)) nb.h <- length(A)
  if (missing(A)) A <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  if (missing(B)) B <- runif(nb.h, -pi, pi) / (1:nb.h)^alpha
  df  <- list(A=A, B=B)
  shp <- dct.i(df, nb.pts=nb.pts)      
  if (plot) coo.plot(shp)
  return(shp)}

##### end core-dct

