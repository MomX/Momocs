##### Miscellaneous functions for Fourier-based approaches

#' Helps to select a given number of harmonics from a numerical vector.
#'
#' \code{coeff_sel} helps to select a given number of harmonics by returning
#' their indices when arranged as a numeric vector. For instance, harmonic
#' coefficients are arranged in the \code{$coe} slot of \code{\link{Coe}}-objects in
#' that way: \eqn{A_1, \dots, A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1,
#' \dots, D-n} after an elliptical Fourier analysis (see \link{efourier} and
#' \link{efourier}) while \eqn{C_n and D_n} harmonic are absent for radii
#' variation and tangent angle approaches (see \link{rfourier} and
#' \link{tfourier} respectively). . This function is used internally but might
#' be of interest elwewhere.
#'
#' @param retain \code{numeric}. The number of harmonics to retain.
#' @param drop \code{numeric}. The number of harmonics to drop
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return \code{coeff_sel} returns indices that can be used to select columns
#' from an harmonic coefficient matrix. \code{coeff_split} returns a named list
#' of coordinates.
#' @examples
#' bot.f <- efourier(bot, 32)
#' coe <- bot.f$coe # the raw matrix
#' coe
#' # if you want, say the first 8 harmonics but not the first one
#' retain <- coeff_sel(retain=8, drop=1, nb.h=32, cph=4)
#' head(coe[, retain])
#' @export
coeff_sel <- function(retain = 8, drop = 0, nb.h = 32, cph = 4) {
  cs <- numeric()
  for (i in 1:cph) {
    cs <- c(cs, (1 + drop):retain + nb.h * (i - 1))
  }
  return(cs)
}

#' Converts a numerical description of harmonic coefficients to a named list.
#'
#' \code{coeff_split} returns a named list of coordinates from a vector of
#' harmonic coefficients. For instance, harmonic coefficients are arranged in
#' the \code{$coe} slot of \code{Coe}-objects in that way: \eqn{A_1, \dots,
#' A_n, B_1, \dots, B_n, C_1, \dots, C_n, D_1, \dots, D-n} after an elliptical
#' Fourier analysis (see \link{efourier} and \link{efourier}) while \eqn{C_n
#' and D_n} harmonic are absent for radii variation and tangent angle
#' approaches (see \link{rfourier} and \link{tfourier} respectively). This
#' function is used internally but might be of interest elwewhere.
#'
#' @param cs A \code{vector} of harmonic coefficients.
#' @param nb.h \code{numeric}. The maximum harmonic rank.
#' @param cph \code{numeric}. Must be set to 2 for \code{rfourier} and
#' \code{tfourier} were used.
#' @return Returns a named list of coordinates.
#' @examples
#' coeff_split(1:128, nb.h=32, cph=4) # efourier
#' coeff_split(1:64, nb.h=32, cph=2)  # t/r fourier
#' @export
coeff_split <- function(cs, nb.h = 8, cph = 4) {
  if (missing(nb.h)) {
    nb.h <- length(cs)/cph
  }
  cp <- list()
  for (i in 1:cph) {
    cp[[i]] <- cs[1:nb.h + (i - 1) * nb.h]
  }
  names(cp) <- paste(letters[1:cph], "n", sep = "")
  return(cp)
}

#' Rearrange a matrix of (typically Fourier) coefficients
#'
#' Momocs uses colnamed matrices to store (typically) Fourier coefficients
#' in \link{Coe} objects (typically \link{OutCoe}). They are arranged as rank-wise:
#' \code{A1, A2, ..., An, B1, ..., Bn, C1, ..., Cn, D1, ..., Dn}. From other softwares they may arrive
#' as \code{A1, B1, C1, D1, ..., An, Bn, Cn, Dn}, this functions helps to go
#' from one to the other format. In short, this function rearranges column order. See examples.
#'
#' @param x matrix (with colnames)
#' @param by character either "name" (\code{A1, A2, ..}) or "rank" (\code{A1, B1, ...})
#' @examples
#' m_name <- m_rank <- matrix(1:32, 2, 16)
#' # this one is order by name
#' colnames(m_name) <- paste0(rep(letters[1:4], each=4), 1:4)
#' # this one is order by rank
#' colnames(m_rank) <- paste0(letters[1:4], rep(1:4, each=4))
#'
#' m_rank
#' m_rank %>% coeff_rearrange(by="name")
#' m_rank %>% coeff_rearrange(by="rank") #no change
#'
#' m_name
#' m_name %>% coeff_rearrange(by="name") # no change
#' m_name %>% coeff_rearrange(by="rank")
#' @export
coeff_rearrange <- function(x, by=c("name", "rank")[1]){
  map <- data.frame(old_id=1:ncol(x),
                    old_cn=colnames(x),
                    name=x %>% colnames %>% substr(1, 1),
                    rank=x %>% colnames %>% substr(2, nchar(.)) %>% as.numeric) %>%
    dplyr::arrange_(by) %>%
    mutate(new_cn=paste0(name, rank))
  return(x[, map$old_id])
}


#' Calculates harmonic power given a list from e/t/rfourier
#'
#' Given a list with \code{an, bn (and eventually cn and dn)}, returns the
#' harmonic power.
#'
#' @param xf A list with an, bn (and cn, dn) components, typically from a
#' e/r/tfourier passed on coo_
#' @return Returns a \code{vector} of harmonic power
#' @examples
#'
#' ef <- efourier(bot[1], 24)
#' rf <- efourier(bot[1], 24)
#' harm_pow(ef)
#' harm_pow(rf)
#'
#' plot(cumsum(harm_pow(ef)[-1]), type='o',
#'   main='Cumulated harmonic power without the first harmonic',
#'   ylab='Cumulated harmonic power', xlab='Harmonic rank')
#'
#' @export
harm_pow <- function(xf) {
  if (is.list(xf)) {
    if (all(c("an", "bn", "cn", "dn") %in% names(xf))) {
      hp <- (xf$an^2 + xf$bn^2 + xf$cn^2 + xf$dn^2)/2
    } else {
      if (all(c("an", "bn") %in% names(xf))) {
        hp <- (xf$an^2 + xf$bn^2)/2
      }
    }
    names(hp) <- paste0("H", 1:length(hp))
    return(hp)
  } else {
    stop("a list containing 'an', 'bn' ('cn', 'dn') harmonic coefficients must be provided")
  }
}

##### end misc Fourier
