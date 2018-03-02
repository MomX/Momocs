##### export functions and methods for use in foreign programs

#' Exports Coe objects and shapes
#'
#' Writes a \code{.txt} or \code{.xls} or whatever readable from a single shape,
#' a  \link{Coe}, or a \link{PCA} object, along with individual names and \code{$fac}.
#'
#' @note This is a simple wrapper around \link{write.table}.
#' @param x a \code{Coe} or \code{PCA} object
#' @param file the filenames \code{data.txt} by default
#' @param sep the field separator string to feed \link{write.table}). (default to tab)
#' tab by default
#' @param dec the string  to feed \link{write.table}) (default \code{"."})
#' by default.
#'
#' @note Default parameters will write a \code{.txt} file,
#' readable by foreign programs.
#' With default parameters, numbers will use dots as decimal points, which is
#' considered as a character chain in Excel in many countries (locale versions).
#' This can be solved by using \code{dec=','} as in the examples below.
#'
#' If you are looking for your file, and did not specified \code{file},
#'  \code{getwd()} will help.
#'
#' I have to mention that everytime you use this function,
#' and cowardly run from R to Excel and
#' do 'statistics' there, an innocent and adorable kitten
#' is probably murdered somewhere. Use R!
#'
#' @examples
#' \dontrun{
#' # Will write files on your machine!
#' bf <- efourier(bot, 6)
#' # Export Coe (here Fourier coefficients)
#' export(bf) # data.txt which can be opened by every software including MS Excel
#'
#' # If you come from a country that uses comma as decimal separator (not recommended, but...)
#' export(bf, dec=',')
#' export(bf, file='data.xls', dec=',')
#'
#' # Export PCA scores
#' bf %>% PCA %>% export()
#'
#' # for shapes (matrices)
#'  export(bot[1], file='bot1.txt')
#'
#'  # remove these files from your machine
#'  file.remove("coefficients.txt", "data.xls", "scores.txt")
#' }
#' @family bridges functions
#' @export
export <- function(x, file, sep, dec) {
    UseMethod("export")
}

#' @export
export.Coe <- function(x, file = "coefficients.txt", sep = "\t", dec = ".") {
    data <- cbind(name = rownames(x$coe), x$fac, x$coe)
    write.table(data, file = file, row.names = FALSE, col.names = TRUE,
        quote = FALSE, sep = sep, dec = dec)
    message("File written: ", file)
}

#' @export
export.PCA <- function(x, file = "scores.txt", sep = "\t", dec = ".") {
  data <- cbind(name = rownames(x$x), x$fac, x$x)
  write.table(data, file = file, row.names = FALSE, col.names = TRUE,
              quote = FALSE, sep = sep, dec = dec)
  message("File written: ", file)
}

#' @export
export.matrix <- function(x, file = "data.txt", sep = "\t", dec = ".") {
    colnames(x) <- c("x", "y")
    write.table(x = x, file = file, quote = FALSE, row.names = FALSE,
        col.names = TRUE, sep = "\t", dec = ".")
    message("File written: ", file)
}

##### end export
