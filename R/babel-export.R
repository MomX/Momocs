##### export functions and methods for use in foreign programs

#' Export Coe objets
#' 
#' Writes a \code{.txt} or \code{.xls} or whatever readable from a \link{Coe}
#' object, along with individual names and grouping factors. A simple wrapper
#' around \link{write.table}.
#' @param x a Coe object
#' @param file the filenames \code{data.txt} by default
#' @param sep the field separator string (see \code{sep} in \link{write.table}). A
#' tab by default
#' @param dec the string to use for decimal points (see \code{dec} in \link{write.table}). A dot
#' by default.
#' @keywords Babel
#' @note Default parameters will write a \code{.txt} file, directly readable by MS Excel 
#' and other programs. With default parameters, numbers will dots as decimal points, which is
#' considered as a character chain in Excel in many countries (locale versions.) this can be solved
#' using \code{dec=","} as in the examples below.
#' 
#' If you are new to R, you may be looking for where this damn file has been saved. With the defaults
#' settings, \code{getwd()} will provide the answer.
#' 
#' I have to mention that everytime you use this function, escape from R to use Excel and
#' do "statistics" there, an adorable kitten is probably murdered somewhere. Use R, not Excel!
#' 
#' @examples
#' \dontrun{
#' # Will write files on your machine!
#' data(bot)
#' bot.f <- eFourier(bot, 6)
#' export(bot.f) # data.txt which can be opened by every software including MS Excel
#' # If you are French, or another country that has not been invaded by anglo-american rules.
#' # and for use in Excel.
#' export(bot.f, dec=",") 
#' export(bot.f, file="data.xls", dec=",")
#' }
#' @rdname export
#' @export
export <- function(x, file, sep, dec){UseMethod("export")}
#' @rdname export
#' @export
export.Coe <- function(x, file="data.txt", sep="\t", dec="."){
  data <- cbind(name=rownames(x$coe), x$fac, x$coe)
  write.table(data, file=file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep=sep, dec=dec)
  cat(" * File written:", file, "\n")}

##### end export