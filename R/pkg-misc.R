##### Miscellaneous functions

# andnow --------
#' And now, what to do?
#'
#' [andnow], given an object, return available methods for its class(es);
#' [andnow_method], given a function/method name, return supported classes.
#'
#' @param x any object, or class (quoted or not)
#' @examples
#' #methods for data.frame
#' andnow(iris)
#'
#' #methods for Coo objects
#' andnow(bot)
#'
#' #classes supported by efourier
#' andnow_method("efourier")
#'
#' # methods for plot
#' andnow_method("plot")
#' @rdname andnow
#' @export
andnow <- function(x){
  for (i in class(x)){
    cat("* '", i, "' class:\n", sep="")
    cat(paste0("  - ", methods(class=i), "\n"), sep="")
  }
}

#' @rdname andnow
#' @export
andnow_method <- function(x) {
  m <- try(suppressWarnings(methods(x)), silent=TRUE)
  if (!("try-error" %in% class(m)) && length(m)!=0)
    cat(paste0("  - ", m, "\n"), sep="")
  else
    NULL
}


# Momocs_*() -----------
#' Install last version of Momocs
#'
#' Download the last version of Momocs from its GitHub account
#' from \code{http://www.github.com/MomX/Momocs}), install it and load it (\code{library(Momocs)}).
#' You need devtools, but it is checked anyway.
#' @rdname Momocs_version
#' @name Momocs_version
#' @examples
#' \dontrun{
#' Momocs_currentGitHubversion()
#' Momocs_currentCRANversion()
#' }
#' @rdname Momocs_version
#' @name Momocs_version
#' @export
Momocs_lastversion <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools needed for this function to work. Please install it with install.packages('devtools')",
         call. = FALSE)
  }
  devtools::install_github("MomX/Momocs", build_vignettes= TRUE)
  library(Momocs)
  message("last Momocs version has been installed from GitHub. Loaded into R")
}

#' @rdname Momocs_version
#' @name Momocs_version
#' @export
Momocs_currentGitHubversion <- function(){
  "https://raw.githubusercontent.com/MomX/Momocs/master/DESCRIPTION" %>%
    readLines(n=3) %>%
    `[`(3) %>%
    gsub("Version: ", "", .)
}

#' @rdname Momocs_version
#' @name Momocs_version
#' @export
Momocs_currentCRANversion <- function(){
  x <- readLines("https://cran.r-project.org/web/packages/Momocs/index.html")
  x[grep("<td>Version:</td>", x)+1] %>%
    gsub("<td>", "", .) %>% gsub("</td>", "", .)
}


#' Browse Momocs online doc
#'
#' Launch a browser to an online version of the manual
#' @param topic the function name to access. If not specified the homepage of
#' the online manual is accessed.
#' @examples
#' \dontrun{
#' Momocs_help("efourier")
#' }
#' @export
Momocs_help <- function(topic=NULL){
  url <- "http://momx.github.io/Momocs/reference/"
  if (!is.null(topic)) url <- paste0(url, topic, ".html")
  browseURL(url)
}

#' @rdname Momocs_version
#' @name Momocs_version
#' @export
Momocs_installedversion <- function() {
  packageVersion("Momocs")
}

