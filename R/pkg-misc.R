##### Miscellaneous functions

# utils ---------

which_collinear <- function(x, tol=1-1e-4){
  # correlation matrix
  cor_m <- cor(x)
  # dont need the diag and upper tri
  cor_m[upper.tri(cor_m, diag = TRUE)] <- 0
  # find which are above threshold
  apply(cor_m, 2, function(.x) any(abs(.x) > tol)) %>%
    which
}

remove_collinear <- function(x, tol=1-1e-4){
  ids <- which_collinear(x, tol=tol)
  # if none, return x
  if (length(ids)==0)
    return(x)

  # otherwise return x minus collinear columns
  # with message if verbose
  if (.is_verbose()){
    if (is.null(colnames(x)))
      message("removed ", length(ids), " collinear columns")
    else
      message("removed these collinear columns: ", paste(colnames(x)[ids], collapse=", "))
  }
  x[, -ids]
}


which_constant <- function(x, tol=1e-4){
  which(apply(x, 2, sd) < tol)
}

remove_constant <- function(x, tol=1e-4){
  ids <- which_constant(x, tol=tol)
  # if none, return x
  if (length(ids)==0)
    return(x)

  # otherwise return x minus collinear columns
  # with message if verbose
  if (.is_verbose()){
    if (is.null(colnames(x)))
      message("removed ", length(ids), " constant columns")
    else
      message("removed these constant columns: ", paste(colnames(x)[ids], collapse=", "))
  }
  x[, -ids]
}

reinsert_columns <- function(x_keep, x_drop, ids_keep, ids_drop){
  # if one of ids is empty, return the other x
  if (length(ids_drop)==0)
    return(x_keep)
  if (length(ids_keep)==0)
    return(x_drop)

  # a prototypic new matrix filled with NAs
  x_back <- cbind(x_keep, x_drop)
  dimnames(x_back) <- NULL # safer
  x_back[] <- NA
  # ensure sets are disjuncts
  n <- ncol(x_back)
  .check(length(unique(c(ids_keep, ids_drop)))==n,
         "set are not disjunct are matrices do not have the right dimensions")
  # bind them back - safer than sapply when single LD
  lapply(1:n,
         # one of the two which will return an integer(0)
         function(i) cbind(x_keep[, which(ids_keep==i)], x_drop[, which(ids_drop==i)])) %>%
    do.call("cbind", .)
}

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
    cat(paste0("  - ", utils::methods(class=i), "\n"), sep="")
  }
}

#' @rdname andnow
#' @export
andnow_method <- function(x) {
  m <- try(suppressWarnings(utils::methods(x)), silent=TRUE)
  if (!("try-error" %in% class(m)) && length(m)!=0)
    cat(paste0("  - ", m, "\n"), sep="")
  else
    NULL
}


# Momocs_*() -----------
#' Momocs helpers
#'
#' Help access Momocs website (`Momocs_website`)
#' and online help (`Momocs_help`),
#' last released function (`Momocs_currentCRANversion`),
#' last development version on GitHub (`Momocs_currentGitHubversion`)`,
#' install the latter (`Momocs_installlastversion`) ,
#' know the currently installed version (`Momocs_installedversion`).
#' @param topic `character` quoted function name for which you want to browse online help
#' @rdname Momocs_helpers
#' @examples
#' \dontrun{
#' Momocs_website()
#' Momocs_help("efourier")
#' Momocs_currentCRANversion()
#' Momocs_currentGitHubversion()
#' Momocs_installlastversion()
#' Momocs_installedversion
#' }
#' @rdname Momocs_helpers
#' @export
Momocs_installedversion <- function() {
  utils::packageVersion("Momocs")
}

#' @rdname Momocs_helpers
#' @export
Momocs_installlastversion <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools needed for this function to work. Please install it with install.packages('devtools')",
         call. = FALSE)
  }
  devtools::install_github("MomX/Momocs", build_vignettes= TRUE)
  library(Momocs)
  message("last Momocs version has been installed from GitHub. Loaded into R")
}

#' @rdname Momocs_helpers
#' @export
Momocs_currentGitHubversion <- function(){
  "https://raw.githubusercontent.com/MomX/Momocs/master/DESCRIPTION" %>%
    readLines(n=3) %>%
    `[`(3) %>%
    gsub("Version: ", "", .)
}

#' @rdname Momocs_helpers
#' @export
Momocs_currentCRANversion <- function(){
  x <- readLines("https://cran.r-project.org/web/packages/Momocs/index.html")
  x[grep("<td>Version:</td>", x)+1] %>%
    gsub("<td>", "", .) %>% gsub("</td>", "", .)
}

#' @rdname Momocs_helpers
#' @export
Momocs_help <- function(topic=NULL){
  url <- "http://momx.github.io/Momocs/reference/"
  if (!is.null(topic)) url <- paste0(url, topic, ".html")
  utils::browseURL(url)
}

#' @rdname Momocs_helpers
#' @export
Momocs_website <- function() {
    utils::browseURL("http://momx.github.io/Momocs")
}

