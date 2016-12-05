# ready -------
rm(list=ls())
devtools::load_all("~/Research/Momocs")

#' List available methods for any object
#'
#' A simple wrapper around \link{methods} that given any object
#' returns all available methods for its class.
#'
#' @param x any object
#' @examples
#' methods for data.frame
#' list_methods(iris)
#'
#' methods for Coo objects
#' list_methods(bot)
#'
#' methods for Coe objects
#' list_methods(bot %>% efourier)
#'
#' methods for PCA objects
#' list_methods(bot %>% efourier %>% PCA)
#'
#' @export
list_methods <- function(x){
  for (i in class(x)){
    cat("* '", i, "' class:\n", sep="")
    cat(paste0("  - ", methods(class=i), "\n"), sep="")
  }
}

tell_methods <- function(x) {
  m <- try(suppressWarnings(methods(x)), silent=TRUE)
  if (!("try-error" %in% class(m)) && length(m)!=0)
    cat(paste0("  - ", m, "\n"), sep="")
}
tell_methods(PCAAA) # nothing
tell_methods(PCA)
tell_methods(sum) # nothing

coo_chull_onion <- function(coo, ...){
  res <- list()
  i <- 1
  while(is.matrix(coo) && nrow(coo) > 3){
    chi_ids <- chull(coo[, 1], coo[, 2])
    res$ids[[i]] <- chi_ids
    res$coo[[i]] <- coo[chi_ids, ]
    coo <- coo[-chi_ids, ]
    i <- i + 1
  }
  res
}
# shapes[4] %>% coo_chull_onion


draw_rads <- function(shp, ...){
  segments(shp[, 1], shp[, 2], coo_centpos(shp)[1], coo_centpos(shp)[2], ...)
}
# see coo_samplerr for an example


# turns many things into a coo (formely a shp)
prepend_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(class_to_add, .)
  x
}

append_class <- function(x, class_to_add){
  if (!(class_to_add %in% class(x)))
    class(x) %<>% c(., class_to_add)
  x
}

as_out <- function(x){
  x$coo %<>% lapply(prepend_class, "out")
  x
}


# bot %>% class
# add_class(bot, "Out") %>% class()
# add_class(bot, "Coo") %>% class()
# add_class(bot, "plop") %>% class()

as_coo <- function(x, ...){
  x %>% coo_check() %>% prepend_class("coo")
}

complex2coo <- function(Z){
  cbind(Re(Z), Im(Z)) %>% `colnames<-`(c("x", "y"))
}



