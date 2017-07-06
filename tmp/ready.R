



# ready -------
rm(list=ls())
devtools::load_all("~/Research/Momocs")



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



