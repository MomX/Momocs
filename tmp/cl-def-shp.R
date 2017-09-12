
as_shp <- function(x){
  UseMethod("as_shp")
}

as_shp.default <- function(x){
  message("* not defined on this class")
}

as_shp.Coo <- function(x){
  x$coo %<>% .prepend_class(class_to_add = "shp")
  x
}

as_shp.matrix <- function(x){
  x %>% .prepend_class(class_to_add = "shp")
}

as_shp.list <- function(x){
  x %>% l2m %>% .prepend_class(class_to_add = "shp")
}

as_shp.matrix <- function(x){
  x %>% .prepend_class(class_to_add = "shp")
}

plot.shp <- function(x, y, ...){w
  legend.args <- names(formals(legend))
  plot.args <- c(names(formals(plot.default)), names(par()))
  do.call('plot', c(list(x = x, y = x), dots[!(names(dots) %in% leg.args.unique)]))
  do.call('legend', c(list("bottomleft", "bar"), dots[names(dots) %in% leg.args.all]))
}
bot[4] %>% as_shp %>% plot()
