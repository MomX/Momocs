# This file contains some cheap yet sufficient
# non-standard evaluation (NSE) mechanisms for Momocs.

# Ellipsis (...) dispatcher -----------------
# Used internally to dispatch arguments, in conjunction with do_with_arg
# within a function, grabs all arguments actually passed and those
# defined in the definition. If some intersect, then what's passed superseed
# what's in the definition
# eg:
# foo <- function(x, wow=FALSE, ...){
#   prepare_dots()
# }
#
# foo()
# foo(wow=TRUE, yop=0.5)
prepare_dots <- function(){
  # get full call as a list
  s <- as.list(sys.call(1))
  # return(s)
  # case where passed within a pipe
  if (s[[1]]=="%>%")
    s <- as.list(s[[3]])
  # get formals list
  f <- formals(as.character(s[[1]]))
  # return(list(s=s, f=f))
  # remove any "..." from formals list
  dots_id <- match("...", names(f))
  if (!is.na(dots_id))
    f <- f[-dots_id]
  # remove function name from the call
  s <- s[-1]
  # don't include unnammed arguments
  f <- f[names(f)!=""]
  s <- s[names(s)!=""]

  vetoed <- intersect(names(f), names(s))
  c(f[!(names(f) %in% vetoed)], s)
}

# Given a function name, say `foo` and a list produced by `prepare_dots`
# dispatch corresponding arguments and eval `.foo` with it (note the `.`)
# For instance, if `.foo` has arguments `arg1` and `arg2` and is
# included within a function, say f(), then we can
# f(foo.arg1=0.5, anotherfun.arg2=FALSE). When the code will encounter do_with_args("foo", dots)
# it will eval .foo(arg=0.5).
# - Any argument will trigger the entire function, unless the function is defined within
# the generic itself.
# - It has a special mechanism to allow evaluation vanilla function when we call
# f(foo=TRUE). In this case, .foo() will be called.
#
# The example below may help understand how it works
# .draw_poly <- function(xy, ...)
#   polygon(xy[, 1], xy[, 2], ...)
#
# .draw_points <- function(xy, ...)
#   points(xy[, 1], xy[, 2], ...)
#
# .draw_axes <- function(...)
#   abline(h=0, v=0, ...)
#
# myplot <- function(xy, draw_poly=TRUE, ...){
#   dots <- prepare_dots()
#   print(dots)
#   plot(xy, asp=1, type="n")
#   do_with_args("draw_axes", dots)
#   do_with_args("draw_poly", dots, with=list(xy=xy, density=45))
#   do_with_args("draw_points", dots, with=list(xy=xy))
# }
#
# myplot(bot[1], draw_poly.col="pink", draw_points.pch=20)
# bot[1] %>% myplot(draw_axes.col="red")
# bot[1] %>% myplot(draw_axes.lty=2, draw_points.col="red", draw_poly=FALSE)


# In Momocs, this is particularly useful for graphics functions that may have many
# parameters and for which fine tuning is desirable but we do not want to surcharge
# either definitions or implementations.
do_with_args <- function(what, dots, with=NULL){
  dot_what <- paste0(".", what)
  what_dot <- paste0(what, ".")
  # turns dots into a named list
  l <- dots
  # tests for empty list
  if (length(l)==0)
    return()
  args <- l %>% names %>%
    # only take what's on the left of the dot
    strsplit("\\.") %>% sapply(`[`, 1) %>%
    # test if a least one correspond to the function of interest
    `%in%`(., what)
  # test for corresponding function
  if (!any(args))
    return()
  # only pick corresponding arguments
  l <- l[args]


  # case where only the function with default argument is asked
  if (what %in% names(l)){
    l_flag <- l[[which(names(l)==what)]]
    if (is.logical(l_flag) && !l_flag)
      return()
    do.call(what=dot_what, args=c(list(), with))
    return()
  }
  # removes the "functioname" pattern (if a default value for plotting
  # the layer is declared in the generic)
  l <- l[!(names(l) %in% what)]
  # removes the "functionname." pattern
  names(l) %<>% gsub(what_dot, "", .)
  # finally call the function which name needs a '.' before
  # here a special Momocs case where
  do.call(dot_what, args=c(l, with))
  return()
}

# Fac handler -----------------

### prepare a factor according to waht is passed to various methods,
# notably multivariate plotters.prepare_fac(bp, 1)
# eg
#  olea$fac$fake <-  rnorm(length(olea))
#  summary(olea$fac)
#
# # Valid ways
# # nothing
# prepare_fac(olea)
# # column id
# prepare_fac(olea, 2)
# # column name
# prepare_fac(olea, "domes")
# # column name NSE style
# prepare_fac(olea, domes)
# # column name formula style
# prepare_fac(olea, ~domes)
# # formulas allow interaction of factors
# prepare_fac(olea, ~domes+var)
# # passing a factor on the fly
# prepare_fac(olea, factor(rep(letters[1:7], each=30)))
# # passing a numeric on the fly
# prepare_fac(olea, rnorm(length(olea)))
#
# # Non valid ways
# # column beyond defined columns
# prepare_fac(olea, 84)
# # non existing column name
# prepare_fac(olea, "rock_and_roll")
# # also, formula style
# prepare_fac(olea, ~rock_and_roll)
# # passing a factor of the wrong length
# prepare_fac(olea, factor(rep(letters[1:7], each=10)))
# # passing a numeric of the wrong length
# prepare_fac(olea, rnorm(210))
prepare_fac <- function(x, fac){
  ### missing case
  if (missing(fac)){
    fac <- NULL
    return(fac)
  }

  # raw column name case, use NSE with the help of dplyr's select
  if (is.name(substitute(fac))){
    x_try <- try(dplyr::select_(x$fac, substitute(fac))[[1]], silent=TRUE)
    if (class(x_try) != "try-error")
      return(x_try)
  }

  ### formula case (is.formula doesnt exist)
  if (class(fac)=="formula"){
    f0 <- x$fac[, attr(terms(fac), "term.labels")]
    fac <- interaction(f0)
    return(fac)
  }

  ### column id case
  if (is.numeric(fac)) {
    # case of a numeric not likely a column id is passed on the fly
    if (length(fac)>1){
      .check(length(fac) == length(x),
             paste0("numeric passed is not of the correct length (", length(x), ")"))
      return(fac)
    }
    # otherwise, we will pick a column
    .check(fac <= ncol(x$fac),
           paste(fac, "is not an existing column name"))
    fac <- factor(x$fac[, fac])
    return(fac)
  }
  ### column name case
  if (is.character(fac)) {
    .check(any(colnames(x$fac) == fac),
           paste(fac, "is not an existing column name"))
    fac <- factor(x$fac[, fac])
    return(fac)
  }
  ### factor case
  if (is.factor(fac)) {
    # case where a factor is directly passed
    .check(length(fac) == length(x),
           paste0("factor passed is not of the correct length (", length(x), ")"))
    # we need it to refactor in subset cases
    fac <- factor(fac)
    return(fac)
  }
}


