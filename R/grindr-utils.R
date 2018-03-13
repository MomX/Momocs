# utils ----------------------------------------------------
is_palette <- function(x){
  any(class(x)=="palette")
}

as_palette <- function(x){
  class(x) <- unique(c(class(x), "palette"))
  x
}

this_dispatch <- function(f, this){
  # non factor case
  if (!is.factor(f) && length(f)==length(this))
    return(this)
  # right length case
  if (length(this)==length(f))
    return(this)
  # one for each level case
  if (length(this)==nlevels(f))
    return(this[f])
  if (is.function(this))
    return(this(nlevels(f))[f])
  # single value case
  if (length(this)==1)
    return(rep(this, length(f)))
  # otherwise
  message("bad length for 'this' argument")
}
