
# Out builder and domestic functions -------------------------------------------

#' Builds an Out object
#'
#' In Momocs, Out classes objects are lists of closed outlines, on which generic methods
#' such as plotting methods (e.g. stack()) and specific methods (e.g. efourier()) can be applied.
#' 
#' They must be built from a list (or an array) of coordinates.
#'  
#' @export Out Coo 
#' 
#' @param coo a matrix of (x,y) coordinates or a \code{Out} object.
#' @param ldk (optionnal) a list of landmarks on these coordinates (provided as the row numbers) for every outline
#' @param fac (optionnal) a data.frame of factors, specifying the grouping structure.
#' @return a \code{Out} object.
#' @aliases Coo
#' @family Out
#' @keywords Out
#' @examples
#' coo.list <- list(out1=matrix(1:10, nc=2),
#' out2=matrix(1:20, nc=2))
#' anOutobject <- Out(coo.list)
#' anOutobject
# Out class builder
Out  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=list(), fac=fac)
  class(Out) <- "Out"
  return(Out)}

# For historical reasons.
# Before version 1.xxx 'Out' classes used to be S4 classes and called 'Coo'
# See http://www.jstatsoft.org/v56/i13
Coo  <- function(coo.list, ldk=list(), fac=data.frame()){
  Out <- list(coo=coo.list, ldk=list(), fac=fac)
  class(Out) <- "Out"
  return(Out)}


# The print method for Out objects
print.Out <- function(Out){
  ### Header
  cat("An Out object (see ?Out) with: \n")
  cat(rep("-", 20),"\n", sep="")
  coo.nb  <- length(Out)
  coo.len <- sapply(Out$coo, nrow)
  coo.closed <- sapply(Out$coo, is.closed)
  # number of outlines
  cat(" -", coo.nb, "outlines\n")
  # one random outline
  eg <- sample(length(Out), 1)
  coo.eg <- Out$coo[[eg]]
  colnames(coo.eg) <- c("x", "y")
  cat(" - One random outline in $coo: '", names(Out)[eg], "':\n", sep="")
  if (nrow(coo.eg) > 5) {
    print(coo.eg[1:5, ], print.gap=2)
    cat("etc.\n")
  } else {
    print(coo.eg, print.gap=2)
    cat("\n\n")}
  # number of coordinates
  cat(" -", round(mean(coo.len )), "+/-", round(sd(coo.len )), "coordinates per outline\n")
  # outlines closed or not
  if (all(coo.closed)) {
    cat(" - All outlines are closed\n")
  } else {
    if (any(!coo.closed)) {
      cat(" - All outlines are unclosed\n")
    } else {
      cat(" -", sum(coo.closed), "outlines are closed\n")}}
  # number of landmarks
  if (length(Out$ldk)!=0) {
    cat(" -", length(Out$ldk[[1]]), "landmark(s) defined\n")
  } else {
    cat(" - No landmark defined\n")}
  # number of grouping factors
  df <- Out$fac
  nf <- ncol(df)
  if (nf==0) {
    cat(" - No groups defined\n")
  } else {
    cat(" -", nf, "grouping factor(s) defined:\n")
    for (i in 1:nf) {
      cat("     ", colnames(df)[i], ": ", levels(df[, i]),"\n")}}}

# allows to maintain the tradition str() behaviour
#' @export str.Out "[.Out" "[[.Out" length.Out names.Out "names<-.Out" print.Out
#' @export plot.Out stack.Out panel panel.Out 
str.Out <- function(Out){
  ls.str(Out)}

# Out can be indexing both to [ ] and [[ ]]
# and returns the corresponding coordinate(s)
# We define some getters
"[.Out" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

"[[.Out" <- function(x, i, ...) {
  if (missing(i))    { return(x$coo[])    }
  if (is.integer(i)) { return(x$coo[i])   }
  if (is.numeric(i)) { return(x$coo[[i]]) }}

# length on an Out return the length of Out$coo, ie the number of coordinates
length.Out <- function(Out) {
  return(length(Out$coo))}

# names() on a Out retrieves the names of the Out$coo
names.Out <- function(Out){
  return(names(Out$coo))}

# which can in return may be named using names(Out) <- 
"names<-.Out" <- function(x, value){
  names(x$coo) <- value
  return(x)}

# candidate for the dirtiest function ever
subset.Out <- function(Out, subset){
  e <- substitute(subset)
  retain <- eval(e, Out$fac, parent.frame())
  Out2 <- Out
  Out2$coo <- Out$coo[retain]
  if (length(Out$ldk)>0) Out2$ldk <- Out$ldk[retain]
  if (ncol(Out$fac)>0) {
    Out2$fac <- Out$fac
    Out2$fac <- as.data.frame(Out2$fac[retain, ])
    names(Out2$fac) <- names(Out$fac)
    for (i in ncol(Out2$fac)){
      Out2$fac[, i] <- factor(Out2$fac[, i])}
  }
  return(Out2)}

# Out plotting methods ----------------------------------------------------
# The main plot method that when plot(Out)
# For a quick investigation of the shpes included in a Coo object
plot.Out <- function(Out, id, ...){
  if (missing(id)) {
    repeat{
      id <- sample(length(Out), 1)
      coo.plot(Out$coo[[id]], main=names(Out)[id], ...)
      readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}
  if (id[1]=="all") { id <- 1:length(Out)}
  if (is.numeric(id)){
    if (length(id)==1) {
      coo.plot(Out$coo[[id]], main=names(Out)[id], ...)
    } else {
      for (i in seq(along=id)) {
        coo.plot(Out$coo[[id[i]]], main=names(Out)[id[i]], ...)
        readline(prompt = "Press <Enter> to continue, <Esc> to quit...")}}}}

# stack(Out) shows all the shapes stacked on the same plane
stack.Out <- function(x, cols, borders,
                      points=FALSE, first.point=TRUE, centroid=TRUE,
                      ldk=TRUE, ldk.pch=3, ldk.col="red", ldk.cex=1, xy.axis=TRUE){
  Out <- x
  if (missing(cols)) {
    cols     <- rep(NA, length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#33333355", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))}
  op <- par(mar=c(3, 3, 2, 1))
  on.exit(par(op))
  wdw <- apply(l2a(lapply(Out$coo, function(x) apply(x, 2, range))), 2, range)
  plot(NA, xlim=wdw[, 1], ylim=wdw[, 2], asp=1, las=1, cex.axis=2/3, ann=FALSE, frame=FALSE)
  if (xy.axis) {abline(h=0, v=0, col="grey80", lty=2)}
  for (i in 1:length(Out)) {
    coo.draw(Out$coo[[i]], col=cols[i], border=borders[i],
             points=points, first.point=TRUE, centroid=centroid)}
  if (ldk & length(Out$ldk)!=0) {
    points(Out[Out$ldk, ], pch=ldk.pch, col=ldk.col, cex=ldk.cex)}}

# panel(Out) for a family picture of the shapes
panel <- function(x, ...){UseMethod("panel")}
panel.Out <- function(Out, cols, borders, names=NULL, cex.names=0.6, ...){
  
  if (missing(cols)) {
    cols     <- rep("#33333322", length(Out))}
  if (length(cols)!=length(Out)) {
    cols     <- rep(cols[1], length(Out))}
  if (missing(borders)) {
    borders     <- rep("#333333", length(Out))}
  if (length(borders)!=length(Out)) {
    cols     <- rep(borders[1], length(Out))}
  pos <- coo.list.panel(Out$coo, cols=cols, borders=borders, ...)
  if (!is.null(names)){
    if (is.logical(names)) {
      text(pos[,1], pos[,2], labels=names(Out), cex=cex.names)
    } else {    
      if (length(names)!=length(Out)) stop("* 'names' and Out lengths differ.")
      text(pos[,1], pos[,2], labels=names, cex=cex.names)}}}
