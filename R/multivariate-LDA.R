
# 4 - LDA ----------------------------------------------------------------
#' Linear Discriminant Analysis on Coe objects
#' 
#' Performs a LDA on Coe objects. Relies on \link{lda} in MASS.
#' @aliases LDA
#' @param x a \link{Coe}, or a PCA object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @param retain the number of PC axis to retain for LDA.PCA
#' @return a "LDA" object on which to apply \link{plot.LDA}, which is a list with components:
#' \describe{
#' \item{x}{matrix of coefficients used for computation}
#' \item{fac}{grouping structure used}
#' \item{removed}{ids of columns in the original matrix that have been removed since constant (if any)}
#' \item{mod}{raw lda object returned by \link{lda}}
#' \item{mod.pred}{predicted classification using the model. Probably optimistic, you can try CV leave-one-out or other approaches.}
#' \item{CV}{cross-validation table using mod.pred results}
#' \item{correct}{proportion of well classified individuals}
#' \item{LDs}{unstandardized LD scores see Claude (2008)}
#' \item{mshape}{mean values of coefficients in the original matrix}
#' \item{method}{inherited from the Coe object (if any)}
#' }
#' @examples
#' data(bot)
#' bot.f <- eFourier(bot, 24)
#' bot.l <- LDA(bot.f, "type")
#' bot.l
#' plot(bot.l)
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(bot.f, "plop")
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(x, fac, retain){UseMethod("LDA")}

#' @export
LDA.Coe <- function(x, fac, retain){
  Coe <- x
  if (missing(fac)) stop(" * no fac provided")
  fac    <- Coe$fac[, fac]
  X      <- Coe$coe
  remove <- which(apply(X, 2, sd)<1e-10)
  if (length(remove)!=0) {
    cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
    X <- X[, -remove] 
  } else { remove <- NULL }  
  # now we calculate two linear models with MASS::lda
  # one with
  mod      <- lda(X, grouping=fac)
  mod.pred <- predict(mod, X)
  mod.CV   <- lda(X, grouping=fac, CV=TRUE)
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  # we calculate unstandardized LDs
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we build the list with all the components that may be useful elsewhere
  # including the lda output, the CV prediction and unstandardized LDs
  # for shape reconstruction
  LDA <- list(x        = X,
              fac      = fac,
              removed  = remove,
              mod      = mod,
              mod.pred = mod.pred,
              CV       = CV,
              correct  = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape   = apply(Coe$coe, 2, mean),
              method   = Coe$method)
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
LDA.default <- function(x, fac, retain){
  X <- x
  if (missing(fac)) stop(" * no fac provided")
  # we compute lda and predictions
  mod      <- lda(X, grouping=fac)
  mod.pred <- predict(mod, X)
  # we prepare a nice CV table
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  # we calculate unstandardized LDs
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we build the list to be returned
  LDA <- list(x        = X,
              fac      = fac,
              removed  = remove,
              mod      = mod,
              mod.pred = mod.pred,
              CV       = CV,
              correct  = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape   = NULL,
              method   = NULL)
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
LDA.PCA <- function(x, fac, retain=3){
  PCA <- x
  if (missing(fac)) stop(" * no fac provided")
  fac    <- PCA$fac[, fac]
  if (missing(retain)) { cat(" * the first", retain, "PC axes are used.") } 
  X      <- PCA$x[, 1:retain]
  if (is.matrix(X)){
    remove <- which(apply(X, 2, sd)<1e-10)
    if (length(remove)!=0) {
      cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
      X <- X[, -remove] }
  } else { remove <- NULL } 
  X <- as.matrix(X)
  # we compute lda and predictions
  mod      <- lda(X, grouping=fac, tol=1e-10)
  mod.pred <- predict(mod, X)
  # we calculate unstandardized LDs (wrong here for use in shape reconstruction, 
  # would need one more step (pca2shp?) but not sure how useful it is)
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw    <- n - nlevels(fac)
  SSw    <- var(lm.mod$residuals) * (n-1)
  VCVw   <- SSw / dfw
  LDs    <- VCVw %*% mod$scaling
  # we prepare a nice CV table
  CV <- table(fac, mod.pred$class)
  names(dimnames(CV)) <- c("actual", "classified")
  LDA <- list(x          = X,
              fac        = fac,
              removed    = remove,
              mod        = mod,
              mod.pred   = mod.pred,
              CV         = CV,
              correct    = sum(diag(CV))/sum(CV),
              LDs      = LDs,
              mshape     = NULL,
              method     = PCA$method) # may be interesting to add LDA on PCA here?
  LDA$method <- PCA$method
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)}

#' @export
print.LDA <- function(x, ...){
  cat("An 'LDA' object. See ?LDA.\n")
  cat("Actual / predicted groups (", signif(x$correct * 100, 3), "% ): \n")
  print(x$CV)}
