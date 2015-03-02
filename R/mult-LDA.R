# LDA methods on Coe -------------

#' Linear Discriminant Analysis on Coe objects
#'
#' Performs a LDA on Coe objects. Relies on \link{lda} in MASS.
#' @aliases LDA
#' @rdname LDA
#' @param x a  PCA object
#' @param fac the grouping factor (names of one of the $fac column or column id)
#' @param retain the proportion of the total variance to retain (if retain<1) using \link{scree}, or the number of PC axis (if retain>1).
#' @param ... additional arguments to feed \link{lda}
#' @return a 'LDA' object on which to apply \link{plot.LDA}, which is a list with components:
#' \itemize{
#'  \item \code{x} any \link{Coe} object (or a matrix)
#'  \item \code{fac} grouping factor used
#'  \item \code{removed} ids of columns in the original matrix that have been removed since constant (if any)
#'  \item \code{mod} the raw lda mod from \link{lda}
#'  \item \code{mod.pred} the predicted model using x and mod
#'  \item \code{CV.fac} cross-validated classification
#'  \item \code{CV.tab} cross-validation tabke
#'  \item \code{CV.correct} proportion of correctly classified individuals
#'  \item \code{CV.ce} class error
#'  \item \code{LDs} unstandardized LD scores see Claude (2008)
#'  \item \code{mshape} mean values of coefficients in the original matrix
#'  \item \code{method} inherited from the Coe object (if any)
#' }
#' @seealso \link{plot.LDA}, \link{plot_CV}
#' @keywords Multivariate
#' @examples
#' data(bot)
#' bot.f <- efourier(bot, 24)
#' bot.p <- PCA(bot.f)
#' LDA(bot.p, 'type', retain=0.99) # retains 0.99 of the total variance
#' LDA(bot.p, 'type', retain=5) # retain 5 axis
#' bot.l <- LDA(bot.p, 'type', retain=0.99)
#' bot.l
#' plot(bot.l)
#' bot.f$fac$plop <- factor(rep(letters[1:4], each=10))
#' bot.l <- LDA(PCA(bot.f), 'plop')
#' bot.l
#' plot(bot.l)
#' @export
LDA <- function(x, fac, retain, ...) {
  UseMethod("LDA")
}

#' @rdname LDA
#' @export
LDA.default <- function(x, fac, retain, ...) {
  X <- x
  if (!is.matrix(X))
    X <- as.matrix(X)
  if (missing(fac))
    stop(" * no fac provided")
  # now we calculate two lda models with MASS::lda one with
  mod <- lda(X, grouping = fac)
  mod.pred <- predict(mod, X)
  # leave-one-out cross validation
  CV.fac <- lda(X, grouping = fac, tol = 1e-08, CV = TRUE,
                ...)$class
  # we build a nice table from it
  CV.tab <- table(fac, CV.fac)
  names(dimnames(CV.tab)) <- c("actual", "classified")
  CV.correct <- sum(diag(CV.tab))/sum(CV.tab)
  # we calculate unstandardized LDs
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw <- n - nlevels(fac)
  SSw <- var(lm.mod$residuals) * (n - 1)
  VCVw <- SSw/dfw
  LDs <- VCVw %*% mod$scaling
  
  # class error
  tab <- CV.tab
  ce <- numeric(nrow(tab))
  for (i in 1:nrow(tab)) ce[i] <- sum(tab[i, -i])/sum(tab[i, ])
  names(ce) <- rownames(tab)
  
  # we build the list to be returned
  LDA <- list(x = X, fac = fac, removed = remove, mod = mod,
              mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab,
              CV.correct = CV.correct, CV.ce = ce, LDs = LDs, mshape = NULL, method = "other")
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)
}

#' @export
LDA.Coe <- function(x, fac, retain, ...) {
  stop(" * LDA on Coe is deprecated. Try on a PCA object.")
}

#' @rdname LDA
#' @export
LDA.PCA <- function(x, fac, retain = 0.99, ...) {
  PCA <- x
  f0 <- fac
  #fac handling
  if (missing(fac))
    stop(" * no 'fac' provided.")
  if (class(fac)=="formula"){
    fform <- x$fac[, attr(terms(fac), "term.labels")]
    fac <- interaction(fform)}
  if (!is.factor(fac)) { fac <- factor(x$fac[, fac]) }
  # PC number selection
  if (retain <= 1)  {
    cat(" *", retain, "total variance.\n")
    retain <- scree_min(x, prop = retain)
  }
  cat(" *", retain, "PC retained.\n")
  X <- PCA$x[, 1:retain]
  if (is.matrix(X)) {
    remove <- which(apply(X, 2, sd) < 1e-10)
    if (length(remove) != 0) {
      cat(" * variables", colnames(X)[remove], "are removed since they are constant.\n")
      X <- X[, -remove]
    }
  } else {
    remove <- NULL
  }
  X <- as.matrix(X)
  # now we calculate two lda models with MASS::lda one with
  mod <- lda(X, grouping = fac, tol = 1e-08, ...)
  mod.pred <- predict(mod, X)
  # leave-one-out cross validation
  CV.fac <- lda(X, grouping = fac, tol = 1e-08, CV = TRUE,
                ...)$class
  # we build a nice table from it
  CV.tab <- table(fac, CV.fac)
  names(dimnames(CV.tab)) <- c("actual", "classified")
  CV.correct <- sum(diag(CV.tab))/sum(CV.tab)
  # we calculate unstandardized LDs (wrong here for use in
  # shape reconstruction, would need one more step (PCA2shp?)
  # but not sure how useful it is)
  n <- nrow(X)
  lm.mod <- lm(X ~ fac)
  dfw <- n - nlevels(fac)
  SSw <- var(lm.mod$residuals) * (n - 1)
  VCVw <- SSw/dfw
  LDs <- VCVw %*% mod$scaling
  #
  # class error
  tab <- CV.tab
  ce <- numeric(nrow(tab))
  for (i in 1:nrow(tab)) ce[i] <- sum(tab[i, -i])/sum(tab[i, ])
  names(ce) <- rownames(tab)
  
  LDA <- list(x = X, fac = fac, f0 = f0, removed = remove, mod = mod,
              mod.pred = mod.pred, CV.fac = CV.fac, CV.tab = CV.tab,
              CV.correct = CV.correct, CV.ce = ce, LDs = LDs, 
              mshape = NULL, method = "LDAPCA")  # may be interesting to add LDA on PCA here?
  class(LDA) <- c("LDA", class(LDA))
  return(LDA)
}

#' @export
print.LDA <- function(x, ...) {
  cat("Leave-one-out cross-validation ($CV.correct): (",
      signif(x$CV.correct * 100, 3), "% - ", 
      sum(diag(x$CV.tab)), "/", sum(x$CV.tab),
      "): \n", sep = "")
  
  cat("\nCross-validation table ($CV.tab):\n")
  print(x$CV.tab)
  
  cat("\nClass error ($CV.ce):\n")
  print(x$CV.ce)
}

# reLDA -----------


#' "Redo" a LDA on new data
#' 
#' Basically a wrapper around \link{predict.lda} from the package MASS. Uses a LDA model
#' to classify new data.
#' @param LDA a \link{LDA} object
#' @param newdata to use; only implemented for \link{PCA} object/scores.
#' @return a list with components (see ?predict.lda for the first three). If the model LDA was done using a single factor
#' (not a formula), then additional informations are returned around actual vs. predicted classes :
#' \itemize{
#' \item class factor of classification
#' \item posterior posterior probabilities for the classes
#' \item x the scores of test cases 
#' \item res data.frame of the results
#' \item CV.tab a confusion matrix of the results
#' \item CV.correct proportion of the diagonal of CV.tab
#' \item newdata the data used to calculate passed to predict.lda
#' }
#' @note Uses the same number of PC axis as the LDA object provided. You should probably use \link{rePCA} in 
#' conjonction with reLDA to get 'homologous' scores.
#' @examples
#' data(bot)
#' # We select the first 10 individuals in bot, 
#' # for whisky and beer bottles. It will be our referential.
#' bot1   <- slice(bot, c(1:10, 21:30))
#' # Same thing for the other 10 individuals. 
#' # It will be our unknown dataset on which we want
#' # to calculate classes.
#' bot2   <- slice(bot, c(11:20, 31:40))
#' 
#' # We calculate efourier on these two datasets
#' bot1.f <- efourier(bot1, 8)
#' bot2.f <- efourier(bot2, 8)
#' 
#' # Here we obtain our LDA model: first, a PCA, then a LDA
#' bot1.p <- PCA(bot1.f)
#' bot1.l <- LDA(bot1.p, "type")
#' 
#' # we redo the same PCA since we worked with scores
#' bot2.p <- rePCA(bot1.p, bot2.f)
#' 
#' # we finally "predict" with the model obtained before
#' bot2.l <- reLDA(bot1.l, bot2.p)
#' 
#' # Note that we can use plot_CV and plot_CV2 as follows
#' plot_CV(bot2.l$CV.tab)
#' 
#' @rdname reLDA
#' @export 
reLDA <- function(LDA, newdata){
  UseMethod("reLDA")
}

#' @rdname reLDA
#' @export 
reLDA.default <- function(LDA, newdata){
  stop(" * method only defined for LDA objects")
}

#' @rdname reLDA
#' @export 
reLDA.LDA <- function(LDA, newdata){
  if (missing(newdata) | !any(class(newdata) == "PCA"))
    stop(" * a PCA object must be provided")
  mod <- LDA$mod
  nr <- ncol(LDA$x)
  reLDA <- predict(mod, newdata$x[, 1:nr])
  #   return(reLDA)
  if (length(LDA$f0)==1) {
    actual <- newdata$fac[, LDA$f0]
    if (!is.null(actual)) {
      reLDA$res <- data.frame(actual=actual, classified=reLDA$class)
      reLDA$CV.tab <- table(reLDA$res)
      reLDA$CV.correct <- sum(diag(reLDA$CV.tab)) / sum(reLDA$CV.tab)
    }
  }
  reLDA$newdata <- newdata$x[, 1:nr]
#   class(reLDA) <- "LDA"
  return(reLDA)
}



